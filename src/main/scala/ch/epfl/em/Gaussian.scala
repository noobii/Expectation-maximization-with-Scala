package ch.epfl.em

import scala.Array.canBuildFrom
import scala.math.Pi
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._
import scala.collection.GenSeq
import ch.epfl.em.Conversions._

case class MatricesTupple(weights: DenseVector[Double], means: DenseMatrix[Double], covariances: Array[DenseMatrix[Double]])

object Gaussian {
  def main(args: Array[String]): Unit = {
    
    println("Available cores: " + Runtime.getRuntime().availableProcessors())
    
    for(i <- 0 until 5) {
	    printStatus("Runing algo 50k")
	    
	    val k50k = 6
	    val X50k = FileParser("src/test/ressources/em/50k/X.csv").data
	    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
	    
	    val gaussian50k = new GaussianClassic(strategy50k)(X50k, k50k)
	    
	    val out = gaussian50k.runAlgo()
	    
	    printStatus("Runing algo 600k")
	    val k500k = 5
	    val X500k = FileParser("src/test/ressources/em/500k/X.csv").data
	    val strategy500k = new InitFromMatlab("src/test/ressources/em/500k/")
	    
	    val gaussian500k = new GaussianClassic(strategy500k)(X500k, k500k)
	    
	    val out500k = gaussian500k.runAlgo()
	    
	    printStatus("Runing algo 1M")
	    val k1M = 5
	    val X1M = FileParser("src/test/ressources/em/1M/X.csv").data
	    val strategy1M = new InitFromMatlab("src/test/ressources/em/1M/")
	    val gaussian1M = new GaussianClassic(strategy1M)(X1M, k1M)
	    
	    val out1M = gaussian1M.runAlgo()
    }
  }
  
  def printStatus(text: String) {
    val now = new java.util.Date
    
    println(now + ": " + text)
  }
  
}

abstract class Gaussian(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) {
  import Gaussian.printStatus // is this really the best way to do it?

  val measurements = dataIn.length
  val dimensions = dataIn.head.length
  
  protected val data = dataIn
  
  /**
   * The implementation of the Expecatation-maximization algorithm
   */
  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int
      ): (MatricesTupple, Double, Int)
  
  def runAlgo(minLikelihoodVar: Double = 0.05, maximumIterations: Int = 1000) = {
    
    val initial = initStrategy.init
        
    printStatus("Run algo"); 
    GChrono.start
    val (est, lg, iter) = em(initial, minLikelihoodVar, maximumIterations)
    GChrono.stop
    println("estTime: " + GChrono.count/1000.0)

    GChrono.reset
    
    est
  }
  
  // This data will be used several times and do not need to be recomputed
  var dataMean = (data reduce(_ + _)).asCol / measurements
  var dataCovariance = covariance(dataGenSeqToMat(data), Axis.Vertical)._1

  
  /**
   * Computes the log-likelihood that the estimated values are correct.
   */
  def likelihood(estimate: MatricesTupple): Double = {

    val estCWithIndex = estimate.covariances zipWithIndex

    // Thought it could be an elegant solution to use a map with indices
    val elements = estCWithIndex map {
      case (matrix, index) => {
        val invEstC = inv(matrix)
      
        val lg = log(det(matrix * 2 * Pi))
        val tr = (invEstC * dataCovariance).trace + (dataMean - estimate.means(::, index)).t * invEstC * (dataMean - estimate.means(::, index))
      
        estimate.weights(index) * (-0.5 * measurements * lg - 0.5 * (measurements - 1) * tr)
      }
    }
    
    elements.sum
  }
}
