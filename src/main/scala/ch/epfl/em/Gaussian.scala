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
        
    val runConfigs = RunConfiguration.load("src/main/ressources/data/benchmark-run.xml")
    
    println("Available cores: " + Runtime.getRuntime().availableProcessors())
    
    for(rc <- runConfigs) {
      printStatus("Runing: " + rc.name) 
      
      // Initializes the strategy so we it is equal for all runs
      rc.initStrategy
      
      for(i <- 1 to 5) {
        printStatus("Iteration #" + i) 
        
        printStatus("Classic implementation")
        val classic = new GaussianClassic(rc.strategy)(rc.data, rc.k)
        classic.runAlgo()
        
        printStatus("Parrallel implementation")
        val parallel = new GaussianClassic(rc.strategy)(rc.parData, rc.k)
        parallel.runAlgo()
        
        printStatus("Menthor implementation")
        val menthor = new GaussianMenthor(rc.strategy)(rc.data, rc.k)
        menthor.runAlgo()
      }
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

    println("iterations: " + iter)
    
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
