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
import Gaussian.printStatus
import scala.io.Source

case class MatricesTupple(weights: DenseVector[Double], means: DenseMatrix[Double], covariances: Array[DenseMatrix[Double]])

object Gaussian {
  def main(args: Array[String]): Unit = {
    
    // Default number of times the algo will be run
    val defaultNumberOfRuns = 5
    
    // If the parameter is given we use it else we take the default
    val numberOfRuns = if(args.isDefinedAt(0)) args(0).toInt else defaultNumberOfRuns
    
    // Loads the configuration from file
    val runConfigs = RunConfiguration.load("src/main/ressources/data/benchmark-run.xml")
    
    // Informations about the environement
    val runtime = Runtime.getRuntime()
    println("Available cores: " + runtime.availableProcessors())
    println("Total Memory: " + runtime.totalMemory());
    println("Max Memory: " + runtime.maxMemory());
    
    // The configurations are run sequentially
    for(rc <- runConfigs) {
      printStatus("Runing: " + rc.name) 
      
      // Initializes the strategy beforehand so we it is equal for all runs
      rc.initStrategy
      
      for(i <- 1 to numberOfRuns) {
        println("Iteration #" + i + "-----------------------------------------") 
        
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
  
  /**
   * Handy method to see what is going on and when.
   */
  def printStatus(text: String) {
    val now = new java.util.Date
    
    println(now + ": " + text)
  }
  
}

abstract class Gaussian(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) {

  val measurements = dataIn.length
  val dimensions = dataIn.head.length
  
  // Not really necessary anymore
  protected val data = dataIn
  
  /**
   * The expectation-maximization algorithm. Must be implemented.
   */
  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int): (MatricesTupple, Double, Int)
  
  /**
   * Runs the em algo. The method has default values to be called with only runAlgo()
   */
  def runAlgo(
      minLikelihoodVar: Double = 0.05, 
      maximumIterations: Int = 1000) = {
    
    val initial = initStrategy.init
        
    GChrono.start
    val (est, lg, iter) = em(initial, minLikelihoodVar, maximumIterations)
    GChrono.stop
    
    // TODO cleanup
    printStatus("time: " + GChrono.count/1000.0)
    
    GChrono.reset
    
    est
  }
  
  // This data will be used several times by the likelihood method
  var dataMean = ((data reduce(_ + _)) / measurements).asCol
  var dataCovariance = covariance(dataGenSeqToMat(data), Axis.Vertical)._1

  
  /**
   * Computes the log-likelihood that the estimated values are correct.
   */
  def likelihood(estimate: MatricesTupple): Double = {

    val estCWithIndex = estimate.covariances zipWithIndex

    // Algo to compute the likelihood value
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
