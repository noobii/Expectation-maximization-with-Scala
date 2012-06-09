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

/**
 * The parallel implementation of the GaussianCollection providing a sequential imput.
 */
class GaussianParallel(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int)
    extends GaussianCollections(initStrategy)(dataIn.par, gaussianComponents)

/**
 * The concrete implementation of the GaussianCollection providing a sequential imput.
 */
class GaussianSequential(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int)
    extends GaussianCollections(initStrategy)(dataIn.seq, gaussianComponents)

/**
 * Abstract implementation using generic collections. Should be sub-classed and provide a concrete data input. 
 */
abstract class GaussianCollections(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) 
    extends Gaussian(initStrategy)(dataIn, gaussianComponents) {

  /**
   * The implementation of the Expecatation-maximization algorithm
   */
  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int) = {

    var iterations = 0;
    
    // Initalizes the likelihood values
    var newLikelihood = minLikelihoodVar
    var oldLikelihood = Double.MaxValue
    
    // Determines if the likelihood variation is small engough to stop the iteration
    def hasConverged = (abs(100*(newLikelihood - oldLikelihood) / oldLikelihood) <= minLikelihoodVar)
    
    var newEstimates = estimates
    
    tic
    while(!hasConverged && (iterations < maximumIterations)) {
      println(iterations)
      val exp = expectation(newEstimates)
      newEstimates = maximization(exp)
            
      oldLikelihood = newLikelihood
      newLikelihood = likelihood(newEstimates)
      
      iterations += 1
    }
    toc("algo")
    
    (newEstimates, newLikelihood, iterations)
  }

  /**
   * Expectation part of the algorithm.
   * @param estimates the current estimates to be used
   * @return the current expectation values
   */
  def expectation(estimates: MatricesTupple): GenSeq[DenseVector[Double]] = {

    // Function to make the sum of the elements equal 1
    def normalize(v: DenseVector[Double]) = v :/ v.sum

    
    // Creates new empty covariances matrices if needed
    val estimatedCovariances = estimates.covariances map {matrix => 
      if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
      else matrix
    }
    
    // Computes values that are used later in the algo
    val S = estimatedCovariances map (matrix => sqrt(det(matrix)))
    val invEstC = estimatedCovariances map (matrix => inv(matrix))

    val a = pow(2 * Pi, dimensions / 2.0)

    // Corresponds to the computation of T(j,i)
    val expect = data map(point => {
      // Computes the multivariate normal distribution
      val vector = DenseVector.tabulate[Double](gaussianComponents)(j => {

        val delta = point.asCol - estimates.means(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))
        
        estimates.weights(j) * pl
      })
      
      normalize(vector)
    })
    
    expect
  }

  /**
   * Maximization part of the algorithm
   * @param expect the computed value from the expectation step
   * @return the expected weights, means and covariances
   */
  def maximization(expect: GenSeq[DenseVector[Double]]): MatricesTupple = {
        
    val estWeight = expect reduce(_ + _)
        
    // The weights repeated in each line of a [dim x gaussianComp] matrix. It is later used for element-wise divisions
    val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * estWeight.asRow
    
    val estMean = ((data zip expect) map {case(point, est) => point.asCol * est.asRow} reduce(_ + _)) :/ weightsAsMatrix

    val estCovariance = (0 until gaussianComponents).toArray map(k => {
      
      val sumMat = ((data zip expect) map {case(point, ex) =>
        val delta = point.asCol - estMean(::, k)
        (delta * delta.t) :* ex(k)
      }) reduce (_ + _)
            
      sumMat :/ estWeight(k)
    })
    
    estWeight := estWeight / measurements
    
    MatricesTupple(estWeight, estMean, estCovariance)
  } 

}