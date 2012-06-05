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
import scala.collection.parallel.immutable.ParVector

/**
 * The parallel implementation uses exactly the same code as the classic implementation.
 * The only difference is the input. When creating the object a parallel version of the
 * input data is used.
 */
class GaussianParallel(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int)
    extends GaussianCollections(initStrategy)(dataIn.toParArray, gaussianComponents)

class GaussianSequential(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int)
    extends GaussianCollections(initStrategy)(dataIn.seq, gaussianComponents)
    
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
      val exp = expectation(newEstimates)
      newEstimates = maximization(exp)
            
      oldLikelihood = newLikelihood
      newLikelihood = likelihood(newEstimates)
      
      //println(iterations)
      iterations += 1
    }
    toc("algo")
    
    (newEstimates, newLikelihood, iterations)
  }

  /**
   * Expectation part of the algorithm.
   * Return the expectation of the value
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

    val expec = data map(point => {
      val vector = DenseVector.tabulate[Double](gaussianComponents)(j => {

        val delta = point.asCol - estimates.means(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))
        
        estimates.weights(j) * pl
      })
      
      normalize(vector)
    })
    
    expec
  }

  /**
   * Maximization part of the algorithm.
   * Returns Estimated weight, mean and covariance
   */
  def maximization(estimate: GenSeq[DenseVector[Double]]): MatricesTupple = {
        
    val estWeight = estimate reduce(_ + _)
        
    // The weights repeated in each line of a (dim, gaussianComp) matrix
    val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * estWeight.asRow
    
    val estMean = ((data zip estimate) map {case(point, est) => point.asCol * est.asRow} reduce(_ + _)) :/ weightsAsMatrix
        
    val estCovariance = (0 until gaussianComponents).toArray map(k => {
      // Hotest point in the algo
      
      val sumMat = ((data zip estimate) map {case(point, est) =>
        val delta = point.asCol - estMean(::, k)
        (delta * delta.t) :* est(k)
      }) reduce (_ + _)
      
      /*
      def co(point: DenseVector[Double], est: DenseVector[Double]) = {
        val dXM = point.asCol - estMean(::, k)
        (dXM * dXM.t) :* est(k)
      }
      
      val sumMat = ((data zip estimate).foldLeft(DenseMatrix.zeros[Double](dimensions, dimensions)){
          case(runingSum, (point, est)) => runingSum + co(point, est)}
      )*/
            
      sumMat :/ estWeight(k)
    })
    
    /*
    // This is crazy impossible to understand but was just a test to check if it maybe was runing faster...
    val estCovariance = ((data zip estimate) map {case(point, est) => 
        
        (0 until gaussianComponents).toArray map {k => {
          val delta = point.asCol - estMean(::, k)
          (delta * delta.t) :* (est(k) / estWeight(k))
        }}
        
      }) reduce((x, y) => (x zip y) map (z => z._1 + z._2)) */
    
    estWeight := estWeight / measurements
    
    MatricesTupple(estWeight, estMean, estCovariance)
  } 

}