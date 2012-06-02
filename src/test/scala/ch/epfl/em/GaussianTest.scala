package ch.epfl.em

import org.junit.Assert.assertFalse
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

import ch.epfl.em.NumericalChecks._
import ch.epfl.em.NumericalChecks.CheckVector._
import ch.epfl.em.NumericalChecks.CheckMatrix._
import ch.epfl.em.NumericalChecks.CheckArrayOfMatrices._
import ch.epfl.em.NumericalChecks.CheckDouble._
import ch.epfl.em.Conversions._

class GaussianTest extends AssertionsForJUnit {
  
  def freshTestValues = {
    
    var X = for(i <- 0 until 10) yield {
      if(i < 5) DenseVector.ones[Double](2)
      else DenseVector.ones[Double](2) :* 2
    }
    
    val k = 2
    
    val W = DenseVector(0.5, 0.5).asRow
    val M = DenseMatrix((1., 2.), (1., 2.))
    
    val V = Array(DenseMatrix.eye[Double](2, 2), DenseMatrix.eye[Double](2, 2))
    
    val gaussian = new GaussianSequential(null)(X, k)
    
    (gaussian, X, k, MatricesTupple(W, M, V))
  }
  
  @Test def compareImplementations() {
    
    val k = 6
    val X = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy = new InitFromMatlab("src/test/ressources/em/50k/")
    
    val gaussianMenthor = new GaussianMenthor(strategy)(X, k)
    val gaussianClassic = new GaussianSequential(strategy)(X, k)
    
    val classicEstimates = gaussianClassic.runAlgo(maximumIterations = 1)
    val menthorEstimates = gaussianMenthor.runAlgo(maximumIterations = 1)
    
    assert(classicEstimates.weights closeEnough menthorEstimates.weights)
    assert(classicEstimates.means closeEnough menthorEstimates.means)
    assert(classicEstimates.covariances closeEnough menthorEstimates.covariances)
  }
  
  @Test def testExpectation() {
    val (gaus, a, b, mat) = freshTestValues
    
    val exp = gaus.expectation(mat)
    
    val matlabExpVal = DenseMatrix(
      (0.7311,    0.2689),
      (0.7311,    0.2689),
      (0.7311,    0.2689),
      (0.7311,    0.2689),
      (0.7311,    0.2689),
      (0.2689,    0.7311),
      (0.2689,    0.7311),
      (0.2689,    0.7311),
      (0.2689,    0.7311),
      (0.2689,    0.7311)
    )
    
    assert(matlabExpVal closeEnough dataGenSeqToMat(exp))
  }
  
  
  @Test def testMaximization() {
    val (gaus, a, b, mat) = freshTestValues
    
    //val E = DenseMatrix.ones[Double](a.length, a.head.length)

    val E = (0 until a.length).toArray map (x => DenseVector.ones[Double](a.head.length))
    
    val res = gaus.maximization(E)
    
    val matlab1 = DenseVector(1.0, 1.0)
    val matlab2 = DenseMatrix((1.5, 1.5), (1.5, 1.5))
    val matlab3 = Array(DenseMatrix((0.25, 0.25), (0.25, 0.25)), DenseMatrix((0.25, 0.25), (0.25, 0.25)))
    
    assert(res.weights closeEnough matlab1)
    assert(res.means closeEnough matlab2)
    assert(res.covariances closeEnough matlab3)
  }
  
  @Test def testLoglikelihood() {
    // Why doesnt (X, k, W, M, V) work ?!!?!
    val (gaus, a, b, mat) = freshTestValues
    
    val res = gaus.likelihood(mat)
    
    // Value computed with Matlbab with the same inputs
    val matlabRes = 2 * -11.5643
    
    assert(res closeEnough matlabRes)
  }
}