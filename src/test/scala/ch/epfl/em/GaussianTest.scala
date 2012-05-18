package ch.epfl.em

import org.junit.Assert.assertFalse
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

import ch.epfl.em.NumericalChecks._
import ch.epfl.em.Conversions._

class GaussianTest extends AssertionsForJUnit {

  def delta = 0.1
  
  def freshTestValues = {
    
    var X = for(i <- 0 until 10) yield {
      if(i < 5) DenseVector.ones[Double](2)
      else DenseVector.ones[Double](2) :* 2
    }
    
    val k = 2
    
    val W = DenseVector(0.5, 0.5).asRow
    val M = DenseMatrix((1., 2.), (1., 2.))
    
    val V = Array(DenseMatrix.eye[Double](2, 2), DenseMatrix.eye[Double](2, 2))
    
    val gaussian = new GaussianClassic(null)(X, k)
    
    (gaussian, X, k, MatricesTupple(W, M, V))
  }
  
  @Test def compareImplementations() {
    
    val k = 6
    val X = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy = new InitFromMatlab("src/test/ressources/em/50k/")
    
    val gaussianMenthor = new GaussianMenthor(strategy)(X, k)
    val gaussianClassic = new GaussianClassic(strategy)(X, k)
    
    val classicEstimates = gaussianClassic.runAlgo(maximumIterations = 1)
    val menthorEstimates = gaussianMenthor.runAlgo(maximumIterations = 1)
    
    assert(closeEnough(classicEstimates.weights, menthorEstimates.weights))
    assert(closeEnough(classicEstimates.means, menthorEstimates.means))
    assert(closeEnough(classicEstimates.covariances, menthorEstimates.covariances))
  }
  
  @Test def testInitEm() {
    
  }
  
  @Test def testEM() {
    
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
    
    assert(closeEnough(matlabExpVal, dataGenSeqToMat(exp)))
  }
  
  
  @Test def testMaximization() {
    val (gaus, a, b, mat) = freshTestValues
    
    //val E = DenseMatrix.ones[Double](a.length, a.head.length)

    val E = (0 until a.length).toArray map (x => DenseVector.ones[Double](a.head.length))
    
    val res = gaus.maximization(E)
    
    val matlab1 = DenseVector(1.0, 1.0)
    val matlab2 = DenseMatrix((1.5, 1.5), (1.5, 1.5))
    val matlab3 = Array(DenseMatrix((0.25, 0.25), (0.25, 0.25)), DenseMatrix((0.25, 0.25), (0.25, 0.25)))
    
    assert(res.weights == matlab1)
    assert(NumericalChecks.closeEnough(res.means, matlab2))
    assert(res.covariances(0) == matlab3(0))
    assert(res.covariances(1) == matlab3(1))
  }
  
  @Test def testLoglikelihood() {
    // Why doesnt (X, k, W, M, V) work ?!!?!
    val (gaus, a, b, mat) = freshTestValues
    
    val res = gaus.likelihood(mat)
    
    // Value computed with Matlbab with the same inputs
    val matlabResult = 2 * -11.5643
    
    assert(matlabResult < res + delta && matlabResult > res - delta)
  }
}