package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

class GaussianTest extends AssertionsForJUnit {

  def delta = 0.1
  
  def freshTestValues = {
    val X = DenseMatrix.ones[Double](10, 2)
    X(5 until 10, ::) := DenseMatrix.ones[Double](5, 2) :* 2
    
    val k = 2
    
    val W = DenseVector(0.5, 0.5).asRow
    val M = DenseMatrix((1., 2.), (1., 2.))
    
    val V = Array(DenseMatrix.eye[Double](2, 2), DenseMatrix.eye[Double](2, 2))
    
    val gaussian = new Gaussian(X, k)
    
    (gaussian, X, k, W, M, V)
  }
  
  @Test def testInitEm() {
    
  }
  
  @Test def testEM() {
    
  }
  
  @Test def testExpectation() {
    val (gaus, a, b, c, d, e) = freshTestValues
    
    val exp = gaus.expectation(c, d, e)
    
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
    
    assert((matlabExpVal(::, 0) - exp(::, 0)).norm(2) < delta)
    assert((matlabExpVal(::, 1) - exp(::, 1)).norm(2) < delta)
  }
  
  
  @Test def testMaximization() {
    val (gaus, a, b, c, d, e) = freshTestValues
    
    val E = DenseMatrix.ones[Double](a.numRows, a.numCols)

    val res = gaus.maximization(E)
    
    val matlab1 = DenseVector(1.0, 1.0)
    val matlab2 = DenseMatrix((1.5, 1.5), (1.5, 1.5))
    val matlab3 = Array(DenseMatrix((0.25, 0.25), (0.25, 0.25)), DenseMatrix((0.25, 0.25), (0.25, 0.25)))
    
    assert(res._1 == matlab1)
    assert(res._2 == matlab2)
    assert(res._3(0) == matlab3(0))
    assert(res._3(1) == matlab3(1))
  }
  
  @Test def testLoglikelihood() {
    // Why doesnt (X, k, W, M, V) work ?!!?!
    val (gaus, a, b, c, d, e) = freshTestValues
    
    val res = gaus.likelihood(c, d, e)
    
    // Value computed with Matlbab with the same inputs
    val matlabResult = 2 * -11.5643
    
    assert(matlabResult < res + delta && matlabResult > res - delta)
  }
}