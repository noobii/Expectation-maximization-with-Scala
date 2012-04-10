package ch.epfl.em

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.scalatest.FunSuite
import org.scalatest.junit.AssertionsForJUnit
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import scalala.operators._
import scalala.tensor._
import scalala.library.Library._
import scalala.library.LinearAlgebra._

class GaussianTest extends AssertionsForJUnit {

  def delta = 0.1
  
  @Test def testLoglikelihood() {
    val X = DenseMatrix.ones[Double](10, 2)
    X(5 until 10, ::) := DenseMatrix.ones[Double](5, 2) :* 2
    
    val k = 2
    
    val W = DenseVector(0.5, 0.5).asRow
    val M = DenseMatrix((1., 2.), (1., 2.))
    
    val V = Array(DenseMatrix.eye[Double](2, 2), DenseMatrix.eye[Double](2, 2))
    
    val res = Gaussian.likelihood(X, k, W, M, V)
    
    // Value computed with Matlbab with the same inputs
    val matlabResult = 2 * -11.5643
    
    assert(matlabResult < res + delta && matlabResult > res - delta)
  }
}