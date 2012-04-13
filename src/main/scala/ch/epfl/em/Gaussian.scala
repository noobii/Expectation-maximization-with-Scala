package ch.epfl.em

import scala.Array.canBuildFrom

import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

object Gaussian {

  def main(args: Array[String]): Unit = {
        
    
    
  }

  // TODO write test suite
  def initEm(
      data: DenseMatrix[Double], 
      gaussianComp: Int
      ): (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) = {

    val toto = Kmean.kmeans(data, gaussianComp, Int.MaxValue)
    // TODO must rewrite kmeans to finish the function
    null
  }

  /**
   * The implementation of the Expecatation-maximization algorithm
   * TODO !!! WAY to many arguments and return values !!!
   * TODO Write test suite
   */
  def em(
      data: DenseMatrix[Double], 
      gaussianComp: Int, 
      estW: DenseVector[Double], 
      estM: DenseMatrix[Double], 
      estC: Array[DenseMatrix[Double]], 
      likelih: Double, 
      maxIter: Int
      ): (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]], Double) = {

    var iterations = 0;
    
    var Ln = likelih
    var Lo = 2 * Ln
    
    def approxGoodEnough = !(abs(100*(Ln - Lo) / Lo) > likelih)
    
    var lEstW: DenseVector[Double] = estW
    var lEstM: DenseMatrix[Double] = estM
    var lEstC: Array[DenseMatrix[Double]] = estC
    
    while(!approxGoodEnough && (iterations < maxIter)) {
      val exp = expectation(data, gaussianComp, lEstW, lEstM, lEstC)
      val maxRes = maximization(data, gaussianComp, exp)
      Lo = Ln
      Ln = likelihood(data, gaussianComp, lEstW, lEstM, lEstC)
      iterations += 1
    }
    
    (lEstW, lEstM, lEstC, Ln)
  }

  /**
   * Expectation part of the algorithm.
   * Return the expectation of the value
   */
  def expectation(
      data: DenseMatrix[Double], 
      gaussianComp: Int, 
      estW: DenseVector[Double], 
      estM: DenseMatrix[Double], 
      estC: Array[DenseMatrix[Double]]
      ): DenseMatrix[Double] = {

    val n = data.numRows
    val d = data.numCols
    val a = pow(2 * Math.Pi, d / 2);
    
    val matrixOfZeros = DenseMatrix.zeros[Double](d, d)
    
    val nEstC = estC map{matrix => 
      if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](d, d)(Double.MinValue)
      else matrix
    }
    
    val S = nEstC.map(matrix => sqrt(det(matrix)))
    val invEstC = nEstC.map(matrix => inv(matrix))


    val E = DenseMatrix.tabulate[Double](n, gaussianComp)((i, j) => {
        val dXM = data(i, ::).t - estM(::, j)
        val coef = dXM.t * invEstC(j) * dXM
        val pl = exp(-0.5 * coef) / (a * S(j))
      
        estW(j) * pl
      }
    )
    
    // Don't know yet how to make this any more functionnal... scalala doesn't provide a map per line
    for(i <- 0 until E.numRows) E(i, ::) := E(i, ::) :/ E(i, ::).sum
    
    //val E = DenseMatrix.zeros[Double](n, gaussianComp)
    /*
    for(i <- 0 until n) {
      for(j <- 0 until gaussianComp) {
        val dXM = data(i, ::).t - estM(::, j)
        val coef = dXM.t * invEstC(j) * dXM
        val pl = exp(-0.5 * coef) / (a * S(j))
 
        E(i, j) = estW(j) * pl
      }
       
      E(i, ::) := E(i, ::) :/ E(i, ::).sum
    }*/
    
    E
  }

  /**
   * Maximization part of the algorithm.
   * Returns Estimated weight, mean and covariance
   */
  def maximization(data: DenseMatrix[Double], gaussianComp: Int, estimate: DenseMatrix[Double]):
     (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) = {
    
    val n = data.numRows
    val d = data.numCols

    // Sets all the estimate to zero
    val estW = DenseVector.zeros[Double](gaussianComp)
    val estM = DenseMatrix.zeros[Double](d, gaussianComp)
    val estC = (1 to gaussianComp).toArray.map(_ => DenseMatrix.zeros[Double](d, d))
    
    for(i <- 0 until gaussianComp) {
      for(j <- 0 until n) {
        estW(i) = estW(i) + estimate(j, i)
        estM(::, i) := estM(::, i) + (data(j, ::).t * estimate(j, i))
      }
      estM(::, i) := estM(::, i) / estW(i)
    }

    for(i <- 0 until gaussianComp) {
      for(j <- 0 until n) {
        val dXM = data(j, ::).t - estM(::, i)
        estC(i) = estC(i) + ((dXM * dXM.t) :* estimate(j, i))
      }
      estC(i) = estC(i) / estW(i)
    }
    
    estW := estW / n
    
    // Returns the estimate weigth, mean and covariance
    (estW, estM, estC)
  }

  /**
   * Computes the log-likelihood that the estimated values are correct.
   */
  def likelihood(
      data: DenseMatrix[Double], 
      gaussianComp: Int, 
      estW: DenseVector[Double], 
      estM: DenseMatrix[Double], 
      estC: Array[DenseMatrix[Double]]
      ): Double = {

    val n = data.numRows
    
    val meanVect = mean(data, Axis.Vertical).asCol // OK
    val covarianceMat = covariance(data, Axis.Vertical)._1 // OK
    
    var L = 0.0;
    
    for(i <- 0 until gaussianComp) {
      val invEstC = inv(estC(i))
      
      val lg = log(det(estC(i) * 2 * Math.Pi))
      val tr = (invEstC * covarianceMat).trace + (meanVect - estM(::, i)).t * invEstC * (meanVect - estM(::, i))
      
      L += estW(i) * (-0.5 * n * lg - 0.5 * (n-1) * tr)
    }
    
    L
  } 

}