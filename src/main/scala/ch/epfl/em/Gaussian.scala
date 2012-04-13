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

    val dimensions = data.numCols

    val nEstC = estC map{matrix => 
      if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
      else matrix
    }
    
    val S = nEstC.map(matrix => sqrt(det(matrix)))
    val invEstC = nEstC.map(matrix => inv(matrix))

    val a = pow(2 * Math.Pi, dimensions / 2.0)

    val E = DenseMatrix.tabulate[Double](data.numRows, gaussianComp)((i, j) => {
        val delta = data(i, ::).asCol - estM(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))
      
        estW(j) * pl
      }
    )
    
    // Make all row have 1 as element sum
    // Don't know yet how to make this any more functionnal... scalala doesn't provide a map per line
    for(i <- 0 until E.numRows) E(i, ::) := E(i, ::) :/ E(i, ::).sum
    
    E
  }

  /**
   * Maximization part of the algorithm.
   * Returns Estimated weight, mean and covariance
   */
  def maximization(data: DenseMatrix[Double], gaussianComp: Int, estimate: DenseMatrix[Double]):
     (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) = {
    
    val measurements = data.numRows
    val dimensions = data.numCols

    val estW = DenseVector.tabulate(gaussianComp)(i => estimate(::, i).sum)
    
    val estM = DenseMatrix.tabulate[Double](dimensions, gaussianComp)((dim, comp) => {
      val col: DenseVector[Double] = data(::, dim)
      
      val weightCol = col.mapPairs((index: Int, value: Double) => value * estimate(index, comp))
      
      val weightSum = weightCol.sum / estW(comp)
      weightSum
    })

    val estC = (0 until gaussianComp) map(index => {
      val matrix = DenseMatrix.zeros[Double](dimensions, dimensions)
      for(j <- 0 until measurements) {
        val dXM = data(j, ::).asCol - estM(::, index)
        matrix += (dXM * dXM.t) :* estimate(j, index)
      }
      matrix := matrix / estW(index)
    }) toArray
    
    estW := estW / measurements
    
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

    val measurements = data.numRows
    
    val meanVect = mean(data, Axis.Vertical).asCol // OK
    val covarianceMat = covariance(data, Axis.Vertical)._1 // OK

    val estCWithIndex = estC zipWithIndex

    val L = estCWithIndex.map(x => {
      val (mat, i) = x
      val invEstC = inv(mat)
      
      val lg = log(det(mat * 2 * Math.Pi))
      val tr = (invEstC * covarianceMat).trace + (meanVect - estM(::, i)).t * invEstC * (meanVect - estM(::, i))
      
      estW(i) * (-0.5 * measurements * lg - 0.5 * (measurements - 1) * tr)
    }).sum
    
    L
  } 

}