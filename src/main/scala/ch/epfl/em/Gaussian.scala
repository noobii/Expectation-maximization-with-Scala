package ch.epfl.em
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import scalala.operators._
import scalala.tensor._
import scalala.library.LinearAlgebra._
import scalala.library.Library._
import scalala.tensor.dense.DenseVectorRow
import scala.util.Random
import scalala.tensor.dense.DenseVectorCol

object Gaussian {

  def main(args: Array[String]): Unit = {
        
    
    
  }

  def em(data: DenseMatrix[Double], gaussianComp: Int, estW: DenseVector[Double], estM: DenseMatrix[Double], estC: Array[DenseMatrix[Double]], likelihood: Double, maxIter: Int):
        (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]], Double) = {
    /*
     * [W,M,V,L]
     * %%%% EM algorithm %%%%
		niter = 0;
		while (abs(100*(Ln-Lo)/Lo)>ltol) & (niter<=maxiter),
		    E = Expectation(X,k,W,M,V); % E-step    
		    [W,M,V] = Maximization(X,k,E);  % M-step
		    Lo = Ln;
		    Ln = Likelihood(X,k,W,M,V);
		    niter = niter + 1;
		end 
		L = Ln;
     */
    
    var iterations = 0;
    
    // TODO def
    var Ln = 0.0;
    var Lo = 0.0;
    
    
    
    while((abs(100*(Ln - Lo) / Lo) > likelihood) && (iterations < maxIter)) {
      val E = expectation(data, gaussianComp, estW, estM, estC)
    }
    
    null
    
  }
  
  def expectation(data: DenseMatrix[Double], gaussianComp: Int, estW: DenseVector[Double], estM: DenseMatrix[Double], estC: Array[DenseMatrix[Double]]): DenseMatrix[Double] = {
    val n = data.numRows
    val d = data.numCols
    val a = pow(2 * Math.Pi, d / 2);
    
    val S = DenseVector.zeros[Double](gaussianComp)
    
    val invEstC = (1 to gaussianComp).toArray.map(_ => DenseMatrix.zeros[Double](d, d))
    
    for(j <- 0 until gaussianComp) {
      if(estC(j) == DenseMatrix.zeros[Double](d, d)) {
        estC(j) = DenseMatrix.ones[Double](d, d) * Double.MinValue
      }
        
      S(j) = sqrt(det(estC(j)))
      invEstC(j) = inv(estC(j))
   }

     val E = DenseMatrix.zeros[Double](n, gaussianComp)
     for(i <- 0 until n) {
       for(j <- 0 until gaussianComp) {
         val dXM = data(i, ::).t - estM(::, j)
         val toto = dXM.t * invEstC(j) * dXM
         val pl = exp(-0.5 * toto) / (a * S(j))
         
         E(i, j) = estW(j) * pl
       }
       
       E(i, ::) := E(i, ::) :/ E(i, ::).sum
     }
    
    E
  }

  /**
   * Returns Estimated weight, mean and covariance
   */
  def maximization(data: DenseMatrix[Double], gaussianComp: Int, E: DenseMatrix[Double]):
     (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) = {
    
    val n = data.numRows
    val d = data.numCols
    val estW = DenseVector.zeros[Double](gaussianComp)
    val estM = DenseMatrix.zeros[Double](d, gaussianComp)
    val estC = (1 to gaussianComp).toArray.map(_ => DenseMatrix.zeros[Double](d, d))
    
    for(i <- 0 until gaussianComp) {
      for(j <- 0 until n) {
        estW(i) = estW(i) + E(j, i)
        estM(::, i) := estM(::, i) + (data(j, ::).t * E(j, i))
      }
      estM(::, i) := estM(::, i) / estW(i)
    }
    for(i <- 0 until gaussianComp) {
      for(j <- 0 until n) {
        val dXM = data(j, ::).t - estM(::, i)
        estC(i) = estC(i) + ((dXM * dXM.t) :* E(j, i))
      }
      estC(i) = estC(i) / estW(i)
    }
    
    estW := estW / n
    
    (estW, estM, estC)
  }

  // CHECKED
  def likelihood(data: DenseMatrix[Double], gaussianComp: Int, estW: DenseVector[Double], estM: DenseMatrix[Double], estC: Array[DenseMatrix[Double]]): Double = {
    val n = data.numRows
    val d = data.numCols
    
    val U = mean(data, Axis.Vertical).asCol // OK
    val S = covariance(data, Axis.Vertical)._1 // OK
    
    var L = 0.0;
    
    for(i <- 0 until gaussianComp) {
      val iV = inv(estC(i))
      
      val lg = log(det(estC(i) * 2 * Math.Pi))
      val tr = (iV * S).trace + (U - estM(::, i)).t * iV * (U - estM(::, i))
      
      L += estW(i) * (-0.5 * n * lg - 0.5 * (n-1) * tr)
    }
    
    L
  } 

}