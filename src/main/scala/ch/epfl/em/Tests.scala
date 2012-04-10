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

object Tests {

  def main(args: Array[String]): Unit = {
        
    val X = DenseMatrix.ones[Double](10, 2)
    for(i <- 5 until 10) X(i, ::) := X(i, ::) * 2
    
    println("X: \n" + X)
    
    println("Res: \n" + Kmean.kmeans(X, 2, 10000))
    
  }
  
}