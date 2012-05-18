package ch.epfl.em
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scala.math.abs

object NumericalChecks {

  val delta = 0.01
  
  // TODO Could use implicits !!! =)
  
  def closeEnough(mat1: DenseMatrix[Double], mat2: DenseMatrix[Double]): Boolean = {
    
    if(mat1.numCols == mat2.numCols && mat1.numRows == mat2.numRows) {
      
      val diff = mat1 - mat2
            
      return diff.forallValues(abs(_) < delta)
      
    }
    
    false
  }
  
  def closeEnough(vect1: DenseVector[Double], vect2: DenseVector[Double]): Boolean = {
    if(vect1.size == vect2.size) {
      val diff = vect1 - vect2
      
      return diff.forallValues(abs(_) < delta)
    }
    
    false
  }
  
  def closeEnough(cov1: Array[DenseMatrix[Double]], cov2: Array[DenseMatrix[Double]]): Boolean = {
    cov1 foreach(println(_))
    println("------")
    cov2 foreach(println(_))
    
    if(cov1.length == cov2.length) {
      
      return (cov1 zip cov2) forall {case(mat1, mat2) => closeEnough(mat1, mat2)}
      
    }
    
    false
  }
}