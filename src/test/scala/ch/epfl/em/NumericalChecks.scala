package ch.epfl.em
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scala.math.abs
import CheckMatrix.matrix2CheckMatrix

object NumericalChecks {

  val delta = 0.01
  
  def closeEnough(mat1: DenseMatrix[Double], mat2: DenseMatrix[Double]): Boolean = {
    
    (mat1.numCols == mat2.numCols && mat1.numRows == mat2.numRows) && {
      
      val diff = mat1 - mat2
            
      diff.forallValues(abs(_) < delta)
      
    }

  }
  
  def closeEnough(vect1: DenseVector[Double], vect2: DenseVector[Double]): Boolean = {
    (vect1.size == vect2.size) && {
      val diff = vect1 - vect2
      
      diff.forallValues(abs(_) < delta)
    }
  }
  
  def closeEnough(cov1: Array[DenseMatrix[Double]], cov2: Array[DenseMatrix[Double]]): Boolean = {
    (cov1.length == cov2.length) && ((cov1 zip cov2) forall {case(mat1, mat2) => mat1 closeEnough mat2})
  }
}

class CheckMatrix(matrix: DenseMatrix[Double]) {  
  def closeEnough(otherMatrix: DenseMatrix[Double]): Boolean = NumericalChecks.closeEnough(matrix, otherMatrix)
}

object CheckMatrix {
  implicit def matrix2CheckMatrix(matrix: DenseMatrix[Double]): CheckMatrix = new CheckMatrix(matrix)
}

class CheckVector(vector: DenseVector[Double]) {
  def closeEnough(otherVector: DenseVector[Double]): Boolean = NumericalChecks.closeEnough(vector, otherVector)
}

object CheckVector {
  implicit def vector2CheckVector(vector: DenseVector[Double]): CheckVector = new CheckVector(vector)
}