package ch.epfl.em
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scala.math.abs
import scala.math.min
import scala.math.max
import NumericalChecks.CheckMatrix._
import NumericalChecks.CheckDouble._

object NumericalChecks {

  def delta = 0.01 // 1%
  
  def closeEnough(double1: Double, double2: Double): Boolean = {
    val small = min(double1, double2)
    val big = max(double1, double2)
    
    ((big - small) / big) < delta
  }
  
  def closeEnough(mat1: DenseMatrix[Double], mat2: DenseMatrix[Double]): Boolean = {
    
    (mat1.numCols == mat2.numCols && mat1.numRows == mat2.numRows) && {
      
      mat1.forallPairs{case((i, j), value) => value closeEnough mat2(i, j)}
      
    }

  }
  
  def closeEnough(vect1: DenseVector[Double], vect2: DenseVector[Double]): Boolean = {
    (vect1.size == vect2.size) && {
      
      vect1.forallPairs{case(i, value) => value closeEnough vect2(i)}
    }
  }
  
  def closeEnough(cov1: Array[DenseMatrix[Double]], cov2: Array[DenseMatrix[Double]]): Boolean = {
    (cov1.length == cov2.length) && ((cov1 zip cov2) forall {case(mat1, mat2) => mat1 closeEnough mat2})
  }
  
  // TODO is there a way to make them generic? 
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

  class CheckArrayOfMatrices(array: Array[DenseMatrix[Double]]) {
    def closeEnough(otherArray: Array[DenseMatrix[Double]]): Boolean = NumericalChecks.closeEnough(array, otherArray)
  }

  object CheckArrayOfMatrices {
    implicit def arrayOfMatrices2CheckArrayOfMatrices(array: Array[DenseMatrix[Double]]) = new CheckArrayOfMatrices(array)
  }
  
  class CheckDouble(double: Double) {
    def closeEnough(otherDouble: Double): Boolean = NumericalChecks.closeEnough(double, otherDouble)
  }
  
  object CheckDouble {
    implicit def double2CheckDouble(double: Double): CheckDouble = new CheckDouble(double)
  }
}
