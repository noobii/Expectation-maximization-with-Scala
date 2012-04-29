package ch.epfl.em

import scala.Array.canBuildFrom
import scala.io.Source.fromFile

import scalala.operators.Implicits.richArray
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector


object FileParser {

  def toVectorSeq(fileName: String): Array[DenseVector[Double]] = {
    def lineToVector(line: String): DenseVector[Double] = line.split(',').map(_.toDouble).asVector.asRow 

    processFile(fileName, lineToVector(_)) toArray

  }  
  
  def toMatrix(fileName: String): DenseMatrix[Double] = {
    def lineToMatrix(line: String): DenseMatrix[Double] = line.split(',').map(_.toDouble).asMatrix(1)

    val lineMatrices = processFile(fileName, lineToMatrix(_))
    
    val matrix = lineMatrices reduce (DenseMatrix.vertcat(_, _))
    
    matrix
  }

  def toVector(fileName: String): DenseVector[Int] = processFile(fileName, _.toInt).toArray.asVector
  
  private def processFile[V](fileName: String, applyToLine: String => V) = {
    val source = fromFile(fileName)
    
    val lines = source getLines() toArray
    
    val maped = lines map(applyToLine(_))
            
    source.close()
        
    maped
  }
  
}