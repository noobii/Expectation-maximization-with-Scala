package ch.epfl.em

import scala.Array.canBuildFrom
import scala.io.Source.fromFile

import scalala.operators.Implicits.richArray
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector


case class FileParser(var fileName: String) {

  val separator = System.getProperty("file.separator")
  if(fileName.contains("\\") && separator == "/") fileName = fileName.replace("\\", "/")
  if(fileName.contains("/") && separator == "\\") fileName = fileName.replace("/", "\\")
  
  def toVectorSeq: Array[DenseVector[Double]] = {
    def lineToVector(line: String): DenseVector[Double] = line.split(',').map(_.toDouble).asVector.asRow 

    processFile(lineToVector(_)) toArray

  }  
  
  def toMatrix: DenseMatrix[Double] = {
    def lineToMatrix(line: String): DenseMatrix[Double] = line.split(',').map(_.toDouble).asMatrix(1)

    val lineMatrices = processFile(lineToMatrix(_))
    
    val matrix = lineMatrices reduce (DenseMatrix.vertcat(_, _))
    
    matrix
  }

  def toVector: DenseVector[Int] = processFile(_.toInt).toArray.asVector
  
  private def processFile[V](applyToLine: String => V) = {
    val source = fromFile(fileName)
    
    val lines = source getLines() toArray
    
    val maped = lines map(applyToLine(_))
            
    source.close()
        
    maped
  }
  
}