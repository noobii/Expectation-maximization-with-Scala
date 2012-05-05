package ch.epfl.em

import scala.Array.canBuildFrom
import scala.io.Source.fromFile

import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;


case class FileParser(var fileName: String) {

  val separator = System.getProperty("file.separator")
  if(fileName.contains("\\") && separator == "/") fileName = fileName.replace("\\", "/")
  if(fileName.contains("/") && separator == "\\") fileName = fileName.replace("/", "\\")
  
  def toVectorSeq: Array[DenseVector[Double]] = {
    def lineToVector(line: String): DenseVector[Double] = line.split(',').map(_.toDouble).asVector.asRow 

    processFile(lineToVector(_)) toArray

  }  
  
  def toMatrix: DenseMatrix[Double] = {
    def lineToMatrix(line: String) = line.split(',').map(_.toDouble)

    val lineMatrices = processFile(lineToMatrix(_)) toArray
    //val matrix = lineMatrices reduce (DenseMatrix.vertcat(_, _))
    
    val matrix = DenseMatrix.zeros[Double](lineMatrices.length, lineMatrices(0).size)
    
    for(i <- 0 until lineMatrices.length) matrix(i, ::) := lineMatrices(i).asVector
    
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