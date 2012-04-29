package ch.epfl.em

import scala.Array.canBuildFrom
import scala.io.Source.fromFile

import scalala.operators.Implicits.richArray
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector

/**
 * WARING COPY / PASTE CODE !
 */
object FileParser {

  // TODO make code nicer, and not copy paste !
  def toVectorSeq(fileName: String): Array[DenseVector[Double]] = {
    def lineToVector(line: String): DenseVector[Double] = line.split(',').map(_.toDouble).asVector.asRow

    val source = fromFile(fileName)
    
    val lines = source.getLines() toArray
    

    val vects = lines map(lineToVector(_))
            
    source.close();
    
    vects    
  }  
  
  def toMatrix(fileName: String): DenseMatrix[Double] = {
    def lineToMatrix(line: String): DenseMatrix[Double] = line.split(',').map(_.toDouble).asMatrix(1)

    val source = fromFile(fileName)
    
    val lines = source.getLines() toArray
    

    val vects = lines map(lineToMatrix(_))
    
    val matrix = vects.reduce(DenseMatrix.vertcat(_, _))
        
    source.close();
    
    matrix
  }

  def toVector(fileName: String): DenseVector[Int] = {
    val source = fromFile(fileName)
    
    val lines = source.getLines().map(_.toInt) toArray
    val vector = lines asVector
    
    source.close()
    
    vector
  }
  
  private def processFile[V](fileName: String, applyToRow: Array[_] => V): Array[V] = {
    
    null
  }
  
}