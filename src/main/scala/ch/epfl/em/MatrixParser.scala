package ch.epfl.em
import scalala.tensor.dense.DenseMatrix
import scala.io.Source._
import scalala.tensor.dense.DenseVector
import scalala.operators._
import scalala.operators.Implicits._

object MatrixParser {

  def parseFile(fileName: String): DenseMatrix[Double] = {
    def lineToMatrix(line: String): DenseMatrix[Double] = line.split(',').map(_.toDouble).asMatrix(1)

    val source = fromFile(fileName)
    
    val lines = source.getLines() toArray
    

    val vects = lines map(lineToMatrix(_))
    
    val matrix = vects.reduce(DenseMatrix.vertcat(_, _))
        
    source.close();
    
    matrix
  }
  
}