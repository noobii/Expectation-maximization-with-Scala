package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector

class FileParserTest extends AssertionsForJUnit {
  
  val path = "src/test/ressources/parsing/"

  @Test def toVectorSeq() {
    val fileName = path + "vectSeq.csv"
    
    val parsedSeq = FileParser(fileName).toVectorSeq
    
    val rightSeq = Array(DenseVector(1.0, 2.0, 3.0) asRow, DenseVector(4.0, 5.0, 6.0) asRow)
    
    assert(parsedSeq(0) == rightSeq(0))
    assert(parsedSeq(1) == rightSeq(1))
  }  
    
  @Test def toVectorTest() {
    val fileName = path + "intVector.csv"
    
    val parsedVector = FileParser(fileName).toVector
    
    val rightVector = DenseVector(1, 2, 3, 4, 5, 4, 3, 2, 1)
    
    assert(parsedVector == rightVector)
  }
    
  @Test def toMatrixTest() {
    val fileName = path + "doubleMatrix.csv"
      
    val parsedMatrix = FileParser(fileName).toMatrix
    
    val rightMatrix = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    
    assert(parsedMatrix == rightMatrix)
  }
  
  @Test def meanTest() {
    val fileName = path + "meanMatrix.csv"
    
    val parsed = FileParser(fileName).means
    
    val right = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0), DenseVector(5.0, 6.0))
    
    assert((parsed zip right) forall(x => x._1.asCol == x._2.asCol))
  }
  
  @Test def dataTest() {
    val fileName = path + "data.csv"
    
    val parsed = FileParser(fileName).data
    
    val right = Array(DenseVector(1.0, 2.0).asRow, DenseVector(3.0, 4.0).asRow, DenseVector(5.0, 6.0))
    
    assert((parsed zip right) forall(x => x._1 == x._2))
  }
  
  /*
  @Test def parseForFun() {
    val fileName = path + "X.csv"
    
    println(FileParser.toMatrix(fileName))
  }*/
  
}