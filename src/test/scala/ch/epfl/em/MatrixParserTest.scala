package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

class MatrixParserTest extends AssertionsForJUnit {
  
  val path = "src\\test\\ressources\\matrices\\"

  @Test def parseFileTest() {
    val fileName = path + "parseTest.csv"
      
    val parsedMatrix = MatrixParser.parseFile(fileName)
    
    val rightMatrix = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    
    assert(parsedMatrix == rightMatrix)
  }
  
  @Test def parseForFun() {
    val fileName = path + "X.csv"
    
    println(MatrixParser.parseFile(fileName))
  }
  
}