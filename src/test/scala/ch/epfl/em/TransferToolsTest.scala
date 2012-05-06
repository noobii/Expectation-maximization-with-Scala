package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalala.tensor.dense._

class TransferToolsTest extends AssertionsForJUnit {

  @Test def dataMatToGenSeqTest() {
    
    val matrix = DenseMatrix((1.0, 2.0), (3.0, 4.0))
    
    val genSeq = TransferTools.dataMatToGenSeq(matrix)
    
    assert(genSeq(0) == DenseVector(1.0, 2.0))
    assert(genSeq(1) == DenseVector(3.0, 4.0))
  }
  
  @Test def dataGenSeqToMat() {
    
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = TransferTools.dataGenSeqToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 2.0), (3.0, 4.0)))
    
  }
}