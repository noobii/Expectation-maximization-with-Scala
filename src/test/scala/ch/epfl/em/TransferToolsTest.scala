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
    
    assert(matrix == TransferTools.dataGenSeqToMat(TransferTools.dataMatToGenSeq(matrix)))
  }
  
  @Test def dataGenSeqToMatTest() {
    
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = TransferTools.dataGenSeqToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 2.0), (3.0, 4.0)))
    
    val chain = TransferTools.dataMatToGenSeq(TransferTools.dataGenSeqToMat(genSeq))
    
    assert(genSeq(0) == chain(0) && genSeq(1) == chain(1))
    
  }
}