package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalala.tensor.dense._

class ConversionsTest extends AssertionsForJUnit {

  @Test def dataMatToGenSeqTest() {
    
    val matrix = DenseMatrix((1.0, 2.0), (3.0, 4.0))
    
    val genSeq = Conversions.dataMatToGenSeq(matrix)
    
    assert(genSeq(0) == DenseVector(1.0, 2.0))
    assert(genSeq(1) == DenseVector(3.0, 4.0))
    
    assert(matrix == Conversions.dataGenSeqToMat(Conversions.dataMatToGenSeq(matrix)))
  }
  
  @Test def dataGenSeqToMatTest() {
    
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = Conversions.dataGenSeqToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 2.0), (3.0, 4.0)))
    
    val chain = Conversions.dataMatToGenSeq(Conversions.dataGenSeqToMat(genSeq))
    
    assert(genSeq(0) == chain(0) && genSeq(1) == chain(1))
    
  }
}