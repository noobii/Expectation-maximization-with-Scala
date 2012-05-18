package ch.epfl.em

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import scalala.tensor.dense._

import ch.epfl.em.Conversions._

class ConversionsTest extends AssertionsForJUnit {

  @Test def dataMatToGenSeqTest() {
    
    val matrix = DenseMatrix((1.0, 2.0), (3.0, 4.0))
    
    val genSeq = dataMatToGenSeq(matrix)
    
    assert(genSeq(0) == DenseVector(1.0, 2.0))
    assert(genSeq(1) == DenseVector(3.0, 4.0))
    
    assert(matrix == dataGenSeqToMat(dataMatToGenSeq(matrix)))
  }
  
  @Test def dataGenSeqToMatTest() {
    
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = dataGenSeqToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 2.0), (3.0, 4.0)))
    
    val chain = dataMatToGenSeq(dataGenSeqToMat(genSeq))
    
    assert(genSeq(0) == chain(0) && genSeq(1) == chain(1))
    
  }
  
  @Test def meansMatToArrayTest() {
    val matrix = DenseMatrix((1.0, 3.0), (2.0, 4.0))
    
    val genSeq = meansMatToArray(matrix)
    
    assert(genSeq(0) == DenseVector(1.0, 2.0))
    assert(genSeq(1) == DenseVector(3.0, 4.0))
    
    val chain = meansArrayToMat(meansMatToArray(matrix))
    
    assert(matrix == chain)
  }
  
  @Test def meansArrayToMatTest() {
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = meansArrayToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 3.0), (2.0, 4.0)))
    
    val chain = meansMatToArray(meansArrayToMat(genSeq))
    
    assert(genSeq(0) == chain(0) && genSeq(1) == chain(1))
  }
  
  
}