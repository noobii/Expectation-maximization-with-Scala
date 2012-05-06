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
  
  @Test def meansMatToGenSeqTest() {
    val matrix = DenseMatrix((1.0, 3.0), (2.0, 4.0))
    
    val genSeq = meansMatToGenSeq(matrix)
    
    assert(genSeq(0) == DenseVector(1.0, 2.0))
    assert(genSeq(1) == DenseVector(3.0, 4.0))
    
    val chain = meansGenSeqToMat(meansMatToGenSeq(matrix))
    
    println(chain)
    println(matrix)
    
    assert(matrix == chain)
  }
  
  @Test def meansGenSeqToMatTest() {
    val genSeq = Array(DenseVector(1.0, 2.0), DenseVector(3.0, 4.0))
    
    val matrix = meansGenSeqToMat(genSeq)
    
    assert(matrix == DenseMatrix((1.0, 3.0), (2.0, 4.0)))
    
    val chain = meansMatToGenSeq(meansGenSeqToMat(genSeq))
    
    assert(genSeq(0) == chain(0) && genSeq(1) == chain(1))
  }
  
  
}