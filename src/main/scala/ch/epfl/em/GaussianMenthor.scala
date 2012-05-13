package ch.epfl.em

import processing.parallel._
import benchmark.TicToc
import processing.parallel.Vertex
import scalala.tensor.dense.DenseVector
import scala.collection.GenSeq
import scalala.tensor.dense.DenseMatrix
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap

object GaussianMenthor extends App {

  override def main(args: Array[String]):Unit = {
      
    println("Runing algo 50k Menthor")
    
    val k50k = 6
    val X50k = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
    val gaussian50k = new GaussianMenthor(strategy50k)(X50k, k50k)
  }
}

case class PointWithEstimate(point: DenseVector[Double], estimate: DenseVector[Double])

// Stupid name
//case class UsedByExpectation(est: MatricesTupple, S: Array[Double], invEstC: Array[DenseMatrix[Double]])

//object SharedData {
//  var estimates: ConcurrentMap[Int, UsedByExpectation] = new ConcurrentHashMap[Int, UsedByExpectation]
//}

class GaussianMenthor(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) {

  val graph = new Graph[DenseVector[Double]]
  
  val init = initStrategy.init
  
  for(point <- dataIn) {
    val newVertex = new DataVertex(point, init)
    graph.addVertex(newVertex)
  }
  
  
  
  graph.start
  graph.iterate(8)
  graph.terminate()
}

class DataVertex(point: DenseVector[Double], var estimates: MatricesTupple) extends Vertex[DenseVector[Double]]("point", point) {

  var est: DenseVector[Double] = _
  
  def update(superstep: Int, incoming: List[Message[DenseVector[Double]]]) = {
    def normalize(v: DenseVector[Double]) = v :/ v.sum
    
    
    List()
  }
  /*
    def update(superstep: Int, incoming: List[Message[DenseVector[Double]]]) = {
      List()
    } crunch ((v1, v2) => v1 + v2) then {
		incoming match { 
		  case List(reduced) => {
		    List(Message(this, this, incoming.head.value * 2))
		  }
		  case Nil => {
		    println("ah")
		    List()
		  }
	   }
    }
    */
}