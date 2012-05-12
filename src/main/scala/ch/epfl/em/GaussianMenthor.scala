package ch.epfl.em

import processing.parallel._
import benchmark.TicToc
import processing.parallel.Vertex

object GaussianMenthor extends App {
  
  override def main(args: Array[String]):Unit = {
      
    //PageRank.runPageRank(10)
	  val graph = new Graph[Double]
	  
	  val v1 = new Toto("ah", 10.0)
	  //val v2 = new Toto("bg", 20.0)
	  //val v3 = new Toto("ch", 30.0)
	  
	  val d1 = graph.addVertex(v1)

	  
	  for(i <- 0 until 50) {
	    val d2 = graph.addVertex(new Toto(i.toString, i.toDouble))
	    d2.connectTo(d1)
	  }
	  
	  println("Go")
	  
	  
	  graph.start
	  graph.iterate(1)
	  graph.terminate()
  }

  
  case class Toto(la: String, va: Double) extends Vertex[Double](la, va) {
    
    def update(superstep: Int, incoming: List[Message[Double]]): Substep[Double] = {
      /*{
        if(superstep == 0) 
          println(this)
        List()
      } then {
        println("oh")
        List()
      }*/
      println("oh")
      List()
    }
  }
  
}