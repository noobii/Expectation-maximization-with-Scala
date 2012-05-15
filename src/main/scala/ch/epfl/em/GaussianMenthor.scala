package ch.epfl.em

import processing.parallel._
import benchmark.TicToc
import processing.parallel.Vertex
import scala.math.Pi
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._
import scala.collection.GenSeq
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap

object GaussianMenthor extends App {

  override def main(args: Array[String]):Unit = {
      
    println("Runing algo 50k Menthor")
    
    val k50k = 6
    val X50k = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
    val gaussian50k = new GaussianMenthor(strategy50k)(X50k, k50k)
    
    gaussian50k.runAlgo
  }
}

case class PointWithEstimate(point: DenseVector[Double], estimate: DenseVector[Double])

// Stupid name
//case class UsedByExpectation(est: MatricesTupple, S: Array[Double], invEstC: Array[DenseMatrix[Double]])

//object SharedData {
//  var estimates: ConcurrentMap[Int, UsedByExpectation] = new ConcurrentHashMap[Int, UsedByExpectation]
//}

class GaussianMenthor(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) extends
      Gaussian(initStrategy)(dataIn, gaussianComponents){

  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int
      ): (MatricesTupple, Double) = {
      
    
    val graph = new Graph[VertexValue]
    
    /*
    val masterNode = new Vertex[DenseVector[Double]]("master", DenseVector(.0, .0, .0, .0, .0, .0)) {
      def update(superstep: Int, incoming: List[Message[DenseVector[Double]]]) = {List()}
    }*/
    
    //graph.addVertex(masterNode)
    
    for(point <- dataIn) {
      val newVertex = new DataVertex(point, estimates)
      graph.addVertex(newVertex)
      //newVertex.connectTo(masterNode)
      //masterNode.connectTo(newVertex)
    }
  
    println("go start!")
  
    graph.start
    graph.iterate(10)
    graph.terminate()
    
    null
  }

  case class VertexValue(val point: DenseVector[Double], var expectation: DenseVector[Double])

  class DataVertex(point: DenseVector[Double], var estimates: MatricesTupple) extends Vertex[VertexValue]("point", VertexValue(point, null)) {
		  
    def update(superstep: Int, incoming: List[Message[VertexValue]]) = {
      def normalize(v: DenseVector[Double]) = v :/ v.sum
	    
      // Creates new empty covariances matrices if needed
	  val estimatedCovariances = estimates.covariances map {matrix => 
	    if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
	    else matrix
	  }
	    
	  // Computes values that are used later in the algo
	  val S = estimatedCovariances map (matrix => sqrt(det(matrix)))
	  val invEstC = estimatedCovariances map (matrix => inv(matrix))
	
	  val a = pow(2 * Pi, dimensions / 2.0)
	    
	  val ex = normalize(DenseVector.tabulate[Double](gaussianComponents)(j => {
	
	    val delta = point.asCol - estimates.means(::, j)
	    val coef = delta.t * invEstC(j) * delta
	    val pl = exp(-0.5 * coef) / (a * S(j))
	        
	    estimates.weights(j) * pl
	  }))
	      	    
	  value.expectation = ex
	    
	  List()
	} crunch((x, y) => VertexValue(null, x.expectation + y.expectation)) then {
	  incoming match {
	    case toto => {
	      //estimates.weights = toto.
	      List()
	        
	    }
	  }
	}
	  
  }
}