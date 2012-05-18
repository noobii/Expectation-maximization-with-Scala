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

object GaussianMenthor extends App {

  override def main(args: Array[String]):Unit = {
      
    println("Runing algo 50k Menthor")
    
    val k50k = 6
    val X50k = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
    val gaussian50k = new GaussianMenthor(strategy50k)(X50k, k50k)
    
    gaussian50k.runAlgo()
  }
}


class GaussianMenthor(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) extends
      Gaussian(initStrategy)(dataIn, gaussianComponents){

  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int
      ): (MatricesTupple, Double) = {
      
    
    val graph = new Graph[VertexValue]
    
    for(point <- dataIn) {
      val newVertex = new GaussianVertex(point)
      graph.addVertex(newVertex)
    }
  
    CurrentEstimaes.weights = estimates.weights
    CurrentEstimaes.means = estimates.means
    CurrentEstimaes.covariances = estimates.covariances
    
    println("go start!")
  
    graph.start
    graph.iterate(8)
    graph.terminate()
    
    System.exit(0)
    null
  }

  
  class VertexValue(
      val point: DenseVector[Double] = null,
      var exp: DenseVector[Double] = null,
      val estWeights: DenseVector[Double] = null,
      val estMeans: DenseMatrix[Double] = null,
      val estCovariances: Array[DenseMatrix[Double]] = null
  ) {
    def means = estMeans
    def covariances = estCovariances
  }
  
  class RealVertexValue(point: DenseVector[Double]) extends VertexValue(point = point) {
    override def means = point.asCol * exp.asRow
    override def covariances = {
      (0 until gaussianComponents).toArray map(k => {
        val delta = point.asCol - CurrentEstimaes.means(::, k)
        (delta * delta.t) :* exp(k) :/ CurrentEstimaes.weights(k)
      })
    }
  }
  
  object CurrentEstimaes {
    @volatile var weights: DenseVector[Double] = _
    @volatile var means: DenseMatrix[Double] = _
    @volatile var covariances: Array[DenseMatrix[Double]] = _
  }

  class GaussianVertex(point: DenseVector[Double]) extends 
        Vertex[VertexValue]("point", new RealVertexValue(point)) {
    
    def update(superstep: Int, incoming: List[Message[VertexValue]]) = {
      def normalize(v: DenseVector[Double]) = v :/ v.sum
	    
      // Creates new empty covariances matrices if needed
	  val estimatedCovariances = CurrentEstimaes.covariances map {matrix => 
	    if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
	    else matrix
	  }
	    
	  // Computes values that are used later in the algo
	  val S = estimatedCovariances map (matrix => sqrt(det(matrix)))
	  val invEstC = estimatedCovariances map (matrix => inv(matrix))
	
	  val a = pow(2 * Pi, dimensions / 2.0)
	    
	  val ex = DenseVector.tabulate[Double](gaussianComponents)(j => {
	
	    val delta = point.asCol - CurrentEstimaes.means(::, j)
	    val coef = delta.t * invEstC(j) * delta
	    val pl = exp(-0.5 * coef) / (a * S(j))
	        
	    CurrentEstimaes.weights(j) * pl
	  })
	  
	  val exNorm = normalize(ex)
	  
	  value.exp = exNorm
	  
	  List()
    } crunch((x, y) => new VertexValue(exp = x.exp + y.exp)) then {
      if(this == graph.vertices(0)) {
        incoming match {
          case List(message) => CurrentEstimaes.weights = message.value.exp
          case _ => throw new Exception("Something went wrong with weights")
        }
        
        println(CurrentEstimaes.weights)
      }
      List()
    } crunch((x, y) => new VertexValue(estMeans = x.means + y.means)) then {
      if(this == graph.vertices(0)) {
        incoming match {
          case List(newMessage) => {
            val sumedMeans = newMessage.value.estMeans
            // The weights repeated in each line of a (dim, gaussianComp) matrix
            val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * CurrentEstimaes.weights.asRow
            
            CurrentEstimaes.means = sumedMeans :/ weightsAsMatrix
            
            println(CurrentEstimaes.means)
          }
          case _ => // Should throw an exception
        }
      }
      List()
    } crunch((x, y) => {
      val newSum = (x.covariances zip y.covariances) map(x => x._1 + x._2)
      new VertexValue(estCovariances = newSum)
    }) then {
      if(this == graph.vertices(0)) {
        incoming match {
          case List(message) => CurrentEstimaes.covariances = message.value.estCovariances
          case _ => throw new Exception("Boom!")
        }
        // Other small cleanup tasks
        
        println(CurrentEstimaes.covariances(0))
        CurrentEstimaes.weights = CurrentEstimaes.weights / measurements
      
      }
      List()
    }
    
  }

}