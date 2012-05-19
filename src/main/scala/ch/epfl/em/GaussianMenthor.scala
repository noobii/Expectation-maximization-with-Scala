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
      ) = {
      
    
    val graph = new Graph[VertexValue]
    
    for(point <- dataIn) {
      val newVertex = new GaussianVertex(point)
      graph.addVertex(newVertex)
    }
  
    CurrentEstimaes.init(estimates, minLikelihoodVar)

    graph.start
    graph.iterate(8)
    graph.terminate()
    
    
    val finalEstimates = new MatricesTupple(CurrentEstimaes.weights, CurrentEstimaes.means, CurrentEstimaes.covariances)
    val finalLoglikelihood = CurrentEstimaes.loglikelihood
    
    (finalEstimates, finalLoglikelihood, CurrentEstimaes.iteration)
  }
  
  class VertexValue(
      val point: DenseVector[Double] = null,
      var exp: DenseVector[Double] = null,
      private val estMeans: DenseMatrix[Double] = null,
      private val estCovariances: Array[DenseMatrix[Double]] = null
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
    @volatile var loglikelihood: Double = Double.MaxValue
    var minLikelihood: Double = _
    var iteration: Int = 0
    
    def init(in: MatricesTupple, likelihood: Double) {
      weights = in.weights
      means = in.means
      covariances = in.covariances
      minLikelihood = likelihood
    }
    
    def tupples = new MatricesTupple(weights, means, covariances)
  }

  class GaussianVertex(point: DenseVector[Double]) extends 
        Vertex[VertexValue]("point", new RealVertexValue(point)) {
    
    lazy val justOneVertex = (this == graph.vertices(0))
    
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
    } crunch((x, y) => 
         new VertexValue(exp = x.exp + y.exp)
         // The estimated weights are computed by summing up all expectations
    ) then {
      // The result of the crunch is sent out to all vertices
      if(justOneVertex) {
        // Only one vertex takes the computed weight and stores it
        incoming match {
          // Stores the new estimated weights
          case List(message) => CurrentEstimaes.weights = message.value.exp
          case _ => // Will never happen
        }
      }
      List()
    } crunch((x, y) => 
         new VertexValue(estMeans = x.means + y.means)
         // The estimated means are computed by summing computed values at each vertex
    ) then {
      // The result is available to all vertices
      if(justOneVertex) {
        // Only one vertex takes care of computing the value and storing it
        incoming match {
          case List(newMessage) => {
            // The value that has been summed with the crunch step
            val sumedMeans = newMessage.value.means
            // The weights repeated in each line of a (dim, gaussianComp) matrix
            val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * CurrentEstimaes.weights.asRow
            
            // Stores the new estimated means
            CurrentEstimaes.means = sumedMeans :/ weightsAsMatrix
          }
          case _ => // Will never happen
        }
      }
      List()
    } crunch((x, y) => {
      val covarianceSum = (x.covariances zip y.covariances) map{case(mat1, mat2) => mat1 + mat2}
      
      new VertexValue(estCovariances = covarianceSum)
      // The estimated covariances are computed by summing computed values at each vertex
    }) then {
      // The result is available to all vertices
      if(justOneVertex) {
        // Only one vertex takes care of storing the result
        incoming match {
          case List(message) => CurrentEstimaes.covariances = message.value.covariances
          case _ => // Will never happen
        }

        // Readjusting the weight coefficients
        CurrentEstimaes.weights = CurrentEstimaes.weights / measurements
      
      }
      List()
    } then {
      if(justOneVertex) {
        // Just one vertex takes care of checking if the algo converges
        val newLikelihood = likelihood(CurrentEstimaes.tupples)
        var oldLikelihood = CurrentEstimaes.loglikelihood
        
        def hasConverged = (abs(100*(newLikelihood - oldLikelihood) / oldLikelihood) <= CurrentEstimaes.minLikelihood)
        
        // We increment the iteration counter to keep track of when the algo stops
        CurrentEstimaes.iteration += 1
        
        if(hasConverged) {
          // Our result is good enough we stop the iterations
          graph.terminate()
        }
        
        // Sets the newly computed likelihood
        CurrentEstimaes.loglikelihood = newLikelihood

      }
      List()
    }
    
  }

}