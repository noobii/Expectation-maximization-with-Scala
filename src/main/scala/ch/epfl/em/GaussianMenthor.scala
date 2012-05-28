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

class GaussianMenthor (
    initStrategy: GaussianInit)
    (dataIn: GenSeq[DenseVector[Double]], 
     gaussianComponents: Int) 
  extends Gaussian(initStrategy)(dataIn, gaussianComponents) {

  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maximumIterations: Int) = {
    
    val graph = new Graph[VertexValue]
    
    // Hack ! 
    Graph.count = 0
    
    // Build the graph. It is unconnected !
    // Each vertex hold the data associated with one point.
    for(point <- dataIn) {
      val newVertex = new GaussianVertex(point)
      graph.addVertex(newVertex)
    }
  
    // Initializes the current data
    CurrentData.init(estimates, minLikelihoodVar)

    // Starts to run the algo
    graph.start
    // Why 8? It is the number of substeps in the algo
    tic
    graph.iterate(8 * maximumIterations)
    toc("algo")
    graph.terminate()
    
    // Gets back the current data to return it
    val finalEstimates = new MatricesTupple(CurrentData.weights, CurrentData.means, CurrentData.covariances)
    val finalLoglikelihood = CurrentData.loglikelihood
    val iterations = CurrentData.iteration
    (finalEstimates, finalLoglikelihood, iterations)
  }
  
  /**
   * Serves as wrapper for all the data that must be passed around as message in the algo.
   * VertexValue is in practice only instancieted during the crunch operations later.
   * For the vertices use RealVertexValue
   */
  class VertexValue(
      val point: DenseVector[Double] = null,
      var exp: DenseVector[Double] = null,
      private val estMeans: DenseMatrix[Double] = null,
      private val estCovariances: Array[DenseMatrix[Double]] = null) {

    // This is the case where the Vertex value is used to hold a computed valud (runing sum)
    def means = estMeans
    def covariances = estCovariances
  }
  
  /**
   * This class is used as value in the vertex as oposed to its ancestor that is
   * only used for the crunch operations.
   */
  class RealVertexValue(point: DenseVector[Double]) extends VertexValue(point = point) {
    // Called during in the crunch step when we compute the means
    override def means = point.asCol * exp.asRow

    // Called during the crunch step when we compute the covariances
    override def covariances = {
      (0 until gaussianComponents).toArray map(k => {
        val delta = point.asCol - CurrentData.means(::, k)
        (delta * delta.t) :* exp(k) :/ CurrentData.weights(k)
      })
    }
  }
  
  /**
   * Object that holds all the data that must be shared accros the vertices
   * during the algo or that must persist after the algo has run.
   */
  object CurrentData {
    @volatile var weights: DenseVector[Double] = _
    @volatile var means: DenseMatrix[Double] = _
    @volatile var covariances: Array[DenseMatrix[Double]] = _
    @volatile var loglikelihood: Double = Double.MaxValue
    
    /** Variation when the algo should stop */
    var minLikelihood: Double = _
    /** Current iteration count */
    var iteration: Int = 0
    
    /**
     * Easily initialize the object
     */
    def init(in: MatricesTupple, likelihood: Double) {
      weights = in.weights
      means = in.means
      covariances = in.covariances
      minLikelihood = likelihood
    }
    
    /**
     * Gets the data in a convinient way
     */
    def tupples = new MatricesTupple(weights, means, covariances)
  }

  class GaussianVertex(point: DenseVector[Double]) extends 
        Vertex[VertexValue]("point", new RealVertexValue(point)) {
    
    // For certain steps operations must only be performed in one vertex
    lazy val justOneVertex = (this == graph.vertices(0))
    
    def update(superstep: Int, incoming: List[Message[VertexValue]]) = {    
      
      def normalize(v: DenseVector[Double]) = v :/ v.sum
	    
      // Creates new empty covariances matrices if needed
	  val estimatedCovariances = CurrentData.covariances map {matrix => 
	    if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
	    else matrix
	  }
	    
	  // Computes values that are used later in the algo
	  val S = estimatedCovariances map (matrix => sqrt(det(matrix)))
	  val invEstC = estimatedCovariances map (matrix => inv(matrix))
	
	  val a = pow(2 * Pi, dimensions / 2.0)
	    
	  val ex = DenseVector.tabulate[Double](gaussianComponents)(j => {
	
	    val delta = point.asCol - CurrentData.means(::, j)
	    val coef = delta.t * invEstC(j) * delta
	    val pl = exp(-0.5 * coef) / (a * S(j))
	        
	    CurrentData.weights(j) * pl
	  })
	  
	  val exNorm = normalize(ex)
	  
	  value.exp = exNorm
	  
	  List()
    } crunchToOne((x, y) => 
         new VertexValue(exp = x.exp + y.exp)
         // The estimated weights are computed by summing up all expectations
    ) then {
      // The result of the crunch is sent out to all vertices
      if(justOneVertex) {
        // Only one vertex takes the computed weight and stores it
        incoming match {
          // Stores the new estimated weights
          case List(message) => CurrentData.weights = message.value.exp
          case _ => // Will never happen
        }
      }
      List()
    } crunchToOne((x, y) => 
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
            val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * CurrentData.weights.asRow
            
            // Stores the new estimated means
            CurrentData.means = sumedMeans :/ weightsAsMatrix
          }
          case _ => // Will never happen
        }
      }
      List()
    } crunchToOne((x, y) => {
      val covarianceSum = (x.covariances zip y.covariances) map{case(mat1, mat2) => mat1 + mat2}
      
      new VertexValue(estCovariances = covarianceSum)
      // The estimated covariances are computed by summing computed values at each vertex
    }) then {
      // The result is available to all vertices
      if(justOneVertex) {
        // Only one vertex takes care of storing the result
        incoming match {
          case List(message) => CurrentData.covariances = message.value.covariances
          case _ => // Will never happen
        }

        // Readjusting the weight coefficients
        CurrentData.weights = CurrentData.weights / measurements
      
      }
      List()
    } then {
      if(justOneVertex) {
        // Just one vertex takes care of checking if the algo converges
        val newLikelihood = likelihood(CurrentData.tupples)
        var oldLikelihood = CurrentData.loglikelihood
        
        // Checks if the algorithm has converged or not
        def hasConverged = (abs(100*(newLikelihood - oldLikelihood) / oldLikelihood) <= CurrentData.minLikelihood)
        
        if(hasConverged) {
          // Our result is good enough we stop the iterations
          graph.terminate()
        }
        
        // Sets the newly computed likelihood
        CurrentData.loglikelihood = newLikelihood
        // We increment the iteration counter to keep track of when the algo stops
        CurrentData.iteration += 1

      }
      List()
    }
    
  }

}