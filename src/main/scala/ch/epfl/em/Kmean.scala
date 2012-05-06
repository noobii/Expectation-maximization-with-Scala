package ch.epfl.em

import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;
import scala.collection.GenSeq


class Kmean(dataSeq: GenSeq[DenseVector[Double]], k: Int) extends GaussianInit {
  import ch.epfl.em.Kmean._
  
  val data = Conversions.dataGenSeqToMat(dataSeq)
  
  lazy val clusters = run()
  
  lazy val means = {
    val centroids = computeCentroids(clusters)
    // Create a matrix where each columns is a mean (centroid) of a cluster
    val matrix = DenseMatrix.zeros[Double](data.numCols, k)
    for(i <- 0 until centroids.size) matrix(::, i) := centroids(i)
    
    matrix
  }
  
  lazy val weights = {
    weightOfClusters(clusters)
  }
  
  lazy val covariances = {
    covarianceOfClusters(clusters)
  }

  /**
   * Computes the k mean of a given dataset. It uses the standard algorithm. Source wikipedia.
   * The algorithm isn't guaranteed to converge. When it doesn't it throws an exception while trying to access
   * a map. This must be fixed.
   * @param data: the data, where each row is a mesurement and each column a variable
   * @param k: number of clusters
   * @param maxIter: maximum number of iterations used in the algorithm
   * @return 
   */
  def run(maxIter: Int = Int.MaxValue): Array[(Int, DenseVector[Double])] = {

    // First assigns a cluster to each mesurement
    var clusters = initializeClusters
    // Compute the centroids of the clusters
    var centroids = computeCentroids(clusters)
    var hasConverged = false

    // The core of the algo. Successively computes the new clusters and the centroids
    // until either it converges or it reaches the maximum amount of iterations
    for(i <- 0 until maxIter if !hasConverged) {
      val (clusts, hasConv) = updateClusters(clusters, centroids)
      centroids = computeCentroids(clusts)
      
      clusters = clusts
      hasConverged = hasConv
    }
    
    clusters
  }
  
  /**
   * Initial step of the algorithm. It "randomly" assign a cluster to each mesurement.
   * Currently it is purely deterministic and assigns clusters sequentially from 0 to k-1
   * and starts over again. 
   */
  def initializeClusters: Array[(Int, DenseVector[Double])] = {
    // Not so very random but at least we are sure that all clusters indexes are represented
    (for(i <- 0 until data.numRows) yield (i % k, data(i, ::))).toArray
  }
}

object Kmean {

  /**
   * Computes the covariance of each cluster. 
   * TODO Make the code better :)
   */
  def covarianceOfClusters(clusters: Seq[(Int, DenseVector[Double])]): Array[DenseMatrix[Double]] = {
    
    /**
     * Creates a matrix with the given vectors as rows
     */
    def agregatedMatrix(vects: Seq[(Int, DenseVector[Double])]): DenseMatrix[Double] = {
      val matLines = vects map {case(_, vector) => DenseMatrix(vector.asRow)}
      
      val matrix = matLines reduce(DenseMatrix.vertcat(_, _))
      
      matrix
    }
    
    val groupedClusters = clusters groupBy(_._1) toSeq
    
    val orderedClusters = groupedClusters sortBy(_._1)
    
    val matrixClusters = orderedClusters map (x => agregatedMatrix(x._2))
        
    val covariances = matrixClusters map (covariance(_, Axis.Vertical)._1)
    
    covariances.toArray
    
  }
  
  /**
   * Computes the weight of each cluster.
   */
  def weightOfClusters(clusters: Seq[(Int, DenseVector[Double])]): DenseVector[Double] =  {
    def index(x: Tuple2[Int, _]) = x._1
    
    val groupedClusters = clusters groupBy(index(_)) toSeq
    val orderedClusters = groupedClusters sortBy(index(_))
    
    val numberOfMeasures = clusters.size.toDouble
    val numberOfClusters = orderedClusters.size
    
    val weightVector = DenseVector.tabulate[Double](numberOfClusters)(index => {
      val vectorsInCluster = orderedClusters(index)._2
      vectorsInCluster.size / numberOfMeasures
    })
    
    weightVector
  }

  // !!!!
  /**
   * Compute the centroids (mean) of the data that has been assigned to clusters
   */
  def computeCentroids(data: Seq[(Int, DenseVector[Double])]): Map[Int, DenseVector[Double]] = {

    // Groups the data by the cluster index
    val groups = data groupBy { case(clusterIndex, _) => clusterIndex }

    // Creates a map where each each index identifies a the sequence of mesure in the cluster
    val centroids = groups map {
      case(clusterIndex, tuppleList) => {
        (clusterIndex, mean((tuppleList map(_._2)).seq))
      }
    }
    
    centroids
  }

  /**
   * Computes the mean of a sequence of vectors
   */
  def mean(vects: Seq[DenseVector[Double]]): DenseVector[Double] = {
    val sum = vects.fold(DenseVector.zeros[Double](vects.head.size))(_ + _)
    sum :/ vects.size
  }

  /**
   * Assign each measure to the closest centroid. Returns the new assignemend as well as boolean that indiciated convergence
   */
  def updateClusters(data: Array[(Int, DenseVector[Double])], centroids: Map[Int, DenseVector[Double]]): (Array[(Int, DenseVector[Double])], Boolean) = {
    val newClusters = data map {
      case(_, vector) => (closestClusterIndex(centroids, vector), vector)
    }
    
    // If has convereged then no index has changed
    val hasConverged = data zip newClusters forall {case((oldIndex, _), (newIndex, _)) => oldIndex == newIndex}

    (newClusters, hasConverged)

  }


  /**
   * Finds the closest cluster for a given vector
   */
  def closestClusterIndex(means: Map[Int, DenseVector[Double]], vect: DenseVector[Double]): Int = {
    val distances = means map {
      case(clusterIndex, mean) => {
        val difference = vect.asRow - mean.asRow
        val vectNorm = difference.norm(2)
        (clusterIndex, vectNorm)
      }
    }

    distances.minBy{case(_, norm) => norm}._1 // Takes the smallest distance and return the index
  }
}