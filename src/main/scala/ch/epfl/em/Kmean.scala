package ch.epfl.em

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}
import scalala.tensor.dense.DenseMatrix$
import scalala.library.Library._

object Kmean {

  /**
   * Computes the k mean of a given dataset. It uses the standard algorithm. Source wikipedia.
   * The algorithm isn't guaranteed to converge. When it doesn't it throws an exception while trying to access
   * a map. This must be fixed.
   * TODO : Fix when the algo doesn't converge it should do something more gracefull than throw exceptions...
   * @param data: the data, where each row is a mesurement and each column a variable
   * @param k: number of clusters
   * @param maxIter: maximum number of iterations used in the algorithm
   * @return 
   */
  def kmeans(data: DenseMatrix[Double], k: Int, maxIter: Int): (DenseMatrix[Double], Seq[(Int, DenseVector[Double])]) = {

    // First assigns a cluster to each mesurement
    var clusters = initializeClusters(data, k)
    // Compute the centroids of the clusters
    var centroids = computeCentroids(clusters)
    var hasConverged = false

    // The core of the algo. Successively computes the new clusters and the centroids
    // until either it converges or it reaches the maximum amount of iterations
    for(i <- 0 until maxIter if !hasConverged) {
      val (clusts, hasConv) = assignNewClusters(clusters, centroids)
      centroids = computeCentroids(clusts)
      
      clusters = clusts
      hasConverged = hasConv
    }
    
    // Create a matrix where each columns is a mean (centroid) of a cluster
    val matrix = DenseMatrix.zeros[Double](data.numCols, k)
    for(i <- 0 until centroids.size) matrix(::, i) := centroids(i)
    
    (matrix, clusters)
  }

  /**
   * WARNING ! crapy code ahead, will rewrite it but must test if it works first
   */
  def covarianceOfClusters(clusters: Seq[(Int, DenseVector[Double])]): Seq[DenseMatrix[Double]] = {
    def agregatedMatrix(vects: Seq[(Int, DenseVector[Double])]): DenseMatrix[Double] = {
      val matrix = DenseMatrix.zeros[Double](vects.length, vects(0)._2.size) // not verry pretty
      
      for(i <- 0 until vects.length) matrix(i, ::) := vects(i)._2
      
      matrix
    }
    
    val groupedClusters = clusters groupBy(_._1) 
    
    val matrixClusters = groupedClusters map (x => agregatedMatrix(x._2))
    
    val covariances = matrixClusters map (covariance(_, Axis.Vertical)._1)
    
    covariances.toSeq
    
  }

  /**
   * Initial step of the algorithm. It "randomly" assign a cluster to each mesurement.
   * Currently it is purely deterministic and assigns clusters sequentially from 0 to k-1
   * and starts over again. 
   */
  def initializeClusters(data: DenseMatrix[Double], k: Int): Seq[(Int, DenseVector[Double])] = {
    // Not so very random but at least we are sure that all clusters indexes are represented
    for(i <- 0 until data.numRows) yield (i % k, data(i, ::))
  }


  /**
   * Compute the centroids (mean) of the data that has been assigned to clusters
   */
  def computeCentroids(data: Seq[(Int, DenseVector[Double])]): Map[Int, DenseVector[Double]] = {

    // Groups the data by the cluster index
    val groups = data groupBy { case(clusterIndex, _) => clusterIndex }

    // Creates a map where each each index identifies a the sequence of mesure in the cluster
    val centroids = groups map {
      case(clusterIndex, tuppleList) => {
        (clusterIndex, mean(tuppleList map(_._2)))
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
   * Assign each mesure to the closest centroid. Returns the new assignemend as well as boolean that indiciated convergence
   */
  def assignNewClusters(data: Seq[(Int, DenseVector[Double])], centroids: Map[Int, DenseVector[Double]]): (Seq[(Int, DenseVector[Double])], Boolean) = {
    val newClusters = data map {
      case(_, vector) => (closestClusterIndex(centroids, vector), vector)
    }
    
    // If has convereged then no index has changed
    val hasConverged = data zip(newClusters) forall {case((oldIndex, _), (newIndex, _)) => oldIndex == newIndex}

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