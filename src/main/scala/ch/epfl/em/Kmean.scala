package ch.epfl.em

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

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
  def kmeans(data: DenseMatrix[Double], k: Int, maxIter: Int): DenseMatrix[Double] = {

    // First assigns a cluster to each mesurement
    var clusters = initializeClusters(data, k)
    // Compute the centroids of the clusters
    var centroids = computeCentroids(clusters)
    var hasConverged = false

    // The core of the algo. Successively computes the new clusters and the centroids
    // until either it converges or it reaches the maximum amount of iterations
    for(i <- 0 until maxIter if !hasConverged) {
      val newClusters = assignClusts(clusters, centroids)
      clusters = newClusters._1
      hasConverged = newClusters._2
      centroids = computeCentroids(clusters)
    }
    
    // Create a matrix where each columns is a mean (centroid) of a cluster
    val matrix = DenseMatrix.zeros[Double](data.numCols, k)
    for(i <- 0 until centroids.size) matrix(::, i) := centroids(i)
    
    matrix

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
    sum := sum :/ vects.size
    sum
  }

  /**
   * Assign each mesure to the closest centroid. Returns the new assignemend as well as boolean that indiciated convergence
   */
  def assignClusts(clusters: Seq[(Int, DenseVector[Double])], means: Map[Int, DenseVector[Double]]): (Seq[(Int, DenseVector[Double])], Boolean) = {
    val newClusters = clusters.map(x => (closestCluster(means, x._2), x._2))
    
    // If has convereged then no index has changed
    val hasConverged = clusters.zip(newClusters).forall(x => x._1._1 == x._2._1)

    (newClusters, hasConverged)

  }


  /**
   * Finds the closest cluster for a given vector
   */
  def closestCluster(means: Map[Int, DenseVector[Double]], vect: DenseVector[Double]): Int = {
    val distances = means.map(x => {
      val difference = vect.asRow - x._2.asRow
      val vectNorm = difference.norm(2)
      (x._1, vectNorm)
    })

    distances.minBy(_._2)._1 // Takes the smallest distance and return the index
  }
}