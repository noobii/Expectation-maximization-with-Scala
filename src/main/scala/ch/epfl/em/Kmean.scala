package ch.epfl.em

import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import scalala.operators._
import scalala.tensor._
import scalala.library.LinearAlgebra._
import scalala.library.Library._
import scala.util.Random

object Kmean {

  /**
   * Computes the k mean of a given dataset. It uses the standard algorithm. Source wikipedia.
   * The algorithm isn't guaranteed to converge. When it doesn't it throws an exception while trying to access
   * a map. This must be fixed.
   * TODO : Fix when the algo desn't converge
   * @param data: the data, where each row is a mesurement and each column a variable
   * @param k: number of clusters
   * @param maxIter: maximum number of iterations used in the algorithm
   * @return 
   */
  def kmeans(data: DenseMatrix[Double], k: Int, maxIter: Int): (Map[Int, DenseVector[Double]], DenseMatrix[Double]) = {

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
    
    (centroids, matrix)

  } 

  /**
   * Initial step of the algorithm. It "randomly" assign a cluster to each mesurement.
   * Currently it is purely deterministic and assigns clusters sequantially from 0 to k-1
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
    val groups = data.groupBy(_._1)
    //
    val groupedLists = groups.map(x => (x._1, x._2.unzip._2))
    
    val means = groupedLists.map(x => (x._1, mean(x._2)))
    
    means
  }

  def mean(vects: Seq[DenseVector[Double]]): DenseVector[Double] = {
    val sum = vects.foldLeft(DenseVector.zeros[Double](vects.head.size))((x, y) => x + y)
    sum := sum :/ vects.size
    sum
  }

  def assignClusts(clusters: Seq[(Int, DenseVector[Double])], means: Map[Int, DenseVector[Double]]): (Seq[(Int, DenseVector[Double])], Boolean) = {
    val newClusters = clusters.map(x => (closestCluster(means, x._2), x._2))
    
    // To check if have convereged. If has convereged then no index has changed
    val hasConverged: Boolean = clusters.zip(newClusters).forall(x => x._1._1 == x._2._1)

    (newClusters, hasConverged)

  }


  def closestCluster(means: Map[Int, DenseVector[Double]], vect: DenseVector[Double]): Int = {
    val distances = means.map(x => {
      // TODO WHY asRow?
      val difference = vect.asRow - x._2.asRow
      val vectNorm = difference.norm(2)
      (x._1, vectNorm)
    })

    distances.minBy(_._2)._1 // Order by norm, return cluster index
  }
}