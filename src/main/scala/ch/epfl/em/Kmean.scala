package ch.epfl.em

import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import scalala.operators._
import scalala.tensor._
import scalala.library.LinearAlgebra._
import scalala.library.Library._
import scala.util.Random

object Kmean {

  def kmeans(data: DenseMatrix[Double], k: Int, maxIter: Int): (Map[Int, DenseVector[Double]], DenseMatrix[Double]) = {

    var clusts = prepareClusters(data, k)
    var centoids = computeCentroids(clusts)
    //println("cent: " + centoids)
    var hasConverged = false

    for(i <- 0 until maxIter if !hasConverged) {
      val tmp = Kmean.assignClusts(clusts, centoids)
      clusts = tmp._1
      hasConverged = tmp._2
      centoids = computeCentroids(clusts)
      //print(i + "-")
    }

    // TODO delete
    //println("--------------")
    //val toto = for(i <- clusts.groupBy(_._1)) yield i._2.size
    ///println(toto)
    //println("--------------")
    // END DELETE
    
    // In a matrix where means in columns
    val matrix = DenseMatrix.zeros[Double](data.numCols, k)
    for(i <- 0 until centoids.size) matrix(::, i) := centoids(i)
    
    (centoids, matrix)

  } 

  // Test suite exists
  def prepareClusters(data: DenseMatrix[Double], k: Int): Seq[(Int, DenseVector[Double])] = {
    // Not so very random but at least we are sure that all clusters indexes are represented
    // Why the random int? because sometimes the algo doesn't converge and with the random offset we might be lucky...
    // but certainly not optimal...
    val randInt = Math.abs(Random.nextInt())
    for(i <- 0 until data.numRows) yield (i % k, data(i, ::))
  }

  // Test suite exists
  def computeCentroids(clusters: Seq[(Int, DenseVector[Double])]): Map[Int, DenseVector[Double]] = {
    val groups = clusters.groupBy(_._1)
    val groupedLists = groups.map(x => (x._1, x._2.unzip._2))
    
    val means = groupedLists.map(x => (x._1, mean(x._2)))
    
    means
  }

  // Test suite exists
  def mean(vects: Seq[DenseVector[Double]]): DenseVector[Double] = {
    val sum = vects.foldLeft(DenseVector.zeros[Double](vects.head.size))((x, y) => x + y)
    sum := sum :/ vects.size
    sum
  }

  // Test suite exists
  def assignClusts(clusters: Seq[(Int, DenseVector[Double])], means: Map[Int, DenseVector[Double]]): (Seq[(Int, DenseVector[Double])], Boolean) = {
    val newClusters = clusters.map(x => (closestCluster(means, x._2), x._2))
    
    // To check if have convereged. If has convereged then no index has changed
    val hasConverged: Boolean = clusters.zip(newClusters).forall(x => x._1._1 == x._2._1)

    (newClusters, hasConverged)

  }


  // Test suite exists
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