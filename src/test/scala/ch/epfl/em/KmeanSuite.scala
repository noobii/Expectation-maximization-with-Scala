package ch.epfl.em

import scala.util.Random

import org.junit.Assert.assertFalse
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scalala.library.Library.Axis
import scalala.library.Library.mean
import scalala.library.Plotting.plot
import scalala.library.Plotting.saveas
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

class KmeanSuite extends AssertionsForJUnit {
  
  @Test def testComputeMean() {
    val x = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0)
    val y = DenseVector(5.0, 4.0, 3.0, 2.0, 1.0)

    val seq = Seq(x, y)
    val mean = Kmean.mean(seq)

    assert(mean == DenseVector(3.0, 3.0, 3.0, 3.0, 3.0))
  }
  
  @Test def testClosestCluster() {
    val means = Map(
        0 -> DenseVector.zeros[Double](5),
        1 -> DenseVector.ones[Double](5)
    )
    
    val vect0 = DenseVector.zeros[Double](5)
    val vect1 = DenseVector.ones[Double](5)
    val vect2 = DenseVector(2.0, 2.0, 2.0, 2.0, 2.0)
    
    assert(Kmean.closestCluster(means, vect0) == 0)
    assert(Kmean.closestCluster(means, vect1) == 1)
    assert(Kmean.closestCluster(means, vect2) == 1)
  }
  
  @Test def testInitializeClusters() {
    val matrix = DenseMatrix.ones[Double](10, 10)
    val k = 5
    
    val randClusters = Kmean.initializeClusters(matrix, k)
        
    // Check that there are as many rows as objects
    assert(randClusters.length == matrix.numRows)
    
    // Check that all clusters are in [0, k)
    assert(randClusters.forall(_._1 < k))
    assert(randClusters.forall(_._1 >= 0))
    
    // Check if the first row is correct
    assert(randClusters.head._2 == matrix(0, ::))
    
    // Check that all the clusters are present
    for(i <- 0 until k) {
      assert(randClusters.exists(_._1 == i))
    }
  }
  
  @Test def testComputeCentroids() {
    val values = Seq(
        (0, DenseVector(1., 2., 3., 4., 5.0)),
        (1, DenseVector.ones[Double](5)),
        (0, DenseVector(5., 4., 3., 2., 1.0)),
        (1, DenseVector.zeros[Double](5)),
        (2, DenseVector(2., 2., 2., 2., 2.))
    )
    
    val comp = Kmean.computeCentroids(values)

    assert(comp(0) == DenseVector(3., 3., 3., 3., 3.))
    assert(comp(1) == DenseVector(.5, .5, .5, .5, .5))
    assert(comp(2) == DenseVector(2., 2., 2., 2., 2.))
  }
  
  @Test def testAssignClust() {
    val clusts = Seq(
        (0, DenseVector.ones[Double](5)),
        (1, DenseVector.zeros[Double](5))
    )
    
    val means = Map(
        0 -> DenseVector.zeros[Double](5),
        1 -> DenseVector.ones[Double](5)
    )
    
    val iter1 = Kmean.assignClusts(clusts, means)
    assert(iter1._1.exists(x => x._1 == 0 && x._2 == DenseVector.zeros[Double](5)))
    assert(iter1._1.exists(x => x._1 == 1 && x._2 == DenseVector.ones[Double](5)))
    assertFalse(iter1._2)
    
    val iter2 = Kmean.assignClusts(iter1._1, means)
    assert(iter2._2)
  }
  
  /*
  @Test def testHugeKmean() {
    
    println("Start huge: " + new java.util.Date())
    val data = DenseMatrix.rand(1000000, 10)
    
    val kmeans = Kmean.kmeans(data, 2, 1000)
    
    println("End huge: " + new java.util.Date())
    assert(true)
  }*/
  
  @Test def testKmean() {
    
    for(k <- 0 until 3) {
    val data = DenseMatrix.zeros[Double](90, 2)
    
    for(i <- 0 until 30) data(i, ::) := randomVect(2, -0.1, 0.1)
    for(i <- 30 until 60) data(i, ::) := randomVect(2, 0.9, 1.1)
    for(i <- 60 until 90) data(i, ::) := randomVect(2, 1.9, 2.1)
    
    plot.hold = true
    plot(data(::, 0), data(::, 1), '.')
    
    val k = 3
    val maxIter = 1000
    
    val res = Kmean.kmeans(data, k, maxIter)
    
    val means = mean(res, Axis.Vertical).toList

    val mat = res
    plot(mat(0, ::), mat(1, ::), '+')

    saveas("plot.png")
    
        
    println("k mean: (should be around 0, 1 and 2)")
    println(means)
    
    assert(means.exists(x => -0.1 < x && x < 0.1))
    assert(means.exists(x => 0.9 < x && x < 1.1))
    assert(means.exists(x => 1.9 < x && x < 2.1))
    }
  }

  def randomVect(size: Int, from: Double, to: Double): DenseVector[Double] = {
    Random.setSeed(Random.nextLong())
    def randomDouble: Double = {
      val interval = to - from
      val middle = (from + to) / 2
    
      val normalized = interval * Random.nextDouble()
      val range = normalized - interval / 2
    
      middle + range      
    }
    
    val vect = DenseVector.zeros[Double](size)
    
    for(i <- 0 until size) vect(i) = randomDouble
    
    vect
  }
}