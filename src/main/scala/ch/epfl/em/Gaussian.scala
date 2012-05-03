package ch.epfl.em

import scala.Array.canBuildFrom
import scala.math.Pi
import scalala.library.Library.Axis
import scalala.library.Library.abs
import scalala.library.Library.covariance
import scalala.library.Library.exp
import scalala.library.Library.log
import scalala.library.Library.mean
import scalala.library.Library.pow
import scalala.library.Library.sqrt
import scalala.library.Plotting._
import scalala.library.LinearAlgebra.det
import scalala.library.LinearAlgebra.inv
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}

class Chrono {
  
  private var currentCount = 0l
  
  private var lastStart = 0l
  
  def start { lastStart = System.currentTimeMillis() }
  def stop { currentCount += (System.currentTimeMillis() - lastStart)}
  
  def reset {currentCount = 0l}
  
  def count = currentCount
}

object EChrono extends Chrono
object MChrono extends Chrono
object GChrono extends Chrono

case class MatricesTupple(weights: DenseVector[Double], means: DenseMatrix[Double], covariances: Array[DenseMatrix[Double]])

object Gaussian {
  def main(args: Array[String]): Unit = {
  
    val k10k = 3
    val X10k = FileParser("src/test/ressources/em/10k/X.csv").toMatrix
    val strategy10k = new Kmean(X10k, k10k)
    val gaussian = new Gaussian(strategy10k)(X10k, k10k)
    
    gaussian.runAlgo
    
    
    println("Weights should be: (0.6, 0.2, 0.2)")
    println("Means should be:")
    println("0.5\t3\t3")
    println("0\t-2\t3")
    
    printStatus("Runing algo 50k")
    
    val k50k = 6
    val X50k = FileParser("src/test/ressources/em/50k/X.csv").toMatrix
    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
    val gaussian50k = new Gaussian(strategy50k)(X50k, k50k)
    
    val out = gaussian50k.runAlgo
    
    plot.hold = true
    plot(X50k(::, 0), X50k(::, 1), '.')
    plot(out.means(0, ::), out.means(1, ::), '+', "b")
    
    /*
    val fileName2 = "src/test/ressources/em/50k/X.csv"
    val k2 = 6
    
    printStatus("Read file")
    val data2 = FileParser(fileName2).toMatrix
    printStatus("File read")
    
    val g2 = new Gaussian(data2, k2)
    
    g2.runAlgo*/
  }
  
  protected def printStatus(text: String) {
    val now = new java.util.Date
    
    println(now + ": " + text)
  }
  
  /*
  protected def printStatusAround(toSay: String)(toRun: Unit): Unit = {
    printStatus(toSay + ": start!")
    toRun
    printStatus(toSay + ": done!")
  }*/
  /*
  def fromFile(dataSource: String, gaussianComp: Int, initStrategy: GaussianInit): Gaussian = {
    printStatus("Reading file: " + dataSource)
    val data = FileParser(dataSource).toMatrix
    printStatus("File read")
    
    new Gaussian(data, gaussianComp, initStrategy)
  }*/
}

class Gaussian(initStrategy: GaussianInit)(data: DenseMatrix[Double], gaussianComponents: Int) {
  import Gaussian.printStatus // is this really the best way to do it?
  
  private val measurements = data.numRows
  private val dimensions = data.numCols
  
  def runAlgo = {
    
    printStatus("Init data")
    val initial = initStrategy.init
    printStatus("Data init")
    
    printStatus("Run algo"); GChrono.start
    val (est, lg) = em(initial, 0.1, 1000)
    printStatus("End algo"); GChrono.stop
        
    println("Weight: \n" + est.weights)
    println("Means: \n" + est.means)
    
    println("Time: " + GChrono.count/1000.0)
    
    est
  }
  
  /*
  def initEmKmean: MatricesTupple = {

    val kmean = new Kmean(data, gaussianComponents)
    //val (initialMeans, clusters) = kmean.compute(Int.MaxValue)
    val initialMeans = kmean.means
    val initialCovariances = kmean.covariances
    val initialWeights = kmean.weights
    
    MatricesTupple(initialWeights, initialMeans, initialCovariances)
  }
  */
  /*
  def initEmMatlab: (DenseVector[Double], DenseMatrix[Double], Array[DenseMatrix[Double]]) = {
    
  }*/

  /**
   * The implementation of the Expecatation-maximization algorithm
   */
  def em(
      estimates: MatricesTupple, 
      minLikelihoodVar: Double, 
      maxIter: Int
      ): (MatricesTupple, Double) = {

    var iterations = 0;
    
    var Ln = minLikelihoodVar
    var Lo = 2 * Ln
    
    def approxGoodEnough = (abs(100*(Ln - Lo) / Lo) <= minLikelihoodVar)
    
    var lEstW: DenseVector[Double] = estimates.weights
    var lEstM: DenseMatrix[Double] = estimates.means
    var lEstC: Array[DenseMatrix[Double]] = estimates.covariances
    
    
    while(!approxGoodEnough && (iterations < maxIter)) {
      val exp = expectation(MatricesTupple(lEstW, lEstM, lEstC))
      val maxRes = maximization(exp)
      
      lEstW = maxRes.weights
      lEstM = maxRes.means
      lEstC = maxRes.covariances
      Lo = Ln
      Ln = likelihood(MatricesTupple(lEstW, lEstM, lEstC))
      iterations += 1
    }
    
    (MatricesTupple(lEstW, lEstM, lEstC), Ln)
  }

  /**
   * Expectation part of the algorithm.
   * Return the expectation of the value
   */
  def expectation(estimates: MatricesTupple): DenseMatrix[Double] = {

    val nEstC = estimates.covariances map {matrix => 
      if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
      else matrix
    }
    
    val S = nEstC map (matrix => sqrt(det(matrix)))
    val invEstC = nEstC map (matrix => inv(matrix))

    val a = pow(2 * Pi, dimensions / 2.0)

    val E = DenseMatrix.tabulate[Double](data.numRows, gaussianComponents)((i, j) => {
        val delta = data(i, ::).asCol - estimates.means(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))
      
        estimates.weights(j) * pl
      }
    )

    def normalize(v: DenseVector[Double]) = v :/ v.sum
    
    // Make all row have 1 as element sum
    // Don't know yet how to make this any more functionnal... scalala doesn't provide a map per line
    for(i <- 0 until E.numRows) E(i, ::) := normalize(E(i, ::))
    
    E
  }

  /**
   * Maximization part of the algorithm.
   * Returns Estimated weight, mean and covariance
   */
  def maximization(estimate: DenseMatrix[Double]): MatricesTupple = {

    val estWeight = DenseVector.tabulate(gaussianComponents)(i => estimate(::, i).sum)
    
    val estMean = DenseMatrix.tabulate[Double](dimensions, gaussianComponents)((dim, comp) => {
      val col: DenseVector[Double] = data(::, dim)
      
      val weightCol = col.mapPairs((index: Int, value: Double) => value * estimate(index, comp))
      
      val weightSum = weightCol.sum / estWeight(comp)
      weightSum
    })

    val estCovariance = (0 until gaussianComponents) map(index => {
      val matrix = DenseMatrix.zeros[Double](dimensions, dimensions)
      for(j <- 0 until measurements) {
        val dXM = data(j, ::).asCol - estMean(::, index)
        matrix += (dXM * dXM.t) :* estimate(j, index)
      }
      matrix := matrix / estWeight(index)
    }) toArray
    
    estWeight := estWeight / measurements
    
    MatricesTupple(estWeight, estMean, estCovariance)
  }
  
  // This data will be used several times and do not need to be recomputed
  var meanVect = mean(data, Axis.Vertical).asCol
  var covarianceMat = covariance(data, Axis.Vertical)._1
  
  /**
   * Computes the log-likelihood that the estimated values are correct.
   */
  def likelihood(estimate: MatricesTupple): Double = {

    val estCWithIndex = estimate.covariances zipWithIndex

    // Thought it could be an elegant solution to use a map with indices
    val elements = estCWithIndex.map {
      case (matrix, index) => {
        val invEstC = inv(matrix)
      
        val lg = log(det(matrix * 2 * Pi))
        val tr = (invEstC * covarianceMat).trace + (meanVect - estimate.means(::, index)).t * invEstC * (meanVect - estimate.means(::, index))
      
        estimate.weights(index) * (-0.5 * measurements * lg - 0.5 * (measurements - 1) * tr)
      }
    }
    
    elements.sum
  } 

}