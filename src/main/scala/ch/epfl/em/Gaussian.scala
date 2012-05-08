package ch.epfl.em

import scala.Array.canBuildFrom
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
import ch.epfl.em.Conversions._

case class MatricesTupple(weights: DenseVector[Double], means: DenseMatrix[Double], covariances: Array[DenseMatrix[Double]])

object Gaussian {
  def main(args: Array[String]): Unit = {
    
    /*
    printStatus("Runing algo 10k")

    val k10k = 3
    val X10k = FileParser("src/test/ressources/em/10k/X.csv").data
    val strategy10k = new Kmean(X10k, k10k)
    val gaussian = new Gaussian(strategy10k)(X10k, k10k)
    
    gaussian.runAlgo
    */
    /*
    println("Weights should be: (0.6, 0.2, 0.2)")
    println("Means should be:")
    println("0.5\t3\t3")
    println("0\t-2\t3")*/
    
    printStatus("Runing algo 50k")
    
    val k50k = 6
    val X50k = FileParser("src/test/ressources/em/50k/X.csv").data
    val strategy50k = new InitFromMatlab("src/test/ressources/em/50k/")
    val gaussian50k = new Gaussian(strategy50k)(X50k, k50k)
    
    val out = gaussian50k.runAlgo
    
    printStatus("Runing algo 600k")
    val k500k = 5
    val X500k = FileParser("src/test/ressources/em/500k/X.csv").data
    val strategy500k = new InitFromMatlab("src/test/ressources/em/500k/")
    val gaussian500k = new Gaussian(strategy500k)(X500k, k500k)
    
    val out500k = gaussian500k.runAlgo
  }
  
  protected def printStatus(text: String) {
    val now = new java.util.Date
    
    println(now + ": " + text)
  }
  
}

class Gaussian(initStrategy: GaussianInit)(dataIn: GenSeq[DenseVector[Double]], gaussianComponents: Int) {
  import Gaussian.printStatus // is this really the best way to do it?

  private val measurements = dataIn.length
  private val dimensions = dataIn.head.length
  
  private val data = dataIn.par
  
  private val zeroUntilGaussianComp = (0 until gaussianComponents).toArray.par
  
  def runAlgo = {
    
    printStatus("Init data")
    val initial = initStrategy.init
    
    printStatus("Run algo"); 
    GChrono.start
    val (est, lg) = em(initial, 0.05, 10000)
    GChrono.stop
    
    println("Time: " + GChrono.count/1000.0)

    GChrono.reset
    
    est
  }

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
    
    var lEstW = estimates.weights
    var lEstM = estimates.means
    var lEstC = estimates.covariances
    
    
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
  def expectation(estimates: MatricesTupple): GenSeq[DenseVector[Double]] = {

    val nEstC = estimates.covariances map {matrix => 
      if(matrix forallValues(_ == 0.0)) DenseMatrix.fill[Double](dimensions, dimensions)(Double.MinValue)
      else matrix
    }
    
    val S = nEstC map (matrix => sqrt(det(matrix)))
    val invEstC = nEstC map (matrix => inv(matrix))

    val a = pow(2 * Pi, dimensions / 2.0)

    def normalize(v: DenseVector[Double]) = v :/ v.sum

    val E = data map(point => {
      val vector = DenseVector.tabulate[Double](gaussianComponents)(j => {
        val delta = point.asCol - estimates.means(::, j)
        val coef = delta.t * invEstC(j) * delta
        val pl = exp(-0.5 * coef) / (a * S(j))
        
        estimates.weights(j) * pl
      })
      
      normalize(vector)
    })
    
    E
  }

  /**
   * Maximization part of the algorithm.
   * Returns Estimated weight, mean and covariance
   */
  def maximization(estimate: GenSeq[DenseVector[Double]]): MatricesTupple = {
        
    val estWeight = estimate reduce(_ + _)
    
    // The weights repeated in each line of a (dim, gaussianComp) matrix
    val weightsAsMatrix = DenseVector.ones[Double](dimensions).asCol * estWeight.asRow
    
    val estMean = ((data zip estimate) map{case(point, est) =>
      point.asCol * est.asRow 
    } reduce(_ + _)) :/ weightsAsMatrix
    
    val estCovariance = zeroUntilGaussianComp map(k => {
      val sumMat = ((data zip estimate) map {case(point, est) =>
        val dXM = point.asCol - estMean(::, k)
        (dXM * dXM.t) :* est(k)
      }) reduce (_ + _)
      
      /*
      def co(point: DenseVector[Double], est: DenseVector[Double]) = {
        val dXM = point.asCol - estMean(::, k)
        (dXM * dXM.t) :* est(k)
      }
      
      val sumMat = ((data zip estimate).foldLeft(DenseMatrix.zeros[Double](dimensions, dimensions)){
          case(runingSum, (point, est)) => runingSum + co(point, est)}
      )*/
            
      sumMat :/ estWeight(k)
    })
    
    estWeight := estWeight / measurements
    
    MatricesTupple(estWeight, estMean, estCovariance.seq.toArray)
  }
  
  // This data will be used several times and do not need to be recomputed
  var meanVect = mean(data)
  var covarianceMat = covarianceOfData(data)
  
  def mean(data: GenSeq[DenseVector[Double]]): DenseVectorCol[Double] = {
    data.reduce(_ + _).asCol / data.length
  }
  
  def covarianceOfData(data: GenSeq[DenseVector[Double]]): DenseMatrix[Double] = {
    covariance(dataGenSeqToMat(data), Axis.Vertical)._1
  }
  
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