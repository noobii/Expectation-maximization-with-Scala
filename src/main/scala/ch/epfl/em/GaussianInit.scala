package ch.epfl.em
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector

trait GaussianInit {

  def init: MatricesTupple = {
    MatricesTupple(weights, means, covariances)
  }
  
  def weights: DenseVector[Double]
  def means: DenseMatrix[Double]
  def covariances: Array[DenseMatrix[Double]]

}