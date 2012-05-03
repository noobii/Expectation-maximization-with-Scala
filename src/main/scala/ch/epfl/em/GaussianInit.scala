package ch.epfl.em

import java.io.File

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

/**
 * 
 */
class InitFromMatlab(folderPath: String) extends GaussianInit {
  
  def weights = new FileParser(folderPath + "kmeanW.csv").toVectorDouble
  def means = new FileParser(folderPath + "kmeanM.csv").toMatrix
  
  def covariances = {
    val files = new File(folderPath).listFiles
    
    val covFiles = files filter(_.getName().contains("kmeanV")) sortBy(_.getName())
    
    covFiles map(x => new FileParser(x.getAbsolutePath).toMatrix)
  }
}
