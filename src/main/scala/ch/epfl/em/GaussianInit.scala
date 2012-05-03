package ch.epfl.em

import java.io.File

import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalala.tensor.{:: => ::}


trait GaussianInit {

  def init: MatricesTupple = {
    println("toto")
    println(weights)
    println("tata")
    println(means)
    println("titi")
    MatricesTupple(weights, means, covariances)
  }
  
  def weights: DenseVector[Double]
  def means: DenseMatrix[Double]
  def covariances: Array[DenseMatrix[Double]]

}

/**
 * Can be used to initialize the EM algo with the data that matlab typically outputs.
 * The files must have name "kmeanW.csv", "kMeanM.csv" and "kMeanV_.csv" where _ is the index of the covariance matrix 
 */
class InitFromMatlab(folderPath: String) extends GaussianInit {
  
  def weights = {
    val mat = new FileParser(folderPath + "kmeanW.csv").toMatrix
    println("mat")
    println(mat)
    
    mat(0, ::).asCol
  }
  def means = new FileParser(folderPath + "kmeanM.csv").toMatrix
  
  def covariances = {
    val files = new File(folderPath).listFiles
    
    val covFiles = files filter(_.getName().contains("kmeanV")) sortBy(_.getName())
    
    covFiles map(x => new FileParser(x.getAbsolutePath).toMatrix)
  }
}
