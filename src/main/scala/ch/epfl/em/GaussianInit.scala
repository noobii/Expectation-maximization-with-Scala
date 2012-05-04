package ch.epfl.em

import java.io.File

import scalala.scalar._; 
import scalala.tensor.::; 
import scalala.tensor.mutable._; 
import scalala.tensor.dense._; 
import scalala.tensor.sparse._; 
import scalala.library.Library._; 
import scalala.library.LinearAlgebra._; 
import scalala.library.Statistics._; 
import scalala.library.Plotting._; 
import scalala.operators.Implicits._; 



trait GaussianInit {

  def init: MatricesTupple = {
    MatricesTupple(weights, means, covariances)
  }
  
  def weights: Vector[Double]
  def means: Matrix[Double]
  def covariances: Array[Matrix[Double]]

}

/**
 * Can be used to initialize the EM algo with the data that matlab typically outputs.
 * The files must have name "kmeanW.csv", "kMeanM.csv" and "kMeanV_.csv" where _ is the index of the covariance matrix 
 */
class InitFromMatlab(folderPath: String) extends GaussianInit {
  
  def weights = new FileParser(folderPath + "kmeanW.csv").toMatrix(0, ::).asCol

  def means = new FileParser(folderPath + "kmeanM.csv").toMatrix
  
  def covariances = {
    val files = new File(folderPath).listFiles
    
    val covFiles = files filter(_.getName().contains("kmeanV")) sortBy(_.getName())
    
    covFiles map(x => new FileParser(x.getAbsolutePath).toMatrix)
  }
}
