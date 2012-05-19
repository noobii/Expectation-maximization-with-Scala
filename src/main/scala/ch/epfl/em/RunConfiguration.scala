package ch.epfl.em

import scala.xml.XML._
import scala.collection.GenSeq
import scalala.tensor.dense.DenseVector

object RunConfiguration {
  
  def load(configFile: String) = {
  
    val elem = loadFile(configFile: String) 
  
    for(setup <- elem \\"run") yield {

      // Get the name of the run
      val name = (setup \\ "name").text
    
      // Get the data
      val dataPath = (setup \\ "data").text
      val X = FileParser(dataPath).data
      
      // Get the number of gaussian components
      val k = (setup \\ "k").text.toInt
    
      lazy val strategy = new InitFromMatlab((setup \\ "init" \ "folder").text)
      
      RunConfiguration(name, X, k, strategy)
    }

  
  }
  
  
}

case class RunConfiguration(
    val name: String,
    val data: GenSeq[DenseVector[Double]], 
    val k: Int, 
    val strategy: GaussianInit) {
  val parData = data.par
  def initStrategy = strategy.init
}