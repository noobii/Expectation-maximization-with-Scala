package ch.epfl.em

import scala.xml.XML._
import scala.collection.GenSeq
import scalala.tensor.dense.DenseVector

object RunConfiguration {
  
  def load(configFile: String) = new Iterator[RunConfiguration] {
    
    private val elem = loadFile(configFile)
    
    private val ownIterator = (elem \\ "run").iterator
    
    def hasNext = ownIterator.hasNext
    def next = {
      val setup = ownIterator.next
      
      // Get the name of the run
      val name = (setup \\ "name") text
    
      // Get the data
      val dataPath = (setup \\ "data").text
      val X = FileParser(dataPath).data
      
      // Get the number of gaussian components
      val k = (setup \\ "k").text.toInt
    
      val strategy: GaussianInit = if(true) {
        new InitFromMatlab((setup \\ "init" \ "folder").text)
      } else {
        // TODO init from kmeans
        throw new Exception("Should be kmeans")
        null
      }
      
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