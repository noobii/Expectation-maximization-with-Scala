package ch.epfl.em
import scala.collection.GenSeq
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;

object TransferTools {

  def dataMatToGenSeq(data: DenseMatrix[Double]): GenSeq[DenseVector[Double]] = {
    for(i <- 0 until data.numRows) yield data(i, ::)
  }
  
  def dataGenSeqToMat(data: GenSeq[DenseVector[Double]]): DenseMatrix[Double] = {
    DenseMatrix.tabulate[Double](data.length, data.head.length)((x, y) => data(x)(y))
  }
}
