package recommendation.engine

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, DiagonalMatrix, RealMatrix}

trait Engine {
//  def createDiagonalAndLaplacianMatrix(dflist: List[RealMatrix]): (List[DiagonalMatrix], List[RealMatrix])
  def createConsensusEmbedding(dflist: List[RealMatrix]): Either[String, RealMatrix]
//  def createIntermediateEmbeddings(dflist: List[RealMatrix], diagonal: List[DiagonalMatrix], laplacian: List[RealMatrix]): List[RealMatrix]
//  def calculateConsensusEmbedding(consensusEmbedding: RealMatrix): (Array2DRowRealMatrix, Array[Double])
  def getTopNEmbeddings(embeddings: RealMatrix, dflist: List[RealMatrix]): Array[Double]

}
