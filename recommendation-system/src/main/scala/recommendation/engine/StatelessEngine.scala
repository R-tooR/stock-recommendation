package recommendation.engine

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, DiagonalMatrix, RealMatrix}
import recommendation.EngineMethods

class StatelessEngine extends Engine {

  override def createConsensusEmbedding(dflist: List[RealMatrix]): Either[String, RealMatrix] = {
    val matrices = EngineMethods.createDiagonalAndLaplacianMatrix(dflist)
    if (dflist.isEmpty && dflist.head.getRowDimension < 1)
      return Left("Cannot calculate due to lack of neighbors of target node")

    val result = EngineMethods.eigenDecomposition(matrices._1, matrices._2)
    val consensusEmb = EngineMethods.mergeIntermediateEmbeddings(result._1)
    Right(EngineMethods.calculateConsensusEmbedding(consensusEmb)._1)
  }

  override def getTopNEmbeddings(embeddings: RealMatrix, dflist: List[RealMatrix]): Array[Double] = {
    EngineMethods.getTopNEmbeddings(embeddings, dflist)
  }
}
