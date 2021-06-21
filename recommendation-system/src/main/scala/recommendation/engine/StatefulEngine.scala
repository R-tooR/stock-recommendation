package recommendation.engine

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, DiagonalMatrix, RealMatrix}
import recommendation.EngineMethods

class StatefulEngine extends Engine {
  var previousDfList = List.empty[RealMatrix]
  var previousDiagonal = List.empty[DiagonalMatrix]
  var previousLaplacian = List.empty[RealMatrix]
  private var initialization = true
  private var eigenValues: List[Array[Double]] = List.empty[Array[Double]]
  private var eigenVectors: List[RealMatrix] = List.empty[RealMatrix]
  private var consensusEmbeddingResult: RealMatrix = new Array2DRowRealMatrix()
  private var stateChanged: Boolean = true


  override def createConsensusEmbedding(dflist: List[RealMatrix]): Either[String, RealMatrix] = {
    val matrices = EngineMethods.createDiagonalAndLaplacianMatrix(dflist)
    if (dflist.isEmpty && dflist.head.getRowDimension < 1)
      return Left("Cannot calculate due to lack of neighbors of target node")

    if (initialization) {
      val result = EngineMethods.eigenDecomposition(matrices._1, matrices._2)
      eigenVectors = result._1
      eigenValues = result._2
      initialization = false
    } else {
      val result = EngineMethods.updatedEigenDecomposition(matrices._1, previousDiagonal, matrices._2, previousLaplacian, eigenValues, eigenVectors)
      eigenVectors = result._1
      eigenValues = result._2
    }

    previousDiagonal = matrices._1
    previousLaplacian = matrices._2
    val consensusEmb = EngineMethods.mergeIntermediateEmbeddings(eigenVectors)
    consensusEmbeddingResult = EngineMethods.calculateConsensusEmbedding(consensusEmb)._1

    Right(consensusEmbeddingResult)
  }

  override def getTopNEmbeddings(embeddings: RealMatrix, dflist: List[RealMatrix]): Array[Double] = {
    EngineMethods.getTopNEmbeddings(embeddings, dflist)
  }
}
