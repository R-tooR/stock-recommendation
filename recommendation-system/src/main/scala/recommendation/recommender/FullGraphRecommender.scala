package recommendation.recommender
import data.DataExtractor
import data.processor.{DataProcessor, InvestorsSubgraphDataProcessor}
import data.queries.{GetAllInvestorsQuery, GetInvestorsQuery, GetTargetInvestorQuery}
import org.apache.commons.math3.linear.RealMatrix
import recommendation.EngineMethods
import recommendation.EngineMethods.collection2DToRealMatrix
import recommendation.engine.Engine

import java.util
import scala.collection.mutable
import data.queries.GetAllInvestorsQuery.id

class FullGraphRecommender(engine: Engine, extractor: DataExtractor, processor: DataProcessor) extends Recommender() {

  override def findStocksBaseOnSimilarity(baseInvestor: Int, evaluation: (Double, Double)): (mutable.Map[String, Double], mutable.Map[String, Double]) = {
    val normalize = (d: Double) => 1 - (1/((0.1*d) + 1))
    val allInvestors = extractor.get(new GetAllInvestorsQuery())

    val surroundingInvestors = allInvestors.filter(s"$id == $baseInvestor")
    val targetInvestor = allInvestors.filter(s"$id <> $baseInvestor")
    val result = processor.get(List(surroundingInvestors, targetInvestor))

    val data = result match {
      case Right(value) => value map collection2DToRealMatrix
      case Left(value) => println(value)
        List[RealMatrix]()
    }

    val stocks = (matrix: RealMatrix) => {
      val similarities = engine.getTopNEmbeddings(matrix, data)
      val similarStocks = super.getTopNStocks(similarities, surroundingInvestors.select(GetAllInvestorsQuery.companies).collect(), evaluation)

      (similarStocks._1.map(entry => (entry._1, normalize(entry._2))), similarStocks._2)
    }

    engine.createConsensusEmbedding(data) match {
      case Right(matrix) => stocks(matrix)
      case Left(error) =>
        println(error)
        (mutable.Map[String, Double](), mutable.Map[String, Double]())
    }
  }
}
