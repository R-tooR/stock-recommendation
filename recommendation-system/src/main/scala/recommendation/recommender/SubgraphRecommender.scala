package recommendation.recommender

import data.DataExtractor
import data.processor.InvestorsSubgraphDataProcessor
import data.queries.{GetInvestorsQuery, GetTargetInvestorQuery}
import org.apache.commons.math3.linear.RealMatrix
import recommendation.engine.Engine
import data.processor.DataProcessor


class SubgraphRecommender(engine: Engine, extractor: DataExtractor, processor: DataProcessor, limit: Int = 50) extends Recommender() {

  def findStocksBaseOnSimilarity(baseInvestor: Int, evaluation: (Double, Double) = (0.0, 0.0)) = {
    val surroundingInvestors = extractor.get(new GetInvestorsQuery(baseInvestor))
    val targetInvestor = extractor.get(new GetTargetInvestorQuery(baseInvestor))

    val result = processor.get(List(surroundingInvestors.limit(limit), targetInvestor))

    val data = result match {
      case Right(value) => value map collection2DToRealMatrix
      case Left(value) => println(value)
        List[RealMatrix]()
    }

    val stocks = (matrix: RealMatrix) => {
      val similarities = engine.getTopNEmbeddings(matrix, data)
      val similarStocks = super.getTopNStocks(similarities, surroundingInvestors.select(GetInvestorsQuery.theirCompaniesMap).take(limit), evaluation)

      (similarStocks._1.map(entry => (entry._1, normalize(entry._2))), similarStocks._2)
    }

    handleResult(engine, data, stocks)
  }

}
