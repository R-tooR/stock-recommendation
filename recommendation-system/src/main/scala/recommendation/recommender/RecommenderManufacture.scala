package recommendation.recommender

import data.DataExtractor
import data.processor.{InvestorsFullGraphDataProcessor, InvestorsSubgraphDataProcessor}
import recommendation.engine.{StatefulEngine, StatelessEngine}
import recommendation.recommender.RecommenderType.{FULL_GRAPH, RecommenderType, TARGET_NODE_SUBGRAPH}

import java.util.Properties

object RecommenderManufacture {

  def createRecommender(recType: RecommenderType) = {
    recType match {
      case FULL_GRAPH => new FullGraphRecommender(new StatefulEngine(), new DataExtractor(new Properties()), new InvestorsFullGraphDataProcessor())
      case TARGET_NODE_SUBGRAPH => new SubgraphRecommender(new StatelessEngine(), new DataExtractor(new Properties()), new InvestorsSubgraphDataProcessor())
    }
  }

  def createFullGraphRecommender() = new FullGraphRecommender(new StatefulEngine(), new DataExtractor(new Properties()), new InvestorsFullGraphDataProcessor())

  def createSubgraphRecommender() = new SubgraphRecommender(new StatelessEngine(), new DataExtractor(new Properties()), new InvestorsSubgraphDataProcessor())
}

object RecommenderType extends Enumeration {
  type RecommenderType = Value
  val FULL_GRAPH, TARGET_NODE_SUBGRAPH = Value
}
