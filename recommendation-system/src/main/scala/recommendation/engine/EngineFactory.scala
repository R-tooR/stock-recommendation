package recommendation.engine

import recommendation.EngineMethods
import recommendation.recommender.{Recommender, SubgraphRecommender}

trait EngineFactory {
 def createRecommender : Recommender
 def createEngine : Engine
}
