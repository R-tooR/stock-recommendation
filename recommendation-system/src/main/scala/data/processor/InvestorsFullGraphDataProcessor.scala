package data.processor

import data.queries.{GetAllInvestorsQuery, GetInvestorsQuery, GetTargetInvestorQuery}
import org.apache.spark.sql
import org.apache.spark.sql.DataFrame

import scala.util.Try

class InvestorsFullGraphDataProcessor extends DataProcessor {
  override def get(dfs: List[DataFrame]): Either[String, List[Iterable[Iterable[Double]]]] = {

    def getInvestorStocks(df: sql.DataFrame, target: sql.DataFrame) = Try {
//      val theirCompaniesList = df.select(GetAllInvestorsQuery.companies).collect().map(_.getAs[List[Nothing]](0))
      val theirCompaniesList = df.select(GetAllInvestorsQuery.companies).collect().toList.map(_.get(0))
      val targetCompaniesList = target.select(GetAllInvestorsQuery.companies).collect().toList.map(_.get(0))
//      val targetCompaniesList = target.select(GetAllInvestorsQuery.companies).collect().map(_.getAs[List[Nothing]](0))

      (targetCompaniesList ++ theirCompaniesList).toList.map(l => l.asInstanceOf[Seq[Nothing]].toSet)
    }

    def getInvestorCategories(df: sql.DataFrame, target: sql.DataFrame) = Try {
      val theirCategories = df.select(GetAllInvestorsQuery.categories).collect().toList.map(_.get(0))
//      val theirCategories = df.select(GetAllInvestorsQuery.categories).collect().map(_.getAs[List[Nothing]](0))
      val targetCategories = target.select(GetAllInvestorsQuery.categories).collect().toList.map(_.get(0))
//      val targetCategories = target.select(GetAllInvestorsQuery.categories).collect().map(_.getAs[List[Nothing]](0))

      (targetCategories ++ theirCategories).toList.map(l => l.asInstanceOf[Seq[Nothing]].toSet)
    }

    if(dfs.length == 2) {
      val jaccobi = (s1: Iterable[Nothing], s2: Iterable[Nothing]) => s1.toSet.intersect(s2.toSet).size / s1.toSet.union(s2.toSet).size.toDouble

      def similarityMatrix[T](list: Iterable[Iterable[T]], metric: (Iterable[T], Iterable[T]) => Double) = list map (set => {
        list map (set2 => metric(set, set2))
      })

      val investorsStocksMatrix = similarityMatrix(tryInAttempts(getInvestorStocks(dfs(0), dfs(1))), jaccobi)
      val investorsCategoriesMatrix = similarityMatrix(tryInAttempts(getInvestorCategories(dfs(0), dfs(1))), jaccobi)

      Right(List(investorsStocksMatrix, investorsCategoriesMatrix))
    } else {
      Left("There are not specified datasets for neighborhood and target")
    }
  }
}
