package data.processor

import data.queries.{GetInvestorsQuery, GetTargetInvestorQuery}
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.spark.sql
import org.apache.spark.sql.{Column, DataFrame, Row}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.util.{Failure, Success, Try}

class InvestorsSubgraphDataProcessor extends DataProcessor {
  def get(dfs: List[DataFrame]): Either[String, List[Iterable[Iterable[Double]]]] = {

    def getInvestorStocks(df: sql.DataFrame, target: sql.DataFrame) = Try {
      val companiesList = df.select(GetInvestorsQuery.companiesNamesList).collectAsList()
      val theirCompaniesMap = df.select(GetInvestorsQuery.theirCompaniesMap).collectAsList()
      val targetCompaniesMap = target.select(GetTargetInvestorQuery.targetInvestorCompanies).collectAsList()
      val allTheirCompanies = (theirCompaniesMap zip companiesList map(r => r._1.getList(0).toIterable ++ r._2.getList(0).toIterable))
      val allTargetCompanies = (targetCompaniesMap map(r => r.getList(0).toIterable))
      val allCompanies = allTargetCompanies ++ allTheirCompanies
      (allCompanies map(r => r.toSet)).toList
    }

    def getInvestorCategories(df: sql.DataFrame, target: sql.DataFrame) = Try {
      val theirCategories = df.select(GetInvestorsQuery.allTheirCategories).collectAsList()
      val targetCategories = target.select(GetTargetInvestorQuery.targetInvestorCategories).collectAsList()
      val allCategories = targetCategories ++ theirCategories
      (allCategories map(r => r.getList(0).toSet)).toList
    }

    val jaccobi = (s1: Iterable[Nothing], s2: Iterable[Nothing]) => s1.toSet.intersect(s2.toSet).size/s1.toSet.union(s2.toSet).size.toDouble
    def similarityMatrix[T](list: Iterable[Iterable[T]], metric: (Iterable[T], Iterable[T]) => Double) = list map(set => {list map (set2 => metric(set, set2))})

    if (dfs.length == 2) {
      val investorsStocksMatrix = similarityMatrix(tryInAttempts(getInvestorStocks(dfs(0), dfs(1))), jaccobi)
      val investorsCategoriesMatrix = similarityMatrix(tryInAttempts(getInvestorCategories(dfs(0), dfs(1))), jaccobi)

      Right(List(investorsStocksMatrix, investorsCategoriesMatrix))
    } else {
      Left("There are not specified datasets for neighborhood and target")
    }
  }

  def collection2DToRealMatrix(nested: Iterable[Iterable[Double]]): Array2DRowRealMatrix = {
    val doubleArray = nested map(iter => iter.toArray) toArray

    new Array2DRowRealMatrix(doubleArray)
  }



}
// https://stackoverflow.com/questions/57530010/spark-scala-cosine-similarity-matrix <- konwersja do rowmatrix
