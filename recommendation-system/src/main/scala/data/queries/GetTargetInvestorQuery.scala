package data.queries

import data.queries.GetTargetInvestorQuery.{targetInvestorCategories, targetInvestorCompanies}

class GetTargetInvestorQuery(targetId: Int) extends Query {
  override def get: String = "match (iv:Investor)-[rel0]-(cp:Company)-[rel1]-(i:Industry)-[rel2]-(c:Category) " +
    s"where iv.id = $targetId " +
    s"return collect(distinct cp.symbol) as $targetInvestorCompanies, " +
    s"collect(distinct c.category) as $targetInvestorCategories";
}

object GetTargetInvestorQuery{
  val id = "id"
  val targetInvestorCompanies = "targetCompanies"
  val targetInvestorCategories = "targetCategories"

}
