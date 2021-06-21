package data.queries

import data.queries.GetAllInvestorsQuery.{categories, companies, id}

class GetAllInvestorsQuery extends Query {
  override def get: String = "match (inv:Investor)-[rel1]-(cp:Company)-[rel2]-(ind:Industry)-[rel3]-(cat:Category) " +
    s"return inv.id as $id, " +
    s"collect(distinct cp.symbol) as $companies," +
    s"collect(distinct cat.category) as $categories " +
    "order by inv.id"
}

object GetAllInvestorsQuery {
  val id = "id"
  val companies = "companies"
  val categories = "categories"
}
