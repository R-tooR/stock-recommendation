package data.queries

import data.queries.GetInvestorsQuery.{allTheirCategories, commonCategories, commonCompaniesCount, companiesNamesList, id, theirCompaniesCount, theirCompaniesMap}

class GetInvestorsQuery(targetId: Int) extends Query {


  override def get: String = " match (i1:Investor)-[rel0]-(c1:Company)-[rel1]-(i2:Investor)" +
    " match (cat2:Category)-[rr]-(:Industry)-[ss]-(c2:Company)-[rel2]-(i2)" +
    " match (c1)-[rel3]-(i:Industry)-[rel4]-(cat:Category)" +
    " match (i2)-[rel5]-(cat2:Category)" +
    s" where i1.id=$targetId and not (c2)-[:POSSESS]-(i1)" +
    s" return i2.id as $id," +
    s" count(distinct c1) as $commonCompaniesCount," +
    s" collect(distinct c1.symbol) as $companiesNamesList," +
    s" count(distinct c2) as $theirCompaniesCount," +
    s" collect(distinct c2.symbol) as $theirCompaniesMap," +
//    s" collect(distinct {name: c2.symbol, category: cat2.category}) as $theirCompaniesMap," +
    s" collect(distinct cat.category) as $commonCategories, " +
    s" collect(distinct cat2.category) as $allTheirCategories " +
    s" order by $commonCompaniesCount descending "
}

object GetInvestorsQuery{
  val id = "id"
  val commonCompaniesCount = "commonCompaniesCount"
  val companiesNamesList = "companiesNamesList"
  val theirCompaniesCount = "theirCompaniesCount"
  val theirCompaniesMap = "theirCompaniesMap"
  val commonCategories = "commonCategories"
  val allTheirCategories = "theirCategories"
}

// match (i1:Investor)-[rel0]-(c1:Company)-[rel1]-(i2:Investor)
// match (cat2:Category)-[rr]-(:Industry)-[ss]-(c2:Company)-[rel2]-(i2)
// match (c1)-[rel3]-(i:Industry)-[rel4]-(cat:Category)
// where i1.id=0 and not (c2)-[:POSSESS]-(i1)
// return i2.id,
// count(distinct c1) as commonCompanies,
// collect(distinct c1.symbol) as companiesNames,
// count(distinct c2) as theirCompanies,
// collect({name: c2.symbol, category: cat2.category}) as theirCompaniesNames,
// collect(distinct cat.category) as list order by commonCompanies
// descending limit 50