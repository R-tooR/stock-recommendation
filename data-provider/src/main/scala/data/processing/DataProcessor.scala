package data.processing

class DataProcessor {

  def getBatch(lines: Iterable[String]): Vector[(Double, Double, Double, Double, Double)] = {
    lines map getLine toVector
  }

  def getLine(line: String): (Double, Double, Double, Double, Double) = {
    val lineArray = line split ','
    val processed = lineArray slice(1, lineArray.length) map toNumber //andThen (x => (x(0), x(1), x(2), x(3), x(4)))

    toTuple5(processed)
  }

  private def toNumber(s: String) = {
    val money = "\\$\\d{1,}(\\.\\d{1,})?".r
    s match {
      case money(_*) => s.substring(1).toDouble
      case "N/A" => 0.0
      case _ => s.toInt
    }
  }

  val toTuple5: PartialFunction[Array[Double], (Double, Double, Double, Double, Double)] = {
    case x if x.length == 5 => (x(0), x(1), x(2), x(3), x(4))
    case _ => (0.0, 0.0, 0.0, 0.0, 0.0)
  }
}
