package calculator.indicators

class RSI(length: Int) extends Indicator[Double, Double] {
  var avgLoss = .0
  var avgGain = .0
  var lastChange: Vector[Double] = Vector()
  var recentClose = .0

  /**
   *
   * @param data List, which contains: Close
   */
  def initialize(data: Vector[Double]): Double = {
    if (data.size >= length) {
      val first = data.take(length)
      val averageChange = first.slice(1, length) zip first.slice(0, length - 1) map (x => x._1 - x._2)//avgChange
      lastChange = averageChange
      recentClose = first(length - 1)
      val change = averageChange partition (_ > 0)
      avgGain = change._1.sum / (length-1)
      avgLoss = change._2.sum / (length-1)

      return 100 - (100 / (1 + (avgGain / math.abs(avgLoss))))// mth rint * 100 / 100
    }
    0.0
  }

  def update(datum: Double): Double = {
    //      if (lastChange.head > 0) avgGain = updateAvg((avgGain, 14), lastChange.head, _ - _)._1
    //      else avgLoss = updateAvg((avgLoss, 14), lastChange.head, _ - _)._1
    //
    //      val change = avgChange((datum, recentClose))
    //      if (change > 0) avgGain = updateAvg((avgGain, 14), change, _ + _)._1
    //      else avgLoss = updateAvg((avgLoss, 14), change, _ + _)._1

    val change = datum - recentClose
    recentClose = datum
    if(change > 0) {
      avgGain = ((length-2)*avgGain + change)/(length-1)
      avgLoss = ((length-2)*avgLoss)/(length-1)
    } else {
      avgGain = ((length-2)*avgGain)/(length-1)
      avgLoss = ((length-2)*avgLoss + change)/(length-1)
    }
    lastChange = lastChange.slice(1, lastChange.size) ++ Vector(change)
    100 - (100 / (1 + (avgGain / math.abs(avgLoss))))
  }
}

