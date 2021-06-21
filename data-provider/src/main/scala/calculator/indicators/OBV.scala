package calculator.indicators

class OBV extends Indicator[(Double, Double), Double] {
  var latestClose: Double = .0
  var value: Double = .0
  /**
   *
   * @param data (Close, Volume)
   * @return
   */
  val lossOrGainEps: (Double, Double) => Int = (x, eps) => if(x > 0+eps) 1 else if(x < 0-eps) -1 else 0
  val lossOrGain: Double => Int = x => lossOrGainEps(x, 0.01)

  override def initialize(data: Vector[(Double, Double)]): Double = {
    val close = data map(_._1)
    //rozważ jakiś epsilon, żeby małe zmiany też były zerami
    val averageChange = close.slice(1, data.length) zip close.slice(0, data.length - 1) map avgChange map lossOrGain
    latestClose = close(close.size-1)
    value = (data slice(1, data.size) map(_._2)) zip averageChange map(x => x._1 * x._2) sum

    value
  }

  override def update(datum: (Double, Double)): Double = {
    val change = datum._1 - latestClose
    value = value + lossOrGain(change)*datum._2
    latestClose = datum._1
    value
  }
}
