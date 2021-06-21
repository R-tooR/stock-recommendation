package calculator.indicators

class EMA(length: Int) extends Indicator[Double, Double] {

  private var EMAvalue = .0
  private var lastChange: Vector[Double] = Vector()
  private var recentClose = .0

  override def initialize(data: Vector[Double]): Double = {
    if (data.size >= length) {
      val first = data.take(length)
      lastChange = first
      recentClose = first(first.length - 1)
      EMAvalue = first.sum / first.size
    }
    EMAvalue
  }

  override def update(datum: Double): Double = {
    val multiplicator = 2 / (1 + length)

    EMAvalue = datum * multiplicator + EMAvalue * (1 - multiplicator)
    EMAvalue
  }
}
