package calculator.indicators

import calculator.Round
import calculator.Round.Round

/**
 * Dodać jakieś asymptoty ukośne?
 *
 * @param length
 * @param round
 */
abstract class PriceBoundary(length: Int, round: Round) extends Indicator[(Double, Double, Double, Double), (Double, Double)] {
//  assert(length == 7 || length == 14 || length == 30)
  //    var lows: Vector[Double] = Vector()
  //    var averages: Vector[Double] = Vector()
  //    var roundedLows: Vector[Double] = Vector()
  //    var roundedAverages: Vector[Double] = Vector()
  var boundary = .0
  var lastValue = .0
  var roundedAverage = .0

  val c4 = Map(7 -> 0.95937, 14 -> 0.98097, 30 -> 0.99142)

  def sigm(d: Double) = (1.0 / 2) / (1 + math.exp(-(1.0 / 2) * d)) + 0.75

  /**
   * Dobra dla krótkich okresów, dla dłuższych wartałoby znaleźć coś bardziej szczególowego
   * - znajdowanie najniższego supportu w okresie
   * - znajdowanie asymptot - podobna tendencja do wzrostu/spadku, z określonym odchyleniem standardowym
   *
   * @param data (Close, Open, High, Low)
   * @return support value, and monotonicity of support ('a' in y = ax + b)
   */
  override def initialize(data: Vector[(Double, Double, Double, Double)]): (Double, Double) = {
    def findAbsoluteChange(x: (Double, Double)) = math.abs(x._1 - x._2)
    def normalize(x: (Double, Double), n: Double) = (math.pow(x._1/n, 4), math.pow(x._2/n, 4))
    val lows = data.take(length) map (x => x._4)
    lastValue = lows(length - 1)
    val differences = lows.slice(1, length) zip lows.slice(0, length - 1) map findAbsoluteChange

    val averageDifference = differences.sum / (length)
    val std = math.sqrt((differences map (x => math.pow(x - averageDifference, 2)) sum) / length) / c4(length)

    val roundedLows = lows map (x => round(x, round))
    val roundedLowsAverage = (roundedLows sum) / (roundedLows.length)
    val roundedAverages = roundedLows.slice(1, length) zip roundedLows.slice(0, length - 1) map (x => normalize(x, roundedLowsAverage)) map findAbsoluteChange
    roundedAverage = roundedAverages.sum / (length)
    boundary = ((-length / 2 to length / 2).toList.map(x => sigm(x)) zip roundedLows map (x => x._1 * x._2) sum) / length
    Tuple2(round(boundary, Round.MIDDLE), math.pow(1 / (1 + math.pow(roundedAverage, 2)), 4))
  }

  override def update(datum: (Double, Double, Double, Double)): (Double, Double) = {
    //      roundedLows = roundedLows.slice(1, roundedLows.size) ++ Vector(rounded)
    //      roundedAverages = roundedAverages.slice(1, roundedAverages.size) ++ Vector(math.pow(rounded - lastLow, 2))
    val rounded = round(datum._4, round)
    boundary = updateAvg((boundary, length - 1), rounded, _ + _)._1
    roundedAverage = updateAvg((roundedAverage, length - 1), math.pow(rounded - lastValue, 2), _ + _)._1
    lastValue = datum._4
    Tuple2(round(boundary, Round.MIDDLE), 1 / (1 + roundedAverage))
  }

  import Round._

  /**
   *
   * @param datum Data to round
   * @param round Round type (one of DOWN, UP, MIDDLE)
   * @return Rounded number
   */
  private def round(datum: Double, round: Round): Double = {
    val rounding = round match {
      case Round.DOWN => (num: Double, dif: Double, _: AnyVal, _: AnyVal) => num - dif
      case Round.UP => (num: Double, dif: Double, range: Double, _: AnyVal) => num + (range - dif)
      case Round.MIDDLE => (num: Double, dif: Double, range: Double, condition: Boolean) => if (condition) num - dif else num + (range - dif)
    }

    if (datum < 25) {
      val diff = ((datum * 10) % 5) / 10
      rounding(datum, diff, 0.5, diff < 0.25)
    } else //if (datum < 500) {
      rounding(datum, datum % 5, 5, datum % 5 < 2.5)
    //      } else {
    //        rounding(datum, datum % 50, 10, datum % 10 < 5.0)
    //      }
  }




}
