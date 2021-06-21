package calculator.indicators

abstract class Indicator[T, U] {
  def initialize(data: Vector[T]): U

  def update(datum: T): U

  def avgChange(x: (Double, Double)) = (x._1 - x._2) / x._2

  /**
   *
   * @param state     State - usually (average, number_of_population)
   * @param newVal    new value to update state
   * @param function2 operation of updating (e.g. + or - )
   * @return
   */
  def updateAvg(state: (Double, Int), newVal: Double, function2: (Double, Double) => Double): (Double, Int)
  = (function2(state._1 * (state._2 - 1), newVal) / state._2, function2(state._2, if (state._2 - 1 > 0) 1.0 else 0.0).toInt)
}
