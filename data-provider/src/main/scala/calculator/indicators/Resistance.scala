package calculator.indicators

import calculator.Round

class Resistance(length: Int) extends PriceBoundary(length, Round.UP) {
}
