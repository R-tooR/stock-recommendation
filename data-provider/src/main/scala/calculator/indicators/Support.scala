package calculator.indicators

import calculator.Round

class Support(length: Int) extends PriceBoundary(length, Round.DOWN) {
}
