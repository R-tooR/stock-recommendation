package calculator

import calculator.PeakType.{MAX, MIN, PeakType}
import calculator.indicators._

import scala.collection.{GenTraversableOnce, SortedSet}
import scala.util.{Failure, Success, Try}

class TechnicalCalculator {
  val RSI: RSI = new RSI(15)
  val MA: EMA = new EMA(20)
  val OBV: OBV = new OBV
  val support: Support = new Support(7)
  val resistance: Resistance = new Resistance(7)

  var RSIBuffer: Vector[Double] = Vector()
  var MABuffer: Vector[Double] = Vector() //?
  var OBVBuffer: Vector[Double] = Vector()
  var SupportBuffer: Vector[Double] = Vector() //?
  var ResistanceBuffer: Vector[Double] = Vector() //?
  var ClosePriceBuffer: Vector[Double] = Vector()

  var RSIpeaks: Set[(Double, Int, PeakType)] = Set()
  var MApeaks: Set[(Double, Int, PeakType)] = Set()
  var OBVpeaks: Set[(Double, Int, PeakType)] = Set()
  var SupportPeaks: Set[(Double, Double, Int)] = Set() //?
  //  var RSIpeaks: SortedSet[(Double, Int, PeakType)] = SortedSet()(Ordering.by[(Double, Int, PeakType), Int](_._2).reverse)
  //  var MApeaks: SortedSet[(Double, Int, PeakType)] = SortedSet()(Ordering.by[(Double, Int, PeakType), Int](_._2).reverse) //?
  //  var OBVpeaks: SortedSet[(Double, Int, PeakType)] = SortedSet()(Ordering.by[(Double, Int, PeakType), Int](_._2).reverse)
  //  var SupportPeaks: SortedSet[(Double, Int, PeakType)] = SortedSet()(Ordering.by[(Double, Int, PeakType), Int](_._2).reverse) //?
  var ResistancePeaks: Set[(Double, Double, Int)] = Set() //?
  var ClosePricePeaks: Set[(Double, Int, PeakType)] = Set()
  //  var ClosePricePeaks: SortedSet[(Double, Int, PeakType)] = SortedSet()(Ordering.by[(Double, Int, PeakType), Int](_._2).reverse)
  var timeframe: Int = 0
  var latestRecommendation = 0.0

  /**
   *
   * @param data (Close, Volume, Open, High, Low)
   */
  def initialize(data: Vector[(Double, Double, Double, Double, Double)]): Double = {
    if (data.length == 20) {
      val close = data map (x => (x._1))
      val close_volume = data map (x => (x._1, x._2))
      val all_price_params = data map (x => (x._1, x._3, x._4, x._5))

      val rsi = RSI.initialize(close slice(0, 15))
      val ma = MA.initialize(close)
      val sup = support.initialize(all_price_params slice(0, 7))
      val res = resistance.initialize(all_price_params slice(0, 7))
      val obv = OBV.initialize(close_volume slice(0, 15))

      RSIBuffer = (for (i <- 14 until 20) yield RSI.update(close(i))).toVector
      OBVBuffer = (for (i <- 14 until 20) yield OBV.update(close_volume(i))).toVector
      ClosePriceBuffer = close.slice(14, 20)

      OBVpeaks = OBVpeaks ++ getPeaksForOBV(20) //? dawaj je z przodu
      RSIpeaks = RSIpeaks ++ getPeaksForRSI(20)
      ClosePricePeaks = ClosePricePeaks ++ getPeaksForClose(20)
      SupportPeaks = SupportPeaks ++ Vector((sup._1, sup._2, timeframe))
      ResistancePeaks = ResistancePeaks ++ Vector((res._1, res._2, timeframe))

      latestRecommendation = getRecommendation(OBVpeaks.toSeq.sortWith(_._2 > _._2), RSIpeaks.toSeq.sortWith(_._2 > _._2), SupportPeaks.toSeq.sortWith(_._3 > _._3), ResistancePeaks.toSeq.sortWith(_._3 > _._3))
      timeframe = 20
    }
    latestRecommendation
  }

  private def twoFirst(v: Iterable[(Double, Int, PeakType)], t: PeakType): Seq[(Double, Int)] = (v.view).filter(_._3 == t).map(x => (x._1, x._2)).take(2).force.toSeq

  def update(data: (Double, Double, Double, Double, Double)): Double = {
    val rsi = RSI.update(data._1)
    val ma = MA.update(data._1)
    val sup = support.update(data._1, data._3, data._4, data._5)
    val res = resistance.update(data._1, data._3, data._4, data._5)
    val obv = OBV.update(data._1, data._2)
    timeframe = timeframe + 1

    RSIBuffer = RSIBuffer.slice(1, RSIBuffer.size) ++ Vector(rsi)
    OBVBuffer = OBVBuffer.slice(1, OBVBuffer.size) ++ Vector(obv)
    ClosePriceBuffer = ClosePriceBuffer.slice(1, ClosePriceBuffer.size) ++ Vector(data._1)

    if (timeframe % 4 == 0) {
      OBVpeaks = (OBVpeaks ++ getPeaksForOBV(timeframe)).filter(x => x._2 != 0)
      RSIpeaks = (RSIpeaks ++ getPeaksForRSI(timeframe)).filter(x => x._2 != 0)
      ClosePricePeaks = (ClosePricePeaks ++ getPeaksForClose(timeframe)).filter(x => x._2 != 0) //todo: co jeśli jest pik na OBV, a nie ma na close price
      SupportPeaks = SupportPeaks ++ Vector((sup._1, sup._2, timeframe))
      ResistancePeaks = ResistancePeaks ++ Vector((res._1, res._2, timeframe))
      latestRecommendation = getRecommendation(OBVpeaks.toSeq.sortWith(_._2 > _._2), RSIpeaks.toSeq.sortWith(_._2 > _._2), SupportPeaks.toSeq.sortWith(_._3 > _._3), ResistancePeaks.toSeq.sortWith(_._3 > _._3))
    }

    latestRecommendation
  }

  private def getRecommendation(OBVpeaks: Seq[(Double, Int, PeakType)], RSIpeaks: Seq[(Double, Int, PeakType)], sup: Seq[(Double, Double, Int)], res: Seq[(Double, Double, Int)]): Double = {
    def sigm = (d: Double) => 1 / (1 + math.exp(-d))

    val closePriceAndOBV = (
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MAX)), twoFirst(OBVpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MIN)), twoFirst(OBVpeaks, MIN), PeakType.MIN)
    )

    val closePriceAndRSI = (
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MAX)), twoFirst(RSIpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MIN)), twoFirst(RSIpeaks, MIN), PeakType.MIN)
    )

    0.4 * sigm(getOBVRatio(closePriceAndOBV)._2) + 0.4 * sigm(getRSIRatio(closePriceAndRSI, RSIpeaks)) + 0.2 * sigm(getSupportResistanceRatio(sup, res, ClosePriceBuffer))
  }

  private def getClosePricePeaksFor(timeframes: Seq[(Double, Int)]) = {
    ClosePricePeaks.filter(x => timeframes.map(_._2).contains(x._2)).map(x => (x._1, x._2)).toSeq
  }

  type DivergencePoints = ((Seq[(Double, Int)], Seq[(Double, Int)], PeakType), (Seq[(Double, Int)], Seq[(Double, Int)], PeakType))
  type Divergence = (Seq[(Double, Int)], Seq[(Double, Int)], PeakType)

  private def getDivergenceRatio(points: DivergencePoints): (PeakType, Double) = {

    //todo: rozważ refaktor na jakiś partial function
    // znajdujemy takie sety, które zawierają 2 elementy - początkowy, i końcowy, dla obliczenia dywergencji
    val divergencePoints = points.productIterator.map(_.asInstanceOf[(Seq[(Double, Int)], Seq[(Double, Int)], PeakType)])
      .filter(x => x._1.size == 2 && x._2.size == 2).toVector

    // znajdujemy, któro z ekstremów jest najbliższe bieżącemu czasowi (maksymalny timeframe), i dla niego liczymy dywergencję
    def findDivergencesForRecentExtremum = (divergencePoints: Vector[Divergence]) => {
      val result = divergencePoints.max(Ordering.by[Divergence, Int](_._2.head._2))
      (result._3, findDivergences((result._1.head, result._1.last), (result._2.head, result._2.last)))
    }

    if (divergencePoints.nonEmpty) {
      findDivergencesForRecentExtremum(divergencePoints)
    } else {
      (MIN, 0.0)
    }

  }

  def getOBVRatio(closePriceAndOBV: DivergencePoints) = {
    getDivergenceRatio(closePriceAndOBV)
  }

  def getRSIRatio(closePriceAndRSI: DivergencePoints, RSIpeaks: Seq[(Double, Int, PeakType)]) = {
    getDivergenceRatio(closePriceAndRSI)._2 + analyzeRsiTrend(RSIpeaks)
  }

  def getSupportResistanceRatio(support: Seq[(Double, Double, Int)], resistance: Seq[(Double, Double, Int)], priceBuffer: Seq[Double]) = {
    val averageMonotonicity = (priceBuffer.slice(1, priceBuffer.length) zip priceBuffer.slice(0, priceBuffer.length - 1) map (x => x._1 - x._2) sum) / priceBuffer.length
    val validSupport = support.find(_._2 >= 0.8).getOrElse((0.0, 0.0, 0))
    val validResistance = resistance.find(_._2 >= 0.8).getOrElse((0.0, 0.0, 0))
    val closePrice = priceBuffer.reverse.headOption.getOrElse(0.0)
    if (averageMonotonicity < 0) {
      val similarSupport = support.count(x => math.abs(x._1 - validSupport._1) / x._1 < 0.25)
      similarSupport / (0.5 * (validSupport._1 - closePrice))
    } else {
      val similarResistance = resistance.count(x => math.abs(x._1 - validResistance._1) / x._1 < 0.25)
      similarResistance / (0.5 * (closePrice - validResistance._1))
    }
  }

  // https://www.newtraderu.com/2020/07/11/rsi-divergence-cheat-sheet/
  def analyzeRsiTrend(rsiPeaks: Iterable[(Double, Int, PeakType)]): Double = {
    def boost(rsi: Double): Double = math.pow((rsi - 50) / 5, 3) / 2 + math.pow(((rsi - 50) / 20), 3)

    def sigm = (d: Double) => 2 * ((1 / (1 + math.exp(-0.01 * d))) - 0.5)

    if (rsiPeaks.size > 3) {
      sigm(Seq(25.0, 5.0, 1.0).toVector.map(_ / 2).zip(rsiPeaks.take(3).map(x => boost(x._1))).map(x => x._1 * x._2).sum)
    } else {
      0.0
    }
  }

  def getPeaksForOBV(timeFrame: Int): Seq[(Double, Int, PeakType)] = {
    val extrema = findPeaks(OBVBuffer, timeFrame, (-0.03, 0.03))
    Seq(extrema._1, extrema._2)
  }

  def getPeaksForRSI(timeFrame: Int): Seq[(Double, Int, PeakType)] = {
    val extrema = findPeaks(RSIBuffer, timeFrame, (-0.1, 0.1))
    Seq(extrema._1, extrema._2)
  }

  def getPeaksForClose(timeFrame: Int): Seq[(Double, Int, PeakType)] = {
    val extrema = findPeaks(ClosePriceBuffer, timeFrame, (-0.03, 0.03))
    Seq(extrema._1, extrema._2)
  }

  //todo: więcej testów
  //todo: jeśli badamy minima, to jedna z funkcji "napłask" oznacza potencjalny wzrost
  //todo: malejąca cena, rosnący wskaźnik - bearish, czyli spadek
  // https://therobusttrader.com/how-to-trade-the-obv-indicator-complete-obv-guide-on-balance-volume/
  case class Peak(value: Double, timeframe: Int)

  case class DirectedPeak(value: Double, timeframe: Int, peakType: PeakType)

  def findDivergences(indicatorPeaks: ((Double, Int), (Double, Int)), pricePeaks: ((Double, Int), (Double, Int))): Double = {
    def sigm = (d: Double) => 2 * ((1 / (1 + math.exp(-50 * d))) - 0.5)

    def normalize(tuple: ((Double, Int), (Double, Int))) = {
      def greaterFromTuple2(values: (Double, Double)) = Seq(math.abs(values._1), math.abs(values._2)).max

      def absGreaterOfThatBothValues = greaterFromTuple2(tuple._1._1, tuple._2._1)

      ((tuple._1._1 / absGreaterOfThatBothValues, tuple._1._2), (tuple._2._1 / absGreaterOfThatBothValues, tuple._2._2))
    }

    val indicatorPeaksNorm = normalize(indicatorPeaks)
    val pricePeaksNorm = normalize(pricePeaks)

    val indicatorMonotonicity = (indicatorPeaksNorm._2._1 - indicatorPeaksNorm._1._1) / (indicatorPeaksNorm._2._2 - indicatorPeaksNorm._1._2)
    val priceMonotonicity = (pricePeaksNorm._2._1 - pricePeaksNorm._1._1) / (pricePeaksNorm._2._2 - pricePeaksNorm._1._2)

    sigm(indicatorMonotonicity - priceMonotonicity)
  }

  implicit class SafeMaxSyntax[T, A](t: T)(implicit ev: T => Vector[A], ord: Ordering[A]) {
    def safeMax: Option[A] = {
      val coll = ev(t)
      if (coll.nonEmpty) Some(coll.max) else None
    }

    def safeMin: Option[A] = {
      val coll = ev(t)
      if (coll.nonEmpty) Some(coll.min) else None
    }

  }

  // thresholds - poziom zmiany, jaki uznajemy za pik, oraz minimalna wartość do jakiej cena musi wrócić, żeby ektremum było uznane za pik

  import PeakType._

  def findPeaks(buffer: Vector[Double], timeFrame: Int, thresholds: (Double, Double)): ((Double, Int, PeakType), (Double, Int, PeakType)) = {
    def avgChange(x: (Double, Double)) = (x._1 - x._2) / x._2

    val bufferSize = buffer.length
    val averageChange = buffer.slice(1, bufferSize) zip buffer.slice(0, bufferSize - 1) map avgChange
    val cumulative = averageChange.scanLeft(0.0)((x1, x2) => x1 + x2).tail
    val min = cumulative.min
    val max = cumulative.max
    val indexMin = cumulative.indexOf(min) + 1
    val indexMax = cumulative.indexOf(max) + 1

    def extremalValueAfterDetectedExtremum(index: Int, cumulative: Vector[Double], extremumFunction: Vector[Double] => Double) = {
      cumulative.splitAt(index) match {
        case vec if vec._2 nonEmpty => extremumFunction(vec._2)
        case _ => Double.NaN
      }
    }

    val maxValueAfterMinimum = extremalValueAfterDetectedExtremum(indexMin, cumulative, (x: Vector[Double]) => x.max)
    val minValueAfterMaximum = extremalValueAfterDetectedExtremum(indexMax, cumulative, (x: Vector[Double]) => x.min)

    def recoveryValue(index: Int, cumulative: Vector[Double], reduceFunction: (Vector[Double], Vector[Double]) => Double) = {
      cumulative.splitAt(index) match {
        case vec if vec._1.nonEmpty && vec._2.nonEmpty => reduceFunction(vec._1, vec._2)
        case _ => Double.NaN
      }
    }

    val minimumSurrounding = recoveryValue(indexMin, cumulative, (x1: Vector[Double], x2: Vector[Double]) => x2.max - x1.max)
    val maximumSurrounding = recoveryValue(indexMax, cumulative, (x1: Vector[Double], x2: Vector[Double]) => x1.min - x2.min)

    val isMinValid = min <= thresholds._1 && maxValueAfterMinimum != Double.NaN && (minimumSurrounding > -math.abs(thresholds._1) / 3 || maxValueAfterMinimum - min > math.abs(thresholds._1) / 3)
    val isMaxValid = max >= thresholds._2 && minValueAfterMaximum != Double.NaN && (maximumSurrounding > -math.abs(thresholds._2) / 3 || max - minValueAfterMaximum > math.abs(thresholds._2) / 3)
    val minimum = if (isMinValid) (buffer(indexMin), timeFrame - (bufferSize - cumulative.indexOf(min)) + 1, MIN) else (0.0, 0, MIN)
    val maximum = if (isMaxValid) (buffer(indexMax), timeFrame - (bufferSize - cumulative.indexOf(max)) + 1, MAX) else (0.0, 0, MAX)
    (minimum, maximum)
  }
}
