package data.extraction

import scala.io.Source

class FileReader(filename: String) {
  // multi-threading - czytana linijka -> wysyłana do processora -> obliczane wskaźniki, oraz ogólny wynik
  // ogólny wynik można zbierać w concurrent hashmap?
  // application.properties for paths

  val source: Iterator[String] = Source.fromFile(filename).getLines
//  for (line <- Source.fromFile(filename).getLines) {
//    println(line)
//  }

  def readLine: () => String = () => if(source.hasNext) source.next() else ""

  def readLines: (Int) => Iterable[String] = (i: Int) => for(it <- 1 to i) yield readLine.apply()

}
