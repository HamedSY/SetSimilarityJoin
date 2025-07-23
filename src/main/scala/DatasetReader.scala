import scala.io.Source

class DatasetReader {
  def readSetsFromFile(filename: String): Array[Array[Int]] = {
    Source
      .fromFile(filename)
      .getLines()
      .map(line => line.trim.split("\\s+").filter(_.nonEmpty).map(_.toInt))
      .toArray
  }
}
