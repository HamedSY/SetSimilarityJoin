class JaccardHandler(val threshold: Double) {
  def calculateOverlap(sumOfLengths: Int): Int = {
    math.ceil((sumOfLengths * threshold) / (1 + threshold)).toInt
  }
}
