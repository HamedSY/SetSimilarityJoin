class FrequencyHandler {
  private def computeFreqDict(R: Array[Array[Int]], S: Array[Array[Int]]): Map[Int, Int] = {
    val allSets = R ++ S
    val allTokens = allSets.flatten
    allTokens.groupBy(identity).view.mapValues(_.size).toMap
  }

  private def computeFreqDict(R: Array[Array[Int]]): Map[Int, Int] = {
    val allTokens = R.flatten
    allTokens.groupBy(identity).view.mapValues(_.size).toMap
  }

  private def sortSetByFrequency(set: Array[Int], freqDict: Map[Int, Int]): Array[Int] = {
    set.sortBy(token => freqDict.getOrElse(token, 0))
  }

  def createSortedLists(R: Array[Array[Int]], S: Array[Array[Int]]): (Array[Array[Int]], Array[Array[Int]]) = {
    val freqDict = computeFreqDict(R, S)
    val sortedR: Array[Array[Int]] = R.map(r => sortSetByFrequency(r, freqDict))
    val sortedS: Array[Array[Int]] = S.map(s => sortSetByFrequency(s, freqDict))
    (sortedR, sortedS)
  }

  def createSortedLists(R: Array[Array[Int]]): Array[Array[Int]] = {
    val freqDict = computeFreqDict(R)
    R.map(r => sortSetByFrequency(r, freqDict))
  }
}