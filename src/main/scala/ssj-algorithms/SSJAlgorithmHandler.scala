import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

trait SSJAlgorithmHandler(threshold: Double) {
  val jaccardHandler = new JaccardHandler(threshold)
  val prefixHandler = new PrefixHandler

  def findSimilarPairs(
      sortedR: Array[Array[Int]],
      sortedS: Array[Array[Int]]
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    val similarPairs = ArrayBuffer.empty[(Array[Int], Array[Int])]
    val index = new HashMap[Int, ArrayBuffer[Int]]()
    val candidates = ArrayBuffer.empty[Int]

    // Build inverted index from S
    for (j <- sortedS.indices) {
      val s = sortedS(j)
      val prefixLen = s.length - math.ceil(s.length * threshold).toInt + 1
      val sPrefix = s.take(prefixLen)
      for (token <- sPrefix) {
        index.getOrElseUpdate(token, ArrayBuffer.empty) += j
      }
    }

    // Probe using inverted index
    for (i <- sortedR.indices) {
      val r = sortedR(i)
      val rPrefixLen = r.length - math.ceil(r.length * threshold).toInt + 1
      val rPrefix = r.take(rPrefixLen)

      candidates.clear()

      for (token <- rPrefix) {
        for (j <- index.getOrElse(token, ArrayBuffer.empty)) {
          val s = sortedS(j)
          val requiredOverlap = jaccardHandler.calculateOverlap(r.length + s.length)
          val sPrefixLen = s.length - math.ceil(s.length * threshold).toInt + 1
          val sPrefix = s.take(sPrefixLen)

          if (
            !isLengthFiltered(rPrefix, sPrefix) &&
            !isPrefixFiltered(rPrefix, sPrefix) &&
            !isPositionFiltered(rPrefix, sPrefix, r.length, s.length, requiredOverlap)
          ) {
            if (verify(r, s, requiredOverlap)) {
              similarPairs.append((r, s))
            }
          }
        }
      }
    }
    similarPairs
  }

  def findSimilarPairs(
      sortedR: Array[Array[Int]]
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    val similarPairs = ArrayBuffer.empty[(Array[Int], Array[Int])]
    val index = new HashMap[Int, ArrayBuffer[Int]]()
    val seen = Array.fill(sortedR.length)(false)
    val candidates = ArrayBuffer.empty[Int]

    // Build inverted index
    for (i <- sortedR.indices) {
      val record = sortedR(i)
      val prefixLen = record.length - math.ceil(record.length * threshold).toInt + 1
      val prefix = record.take(prefixLen)
      for (token <- prefix) {
        index.getOrElseUpdate(token, ArrayBuffer.empty) += i
      }
    }

    // Probe using inverted index
    for (i <- sortedR.indices) {
      val r1 = sortedR(i)
      val r1PrefixLen = r1.length - math.ceil(r1.length * threshold).toInt + 1
      val r1Prefix = r1.take(r1PrefixLen)

      candidates.clear()
      val seenCandidates = scala.collection.mutable.Set.empty[Int]

      for (token <- r1Prefix) {
        for (j <- index.getOrElse(token, ArrayBuffer.empty)) {
          if (j > i && !seenCandidates.contains(j)) {
            seenCandidates += j
            val r2 = sortedR(j)
            val requiredOverlap = jaccardHandler.calculateOverlap(r1.length + r2.length)
            val r2PrefixLen = r2.length - math.ceil(r2.length * threshold).toInt + 1
            val r2Prefix = r2.take(r2PrefixLen)

            if (
              !isLengthFiltered(r1Prefix, r2Prefix) &&
              !isPrefixFiltered(r1Prefix, r2Prefix) &&
              !isPositionFiltered(r1Prefix, r2Prefix, r1.length, r2.length, requiredOverlap)
            ) {
              if (verify(r1, r2, requiredOverlap)) {
                similarPairs.append((r1, r2))
              }
            }
          }
        }
      }
    }
    similarPairs
  }

  def isPrefixFiltered(rPrefix: Array[Int], sPrefix: Array[Int]): Boolean = {
    var i = 0
    var j = 0
    while (i < rPrefix.length && j < sPrefix.length) {
      if (rPrefix(i) == sPrefix(j)) return false
      else if (rPrefix(i) < sPrefix(j)) i += 1
      else j += 1
    }
    true
  }


  def isLengthFiltered(rPrefix: Array[Int], sPrefix: Array[Int]): Boolean = {
    sPrefix.isEmpty || rPrefix.isEmpty 
  }

  def isPositionFiltered(
      rPrefix: Array[Int],
      sPrefix: Array[Int],
      rSize: Int,
      sSize: Int,
      requiredOverlap: Int
  ): Boolean

  def verify(r: Array[Int], s: Array[Int], requiredOverlap: Int): Boolean = {
    var i = 0
    var j = 0
    var overlap = 0
    while (i < r.length && j < s.length) {
      if (r(i) == s(j)) {
        overlap += 1
        if (overlap >= requiredOverlap) {
          println(s"PASSED! overlap: $overlap\tRequired: $requiredOverlap")
          println("r and s:")
          r.foreach(a => print(s"$a "))
          println()
          s.foreach(a => print(s"$a "))
          println()
          return true
        }
        i += 1
        j += 1
      } else if (r(i) < s(j)) {
        i += 1
      } else {
        j += 1
      }
    }
    println(s"failed! overlap: $overlap\tRequired: $requiredOverlap")
    false
  }
}
