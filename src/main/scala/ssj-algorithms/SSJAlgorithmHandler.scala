import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

trait SSJAlgorithmHandler(threshold: Double) {
  val jaccardHandler = new JaccardHandler(threshold)
  val prefixHandler = new PrefixHandler

  def findSimilarPairs(
      sortedR: Array[Array[Int]],
      sortedS: Array[Array[Int]]
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    val similarPairs = ArrayBuffer.empty[(Array[Int], Array[Int])]
    val index = new HashMap[Int, ArrayBuffer[Int]]()
    val seen = new HashSet[(Int, Int)]()

    // Build inverted index from S
    val sPrefixes = new Array[Array[Int]](sortedS.length)
    for (j <- sortedS.indices) {
      val s = sortedS(j)
      val prefixLen = s.length - math.ceil(s.length * threshold).toInt + 1
      val sPrefix = s.take(prefixLen)
      sPrefixes(j) = sPrefix
      for (token <- sPrefix) {
        index.getOrElseUpdate(token, ArrayBuffer.empty) += j
      }
    }

    // Probe using inverted index
    for (i <- sortedR.indices) {
      val r = sortedR(i)
      val rPrefixLen = r.length - math.ceil(r.length * threshold).toInt + 1
      val rPrefix = r.take(rPrefixLen)
      val rSize = r.length

      val candidates = new HashSet[Int]()

      for (token <- rPrefix) {
        for (j <- index.getOrElse(token, ArrayBuffer.empty)) {
          if (!seen.contains((i, j))) {
            seen.add((i, j))
            candidates += j
          }
        }
      }

      for (j <- candidates) {
        val s = sortedS(j)
        val sPrefix = sPrefixes(j)
        val requiredOverlap =
          jaccardHandler.calculateOverlap(r.length + s.length)
        if (
          !isLengthFiltered(rPrefix, sPrefix)
          && !isPrefixFiltered(rPrefix, sPrefix)
          && !isPositionFiltered(
            rPrefix,
            sPrefix,
            r.length,
            s.length,
            requiredOverlap
          )
        ) {
          if (verify(r, s, requiredOverlap)) {
            similarPairs.append((r, s))
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
      val prefixLen =
        record.length - math.ceil(record.length * threshold).toInt + 1
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
            val requiredOverlap =
              jaccardHandler.calculateOverlap(r1.length + r2.length)
            val r2PrefixLen =
              r2.length - math.ceil(r2.length * threshold).toInt + 1
            val r2Prefix = r2.take(r2PrefixLen)

            if (
              !isLengthFiltered(r1Prefix, r2Prefix) &&
              !isPrefixFiltered(r1Prefix, r2Prefix) &&
              !isPositionFiltered(
                r1Prefix,
                r2Prefix,
                r1.length,
                r2.length,
                requiredOverlap
              )
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
    val rSet = rPrefix.toSet
    !sPrefix.exists(token => rSet.contains(token))
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
    val setR = r.toSet
    var overlap = 0
    s.exists { token =>
      if (setR.contains(token)) {
        overlap += 1
        overlap >= requiredOverlap
      } else false
    }
  }

}
