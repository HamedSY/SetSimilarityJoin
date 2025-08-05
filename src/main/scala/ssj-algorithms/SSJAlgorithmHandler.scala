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
    val index = HashMap[Int, List[(Int, Array[Int], Int)]]()
    val seen = new HashSet[(Int, Int)]()

    // Build inverted index from S
    for ((s, sid) <- sortedS.zipWithIndex) {
      val prefixLen = s.length - Math.ceil(s.length * threshold).toInt + 1
      for (pos <- 0 until Math.min(prefixLen, s.length)) {
        val token = s(pos)
        val entry = (sid, s, pos)
        index(token) = entry :: index.getOrElse(token, Nil)
      }
    }

    // Probe using inverted index
    for ((r, rid) <- sortedR.zipWithIndex) {
      val prefixLen = r.length - Math.ceil(r.length * threshold).toInt + 1
      val candidates = HashMap[Int, Int]()

      for (posR <- 0 until Math.min(prefixLen, r.length)) {
        val token = r(posR)
        for ((sid, s, posS) <- index.getOrElse(token, Nil)) {
          if (!seen.contains((rid, sid))) {
            // Length filter
            val requiredOverlap =
              jaccardHandler.calculateOverlap(r.length + s.length)
            if (Math.min(r.length, s.length) >= requiredOverlap) {
              // Position filter
              val maxRemaining = Math.min(r.length - posR, s.length - posS)
              val currentOverlap = candidates.getOrElse(sid, 0)
              if (currentOverlap + maxRemaining >= requiredOverlap) {
                candidates(sid) = currentOverlap + 1
                seen += ((rid, sid))
              }
            }
          }
        }
      }

      // Verify only promising candidates
      for ((sid, overlap) <- candidates) {
        val s = sortedS(sid)
        val requiredOverlap =
          jaccardHandler.calculateOverlap(r.length + s.length)
        val setR = r.toSet
        val setS = s.toSet
        val inter = (setR intersect setS).size
        if (inter >= requiredOverlap) {
          similarPairs.append((r, s))
        }

      }
    }

    similarPairs
  }

  def findSimilarPairs(
      sortedR: Array[Array[Int]]
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    val similarPairs = ArrayBuffer.empty[(Array[Int], Array[Int])]
    val index = HashMap[Int, List[(Int, Array[Int], Int)]]()
    val seen = new HashSet[(Int, Int)]()

    // Build inverted index from S
    for ((r, rid) <- sortedR.zipWithIndex) {
      val prefixLen = r.length - Math.ceil(r.length * threshold).toInt + 1
      for (pos <- 0 until Math.min(prefixLen, r.length)) {
        val token = r(pos)
        val entry = (rid, r, pos)
        index(token) = entry :: index.getOrElse(token, Nil)
      }
    }

    // Probe using inverted index
    for ((r1, r1id) <- sortedR.zipWithIndex) {
      val prefixLen = r1.length - Math.ceil(r1.length * threshold).toInt + 1
      val candidates = HashMap[Int, Int]()

      for (posR1 <- 0 until Math.min(prefixLen, r1.length)) {
        val token = r1(posR1)
        for ((r2id, r2, posR2) <- index.getOrElse(token, Nil)) {
          if (r1id < r2id && !seen.contains((r1id, r2id))) {
            // Length filter
            val requiredOverlap =
              jaccardHandler.calculateOverlap(r1.length + r2.length)
            if (Math.min(r1.length, r2.length) >= requiredOverlap) {
              // Position filter
              val maxRemaining = Math.min(r1.length - posR1, r2.length - posR2)
              val currentOverlap = candidates.getOrElse(r2id, 0)
              if (currentOverlap + maxRemaining >= requiredOverlap) {
                candidates(r2id) = currentOverlap + 1
                seen += ((r1id, r2id))
              }
            }
          }
        }
      }

      // Verify only promising candidates
      for ((r2id, overlap) <- candidates) {
        val r2 = sortedR(r2id)
        val requiredOverlap =
          jaccardHandler.calculateOverlap(r1.length + r2.length)
        val setR1 = r1.toSet
        val setR2 = r2.toSet
        val inter = (setR1 intersect setR2).size
        if (inter >= requiredOverlap) {
          similarPairs.append((r1, r2))
        }
      }
    }

    similarPairs
  }

}
