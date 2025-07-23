class PrefixHandler() {
    def computePrefixes(r: Array[Int], s: Array[Int], requiredOverlap: Int): (Array[Int], Array[Int]) = {
        val rPrefixSize = r.length - requiredOverlap + 1
        val sPrefixSize = s.length - requiredOverlap + 1

        (r.take(rPrefixSize), s.take(sPrefixSize))
    }
}
