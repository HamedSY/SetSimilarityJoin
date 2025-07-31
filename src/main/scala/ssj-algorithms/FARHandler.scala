import scala.collection.mutable.ArrayBuffer

class FARHandler {
  def fragmentCollection(
      collection: Array[Array[Int]],
      n: Int
  ): ArrayBuffer[Array[Array[Int]]] = {
    val size = collection.size
    val baseSize = size / n
    val remainder = size % n

    val fragmentedCollection = new ArrayBuffer[Array[Array[Int]]](n)

    var index = 0
    for (i <- 0 until n) {
        val chunkSize = baseSize + (if (i < remainder) 1 else 0)
        val chunk = new Array[Array[Int]](chunkSize)
        System.arraycopy(collection, index, chunk, 0, chunkSize)
        fragmentedCollection += chunk
        index += chunkSize
    }

    fragmentedCollection
  }
}
