
object Heaps extends App {
  // 1046. Last Stone Weight
  def lastStoneWeight(stones: Array[Int]): Int = {
    val pq = collection.mutable.PriorityQueue(stones:_*)

    while (pq.size > 1) {
      val y = pq.dequeue
      val x = pq.dequeue
      if (x != y) {
        pq += (y - x)
      }
    }

    pq.lastOption.getOrElse(0)
  }

}
