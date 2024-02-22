
object Heaps extends App {
  // 1046. Last Stone Weight
  def lastStoneWeight(stones: Array[Int]): Int = {
    val pq = collection.mutable.PriorityQueue(stones: _*)

    while (pq.size > 1) {
      val y = pq.dequeue
      val x = pq.dequeue
      if (x != y) {
        pq += (y - x)
      }
    }

    pq.lastOption.getOrElse(0)
  }

  // 2208. Minimum Operations to Halve Array Sum
  def halveArray(nums: Array[Int]): Int = {
    val sum = nums.map(_.toDouble).sum
    val target: Double = sum / 2d
    var currentSum: Double = sum
    val queue = collection.mutable.PriorityQueue[Double](nums.map(_.toDouble):_*)

    var ans = 0
    while (currentSum >= target) {
      val x = queue.dequeue
      queue.enqueue(x / 2)
      currentSum -= x / 2
      ans += 1
    }
    ans
  }


}
