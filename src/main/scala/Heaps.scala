
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

  // 295. Find Median from Data Stream
  class MedianFinder() {
    private val min = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    private val max = collection.mutable.PriorityQueue.empty[Int]

    def addNum(num: Int): Unit = {
      max.enqueue(num)
      min.enqueue(max.dequeue())
      if (min.size > max.size) {
        max.enqueue(min.dequeue())
      }
    }

    def findMedian(): Double = {
      if (max.size % 2 == 1)
        max.head
      else (min.head + max.head) / 2d
    }
  }

  // 1962. Remove Stones to Minimize the Total
  def minStoneSum(piles: Array[Int], k: Int): Int = {
    val queue = collection.mutable.PriorityQueue.from(piles)

    for (_ <- 0 until k) {
      val i = Math.round(queue.dequeue() / 2d).toInt
      queue.enqueue(i)
    }
    queue.sum
  }
  println(minStoneSum(Array(5,4,9), 2))




}
