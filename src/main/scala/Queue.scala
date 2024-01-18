object Queue extends App {
  // 933. Number of Recent Calls
  var list = List.empty[Int]
  def ping(t: Int): Int = {
    list = list :+ t
    while (list.headOption.exists(x => t - x > 3000)) {
      list = list.drop(1)
    }
    list.size
  }

  // 346. Moving Average from Data Stream
  class MovingAverage(_size: Int) {
    private var list = List.empty[Int]
    private var currentSum: Double = 0
    def next(value: Int): Double = {
      list = list :+ value
      currentSum += value
      if (list.size > _size) {
        currentSum -= list.head
        list = list.drop(1)
      }
      currentSum / Math.min(list.size, _size)
    }
  }


  // 239. Sliding Window Maximum
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    val deque = collection.mutable.ArrayDeque.empty[Int]
    val ans = Array.fill(nums.length - k + 1)(0)

    for (x <- nums.zipWithIndex) {
      while (deque.lastOption.exists(x._1 > nums(_))) {
        deque.removeLast()
      }

      deque.append(x._2)

      if (deque.head + k == x._2)
        deque.removeHead()

      if (x._2 >= k - 1)
        ans(x._2 - k + 1) = nums(deque.head)
    }

    ans
  }
  // println(maxSlidingWindow(Array(1, 3, -1, -3, 5, 3, 6, 7), 3).mkString("Array(", ", ", ")"))
  println(maxSlidingWindow(Array(1, -1), 1).mkString("Array(", ", ", ")"))
}
