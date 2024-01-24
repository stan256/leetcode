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
    nums.zipWithIndex.map(x => {
      while (deque.lastOption.exists(nums(_) < x._1))
        deque.removeLast()

      deque.append(x._2)

      if (deque.head + k == x._2)
        deque.removeHead()

      nums(deque.head)
    }).drop(k - 1)
  }
  // println(maxSlidingWindow(Array(1, 3, -1, -3, 5, 3, 6, 7), 3).mkString("Array(", ", ", ")"))
  // println(maxSlidingWindow(Array(1, -1), 1).mkString("Array(", ", ", ")"))

  // 1438. Longest Continuous Subarray With Absolute Diff Less Than or Equal to Limit
  def longestSubarray(nums: Array[Int], limit: Int): Int = {
    val increasing = collection.mutable.ArrayDeque.empty[Int]
    val decreasing = collection.mutable.ArrayDeque.empty[Int]
    var answer = 0
    var left, right = 0

    while (right < nums.length) {
      while (increasing.lastOption.exists(_ > nums(right)))
        increasing.removeLast()
      while (decreasing.lastOption.exists(_ < nums(right)))
        decreasing.removeLast()

      increasing.append(nums(right))
      decreasing.append(nums(right))


      while (decreasing.head - increasing.head > limit) {
        if (nums(left) == increasing.head) increasing.removeHead()
        if (nums(left) == decreasing.head) decreasing.removeHead()
        left += 1
      }

      val diff = right - left + 1
      if (answer < diff) answer = diff

      right += 1
    }

    answer
  }

  // 496. Next Greater Element I
  def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val stack = collection.mutable.Stack.empty[Int]
    val map = collection.mutable.HashMap.empty[Int, Int]

    for (x <- nums2) {
      while (stack.lastOption.exists(l => l < x)){
        val prev = stack.removeLast()
        map.put(prev, x)
      }

      stack.append(x)
    }
    nums1.map(x => map.getOrElse(x, -1))
  }

  // 901. Online Stock Span
  class StockSpanner() {
    var stack = collection.mutable.Stack.empty[(Int, Int)]

    def next(price: Int): Int = {
      var answer = 1
      while (stack.headOption.exists(p => p._1 <=  price)) {
        answer += stack.pop()._2
      }
      stack.push((price, answer)).head._2
    }
  }

  var stockSpanner = new StockSpanner()
  println(stockSpanner.next(100))
  println(stockSpanner.next(80))
  println(stockSpanner.next(60))
  println(stockSpanner.next(70))
  println(stockSpanner.next(60))
  println(stockSpanner.next(75))
  println(stockSpanner.next(85))

}
