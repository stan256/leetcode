
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
    val queue = collection.mutable.PriorityQueue[Double](nums.map(_.toDouble): _*)

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
  // println(minStoneSum(Array(5,4,9), 2))

  // 1167. Minimum Cost to Connect Sticks
  def connectSticks(sticks: Array[Int]): Int = {
    var totalCost = 0
    val queue = collection.mutable.PriorityQueue.from(sticks)(Ordering.Int.reverse)

    while (queue.size > 1) {
      val c = queue.dequeue() + queue.dequeue()
      totalCost += c
      queue.enqueue(c)
    }

    totalCost
  }

  // 347. Top K Frequent Elements
  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    val map = nums.foldLeft(Map.empty[Int, Int]) { (map, i) => map + (i -> (map.getOrElse(i, 0) + 1)) }
    val queue = collection.mutable.PriorityQueue.empty[(Int, Int)](Ordering.by[(Int, Int), Int](-_._2))

    for (pair <- map) {
      queue.enqueue(pair)

      if (queue.size > k) {
        queue.dequeue()
      }
    }

    queue.take(k).map(_._1).toArray
  }

  // 658. Find K Closest Elements
  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
    val pq = collection.mutable.PriorityQueue.empty[(Int, Int)](Ordering.by[(Int, Int), Int](-_._2))

    for (num <- arr) {
      val diff = Math.abs(num - x)
      if (pq.size < k) {
        pq.enqueue((num, diff))
      } else {
        val head = pq.head
        if (head._2 > diff || (head._2 == diff && head._1 > num)) {
          pq.dequeue()
          pq.enqueue((num, diff))
        }
      }
    }

    pq.map(_._1).toList.sorted
  }

  // 215. Kth Largest Element in an Array
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    val pq = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)

    for (x <- nums) {
      pq.enqueue(x)
      if (pq.size > k)
        pq.dequeue()
    }

    pq.head
  }

  // 973. K Closest Points to Origin
  def kClosest(points: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    val pq = collection.mutable.PriorityQueue.empty[(Int, Int, Double)](Ordering.by[(Int, Int, Double), Double](_._3))

    def distance(x: Int, y: Int): Double = Math.sqrt(x * x + y * y)

    for (num <- points) {
      val x = num(0)
      val y = num(1)
      val dist = distance(x, y)

      if (pq.size < k) {
        pq.enqueue((x, y, dist))
      } else {
        if (pq.head._3 > dist) {
          pq.dequeue()
          pq.enqueue((x, y, dist))
        }
      }
    }

    pq.toArray.map(t => Array(t._1, t._2))
  }

  // 703. Kth Largest Element in a Stream
  class KthLargest(_k: Int, _nums: Array[Int]) {
    val pq = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    _nums.foreach {
      num => {
        pq.enqueue(num)
        if (pq.size > _k) {
          pq.dequeue()
        }
      }
    }
    // 2, 3, 4, 5, 8


    def add(`val`: Int): Int = {
      pq.enqueue(`val`)
      if (pq.size > _k) {
        pq.dequeue()
      }
      pq.head
    }
  }


  def kthMinInSubarrays(k: Int, vulnerability: Array[Int], m: Int): Seq[Int] = {
    val pq = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)

    def addElement(num: Int): Unit = {
      pq.enqueue(num)
      if (pq.size > m) pq.dequeue()
    }

    for (i <- 0 until Math.min(m, vulnerability.length)) {
      addElement(vulnerability(i))
    }

    val result = (0 to Math.max(0, vulnerability.length - m)).map { i =>
      if (i + m < vulnerability.length) {
        addElement(vulnerability(i + m)) // Add the next element to the priority queue if within bounds
      }
      val tempPQ = pq.clone()
      for (_ <- 1 until k) tempPQ.dequeue()
      tempPQ.head
    }

    result.toList
  }

  // 253. Meeting Rooms II
  def minMeetingRooms(inputIntervals: Array[Array[Int]]): Int = {
    if (inputIntervals.length == 0) return 0

    val minHeap = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    val intervals = inputIntervals.sortBy(x => x(0))

    minHeap.enqueue(intervals(0)(1))

    for (i <- 1 until intervals.length) {
      if (intervals(i)(0) >= minHeap.headOption.getOrElse(0))
        minHeap.dequeue()

      minHeap.enqueue(intervals(i)(1))
    }
    minHeap.size
  }


  // 23. Merge k Sorted Lists
  case class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    val pq = collection.mutable.PriorityQueue.empty[ListNode](Ordering.by(-_.x))

    val head = new ListNode(-1)
    var point = head

    for (node <- lists) {
      if (node != null) pq.enqueue(node)
    }

    while (pq.nonEmpty) {
      point.next = pq.dequeue()
      point = point.next
      if (point.next != null)
        pq.enqueue(point.next)
    }
    head.next
  }

  // 628. Maximum Product of Three Numbers
  def maximumProduct(nums: Array[Int]): Int = {
    val maxQ = collection.mutable.PriorityQueue.empty[Int]
    val minQ = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    minQ.addAll(nums)
    maxQ.addAll(nums)
    Math.max(
      minQ.dequeue * minQ.dequeue * maxQ.head,
      maxQ.dequeue * maxQ.dequeue * maxQ.dequeue)
  }

  // 695. Max Area of Island
  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val seen = grid.map(arr => arr.map(_ => false))
    var max = 0

    def countArea(r: Int, c: Int): Int = {
      if (r < 0
        || c < 0
        || r >= grid.length
        || c >= grid(0).length
        || seen(r)(c)
        || grid(r)(c) == 0)
        return 0

      seen(r)(c) = true
      1 + countArea(r - 1, c) + countArea(r + 1, c) + countArea(r, c - 1) + countArea(r, c + 1)
    }

    for (i <- grid.indices) {
      for (j <- grid(i).indices) {
        max = Math.max(max, countArea(i, j))
      }
    }

    max
  }

  // 1438. Longest Continuous Subarray With Absolute Diff Less Than or Equal to Limit
  def longestSubarray(nums: Array[Int], limit: Int): Int = {
    val ascending = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)
    val descending = collection.mutable.PriorityQueue.empty[Int]
    var left, right = 0

    val ascRemove = collection.mutable.Map.empty[Int, Int]
    val descRemove = collection.mutable.Map.empty[Int, Int]

    var max = 1

    while (right < nums.length) {
      ascending.enqueue(nums(right))
      descending.enqueue(nums(right))

      if (descending.head - ascending.head > limit) {
        ascRemove(nums(left)) = ascRemove.getOrElse(nums(left), 0) + 1
        descRemove(nums(left)) = descRemove.getOrElse(nums(left), 0) + 1
        left += 1

        while (descRemove.get(descending.head).exists(_ > 0)) {
          descRemove(descending.head) = descRemove(descending.head) - 1
          descending.dequeue()
        }

        while (ascRemove.get(ascending.head).exists(_ > 0)) {
          ascRemove(ascending.head) = ascRemove(ascending.head) - 1
          ascending.dequeue()
        }
      } else {
        max = max.max(right - left + 1)
      }

      right += 1
    }
    max
  }


}
