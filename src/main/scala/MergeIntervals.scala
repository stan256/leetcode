object MergeIntervals extends App {
  // 57. Insert Interval
  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    val lb = collection.mutable.ListBuffer.empty[Array[Int]]
    val pq = collection.mutable.PriorityQueue.from(intervals)(Ordering.by(-_(0)))
    pq.enqueue(newInterval)
    var prev = pq.dequeue()

    while (pq.nonEmpty) {
      val next = pq.dequeue()

      if (prev(1) < next(0)) {
        lb.addOne(prev)
        prev = next
      } else {
        prev(0) = prev(0).min(next(0))
        prev(1) = prev(1).max(next(1))
      }
    }
    lb.addOne(prev)
    lb.toArray
  }

  // 56. Merge Intervals
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    val sorted = intervals.sortBy(_(0))
    var current = sorted.head

    val result = collection.mutable.ListBuffer.empty[Array[Int]]
    for (i <- 1 until sorted.length) {
      if (current(1) < sorted(i)(0)) {
        result.addOne(current)
        current = sorted(i)
      } else {
        current = Array(Math.min(current(0), sorted(i)(0)), Math.max(current(1), sorted(i)(1)))
      }
    }
    result.addOne(current)
    result.toArray
  }

}
