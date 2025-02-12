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

}
