object Intervals extends App {


  // 56. Merge Intervals
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    val sorted = intervals.sortBy(_(0))
    var current = sorted.head
    var result = Array.empty[Array[Int]]
    for (interval <- sorted.drop(1)) {
      if (current(1) < interval(0)) {
        result = result :+ current
        current = interval
      } else {
        current = Array(current(0), Math.max(current(1), interval(1)))
      }
    }
    result = result :+ current
    result
  }

}
