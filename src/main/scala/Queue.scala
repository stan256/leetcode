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
}
