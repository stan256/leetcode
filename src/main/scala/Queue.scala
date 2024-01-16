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

}
