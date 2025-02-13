object TwoDArrays extends App {

  // 54. Spiral Matrix
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    val lb = collection.mutable.ListBuffer.empty[Int]
    var left = 0
    var right = matrix.head.length - 1
    var top = 0
    var bottom = matrix.length - 1

    def shouldProceed: Boolean = lb.length < matrix.length * matrix.head.length

    while (shouldProceed) {
      for (i <- left to right) lb.addOne(matrix(top)(i))
      if (shouldProceed) for (i <- top + 1 to bottom) lb.addOne(matrix(i)(right))
      if (shouldProceed) for (i <- right - 1 to left by -1) lb.addOne(matrix(bottom)(i))
      if (shouldProceed) for (i <- bottom - 1 until top by -1) lb.addOne(matrix(i)(left))

      top += 1
      right -= 1
      bottom -= 1
      left += 1
    }

    lb.toList
  }

  println(spiralOrder(
    Array(
      Array(1,2,3),
      Array(4,5,6),
      Array(7,8,9)
    )
  ))

//  println(spiralOrder(
//    Array(
//      Array(1,2,3,4),
//      Array(5,6,7,8),
//      Array(9,10,11,12)
//    )
//  ))

}
