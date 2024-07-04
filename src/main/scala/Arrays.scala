import scala.collection.mutable

object Arrays extends App {

  def twoSum_binarySearch(nums: Array[Int], target: Int): Array[Int] = {
    /*
    * add indices
    * sort by value
    * iterate though the array of tuples
    * for each x => 'search' y where x + y = target && x.index != y.index
    * use binary search for 'search'
    * use indices in tuple as response
    * */
    import scala.collection.Searching._

    val result = Array.fill(2)(0)
    val sortedWithIndices: Array[(Int, Int)] = nums.zipWithIndex.sortBy(_._1)

    sortedWithIndices.find(t => {
      val v = t._1
      val i = t._2

      // 2nd argument in search doesn't matter
      sortedWithIndices.search((target - v, -1))(Ordering.by(_._1)) match {
        case f@Found(fi) if sortedWithIndices(f.foundIndex)._2 != i => {
          result(0) = sortedWithIndices(fi)._2
          result(1) = t._2
          true
        }
        case Found(_) => false
        case InsertionPoint(_) => false
      }
    })

    result
  }
  //  println(twoSum(Array(3,2,3), 6).mkString("Array(", ", ", ")"))

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val arr = Array.ofDim[Int](2)
    val map = mutable.Map[Int, Seq[Int]]()

    for ((x, i) <- nums.zipWithIndex) {
      val diff = target - x
      val seq = map.getOrElse(diff, Seq[Int]())
      if (diff == x && seq.size > 1) {
        arr(0) = seq.head
        arr(1) = seq(1)
      } else if (seq.nonEmpty) {
        arr(0) = i
        arr(1) = seq.head
      } else {
        map.addOne(x, Seq(i))
      }
    }

    arr
  }

  //  println(twoSum(Array(3,2,3), 6).mkString("Array(", ", ", ")"))
  //  println(twoSum(Array(2, 15, 7, 11), 9).mkString("Array(", ", ", ")"))


  // 49. Group Anagrams
  def groupAnagrams(strs: Array[String]): List[List[String]] = {

    /*
    going through strs and mapping each element to entry: str -> sorted chars of it
    going through map and collecting it by sorted arrays
    * */

    val sorted = strs.map(str => str -> str.toCharArray.sorted.mkString)
    val map = mutable.Map[String, Seq[String]]()
    for ((a, b) <- sorted) {
      val value = map.getOrElse(b, Seq()) :+ a
      map.update(b, value)
    }

    map.values.toList.map(_.toList)
  }
  //  println(groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))


  // 347. Top K Frequent Elements
  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    nums.foldLeft(Map[Int, Int]()) { (acc, x) => {
      acc + (x -> (acc.getOrElse(x, 0) + 1))
    }
    }.toSeq.sortBy(_._2)(Ordering.Int.reverse).take(k).map(_._1).toArray
  }

  def topKFrequentHeap(nums: Array[Int], k: Int): Array[Int] = {
    val x: Map[Int, Int] = nums.foldLeft(Map.empty[Int, Int]) { (acc, num) =>
      acc + (num -> (acc.getOrElse(num, 0) + 1))
    }

    val pq = mutable.PriorityQueue.empty[(Int, Int)](Ordering.by(-_._2)) // Use negation for min-heap

    x.foreach { case (num, count) =>
      pq.enqueue((num, count))
    }

    // Dequeue until the size of pq is k
    while (pq.size > k) {
      pq.dequeue()
    }

    pq.map(_._1).toArray
  }

  //  println(topKFrequent(Array(1, 1, 1, 2, 2, 3), 2).mkString("Array(", ", ", ")"))
  //  println(topKFrequent(Array(-1,-1), 2).mkString("Array(", ", ", ")"))


  // 238. Product of Array Except Self
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    /*
      original - [1, 2, 3, 4]
      left     - [1, 1, 2, 6]
      right    - [24, 12, 4, 1]
      result   - [24, 12, 8, 6]
    */

    val nums_left_product = nums.take(nums.length - 1).foldLeft(Array(1))((acc, x) => acc :+ (acc.last * x))
    val nums_right_product = nums.drop(1).foldRight(Array(1))((x, acc) => acc prepended (acc.head * x))
    nums_left_product.zipWithIndex.map(x => nums_right_product(x._2) * x._1)
  }
  //  println(productExceptSelf(Array(1, 2, 3, 4)).mkString("Array(", ", ", ")"))

  def productExceptSelf_fasterSolutionWithoutFold(nums: Array[Int]): Array[Int] = {
    val arrLeft = new Array[Int](nums.length)
    val arrRight = new Array[Int](nums.length)
    arrLeft(0) = 1
    arrRight(nums.length - 1) = 1
    for (i <- (1 until nums.length)) {
      val i1 = arrLeft(i - 1)
      val i2 = nums(i - 1)
      arrLeft(i) = i1 * i2
    }
    for (i <- (nums.length - 2 to 0 by -1)) {
      val i1 = arrRight(i + 1)
      val i2 = nums(i + 1)
      arrRight(i) = i1 * i2
    }
    val result = new Array[Int](nums.length)
    for (i <- nums.indices) {
      result(i) = arrLeft(i) * arrRight(i)
    }
    result
  }
  //  println(productExceptSelf_fasterSolutionWithoutFold(Array(1, 2, 3, 4)).mkString("Array(", ", ", ")"))


  def productExceptSelf_lessSpace(nums: Array[Int]): Array[Int] = {
    val result = new Array[Int](nums.length)
    result(0) = 1
    for (i <- (1 until nums.length)) {
      val i1 = result(i - 1)
      val i2 = nums(i - 1)
      result(i) = i1 * i2
    }
    var right = 1
    for (i <- (nums.length - 1 to 0 by -1)) {
      result(i) = result(i) * right
      right = right * nums(i)
    }
    result
  }
  //  println(productExceptSelf_lessSpace(Array(1, 2, 3, 4)).mkString("Array(", ", ", ")"))


  // 36. Valid Sudoku
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def checkLine(arr: Array[Char]) = {
      val chars = arr.filter(_ != '.')
      chars.toSet.size == chars.length
    }

    def checkSquare(xFrom: Int, yFrom: Int) = {
      val chars = board.slice(xFrom, xFrom + 3).map(col => col.slice(yFrom, yFrom + 3)).foldLeft(Array.empty[Char])((acc, x) => acc ++ x.filter(_ != '.'))
      chars.toSet.size == chars.length
    }

    for (x <- 0 to 8) {
      if (!checkLine(board(x))) return false
    }
    for (y <- 0 to 8) {
      if (!checkLine(board.map(sub => sub(y)))) return false
    }
    for (x <- 0 to 8 by 3) {
      for (y <- 0 to 8 by 3) {
        if (!checkSquare(x, y)) return false
      }
    }
    true
  }

  def isValidSudoku2(board: Array[Array[Char]]): Boolean = {
    def checkLine(arr: Seq[Char]) = {
      val chars = arr.filter(_ != '.')
      chars.distinct.size == chars.length
    }

    for (x <- 0 to 8) {
      if (!checkLine(board(x))) return false
      for (y <- 0 to 8) {
        if (!checkLine(board.map(b => b(y)))) return false
        if (x % 3 == 0 && y % 3 == 0) {
          if (!checkLine(board.slice(x, x + 3).map(col => col.slice(y, y + 3)).foldLeft(Array.empty[Char])((acc, x) => acc ++ x.filter(_ != '.'))))
            return false
        }
      }
    }

    true
  }

  //  println(isValidSudoku2(Array(
  //    Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
  //    Array('5', '.', '.', '1', '9', '5', '.', '.', '.'),
  //    Array('.', '5', '8', '.', '.', '.', '.', '6', '.'),
  //    Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
  //    Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
  //    Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
  //    Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
  //    Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
  //    Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
  //  )))

  // 128. Longest Consecutive Sequence
  def longestConsecutive(nums: Array[Int]): Int = {
    val set = nums.toSet
    var counter = 0

    for (x <- set) {
      if (!set(x - 1)) {
        var currentCounter = 1
        var el = x
        while (set(el + 1)) {
          el += 1
          currentCounter += 1
        }
        if (currentCounter > counter) counter = currentCounter
      }
    }

    counter
  }
  //  println(longestConsecutive(Array(100,4,200,1,3,2)))

  // 271. Encode and Decode Strings
  class Codec {
    // Encodes a list of strings to a single string.
    val delimiter = "^_"

    def encode(strs: List[String]): String =
      strs.fold("")((a, b) => a + b.length + delimiter + b)

    // Decodes a single string to a list of strings.
    def decode(s: String): List[String] = {
      var res: List[String] = List.empty[String]
      var i = 0
      var nextWordLengthStr: String = ""

      while (i <= s.length - 1) {
        if (i < s.length - 1 && s.slice(i, i + 2) == delimiter) {
          i += 2
          val k = nextWordLengthStr.toInt
          res = res :+ s.slice(i, i + k)
          i += k
          nextWordLengthStr = ""
        } else {
          nextWordLengthStr += s.charAt(i).toString
          i += 1
        }
      }

      res
    }
  }

  //  val encoder = new Codec()
  //  private val str: String = encoder.encode(List("Hello", "World", "hi"))
  //  println(str)
  //  println(encoder.decode(str))

  // 412. Fizz Buzz
  def fizzBuzz(n: Int): List[String] = {
    (1 to n).map {
      case n if n % 15 == 0 => "FizzBuzz"
      case n if n % 5 == 0 => "Buzz"
      case n if n % 3 == 0 => "Fizz"
      case n => n.toString
    }.toList
  }

  // 344. Reverse String
  def reverseString(s: Array[Char]): Unit = {
    var (left, right) = (0, s.length - 1)

    while (left < right) {
      val temp = s(left)
      s(left) = s(right)
      s(right) = temp
      left += 1
      right -= 1
    }
  }

  // 977. Squares of a Sorted Array
  def sortedSquares(nums: Array[Int]): Array[Int] = {
    var positivePointer = 0
    while (positivePointer < nums.length && nums(positivePointer) < 0) {
      positivePointer += 1
    }
    var negativePointer = positivePointer - 1
    var res = Array[Int]()
    while (negativePointer >= 0 || positivePointer < nums.length) {
      if (positivePointer == nums.length || negativePointer >= 0 && Math.abs(nums(negativePointer)) < nums(positivePointer)) {
        res = res :+ nums(negativePointer) * nums(negativePointer)
        negativePointer -= 1
      } else {
        res = res :+ nums(positivePointer) * nums(positivePointer)
        positivePointer += 1
      }
    }
    res
  }
  //  println(sortedSquares(Array(-4,-1,0,3,10)).mkString("Array(", ", ", ")"))
  //  println(sortedSquares(Array(-4)).mkString("Array(", ", ", ")"))
  //  println(sortedSquares(Array(-3, 1)).mkString("Array(", ", ", ")"))


  // PREFIX SUM:

  // 1413. Minimum Value to Get Positive Step by Step Sum
  def minStartValue(nums: Array[Int]): Int = {
    var k = 0
    val prefixSum = nums.map(y => {
      k = k + y
      k
    })
    val min = prefixSum.min
    if (min < 0) Math.abs(min) + 1 else 1
  }

  //  println(minStartValue(Array(-3, 2, -3, 4, 2)))
  //  println(minStartValue(Array(1, -2, -3)))

  // 2090. K Radius Subarray Averages
  def getAverages(nums: Array[Int], k: Int): Array[Int] = {
    val prefixSum = nums.scanLeft[Long](0)(_ + _).drop(1)
    nums.zipWithIndex.map(n => {
      if (n._2 - k < 0 || n._2 + k >= nums.length) -1
      else {
        val rightSum = prefixSum(n._2 + k)
        val leftSum = if (n._2 - k - 1 >= 0) prefixSum(n._2 - k - 1) else 0
        ((rightSum - leftSum) / (2 * k + 1)).toInt
      }
    })
  }

  //  println(getAverages(Array(7, 4, 3, 9, 1, 8, 5, 2, 6), 3).mkString("Array(", ", ", ")"))

  // 2270. Number of Ways to Split Array
  def waysToSplitArray(nums: Array[Int]): Int = {
    val prefixSum = Array.ofDim[Int](nums.length)

    prefixSum(0) = nums(0)
    for (i <- 1 until nums.length) {
      prefixSum(i) = prefixSum(i - 1) + nums(i)
    }

    var counter = 0

    for (i <- nums.indices) {
      val left = prefixSum(i)
      val right = prefixSum(prefixSum.length - 1) - prefixSum(i)
      if (left >= right) counter += 1
    }

    if (prefixSum(prefixSum.length - 1) > 0)  counter - 1 else counter
  }

  def waysToSplitArray_withoutArray(nums: Array[Int]): Int = {
    var left: Long = 0L
    val total: Long =  nums.map(_.toLong).sum
    var counter = 0

    for (i <- 0 until nums.length - 1) {
      left += nums(i)
      if (left >= total - left) counter += 1
    }

    counter
  }

  // 1732. Find the Highest Altitude
  def largestAltitude(gain: Array[Int]): Int = {
    val arr = Array.ofDim[Int](gain.length)
    arr(0) = gain(0)
    for (i <- 1 until gain.length) {
      arr(i) = arr(i - 1) + gain(i)
    }

    Math.max(0, arr.max)
  }

  def largestAltitude_optimal(gain: Array[Int]): Int = {
    var result = 0
    var shift = 0
    for (i <- gain.indices) {
      shift += gain(i)
      result = Math.max(result, shift)
    }
    result
  }

  // 724. Find Pivot Index
  def pivotIndex(nums: Array[Int]): Int = {
    val sum = nums.sum
    var leftSum = 0
    var i = 0
    while (i < nums.length) {

      if (leftSum == sum - leftSum - nums(i)) return i

      leftSum += nums(i)

      i += 1
    }

    -1
  }
}
