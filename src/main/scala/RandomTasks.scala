import scala.collection.{Seq, mutable}

object RandomTasks extends App {

  // #1 two sum
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val result = Array.fill(2)(0)

    for ((x, i) <- nums.zipWithIndex) {

      val diff = target - x

      var startIndex = -1
      // it was a do ... while, but had to rewrite due to Scala 2 => Scala 3
      while (startIndex != -1) {
        startIndex = nums.indexOf(diff, startIndex + 1)

        if (startIndex != i && startIndex != -1) {
          result(0) = startIndex
          result(1) = i
          return result
        }
      } 
    }

    result
  }
  // println(twoSum(Array(3, 3), 6).mkString("Array(", ", ", ")"))

  def process(logs: Array[String], treshold: Int): Array[String] = {
    val maxIndex = Math.pow(10, 5).toInt
    val array = collection.mutable.ArrayBuffer.fill(maxIndex)(0)

    logs.foreach(log => {
      val arr = log.split("\\s")
      if (arr(0) != arr(1))
        arr(arr(1).toInt) = arr(arr(1).toInt) + 1
      arr(arr(0).toInt) = arr(arr(0).toInt) + 1
    })

    array.filter(_>=treshold).map(_.toString).toArray
  }


  // 875. Koko Eating Bananas
  def minEatingSpeed(piles: Array[Int], hours: Int): Int = {
    var left = 1 max (piles.sum / hours)
    var right = piles.max
    while (left < right) {
      val mid = left + (right - left) / 2
      if (piles.map { pile => if (pile % mid == 0) pile / mid else pile / mid + 1 }.sum > hours)
        left = mid + 1
      else
        right = mid
    }
    left
  }
  // println(minEatingSpeed(Array(312884470), 312884469))
  //  println(minEatingSpeed(Array(3,6,7,11), 8))

  // 9. Palindrome Number
  def isPalindrome(x: Int): Boolean = {
    val string = x.toString
    var left = 0
    var right = string.length - 1
    while (left <= right && string.charAt(left) == string.charAt(right)) {
      if (left == right || left == right - 1)
        return true

      left += 1
      right -= 1
    }

    false
  }
  //  println(isPalindrome(121))
  //  println(isPalindrome(12321))
  //  println(isPalindrome(-121))
  //  println(isPalindrome(1))
  //  println(isPalindrome(1234))


  // 13. Roman to Integer
  def romanToInt(s: String): Int = {
    var counter = 0

    val titles = Seq('I', 'V', 'X', 'L', 'C', 'D', 'M')
    val values = Seq(1, 5, 10, 50, 100, 500, 1000)

    val exceptions = Map(
      'I' -> (Seq('V', 'X'), 1),
      'X' -> (Seq('L', 'C'), 10),
      'C' -> (Seq('D', 'M'), 100)
    )

    var i = 0
    while (i < s.length) {
      val c = s(i)
      val maybeException = exceptions.get(c)

      if (maybeException.isDefined && i + 1 < s.length) {
        if (maybeException.get._1.indexOf(s(i + 1)) >= 0)
          counter -= maybeException.get._2
        else
          counter += values(titles.indexOf(c))
      } else
        counter += values(titles.indexOf(c))

      i += 1
    }

    counter
  }
  //  println(romanToInt("III"))
  //  println(romanToInt("VI"))
  //  println(romanToInt("MCMXCIV"))
  //  println(romanToInt("MDCCCLXXXIV"))


  // 14. Longest Common Prefix
  def longestCommonPrefix(strs: Array[String]): String = {
    var x = ""

    for ((c, i) <- strs.min.zipWithIndex) {
      var append = true

      for ((s, _) <- strs.zipWithIndex) {
        if (s.charAt(i) != c) append = false
      }

      if (append)
        x += c
      else
        return x
    }

    x
  }
  //  println(longestCommonPrefix(Array("flower","flow","flight")))

  // 20. Valid Parentheses
  //  def isValid(s: String): Boolean = {
  //    val a = mutable.Stack[Char]()
  //
  //    for (x <- s) {
  //      if (a.isEmpty) {
  //        a.push(x)
  //      } else {
  //        val c = a.pop()
  //
  //        if (!(x == ']' && c == '[' ||
  //          x == '}' && c == '{' ||
  //          x == ')' && c == '(')) {
  //          a.push(c)
  //          a.push(x)
  //        }
  //      }
  //    }
  //
  //    a.isEmpty
  //  }

  def isValid(s: String): Boolean = {
    val openChars = Set('(', '[', '{')
    val charPairs = Map(')' -> '(', ']' -> '[', '}' -> '{')
    val stack = mutable.ListBuffer[Char]()

    for (c <- s) {
      if (openChars.contains(c)) {
        stack += c
      } else if (stack.nonEmpty && charPairs(c) == stack.last) {
        stack.remove(stack.length - 1)
      } else {
        return false
      }
    }

    stack.isEmpty
  }
  //  println(isValid("()"))
  //  println(isValid("(){}[]"))
  //  println(isValid("({}[{}])"))
  //  println(isValid("("))

  // 2348. Number of Zero-Filled Subarrays
  def zeroFilledSubarray(nums: Array[Int]): Long = {
    def zeroArraysLength(l: Long): Long = if (l == 0) 0 else l * (l + 1) / 2

    var all: Long = 0
    var currentCounter: Long = 0

    for (i <- nums) {
      if (i == 0) {
        currentCounter += 1
      } else {
        all += zeroArraysLength(currentCounter)
        currentCounter = 0
      }
    }
    all += zeroArraysLength(currentCounter)

    all
  }
  //  println(zeroFilledSubarray(Array(1,3,0,0,2,0,0,4)))
  //  println(zeroFilledSubarray(Array(0,0,0,2,0,0)))
  //  println(zeroFilledSubarray(Array(2,10,2019)))


  def pivotIndex(nums: Array[Int]): Int = {
    nums.indices.foreach(i => {
      val (left, right) = nums.splitAt(i)
      val sum = left.sum
      val sum1 = right.drop(1).sum
      if (sum == sum1)
        return i
    })
    -1
  }
  //  println((pivotIndex(Array(1, 7, 3, 6, 5, 6))))
  //  println((pivotIndex(Array(-1,-1,-1,1,1,1))))
  //  println((pivotIndex(Array(-1,-1,0,0,-1,-1))))

  // 217. Contains Duplicate
  def containsDuplicate(nums: Array[Int]): Boolean = {
    nums.distinct.length != nums.length
  }


  // 242. Valid Anagram
  def isAnagram(s: String, t: String): Boolean = {
    s.sorted == t.sorted
  }
  //  println(isAnagram("bla", "abl"))
  //  println(isAnagram("aav", "aac"))
}