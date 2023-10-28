
object TwoPointers extends App {

  // 125. Valid Palindrome
  def isPalindrome(s: String): Boolean = {
    val normalised = s.trim().replaceAll("[^a-zA-Z0-9]", "").toLowerCase
    var i = 0
    var j = normalised.length - 1
    while (i <= j) {
      if (normalised.charAt(i) != normalised.charAt(j))
        return false

      i += 1
      j -= 1
    }
    true
  }

  def isPalindrome_2(s: String): Boolean = {
    var i = 0
    var j = s.length - 1

    def charOrDigit(c: Char): Boolean = {
      val bool = (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57)
      bool
    }

    def isDigit(c: Char): Boolean = c >= 48 && c <= 57

    def leftPointerMoveToNextCharacter(): Unit =
      while (i < s.length && (s.charAt(i) match {
        case a if charOrDigit(a) => false
        case _ =>
          i += 1
          true
      }
        )) {}

    def rightPointerMoveToNextCharacter(): Unit =
      while (j >= 0 && (s.charAt(j) match {
        case a if charOrDigit(a) => false
        case _ =>
          j -= 1
          true
      }
        )) {}

    while (i <= j) {
      leftPointerMoveToNextCharacter()
      rightPointerMoveToNextCharacter()

      if (i >= s.length || j < 0)
        return true

      val a = s.charAt(i)
      val b = s.charAt(j)

      if (!(a == b || (a + 32 == b && a > 57) || (b + 32 == a && b > 57)))
        return false
      i += 1
      j -= 1
    }
    true
  }

//  println(isPalindrome("A man, a plan, a canal: Panama"))
//  println(isPalindrome("ab_a"))
//  println(isPalindrome(" "))
//  println(isPalindrome("0P"))

  // 167. Two Sum II - Input Array Is Sorted
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    var i = 0
    var j = numbers.length - 1

    while(i <= j) {
      val res = numbers(i) + numbers(j)
      if (res == target) {
        return Array(i + 1, j + 1)
      } else if (res > target) {
        j -= 1
      } else {
        i += 1
      }
    }

    throw new IllegalStateException()
  }
//  println(twoSum(Array(2,7,11,15), 9).mkString("Array(", ", ", ")"))

  // 15. 3Sum
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted
    var res = Array.empty[Array[Int]]

    for (i <- sorted.indices) {
      for (j <- sorted.indices) {
        val a = sorted(i)
        val b = sorted(j)
        val target = 0 - a - b
        val subResult = Array(a, b, target).sorted
        if (i != j && res.forall(x => !java.util.Arrays.equals(x, subResult))) {
          val ints = sorted.zipWithIndex.filter(x => !Seq(i, j).contains(x._2)).map(_._1)
          import scala.collection.Searching._
          val search = ints.search(target)
          search match {
            case Found(_) => res = res :+ subResult
            case _ =>
          }
        }
      }
    }

    res.map(_.toList).toList
  }
//  println(threeSum(Array(-1, 0, 1, 2, -1, -4)).mkString("Array(", ", ", ")"))
  println(threeSum(Array(-1,0,1,2,-1,-4,-2,-3,3,0,4)).mkString("Array(", ", ", ")"))
}
