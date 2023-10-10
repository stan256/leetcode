import scala.collection.mutable

object Stack extends App {
  // 20. Valid Parentheses
  def isValid(s: String): Boolean = {
    val stack = mutable.Stack
    val chars = s.toCharArray
    stack fill chars

    false
  }

  println(isValid("()[]{}"))


}
