import scala.collection.mutable

object Stack extends App {
  // 20. Valid Parentheses
  def isValid(s: String): Boolean = {
    if (s.isEmpty) return true
    else if (s.length == 1) return false

    val stack = mutable.Stack[Char](s(0))

    for (c <- s.substring(1)) {
      if (c == '{' || c == '(' || c == '[')
        stack.push(c)
      else {
        val maybeHead = stack.headOption

        if ((c == '}' && maybeHead.contains('{')) || (c == ')' && maybeHead.contains('(')) || (c == ']' && maybeHead.contains('['))) {
          stack.pop()
        } else {
          return false
        }
      }
    }

    stack.isEmpty
  }
//  println(isValid("(])"))


}
