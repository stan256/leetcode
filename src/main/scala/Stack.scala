import scala.collection.mutable

object Stack extends App {
  // 20. Valid Parentheses
  def isValid(s: String): Boolean = {
    import scala.collection.mutable

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

  // 155. Min Stack
  class MinStack() {
    import scala.collection.mutable

    var stack = mutable.Stack[(Int, Int)]()
    var min = 0

    def push(i: Int): Unit = {
      if (stack.isEmpty) {
        min = i
      }

      if (i < min) {
        min = i
      }

      stack = stack.push((i, min))
    }

    def pop(): Unit = {
      val a = stack.pop()

      if (a._2 == min) {
        if (stack.isEmpty) {
          min = 0
        } else {
          min = stack.head._2
        }
      }
    }

    def top(): Int = stack.head._1

    def getMin(): Int = stack.head._2
  }

//  println {
//    val x = new MinStack()
//    x.push(-2)
//    x.push(0)
//    x.push(-3)
//    x.getMin()
//    x.pop()
//    x.top()
//    x.getMin()
//    x.pop()
//    x.pop()
//  }




}
