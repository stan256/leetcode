

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

  // 150. Evaluate Reverse Polish Notation
  def evalRPN(tokens: Array[String]): Int = {
    import scala.collection.mutable

    val signs = Set('+', '-', '*', '/')

    def isSign(s: String): Boolean = signs.contains(s(0)) && s.length == 1

    def isDigit(s: String): Boolean = !isSign(s)

    def calculate(sign: String, a: Int, b: Int): Int =
      sign match {
        case "+" => a + b
        case "-" => a - b
        case "*" => a * b
        case "/" => a / b
        case _ => throw new RuntimeException()
      }

    val stack = mutable.Stack.from(tokens)

    var first = tokens.length - 3
    var second = tokens.length - 2
    var third = tokens.length - 1

    while (stack.size != 1) {
      if (isDigit(stack(first)) && isDigit(stack(second)) && isSign(stack(third))) {
        val result = calculate(stack(third), stack(first).toInt, stack(second).toInt)
        stack(first) = result.toString
        stack.remove(third)
        stack.remove(second)
      } else {
        first += -1
        second += -1
        third += -1
      }
    }

    stack.pop().toInt
  }

  def evalRPN_betterStackApproach(tokens: Array[String]): Int = {
    import scala.collection.mutable

    val signs = "+-/*"
    def isSign(s: String): Boolean = signs.contains(s)

    def calculate(sign: String, a: Int, b: Int): Int = sign match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" => a / b
      case _ => throw new RuntimeException()
    }

    val stack = mutable.Stack.empty[String]

    for (s: String <- tokens) {
      if (isSign(s)) {
        val first = stack.pop().toInt
        val second = stack.pop().toInt
        val i = calculate(s, second, first)
        stack.push(i.toString)
      }
      else {
        stack.push(s)
      }
    }

    stack.pop().toInt
  }

  println(evalRPN_betterStackApproach(Array("10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+")))
}


