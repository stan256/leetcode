

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

  //  println(evalRPN_betterStackApproach(Array("10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+")))

  // 22. Generate Parentheses
  def generateParenthesis(n: Int): List[String] = {
    val ans = scala.collection.mutable.ListBuffer[String]()

    def backtrack(tmp: String, open: Int, close: Int): Unit = {
      if (tmp.length == n * 2) ans += tmp
      if (open < n) backtrack(tmp :+ '(', open + 1, close)
      if (close < open) backtrack(tmp :+ ')', open, close + 1)
    }

    backtrack("", 0, 0)
    ans.toList
  }

  //  println(generateParenthesis(3))


  // 739. Daily Temperatures
  def dailyTemperatures(temperatures: Array[Int]): Array[Int] = {
    val stack = scala.collection.mutable.Stack.empty[(Int, Int)]
    val result = Array.fill[Int](temperatures.length)(0)

    for (i <- temperatures.zipWithIndex) {
      if (stack.isEmpty) {
        stack.push(i)
      } else {
        var prev = stack.pop()
        var searchFurther = true

        while (searchFurther) {
          if (prev._1 < i._1) {
            val indexDiff = i._2 - prev._2
            result(prev._2) = indexDiff
            if (stack.nonEmpty) {
              prev = stack.pop()
            } else {
              stack.push(i)
              searchFurther = false
            }
          } else {
            stack.push(prev)
            stack.push(i)
            searchFurther = false
          }
        }
      }
    }

    result
  }

//  println(dailyTemperatures(Array(73, 74, 75, 71, 69, 72, 76, 73)).mkString("Array(", ", ", ")"))
}


