/* Home Assignment
Simple Expression parser
  1 + 2 * 3 = 7
Build an expression evaluator that works on tokens of strings composed of integers and binary operators + and * (see example)
Grammar:
  expression -> number {operator number}*
  operator -> ‘+’ | ‘*’
  number -> regex([0-9]+)
Hints:
  - use pattern matching to match operators and numbers in List
  - use .toInt to parse number
    "1".toInt == 1
*/
def eval(l: List[String]): Int = {
  l match {
    case head :: Nil => head.toInt
    case a :: "+" :: xs => a.toInt + eval(xs)
    case a :: "*" :: b :: xs => eval((a.toInt * b.toInt).toString :: xs)
    case _ => 0
  }
}
println(eval(List("1", "+", "2", "+", "3")))

assert(eval(List("1", "+", "2", "+", "3")) == 6)

assert(eval(List("1", "+", "2", "*", "3")) == 7)
assert(eval(List("5", "*", "3", "+", "1")) == 16)
