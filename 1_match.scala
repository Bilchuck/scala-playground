// write a function which returns last element
// of a given list of integers, using pattern matching and recursion
def last(l: List[Int]): Int = {
  l match {
    case Nil => throw new Error()
    case head :: Nil => head
    case _ :: tail => last(tail)
  }
}

assert(last(1 :: 2 :: 3 :: Nil) == 3)

// Write a function which returns Nth element
// using pattern matching
// you can use 'assert' to test if input arg is valid
// Re-write previous function using this one and list.length
def nTh(n: Int, l: List[Int]): Int = {
  (n,l) match {
    case (_, Nil) => throw new Error()
    case (0, head :: tail) => head
    case (_, head :: Nil) => throw new Error()
    case (_, head :: tail) => nTh(n - 1, tail)
  }
}
assert(nTh(2, List(1,2,3,4)) == 3)
assert(nTh(0, List(1,2,3,4)) == 1)
assert(nTh(3, List(1,2,3,4)) == 4)


// Ex #9
// write a function which computes a median of
// a randomly generated list (reuse function above)
// you can use list.sortWith function to sort a list
def median(l: List[Int]): Float = {
  var sorted = l.sortWith((x,y) => x < y)
  var med = l.length / 2
  if (med % 1 == med) nTh(med, l)
  else {
    var lower = (med - 0.5).toInt
    var higher = (med + 0.5).toInt
    ( nTh(lower, sorted) + nTh(higher, sorted) ) / 2
  }
}

println(median(List(1,2,3,4,5)))
