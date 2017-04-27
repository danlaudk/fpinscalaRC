//recalling traverse 
//parsing "Sometimes we will want to map over a list using a function that might fail,
//returning if applying it to any element of the list returns None. For example, None
//parsing a whole list of strings into a list of patterns. In that case, we can simply
//sequence the results of the : map
//Unfortunately, this is a little inefficient, since it traverses the list twice. Wanting
//to sequence the results of a this way is a common enough occurrence to map
//warrant a new generic function , with the following signature: 
def parsePatterns(a: List[String]): Option[List[Pattern]] =
  sequence(a map pattern)


// where 
//def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
//def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean]
//def sequence[A](a: List[Option[A]]): Option[List[A]]

// and finally
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
// see solution 3
