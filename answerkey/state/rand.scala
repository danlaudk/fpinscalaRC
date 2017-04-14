trait RNG {
  def nextInt: (Int, RNG) 
}



object RNG {
  // use : val rng = simple(42)
  //       rng.nextInt //whenever u want ur next random int
 def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = ??? // a particular simple pseudoRNG
 }
  
 type Rand[+A] = RNG => (A, RNG)

 val int: Rand[Int] = _.nextInt
 def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???
 }


// given 
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

// sequence is the same as before

// derive flatmap
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]
