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
