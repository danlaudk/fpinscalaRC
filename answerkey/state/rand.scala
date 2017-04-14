trait RNG {
  def nextInt: (Int, RNG) 
}



object RNG {
 def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = ??? // a particular simple pseudoRNG
 }
 type Rand[+A] = RNG => (A, RNG)

 val int: Rand[Int] = _.nextInt
 }
