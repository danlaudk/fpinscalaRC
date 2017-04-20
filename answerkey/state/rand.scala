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
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

//
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???
  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update(i: Input)(s: Machine) => Machine = ???
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update)) // think flatmap
    s <- get
  } yield (s.coins, s.candies) // think map
  
  //run it
  Candy.simulateMachine(run(Machine(False, 5, 2)))
}
