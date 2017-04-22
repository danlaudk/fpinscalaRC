package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }

  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  import fpinscala.testing._
  import Prop._


 //checked against Cats
def coproductMonoid[A,B](A: Monoid[A],
 B: Monoid[B]): Monoid[Either[A,B]] = new Monoid[Either[A,B]] {
    def zero = Right(B.zero)
    def op(a: Either[A], Either[B]) = a match {
      a match {
          case left @ Left(_) => left
          case Right(aright) => b match {
            case bleft @ Left(_) => bleft
            case Right(bright) => Right(B.op(aright, bright))
          }
        }

    }
 }

def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
 new Monoid[Map[K, V]] {
 def zero = Map()
 def op(a: Map[K, V], b: Map[K, V]) =
 a.map {
 case (k, v) => (k, V.op(v, b.get(k) getOrElse V.zero))
 }
 }
// mapMergeMonoid[K,V](V:

// option monad's map2. general defn only requires flatmap
override def map2[A, B, C](fa: Option[A],fb: Option[B])(f: (A, B) => C): Option[C] = {
  (fa, fb) match {
    case (None, _) => None
    case (_, None) => None
    case (_, _) => Some(f(fa.get, fb.get)) // runtime-safe get calls
  }
}

// remember map2 !! dfft defn for each monad, wraps after the fn.
def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap(aa => b map (bb => f(aa, bb)))
 }

  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    flatMap(a)(a => map(b)(b => f(a,b)))

// a call eg is a.map2(b)(f) where a is type monad and thus has a flatMap


// recall list flatmap, 
def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
  list.foldLeft(List[B]())(_ ++ f(_))
// can also be defined recursively
// is not a general defn of flatmap and actually quite tricky to do

def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match {
  case (x::xs) => f(x) ++ flatMap(xs)(f)
  case _ => Nil
}

// ex4
  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.


//gathers the results in a single value, where the monad `F`
  // determines how values are actually combined.
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

// doesn't make sense to me (it's supposed to factor out the monad to the outside) (_,_) is the identity fn on tuple
 def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))  // = product on a monad i think so
 
 // follow the types. the last () means fnapplication in order to 'remove' the argument required of the output type of compose (a fn)
 def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
  compose((_:Unit) => ma, f)(())
// ex 12
  object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    (as.map(f)).foldLeft(mb.zero)(mb.op)
    //foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a))) in the book , same?
}

  
