# Introduction to Dependent Typing
![Nick Pollard @ nstack](nstack.svg)

# Welcome

* Nick Pollard @ nstack
* 4 years Scala, now Haskell
* Hiring functional programmers - talk to me!
* nick@nstack.com
* @Nick_enGB

# Dependent typing

> * What it is
> * How it works
> * Why it matters

# What is Dependent Typing

> * Extension to existing type systems
> * Express types that can't otherwise be written
> * Type more programs
> * Type programs more specifically

# What are Type Systems?

> "A type system is a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute." - *Pierce, Types and Programming Languages*

# What are Type Systems?

> * Proving certain properties
> * Guaranteeing behavior
> * More than just how we store data

# List vs NonEmptyList

```scala
case class NonEmptyList[T](head: T, tail: List[T])

case class ::[T](head: T, tail: List[T]) extends List[T]
```

# List vs NonEmptyList

```scala
def unsafe[T](ts: List[T]): T

def safe[T](ts: NonEmptyList[T]): T
```

# Types as Proof Systems

> * Curry-Howard correspondence
> * Types are Propositions
> * Values are Proofs

# Types as Proof Systems

> * Functions are implications
> * A => B means A implies B
> * Arguments are preconditions
> * Results are postconditions

# Types as Proof System

> * Implicit lookup uses the compiler to verify proofs
> * Typechecker verifies our logic
> * aka Prolog

# What is a Dependent Type

> * A type that depends on a value
> * Type will be different for different values
> * Not known until runtime
> * But can still refer to it

# What is a Dependent Type

* Dependent Function
    * Return value depends on argument
    * 'Pi-type'

* Dependent Pair
    * Second value depends on first value
    * 'Sigma-type'

# What do they mean

* Extends Curry-Howard with First-Order (Predicate) Logic
* Allows Quantification
    * Dependent function is 'forall'
    * Dependent pair is 'exists'

# Bridging the Type/Value divide

> * But values are runtime, types compile time?
> * Type members and Existential types to hide types when necessary
> * Pattern-matching on GADTs to restore type information locally

# What can you do with Dependent Typing?

> * Encode program invariants
> * Safe dynamic programming

# Dependent Typing in Scala

* Traditional Dependent Typing example
* Sized-lists (Vectors)
* We're going to write a dependently typed 'map' function

# Type level Naturals (Peano Numbers)

* We can Implement Natural numbers as types - use type information to drive the program code
* Inductive data type

```scala
sealed trait Nat

case object Zero extends Nat
case class S[N_ <: Nat] extends Nat
```

# Type subtype restrictions

* Using a subtyping restriction as a kind - 'type of types'

```scala
N <: Nat
```

* Type param blocks start to look like value param blocks

# Type Members

* Give our Vector a type member to express the (type-level) length of it
* Now we can talk about vector lengths

```scala
trait Vec[T] {
  type Length <: Nat
}
```

# Type Parameters

* Sometimes want to recover type member into an explicit type parameter
* Helps the compiler with unification
* Type parameter will itself be dependent

```scala
sealed trait Vec[T] {
  type Length <: Nat
  def aux: VecN[T,Length]
}
sealed trait VecN[T,N <: Nat] extends Vec[T] {
  type Length = N
  def aux = this
}
```

# Type Projection

* Not dependent - Constant for all values of a type
* Allows some helpful operators

```scala
type length[N <: Nat] = VecN[T,N]

Vec[T]#length[N]
```

# Vector Implementation

```scala
case class VecNil[T]() extends VecN[T,_0]
case class :<:[T, N <: Nat](head: T, tail: VecN[T,N]) 
  extends VecN[T,S[N]]
```

# Type level length

* Statically known lengths:

```scala
val v : VecN[Int,_4] = 1 :<: 2 :<: 3 :<: 4 :<: nil
```

* Does not compile:

```scala
val bad : VecN[Int,_4] = 1 :<: 2 :<: 3 :<: nil
```

# Dependently typed length

* Even when we don't know the input value, we can talk about the relation:
```scala
val foo : Vec[Int] = ???
val good : VecN[Int,S[foo.Length]] = 1 :<: foo
```
* Doesn't compile
```scala
val bad : VecN[Int,S[foo.Length]] = 1 :<: 2 :<: foo
```

# What we'd like to write

* Normally the compiler lets us get away with a lot:
```scala
def map[A,B](f: A => B, v: Vec[A]): Vec[B] = nil[B]
```
* We'd like to enforce another invariant
```scala
def map[A,B](f: A => B, v: Vec[A]): VecN[B,v.Length]
```

# Recovering type information via GADTs

* By matching on subtypes, we can prove to the compiler the value of type members/params
* Allows us to recover some type information from the value-level world
* e.g. if we match on VecNil (value), we know that the length (type) must be zero

# Map Implementation

* Recurse to reify member as parameter
* helps compiler unify types

```scala
def map[A,B](f: A => B, v: Vec[A]): VecN[B,v.Length] = mapN(f, v.aux)

def mapN[A,B,N <: Nat](f: A => B, v: VecN[A,N]): VecN[B,N] = {
  v match {
    case VecNil() => VecNil()
    case h :<: t => f(h) :<: mapN(f, t)
  }
}
```

* Type error if we don't return a vector of correct length

# Booleans

```scala
sealed trait Bool {
  type And[T <: Bool] <: Bool
  def and[T <: Bool](b : T): And[T]
}
case object True extends Bool {
  type And[T <: Bool] = T
  def and[T <: Bool](b : T): And[T] = b
}
case object False extends Bool {
  type And[T <: Bool] = False.type
  def and[T <: Bool](b : T): And[T] = False
}
```

# Booleans

```scala
sealed trait SBool {
  type T <: Bool
  def aux: SBoolAux[T]
  def value: T
}
sealed trait SBoolAux[U <: Bool] extends SBool {
  type T = U
}
case class STrue() extends SBoolAux[True.type] {
  def value : True.type = True
  def aux = this
}
case class SFalse() extends SBoolAux[False.type] {
  def value : False.type = False
  def aux = this
}
```

# Booleans

```scala
object SBool {
 type &&[A <: Bool, B <: Bool] = A#And[B]

 def and(b: SBool, c: SBool): b.T && c.T = {
   def andAux[B <: Bool, C <: Bool](b: SBoolAux[B], c: SBoolAux[C]): B && C = b.value and c.value

   andAux(b.aux,c.aux)
 }
}
```

# More complex example

* Dependently typed red-black tree (Typelevel Philadelphia)
* Miles Sabin has a good implementation

# How safe is Scala?

* can always throw, or recurse forever (it's turing complete after all!)
* Is this worth it?
* In some cases yes
* There's always Coq!

# Conclusion

> * Dependent typing is a powerful tool to prove our programs correct
> * Its usable in Scala today (albeit with judgment and hard work)
> * Dotty is coming soon!

# Questions?

* We're hiring!
* nick@nstack.com
* @Nick_enGB
* Code online at https://github.com/NickPollard/scalax-dependent

![](nstack.svg)
