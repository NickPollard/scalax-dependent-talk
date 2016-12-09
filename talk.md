ScalaX 2016 - An introduction to Dependent Typing in Scala
==========================================================

Outline
* Welcome - who I am, what I'm going to talk about
* Dependent Typing
  * What are type systems for
  * Types as proof systems
  * What are dependent types
  * What do dependent types mean?
* Dependent typing in Scala
  * Type members
  * Dependent functions
  * Dependent pairs
  * Other useful application techniques
* Sized Vector example
* 2-3 Tree example
* Summary + QA

* Welcome

  Hi, I'm Nick Pollard and I work at a startup called nstack. I've worked in Scala for almost 4 years. Right now my day job is actually in Haskell rather than Scala but we're hiring great functional programmers of all types, so come and talk to me.

  Today I'm going to talk about Dependent typing; What it is, how it works, and why it matters. By the end of this talk, hopefully you'll understand what Dependent typing is, you'll be aware of some of the techniques to apply it, and you'll have seen some practical examples of what is actually possible with dependent typing. Also you'll hopefully be more interested in it! If I can get that last one, I'll have done my job. Now I am going to talk about a little bit of the theory behind dependent typing, but I'm not going to go into huge detail - it's going to be more of a practical introduction rather than an airtight mathematical explanation. If you are interested in knowing more, there is a lot of great literature out for you to dig in for a fuller and more precise explanation.

* What is Dependent typing?

  At it's heart, dependent typing is an extension to previous type systems. Type systems with dependent typing can express more complex types - types that would not otherwise be possible to write. This means you can type *more* programs - or type programs *more specifically* - than using non-dependent type systems.

  Now why is this important? To answer that, we need to talk more about type systems.

* What are type systems?

  Let's go straight to the source. Benjamin Pierce wrote in his seminal work 'Types and Programming Languages':

  > "A type system is a tractable syntactic method for proving the absence of certain program behaviors by classifying phrases according to the kinds of values they compute." - *Pierce, Types and Programming Languages*

  Now let's unpack that for a moment. The key point is, I think, 'proving the absence of certain behaviors'. Now most of the time we would think of these as run-time errors. We're proving at compile time that the program won't fail (in certain ways) when we run it. We're achieving this by promising the compiler we'll give it certain kinds of data at certain points in the program. More generally, we're proving to the compiler that certain *properties* hold at those points.

Now these properties don't have just be about the actual data formats in memory. A type system like Scala's is more powerful than many older type systems - we've come a long way since C, and other older mainstream languages. Types don't just mean the difference between Int and Double, between String and List. We might have two equivalent data structures, that are *different* types - because they guarantee different properties.

  Take the example of a NonEmptyList

  ```scala
  case class NonEmptyList[T](head: T, tail: List[T])

  case class ::[T](head: T, tail: List[T]) extends List[T]
  ```

  Now hopefully this is something most of you have seen before. If not, a NonEmptyList is just a list that - as it's name suggests - is not empty: it contains at least one item. This can be encoded as a pair of an item plus a standard list of the remaining items. Interestingly, this is exactly the same as the standard cons cell used to implement a normal List. This is interesting because it means a list of one item and a NonEmptyList of one item, are exactly the same in memory (modulo runtime class info, since this is Java land)

  However, although they are the same in memory, we can do different things with them in our program. Looking at these two functions, from List T and NonEmptyList T respectively, we'd like to return a T. With a standard list this function might throw, as we have to handle the case where the list is empty. With a NonEmptyList, this is entirely safe - we know for sure that we have a T we can provide. By using a richer type, we have added safety to our program.

  ```scala
  def unsafe[T](ts: List[T]): T

  def safe[T](ts: NonEmptyList[T]): T
  ```

  This kind of reasoning leads to us being able to think about Types in a more general way. Types are not just sets of possible forms of data; They're propositions about that data (and about your program)

  This leads us to one of the most famous theorems in Computer science, the Curry-Howard correspondence (or -Lambek or -de Bruijn, depending on which way the wind blows)

  They showed that there is a correspondence between programs and logic.

  * Types are propositions - a statement about the universe
  * Values are proofs - a value of a given type is proof that the statement is true
  * Functions are implications
  * Use types to declare statements you wish to be true
    * Argument types - preconditions
    * Return value - postconditions
  * the typechecker will check your logic
  * See: Using implicit lookup to have the compiler derive proof via induction

  What is a Dependent Type?
    * A *type* that depends upon a *value*

      There you go, that's it. You can all go home now! Ok fine, but what does that mean?

      The type will be different for different values of the dependency. Since we don't know until runtime the value, we don't know the type until runtime either. But we *do* know the relation holds.

      There are two forms of dependent types - Dependent functions and Dependent pairs.

    * Dependent function - a function whose return type depends on its argument value
      * Giving different input values (of the *same* type - not parametrically) mean that the result is of different type
      * e.g. return value is the same length as the input value

      ```scala
      trait Foo {
        type T
      }

      def f(foo: Foo): Foo.T
      ```

    * Dependent pair - Second type depends on first value
      * e.g. pair where second number is greater than the first
      * (a:Int, b:IntGreaterThanA)

      ```scala
      trait {
        val foo: Foo
        val bar: foo.T
      }
      ```

    * How can types depend on values?

      At this point you might be wondering how a type can depend on a value, when a type is a compile-time construct and we may not know a value until runtime, when types have been erased.
      Well, although we can't know know the value yet, we can talk in terms of relations to that value. Think abstracting over a variable in algebra - or in a function. When we write a function we don't know what the input argument will be, but we can still talk at a value level about manipulating it.

      * At compile time we don't know what that value is
      * But we can talk about the relationship between that value and the type
      * This property can the be reasoned about algebraically
        e.g. function that returns a number greater than the input

      ```scala
      def inc(i: Int): Int = i + 1

      trait Integer { type Inc <: Integer }
      def inc(i: Integer): i.Inc
      ```

      * People tend to think of types existing only at compile time, statically,
      * Values exist at runtime, dynamically
      * Two distinct universes
      * Actually they interact in various ways

      * There are 4-different ways of indexing types and values
        * function: Value -> Value
        * type function: Type -> Type
        * implicits, parametric polymorphism: Type -> Value
        * dependent types: Value -> Type

  * What do dependent types mean?
    * logical quantification
    * Dependent functions -> forall
    * Dependent pairs -> there exists

  * Dependent Types encode properties that are dependent on value
    * Useful for enforcing algorithmic invariants
    * Encoding equational reasoning
    * Breaking down the barriers between static/dynamic, compile time/runtime, types/values

* Dependent typing in Scala

  Given what we've discussed, how do we actually implement this in Scala?

  * Type members

  Dependent typing in Scala is accomplished primarily through type members. Type members allow us to define *Path-dependent types* - that is, types that depend on the path of selectors to them. This is a slightly more limited form of dependent typing than that found in more complex lanuages (Agda, Idris), but it's still quite powerful.

    ```scala
    trait Foo {
      type T
    }

    val foo : Foo
    val f : foo.T
    ```

  To make practical use of Dependent types, and to actually do something with these type members, we need to lean on a lot of other type-level techniques in Scala, many of which you might already be familiar with. I'm going to go over some of these here as a brief introduction or refresher.

  Parametric Types
  Implicits
  Type Classes
  Type projection - referring to an inner type from a static outer type

    ```scala
    val ff : Foo#T
    ```

  (Note the important difference between `Foo#T` and `foo.T`)

  * Existential types
    * Sometimes we don't want to be constrained by the inner type, sometimes we do.
    * Convert between existential and parameterized
    * Aux pattern

    ```scala
    type FooAux[U] = Foo { type T = U }

    ```
    * with Aux we lift up the type member to a type parameter, allowing us to talk about it more
    

Although this type member gives us access, sometimes we want to reify this as a type parameter so we can more easily talk about it and fix to the compiler the value of this in a given function. The Aux pattern allows us to do this. In many scala typelevel libraries a type alias is used; Here I'm using a trait so that I can easily prove to the compiler that this property holds for all children.

I also add a function to lift all Vec[T] into VecAux - all VecAuxs are of course Vec[T]s already. This allows me to decide when I want to talk about the parameter, and when I'd rather ignore it




  * Type functions ?
* Practical example - fixed-size lists
  * Peano Naturals

    ```scala
    sealed trait Nat
    case object Z extends Nat
    case class S[N <: Nat] extends Nat
    ```

    ```scala
    object Nat {
      type _0 = Z
      type _1 = S[_0]
      type _2 = S[_1]
      type _3 = S[_2]
    }
    ```

  * Vector class

    Now we can use the Nats as a type to index a Vector type

    ```scala
    sealed trait Vec[T] {
      type L <: Nat
    }
    ```

    We want to be able to reify this as an Aux

    ```scala
    sealed trait VecAux[T,N <: Nat] extends Vec[T] {
      type L = N
    }
    ```

    Implementations, allowing lift to Aux

    ```scala
    sealed trait Vec[T] {
      type L <: Nat
      def aux : VecAux[T,L]
    }

    case class Nil[T] extends VecAux[T,Z]
    case class ::[T, N](h: T, t: VecAux[T,N]) extends VecAux[T,S[N]]
    ```

Now we're not just saying we're returning a vector of Bs, but also that it must be the same size as the input vector. No more just writing nil!

* Conclusion

  * Dependent typing is a powerful tool to let us check and prove our programs correct
  * Its usable in Scala today (albeit with judgment and hard work)
  * Dotty is coming soon!

Thanks for your time. Once again I've been Nick Pollard (@nick_enGb). All of these slides, as well as the accompanying code and examples are on github: (github.com/NickPollard). If you're interested in Dependent Typing and you're willing to learn Haskell, nstack is hiring! Come and grab me later.










*In particular, type-checking ensures that certain *properties* are met by the program. In order to type-check, we have to prove to the compiler that we've done things correctly. Then, the compiler, *knowing* that we've done things correctly, can make inferences based on that.*

  *Error-checking - types allow the compiler (or rather the type-checker) to check that we've written things correctly. It also allows us to guide the copmiler in generating code based on those types.*
