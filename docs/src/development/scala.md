
# Scala Programming Advice

- Tip: Reasonable Scala coding advice [https://twitter.github.io/effectivescala/](https://twitter.github.io/effectivescala/)
- Tip: You can bind names to destructuring pattern matches using the undocumented `@` syntax: 
  ```scala
  case class Person(name: String, age: Int)
  val b = Person("X", 200)
  b match {
    case p @ Person(_, age) => println(s"Person ${p.name} age $age")
  }
  ```
- Tip: Want to define a trait's method which returns a value of the type of the subclass implementing the trait?
    Simply add a type parameter constrained to subtype traits type paramaterised by itself.

  ```scala
  trait IntrusiveListElement[T <: IntrusiveListElement[T]]:
    def succ() : T
    def pred() : T
  ```

## Scala Gotchas

Below is an unorganised list of common pitfalls encountered when programming Scala.

- Statement lists are newline-separated sequences of expressions, which simply evaluate to the last expression in the list. It is easy to accidentally silently 
  throw away a computed value.
  - Throwing away values in the middle of the list isn't a warning by default, but can be made one with `-Wnonunit-statement`. 
    Unfortunately there is no standard library `ignore: _ => Unit` function, and the design of `scala.collections` in always 
    returning non-unit, even purely side-effecting operations makes enabling this warning require somewhat more convoluted code.
- Scala contains syntax to encourage procedural programming; if/else statements, for, while statements; 
  the fact this is implemented through complex syntax sugar on top of functional constructs makes them surprising to both procedural and functional programmers, 
  and best avoided. 
  - You cannot break from a loop because loops are syntax sugar for maps, unless you wrap the loop in a breakable monad. 
    Therefore, break is a regular function returning breakable monad (or something like that).
    - [Nonlocal returns in Scala Reference](https://docs.scala-lang.org/scala3/reference/dropped-features/nonlocal-returns.html)
    - [Scala API docs for boundary break](https://dotty.epfl.ch/api/scala/util/boundary$.html)
  - Similarly, the syntax sugar for `do` notion makes `do...while` loops impossible, 
    Scala instead recommends writing the whole loop in the condition of a regular `while` loop
    - [Scala language reference do-while](https://docs.scala-lang.org/scala3/reference/dropped-features/do-while.html)
- There are at least two ways to define an ADT (Algebraic Data Type), using an `enum` or `trait` and `case class`es. By default `enum`s make you to prefix every case with 
  the enum name, to avoid this you can use `export` to add them to enclosing scope. Scala's case classes are overloaded, they are both the standard record type if defined without parent trait, and the constructor for ADT variants. 
  - Note that `trait`/`case class`-based ADTs are open by default hence don't enable case-exhaustion checking on pattern matches (instead default to `sealed trait`)
  ```scala
  // enum style

  enum Error(r: String):
    case Permission extends Error("permission")
    case NotExists extends Error("not existing")
  export Error._ // adds Permission and Exists to the enclosing scope

  val x = Error.Permission

  // trait style

  sealed trait Error(val r : String)

  case class Permission extends Error("permission")
  case class NotExists extends Error("not existing")

  val x = Permission
  ```

- `class` field and method definitions have no declaration order, but are apparently initialised in textual definition order.
  This means you implicilty get null if you reference a member before is initialised.
  This does not produce any compiler errors or warnings, even with experimental type-based null-checking `-Yexplicit-nulls`. 
  A (different) runtime error also occurs when defining the below with `case class X`.
  - https://godbolt.org/z/Y8da1d9ch

  ```scala
  class Num(val x: Int)
  class X() {
    val x = y.x + 1 // nullpointer exception because y not initialised yet, no compile error or warning
    val y = Num(1)
  }
  ```

- The Scala compiler loves to counter-intuitively infer union types, meaning it doesn't catch (usually) incorrect code that returns different types on different paths.
  For this reason at least, always define return types. 

  ```scala
  def a(c: Boolean) = if c then 1 else "yeah"
  def a(c: Boolean): Int | String // inferred
  ```

- `case class` parameters are fields (implicitly `val` declarations), not constructor parameters. Therefore 
  defining a parent trait's field parameters requires `override`ing the parent field name to avoid declaring a duplicate field
  - https://godbolt.org/z/MKeseGqev

  ```scala
  sealed trait X(val v: Int)

  // A only defines the one field v in A. (correct approach)
  case class A(override val v: Int) extends X(v)
  // C is equivalent to B, and both define a class with both fields y and v.
  case class B(y: Int) extends X(y)      // implicitly define extra field B.y
  case class C(val y: Int) extends X(y)  // explicitly define extra field C.y 
  case class E(v: Int) extends X(v) // compile error due to missing override
  ```

  This is in contrast to regular classes, where class parameters are merely constructor parameters. 
  - https://godbolt.org/z/sYMdKs3Eq

  ```scala
  sealed trait X(val v: Int)

  // A and B both only define a single field named `x`
  class A(a: Int) extends X(a) // (correct approach) defines X.v
  class B(override val v: Int) extends X(v) // defines B.v overriding X.v
  class C(v: Int) extends X(v)     // also compiles due to `v` parameter shadowing
  class E(val v: Int) extends X(v) // compile error due to missing override modifier 
  ```

- `case class`es are accessible like tuples, the below is valid code (never write this)
  ```scala
  case class X(a: Int, b: Int)
  val x = X(1,2)
  x._1 == x.a
  ```
- `extension`, makes method provenance even more unclear (to humans and compilers) on top of Scala's abuse of multiple trait inheritance
  ```scala
  extension (x: (A, B))`
    def add(y: (A,B)) = ...
  ```
  Adds the method `add(y: (A,B))` to the type `(A,B)`
- special syntax for partially-appliable functions, e.g. for a function `func` to require 3 separate calls to finally return an `Int`:
  ```scala
  def func(x: Int)(y: Int, z: Int)(a: String) : Int = {...}
  ```
- Partial function syntax, which looks like all the other syntax for method and expresison list definitions, 
  but whose type cannot be inferred. 
  ```scala
  val xf : PartialFunction[Int | String, Int] {
    case x: Int => x + 1
  }
  scala> List(1,2,"3", "4")
  val res0: List[Int | String] = List(1, 2, 3, 4)
                                                                                                            
  scala> List(1,2,"3", "4").collect(x)
  val res1: List[Int] = List(2, 3)
  scala> List(1,2,"3", "4").collect { // syntax sugar for defining inline partial function eliding ()
     |   case x: Int => x
     | }
  // regular named definitions are not partial functions
  scala> def pf (x: Int | String) = x match {
     |  case x: Int => x 
     | } // error on String case
  ```
  - These are also implicitly convertible to functions returning `Option[T]`.
- type aliases are opaque, for less opaque type aliases use subtypes or upper type constraints
  - `trait X extends Y` is a subtype vs `type X = Y` which is an opaque type alias
  - _Do not_ use opaque type definitions unless the type is purely handled internally to the defining class, where you know the type. If it is ever 
    returned to outside the class, do not define a type alias. You should not return opaquely typed values outside the class since it requires an unsafe 
    cast to get back a type you can do things with.
- Dependent methods considered harmful. 
  - [What are they?](https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types.html)
  - Relying on opaque type definitions makes them generally not useful.
    Opaque type definitions obscure the actual type requiring it to be totally contained behind 
    some interface from which the type definition does not escape.
  - Furthermore, the Scala compiler is bad at proving equality of dependent types. For example using a 
    pattern match to prove x: X actually has subtype Y, loses equality of the dependent type 
    X.st which may be defined in both types X and Y.  
  - Structural equality of values is generally not sufficient for equality of the types dependent on them.
