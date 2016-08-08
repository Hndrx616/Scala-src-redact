#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Classes and Objects*/
class Rational(n: Int, d: Int) extends AnyRef{
	var x: AnyRef = new Rational(1, 2)
	def gcd(x: Int, y: Int): Int = {
		if (x == 0) y
		else if (x < 0) {gcd(-x, y)}
		else if (y < 0) {-gcd(x, -y)}
		else {gcd(y % x, x)}
	}
	val g = gcd(n, d)
	val numer: Int = n/g
	val denom: Int = d/g
	def +(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
	def -(that: Rational) = new Rational(numer * that.denom - that.numer * denom, denom * that.denom)
	def *(that: Rational) = new Rational(numer * that.numer, denom * that.denom)
	def /(that: Rational) = new Rational(numer * that.denom, denom * that.numer)
	override def toString = "" + numer + "/" + denom
	def square = new Rational(numer*numer, denom*denom)
}
class CreateObject {
	var i = 1
	var x = new Rational(0, 1)
	while (i <= 10) {
	x += new Rational(1, i)
	i += 1
	}
	def toString: String =""
	println("" + x.numer + "/" + x.denom)
}
abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
}
trait IntSet {
def incl(x: Int): IntSet
def contains(x: Int): Boolean
}
class EmptySet extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
	def excl(x: Int)
	def isEmpty: Boolean
	val EmptySetVal = new EmptySet
	object EmptySet extends IntSet {
		def contains(x: Int): Boolean = false
		def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)
	}
}
class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x: Int): Boolean = {
		if (x < elem) left contains x
		else if (x > elem) {right contains x}
		else {true}
	}
	def incl(x: Int): IntSet = {
		if (x < elem) new NonEmptySet(elem, left incl x, right)
		else if (x > elem) {new NonEmptySet(elem, left, right incl x)}
		else{this}
	}
	def excl(x: Int)
	def isEmpty: Boolean
}
class Standard_Classes() {
	type boolean = scala.Boolean
	type int = scala.Int
	type long = scala.Long
}
abstract class Boolean {
	def ifThenElse(thenpart: => Boolean, elsepart: => Boolean)
	def && (x: => Boolean): Boolean = ifThenElse(x, false)
	def || (x: => Boolean): Boolean = ifThenElse(true, x)
	def ! : Boolean = ifThenElse(false, true)
	def == (x: Boolean) : Boolean = ifThenElse(x, x.!)
	def != (x: Boolean) : Boolean = ifThenElse(x.!, x)
	def < (x: Boolean) : Boolean = ifThenElse(false, x)
	def > (x: Boolean) : Boolean = ifThenElse(x.!, false)
	def <= (x: Boolean) : Boolean = ifThenElse(x, true)
	def >= (x: Boolean) : Boolean = ifThenElse(true, x.!)
}
case object True extends Boolean {
def ifThenElse(t: => Boolean, e: => Boolean) = t
}
case object False extends Boolean {
def ifThenElse(t: => Boolean, e: => Boolean) = e
}
abstract class Int extends AnyVal {
	def toLong: Long
	def toFloat: Float
	def toDouble: Double
	def + (that: Double): Double
	def + (that: Float): Float
	def + (that: Long): Long
	def + (that: Int): Int
	def << (cnt: Int): Int
	def & (that: Long): Long
	def & (that: Int): Int
	def == (that: Double): Boolean
	def == (that: Float): Boolean
	def == (that: Long): Boolean
}
abstract class Nat {
	def isZero: Boolean
	def predecessor: Nat
	def successor: Nat
	def + (that: Nat): Nat
	def - (that: Nat): Nat
	object Zero extends Nat {
		def isZero: Boolean = true
		def predecessor: Nat = error("negative number")
		def successor: Nat = new Succ(Zero)
		def + (that: Nat): Nat = that
		def - (that: Nat): Nat = if (that.isZero) Zero
		else {error("negative number")}
	}
}
class Succ(x: Nat) extends Nat {
    def isZero: Boolean = false
    def predecessor: Nat = x
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat = x + that.successor
    def - (that: Nat): Nat = if (that.isZero) this
    else {x - that.predecessor}
}
abstract class Expr {
    case class Number(n: Int) extends Expr {
        def Number(n: Int) = new Number(n)
        def n: Int
    }
    case class Sum(e1: Expr, e2: Expr) extends Expr {
        def Sum(e1: Expr, e2: Expr) = new Sum(e1, e2)
        def e1: Expr = e2: Expr
    }
    def isNumber: Boolean
    def isSum: Boolean
    def numValue: Int
    def leftOp: Expr
    def rightOp: Expr
    def eval: Int = this match 
    {
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
    }
    def print
}
class Number(n: Int) extends Expr {
	def isNumber: Boolean = true
	def isSum: Boolean = false
	def numValue: Int = n
	def leftOp: Expr = error("Number.leftOp")
	def rightOp: Expr = error("Number.rightOp")
	def eval: Int = n
	def print { Console.print(n) }
}
class Sum(e1: Expr, e2: Expr) extends Expr {
	def isNumber: Boolean = false
	def isSum: Boolean = true
	def numValue: Int = error("Sum.numValue")
	def leftOp: Expr = e1
	def rightOp: Expr = e2
	def eval: Int = e1.eval + e2.eval
	def print {Console.print("(") print(e1) Console.print("+") print(e2) Console.print(")")}
}
class Prod(e1: Expr, e2: Expr) extends Expr {
	def eval: Int = e1.eval * e2.eval
}
abstract class IntStack {
	def push(x: Int): IntStack = new IntNonEmptyStack(x, this)
	def isEmpty: Boolean
	def top: Int
	def pop: IntStack
}
class IntEmptyStack extends IntStack {
	def isEmpty = true
	def top = error("EmptyStack.top")
	def pop = error("EmptyStack.pop")
}
class IntNonEmptyStack(elem: Int, rest: IntStack) extends IntStack {
	def isEmpty = false
	def top = elem
	def pop = rest
}
class Array[A] {
	def apply(index: Int): A
	def update(index: Int, elem: A)
	val x = new Array[String](1)
	val y: Array[Any] = x
	y(0) = new Rational(1, 2)
}
abstract class Stack[A] {
	def push(x: A): Stack[A] = new NonEmptyStack[A](x, this)
	def isEmpty: Boolean
	def top: A
	def pop: Stack[A]
	val x = new EmptyStack[Int]
	val y = x.push(1).push(2)
	println(y.pop.top)
	def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
		p.isEmpty ||
		p.top == s.top && isPrefix[A](p.pop, s.pop)
	}
	val s1 = new EmptyStack[String].push("abc")
	val s2 = new EmptyStack[String].push("abx").push(s1.top)
	println(isPrefix[String](s1, s2))	
}
class EmptyStack[A] extends Stack[A] {
	def isEmpty = true
	def top = error("EmptyStack.top")
	def pop = error("EmptyStack.pop")
}
class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
	def isEmpty = false
	def top = elem
	def pop = rest
}
abstract class Stack[+A] {
	def push[B >: A](x: B): Stack[B] = new NonEmptyStack(x, this)
	def isEmpty: Boolean
	def top: A
	def pop: Stack[A]
}
object EmptyStack extends Stack[Nothing] {
	def isEmpty = true
	def top = error("EmptyStack.top")
	def pop = error("EmptyStack.pop")
}
class NonEmptyStack[+A](elem: A, rest: Stack[A]) extends Stack[A] {
	def isEmpty = false
	def top = elem
	def pop = rest
}
abstract class Set[A] {
	def incl(x: A): Set[A]
	def contains(x: A): Boolean
	def contains(x: Int): Boolean =
		if (x < elem) {left contains x}
	trait Ordered[A] {
		def compare(that: A): Int
		def < (that: A): Boolean = (this compare that) < 0
		def > (that: A): Boolean = (this compare that) > 0
		def <= (that: A): Boolean = (this compare that) <= 0
		def >= (that: A): Boolean = (this compare that) >= 0
		def compareTo(that: A): Int = compare(that)
	}
	case class Num(value: Double) extends Ordered[Num] {
		def compare(that: Num): Int =
			if (this.value < that.value) {-1}
			else if (this.value > that.value) {1}
			else {0}
		}
	val s = new EmptySet[Num].incl(Num(1.0)).incl(Num(2.0))
	val s = new EmptySet[java.io.File]
	trait Set[A <: Ordered[A]] {
		def incl(x: A): Set[A]
		def contains(x: A): Boolean
	}
}
class EmptySet[A <: Ordered[A]] extends Set[A] {
	def contains(x: A): Boolean = false
	def incl(x: A): Set[A] = new NonEmptySet(x, new EmptySet[A], new EmptySet[A])
}
class NonEmptySet[A <: Ordered[A]]
	(elem: A, left: Set[A], right: Set[A]) extends Set[A] {
	def contains(x: A): Boolean =
		if (x < elem) {left contains x}
		else if (x > elem) {right contains x}
		else {true}
	def incl(x: A): Set[A] = 
		if (x < elem) {new NonEmptySet(elem, left incl x, right)}
		else if (x > elem) {new NonEmptySet(elem, left, right incl x)}
		else {this}
}
class TwoInts(first: Int, second: Int) {
    def divmod(x: Int, y: Int): TwoInts = new TwoInts(x / y, x % y)
    case class Tuple2[A, B](_1: A, _2: B) {
        def divmod(x: Int, y: Int) = new Tuple2[Int, Int](x / y, x % y)
    }
    def divmod(x, y) = match {
        case Tuple2(n, d) =>
        println("quotient: " + n + ", rest: " + d)
    }
val xy = divmod(x, y)
println("quotient: " + xy._1 + ", rest: " + xy._2)
}