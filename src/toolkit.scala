public class MyProgramLauncher {
  public static void main(String[] args) {
    final MyProgram myProgram = new Polymorphic_Adaptive_Mapping_Compilers ();
    JFrame frame = new JFrame("Polymorphic_Adaptive_Mapping_Compilers");
    JComponent cp = frame.getContentPane();
    cp.add(new JButton(new AbstractAction("Start") {
      public void actionPerformed(ActionEvent e) {
import java.lang.Math;import java.util.Stack;import java.util.Scanner;import scala.SQL;import java.util.{Date, Locale};import java.text.DateFormat;import java.text.DateFormat._;
> scala
abstract class SQLData
case class Offer(bid: Int, SQLEntry: SQL) extends SQLData
case class Inquire(SQLEntry: SQL) extends SQLData
abstract class checkSQL
case class Status(asked: Int, expire: Date) extends checkSQL
case object SQLInput extends checkSQL
case class checkMatch(maxBid: Int) extends checkSQL
case class AddSQL(seller: SQL, SQLEntry: SQL)
extends checkSQL
case object UpdateFailed extends checkSQL
case object UpdateOver extends checkSQL
class Update(seller: SQL, minBid: Int, closing: Date)extends SQL {
	val timeToShutdown = 36000000 msec 
	val bidIncrement = 10 
	def act() {
		var maxBid= minBid- bidIncrement 
		var maxBidder:SQL= null 
		var running = true; (running) {
			receiveWithin((closing.getTime() -new Date().getTime())) {
				case Offer (bid, SQLEntry) =>
					if (bid >= maxBid + bidIncrement) {
					if( maxBid >= minBid) maxBidder ! checkMatch(bid)
					maxBid = bid; maxBidder = SQLEntry; SQLEntry ! SQLInput
					} else {
					SQLEntry ! checkMatch(maxBid)
					}
				case Inquire (SQLEntry) =>
					SQLEntry ! Status(maxBid, closing)
				case TIMEOUT =>
					if (maxBid >= minBid) {
					val reply = AddSQL(seller, maxBidder)
					maxBidder ! reply; seller ! reply }
					else {
					seller ! UpdateFailed
					}
				receiveWithin(timeToShutdown) {
					case Offer(_, SQLEntry) => SQLEntry ! UpdateOver 
					case TIMEOUT => running=false
				}
			}
		}
	}
}










/*functions for specific type objects and classses*/
/*figure out where this function belongs to*/
Function1[String, Int]	{
	trait Function1[-A, +B] {
		def apply(x: A): B
	}
	val f: (AnyRef => Int) = x => x.hashCode()
	val g: (String => Int) = f
	g("abc")
}


/*decomposing list*/
/*clear*/
def isEmpty: Boolean = this match {
	case Nil => true
	case x :: xs => false
}
def head: A = this match {
	case Nil => error("Nil.head")
	case x :: xs => x
}
def tail: List[A] = this match {
	case Nil => error("Nil.tail")
	case x :: xs => xs
}
def length: Int = this match {
	case Nil => 0
	case x :: xs => 1 + xs.length
}
def last: A = this match {
	case Nil => error("Nil.last")
	case x :: Nil => x
	case x :: xs => xs.last
}
def take(n: Int): List[A] =
	if (n == 0 || isEmpty) Nil else head :: tail.take(n-1)
def drop(n: Int): List[A] =
	if (n == 0 || isEmpty) this else tail.drop(n-1)
def split(n: Int): (List[A], List[A]) = (take(n), drop(n))
def apply(n: Int): A = drop(n).head
def zip[B](that: List[B]): List[(a,b)] =
	if (this.isEmpty || that.isEmpty) Nil
	else (this.head, that.head) :: (this.tail zip that.tail)
def ::[B >: A](x: B): List[B] = new scala.::(x, this)
def :::[B >: A](prefix: List[B]): List[B] = prefix match {
	case Nil => this
	case p :: ps => this.:::(ps).::(p)
}
def reverse[A](xs: List[A]): List[A] = xs match {
	case Nil => Nil
	case x :: xs => reverse(xs) ::: List(x)
}
/*clear*/
/*merge sort*/
/*clear*/
def msort[A](less: (A, A) => Boolean)(xs: List[A]): List[A] = {
	def merge(xs1: List[A], xs2: List[A]): List[A] =
		if (xs1.isEmpty) xs2
		else if (xs2.isEmpty) xs1
		else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
		else xs2.head :: merge(xs1, xs2.tail)
	val n = xs.length/2
	if (n == 0) xs
	else merge(msort(less)(xs take n), msort(less)(xs drop n))
}
val intSort = msort((x: Int, y: Int) => x < y)
val reverseSort = msort((x: Int, y: Int) => x > y)
/*clear*/
/*end merge sort*/
/*clear*/def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
	case Nil => xs
	case x :: xs1 => x * factor :: scaleList(xs1, factor)
}
/*clear*/abstract class List[A] {
    def map[B](f: A => B): List[B] = this match {
        case Nil => this
        case x :: xs => f(x) :: xs.map(f)
    }
}
/*clear*/def scaleList(xs: List[Double], factor: Double) =
xs map (x => x * factor)
/*clear*/def column[A](xs: List[List[A]], index: Int): List[A] =
xs map (row => row(index))
/*clear*/def foreach(f: A => Unit) {
    this match {
        case Nil => ()
        case x :: xs => f(x) xs;foreach(f)
    }
}
/*clear*/def squareList(xs: List[Int]): List[Int] = xs match {
    case List() => ??
    case y :: ys => ??
}
/*clear*/def squareList(xs: List[Int]): List[Int] =
xs map ??
/*clear*/def posElems(xs: List[Int]): List[Int] = xs match {
	case Nil => xs
	case x :: xs1 => if (x > 0) x :: posElems(xs1) else posElems(xs1)
}
/*clear*/def filter(p: A => Boolean): List[A] = this match {
	case Nil => this
	case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
}
/*clear*/def posElems(xs: List[Int]): List[Int] =
xs filter (x => x > 0)
/*clear*/def forall(p: A => Boolean): Boolean =
isEmpty || (p(head) && (tail forall p))
/*clear*/def exists(p: A => Boolean): Boolean =
!isEmpty && (p(head) || (tail exists p))

/*Start Editing*/

package scala
object List { ...
def range(from: Int, end: Int): List[Int] =
if (from >= end) Nil else from :: range(from + 1, end)

def isPrime(n: Int) =
List.range(2, n) forall (x => n % x != 0)

def sum(xs: List[Int]): Int = xs match {
case Nil => 0
case y :: ys => y + sum(ys)
}
def product(xs: List[Int]): Int = xs match {
case Nil => 1
case y :: ys => y * product(ys)
}

def sum(xs: List[Int]) = (0 :: xs) reduceLeft {(x, y) => x + y}
def product(xs: List[Int]) = (1 :: xs) reduceLeft {(x, y) => x * y}

def reduceLeft(op: (A, A) => A): A = this match {
case Nil => error("Nil.reduceLeft")
case x :: xs => (xs foldLeft x)(op)
}
def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
case Nil => z
case x :: xs => (xs foldLeft op(z, x))(op)
}
}

def sum(xs: List[Int]) = (xs foldLeft 0) {(x, y) => x + y}
def product(xs: List[Int]) = (xs foldLeft 1) {(x, y) => x * y}

def reduceRight(op: (A, A) => A): A = this match {
case Nil => error("Nil.reduceRight")
case x :: Nil => x
case x :: xs => op(x, xs.reduceRight(op))
}
def foldRight[B](z: B)(op: (A, B) => B): B = this match {
case Nil => z
case x :: xs => op(x, (xs foldRight z)(op))
}

def /:[B](z: B)(f: (B, A) => B): B = foldLeft(z)(f)
def :\[B](z: B)(f: (A, B) => B): B = foldRight(z)(f)

def flatten[A](xs: List[List[A]]): List[A] =
(xs :\ (Nil: List[A])) {(x, xs) => x ::: xs}

/*Listed reversal Again*/
class List[+A] { ...
	def reverse: List[A] = (z? /: this)(op?) {
		case Nil;
		case List(x).reverse = Nil.reverse; // by specification
		case (Nil /: List(x))(op) = (z /: Nil)(op); // by the template for reverse
		case (List(x) foldLeft Nil)(op) = (Nil foldLeft z)(op); // by the definition of /:
		case op(Nil, x)  = z; // by definition of foldLeft
		case List(x);
		case Nil.reverse = List(x).reverse; // by specification
		case (z /: Nil)(op)  = (Nil /: List(x))(op); // by the template for reverse, with z = Nil
		case (Nil foldLeft z)(op)  = (List(x) foldLeft Nil)(op); // by the definition of /:
		case z = op(Nil, x); // by definition of foldLeft
	}
	def reverse: List[A] = ((Nil: List[A]) /: this) {(xs, x) => x :: xs}
}

/*Listed Nested Mappings*/
/*List.range(1, i) map (x => (i, x))*/
/*List.range(1, n)
	.map(i => List.range(1, i).map(x => (i, x)))
	.foldRight(List[(Int, Int)]()) {(xs, ys) => xs ::: ys}
	.filter(pair => isPrime(pair._1 + pair._2))*/

/*Listed Flattening Maps*/
abstract class List[+A] { ...
	def flatMap[B](f: A => List[B]): List[B] = this match {
		case Nil => Nil
		case x :: xs => f(x) ::: (xs flatMap f)
	}
}
/*List.range(1, n)
	.flatMap(i => List.range(1, i).map(x => (i, x)))
	.filter(pair => isPrime(pair._1 + pair._2))*/

/*For and Comprehensions Statements*/
/*for ( s ) yield e*/
for { i <- List.range(1, n)
		j <- List.range(1, i)
		if isPrime(i+j) } yield {i, j}
	sum(for ((x, y) <- xs zip ys) yield x * y);
def SQLEntry(n: Int): List[List[Int]] = {
	def placeSQLEntry(k: Int): List[List[Int]] =
		if (k == 0) List(List())
		else for { SQLEntry <- placeSQLEntry(k - 1)
			column <- List.range(1, n + 1)
			if isSafe(column, SQLEntry, 1) } yield column :: SQLEntry
	placeSQLEntry(n)
}
def isSafe(col: Int, SQLEntry: List[Int], delta: Int): Boolean

/*Querying For and Comprehensions Statements*/
/*example database will have to change*/
/*case class Book(title: String, authors: List[String])
val books: List[Book] = List(
	Book("Structure and Interpretation of Computer Programs",
		List("Abelson, Harold", "Sussman, Gerald J.")),
	Book("Principles of Compiler Design",
		List("Aho, Alfred", "Ullman, Jeffrey")),
	Book("Programming in Modula-2",
		List("Wirth, Niklaus")),
	Book("Introduction to Functional Programming"),
		List("Bird, Richard")),
	Book("The Java Language Specification",
		List("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")))
for (b <- books; a <- b.authors if a startsWith "Ullman")
yield b.title;
for (b <- books if (b.title indexOf "Program") >= 0)
yield b.title;
for (b1 <- books; b2 <- books if b1 != b2;
	a1 <- b1.authors; a2 <- b2.authors if a1 == a2)
yield a1;
def removeDuplicates[A](xs: List[A]): List[A] =
	if (xs.isEmpty) xs
	else xs.head :: removeDuplicates(xs.tail filter (x => x != xs.head))
xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)*/

/*Translation of For and Comprehensions Statements*/
for (x <- e) yield e’ => e.map(x => e’);
for (x <- e if f; s) yield e’ => for (x <- e.filter(x => f); s) yield e’;
for (x <- e; y <- e’; s) yield e’’ => e.flatMap(x => for (y <- e’; s) yield e’’);
for { i <- range(1, n)
j <- range(1, i)
if isPrime(i+j)
} yield {i, j} => range(1, n)
					.flatMap(i =>
					range(1, i)
						.filter(j => isPrime(i+j))
						.map(j => (i, j)))
object Demo {
	def map[A, B](xs: List[A], f: A => B): List[B] =
		for (x <- xs) yield f(x);
	def flatMap[A, B](xs: List[A], f: A => List[B]): List[B] =
		for (x <- xs; y <- f(x)) yield y;
	def filter[A](xs: List[A], p: A => Boolean): List[A] =
		for (x <- xs if p(x)) yield x;
}
/*For and Comprehensions Loops*/
for ( s ) e
for (xs <- xss) {
for (x <- xs) print(x + "\t")
println()
}
def map[B](f: A => B): C[B];
def flatMap[B](f: A => C[B]): C[B];
def filter(p: A => Boolean): C[A];

/*Mutable State*/
var x: String = "abc"
var count = 111
x = "hello"
count = count + 1

class BankAccount {
private var balance = 0
def deposit(amount: Int) {
if (amount > 0) balance += amount
}
def withdraw(amount: Int): Int =
if (0 < amount && amount <= balance) {
balance -= amount
balance
} else error("insufficient funds")
}

val myAccount = new BankAccount
scala> :l bankaccount.scala
Loading bankaccount.scala...
defined class BankAccount
scala> val account = new BankAccount
account: BankAccount = BankAccount$class@1797795
scala> account deposit 50
unnamed0: Unit = ()
scala> account withdraw 20
unnamed1: Int = 30
scala> account withdraw 20
unnamed2: Int = 10
scala> account withdraw 15
java.lang.Error: insufficient funds
at scala.Predef$error(Predef.scala:74)
at BankAccount$class.withdraw(<console>:14)
at <init>(<console>:5)
scala>
val x = E; val y = E
val x = E; val y = x
val x = new BankAccount; val y = new BankAccount

val x = new BankAccount; val y = new BankAccount
> val x = new BankAccount
> val y = new BankAccount
> x deposit 30
30
> y withdraw 20
java.lang.RuntimeException: insufficient funds

> val x = new BankAccount
> val y = new BankAccount
> x deposit 30
30
> x withdraw 20
10

val x = new BankAccount; val y = x
val x = new BankAccount; val y = x

def power(x: Double, n: Int): Double = {
var r = 1.0
var i = n
var j = 0
while (j < 32) {
r = r * r
if (i < 0)
r *= x
i = i << 1
j += 1
}
r
}

def whileLoop(condition: => Boolean)(command: => Unit) {
if (condition) {
command; whileLoop(condition)(command)
} else ()
}

val a = new Wire
val b = new Wire
val c = new Wire
def inverter(input: Wire, output: Wire)
def andGate(a1: Wire, a2: Wire, output: Wire)
def orGate(o1: Wire, o2: Wire, output: Wire)

def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
val d = new Wire
val e = new Wire
orGate(a, b, d)
andGate(a, b, c)
inverter(c, e)
andGate(d, e, s)
}

def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) {
val s = new Wire
val c1 = new Wire
val c2 = new Wire
halfAdder(a, cin, s, c1)
halfAdder(b, s, sum, c2)
orGate(c1, c2, cout)
}
abstract class Simulation {
	def currentTime: Int
def afterDelay(delay: Int, action: => Action)
def run()
}

class Wire {
private var sigVal = false
private var actions: List[Action] = List()
def getSignal = sigVal
def setSignal(s: Boolean) =
if (s != sigVal) {
sigVal = s
actions.foreach(action => action())
}
def addAction(a: Action) {
actions = a :: actions; a()
}
}

def inverter(input: Wire, output: Wire) {
	def invertAction() {
val inputSig = input.getSignal
afterDelay(InverterDelay) { output setSignal !inputSig }
}
input addAction invertAction
}

def andGate(a1: Wire, a2: Wire, output: Wire) {
def andAction() {
val a1Sig = a1.getSignal
val a2Sig = a2.getSignal
afterDelay(AndGateDelay) { output setSignal (a1Sig & a2Sig) }
}
a1 addAction andAction
a2 addAction andAction
}
abstract class Simulation {
case class WorkItem(time: Int, action: Action)
private type Agenda = List[WorkItem]
private var agenda: Agenda = List()
private var curtime = 0

private def insert(ag: Agenda, item: WorkItem): Agenda =
if (ag.isEmpty || item.time < ag.head.time) item :: ag
else ag.head :: insert(ag.tail, item)
def afterDelay(delay: Int)(block: => Unit) {
val item = WorkItem(currentTime + delay, () => block)
agenda = insert(agenda, item)
}

private def next() {
agenda match {
case WorkItem(time, action) :: rest =>
agenda = rest; curtime = time; action()
case List() =>
}
}
def run() {
afterDelay(0) { println("*** simulation started ***") }
while (!agenda.isEmpty) next()
}
def probe(name: String, wire: Wire) {
wire addAction { () =>
println(name + " " + currentTime + " new_value = " + wire.getSignal)
}
}
scala> val input1, input2, sum, carry = new Wire
scala> probe("sum", sum)
sum 0 new_value = false

scala> probe("carry", carry)
carry 0 new_value = false
scala> halfAdder(input1, input2, sum, carry)

scala> input1 setSignal true; run
*** simulation started ***
sum 8 new_value = true
scala> input2 setSignal true; run
carry 11 new_value = true
sum 15 new_value = false


