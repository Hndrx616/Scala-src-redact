#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Expressions*/
def Expressions() {
	scala> 87 + 145
		unnamed0: Int = 232;
	scala> 5 + 2 * 3
		unnamed1: Int = 11;
	scala> "hello" + " world!"
		unnamed2: java.lang.String = hello world!;
	scala> def scale = 5
		scale: Int;
	scala> 7 * scale
		unnamed3: Int = 35;
	scala> def pi = 3.141592653589793
		pi: Double;
	scala> def radius = 10
		radius: Int;
	scala> 2 * pi * radius
		unnamed4: Double = 62.83185307179586;
	scala> def square(x: Double) = x * x
		square: (Double)Double;
	scala> square(2)
		unnamed0: Double = 4.0;
	scala> square(5 + 3)
		unnamed1: Double = 64.0;
	scala> square(square(4))
		unnamed2: Double = 256.0;
	scala> def sumOfSquares(x: Double, y: Double) = square(x) + square(y)
		sumOfSquares: (Double,Double)Double;
	scala> sumOfSquares(3, 2 + 2)
		unnamed3: Double = 25.0;
	scala> def loop: Int = loop
		loop: Int;
	scala> def first(x: Int, y: Int) = x
		first: (Int,Int)Int;
	scala> def loop: Int = loop
		loop: Int;
	scala> def first(x: Int, y: Int) = x
		first: (Int,Int)Int;
	scala> def constOne(x: Int, y: => Int) = 1
		constOne: (Int,=> Int)Int;
	scala> constOne(1, loop)
		unnamed0: Int = 1;
	scala> constOne(loop, 2) // gives an infinite loop
	^C // stops execution with Ctrl-C
}