#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Nested Functions*/
def Nested Functions() {
	def sqrt(x: Double) = {
	def sqrtIter(guess: Double, x: Double): Double =
		if (isGoodEnough(guess, x)) guess;
		else sqrtIter(improve(guess, x), x);
	def improve(guess: Double, x: Double) =
		(guess + x / guess) / 2;
	def isGoodEnough(guess: Double, x: Double) =
		abs(square(guess) - x) < 0.001;
		sqrtIter(1.0, x)
}
	def f(x: Int) = x + 1; f(1) + f(2)
	def g1(x: Int) = x + 1; g(1) + g(2)
	def g2(x: Int) = {x + 1}; g2(1) + g2(2)
	def h1(x: Int) = x + y; h1(1) * h1(2)
	def h2(x: Int) = ("x" + y ); h2(1) / h2(2)
	def sqrt(x: Double) = {
	def sqrtIter(guess: Double): Double =
		if (isGoodEnough(guess)) guess;
		else sqrtIter(improve(guess));
	def improve(guess: Double) =
		(guess + x / guess) / 2;
	def isGoodEnough(guess: Double) =
		abs(square(guess) - x) < 0.001;
		sqrtIter(1.0)
	}
}