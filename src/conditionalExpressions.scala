#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Conditional Expressions*/
def ConditionalExpressions() {
scala> def abs(x: Double) = if (x >= 0) x else -x
	abs: (Double)Double
def sqrt(x: Double): Double =
def sqrtIter(guess: Double, x: Double): Double =
	if (isGoodEnough(guess, x)) guess;
	else sqrtIter(improve(guess, x), x);
def improve(guess: Double, x: Double) =
	(guess + x / guess) / 2;
def isGoodEnough(guess: Double, x: Double) =
		abs(square(guess) - x) < 0.001;
def sqrt(x: Double) = sqrtIter(1.0, x)
}