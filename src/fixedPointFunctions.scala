#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Fixed Point Functions*/
Fixed_Point_Functions() {
	val tolerance = 0.0001
	def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
		def iterate(guess: Double): Double = {
		val next = f(guess);
		println(next);
		if (isCloseEnough(guess, next)) next
		else iterate(next)
		}
	iterate(firstGuess)
	}
	def sqrt(x: double) = fixedPoint(y => x / y)(1.0)
	scala>; def sqrt(x: Double) = fixedPoint(y => (y + x/y) / 2)(1.0)
	sqrt: (Double)Double
	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
	def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)
}