#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Anonymous Functions*/
def Anonymous_Functions() {
	def sumInts(a: Int, b: Int): Int = sum((x: Int) => x, a, b)
	def sumSquares(a: Int, b: Int): Int = sum((x: Int) => x * x, a, b)
	def sumInts(a: Int, b: Int): Int = sum(x => x, a, b)
	def sumSquares(a: Int, b: Int): Int = sum(x => x * x, a, b)
	def f (x1: T1 ::: xn: Tn) = {E ; f _ }
}