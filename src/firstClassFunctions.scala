#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*First-Class Functions*/
def First_Class_Functions() {
	def sumInts(a: Int, b: Int): Int =
		if (a > b) {0; }else {a + sumInts(a + 1, b);}
	def square(x: Int): Int = x * x
	def sumSquares(a: Int, b: Int): Int =
		if (a > b) {0; }else {square(a) + sumSquares(a + 1, b);}
	def powerOfTwo(x: Int): Int = if (x == 0) 1 else {2 * powerOfTwo(x - 1);}
	def sumPowersOfTwo(a: Int, b: Int): Int =
		if (a > b) {0; }else {powerOfTwo(a) + sumPowersOfTwo(a + 1, b);}
	def sum(f: Int => Int, a: Int, b: Int): Int =
		if (a > b) {0; }else {f(a) + sum(f, a + 1, b);}
	def sumInts(a: Int, b: Int): Int = sum(id, a, b)
	def sumSquares(a: Int, b: Int): Int = sum(square, a, b)
	def sumPowersOfTwo(a: Int, b: Int): Int = sum(powerOfTwo, a, b)
	def id(x: Int): Int = x;
	def square(x: Int): Int = x * x
	def powerOfTwo(x: Int): Int = 
		if (x == 0) {1; }else {2 * powerOfTwo(x - 1);}
}