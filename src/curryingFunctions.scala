#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Currying Functions*/
def Currying_Functions() {
	def sum(f: Int => Int): (Int, Int) => Int = {
	def sumF(a: Int, b: Int): Int =
		if (a > b) {0; }else {f(a) + sumF(a + 1, b);}
	sumF
	}
	def sumInts = sum(x => x)
	def sumSquares = sum(x => x * x)
	def sumPowersOfTwo = sum(powerOfTwo)
	val sumInts = sum(x => x)
	val sumSquares = sum(x => x * x)
	val sumPowersOfTwo = sum(powerOfTwo)
	def sum(f: Int => Int)(a: Int, b: Int): Int = if (a > b) {0; }else {f(a) + sum(f)(a + 1, b);}
	def f = (args1) => (argsn): E => Int; val f = (args1) => (argsn) => E
	def f = (args1) => (argsn-1):  Int; def g (argsn) = E  g 
	def f = (args1) => (argsn): E => Int; val f (args1) => (argsn-1) = (args n) => E
}