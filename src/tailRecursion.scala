#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Tail Recursion*/
def Tail_Recursion() {
	def gcd(a: Int, b: Int): Int = 
		if (b == 0) {
			a;
		}else {
				gcd(b, a % b);}
	def factorial(n: Int): Int = 
		if (n == 0) {
			1;
		}else {
			n * factorial(n - 1);}
}