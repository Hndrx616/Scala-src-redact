#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Expressions & Definitions*/
def eval(e: Expr): Int = {
	if (e.isNumber) {e.numValue}
	else if (e.isSum) {eval(e.leftOp) + eval(e.rightOp)}
	else {error("unrecognized expression kind")}
}
def eval(e: Expr): Int = e match {
	case Number(n) => n
	case Sum(l, r) => eval(l) + eval(r)
}
def print(e: Expr) {
	if (e.isNumber) Console.print(e.numValue)
	else if (e.isSum) {
	Console.print("(") print(e.leftOp) Console.print("+") print(e.rightOp) Console.print(")")
	} else {error("unrecognized expression kind")}
}