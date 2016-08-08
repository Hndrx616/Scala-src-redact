#!/usr/bin/env scalas 
import sbt._, Path._
import java.io.File
import java.net.{URI, URL}
import sys.process._
/*Lists*/
/*list good*/
val fruit = List("apples", "oranges", "pears")
val nums = List(1, 2, 3, 4)
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty = List()

val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
val nums = 1 :: (2 :: (3 :: (4 :: Nil)))
val diag3 = (1 :: (0 :: (0 :: Nil))) ::
(0 :: (1 :: (0 :: Nil))) ::
(0 :: (0 :: (1 :: Nil))) :: Nil
val empty = Nil

val nums = 1 :: 2 :: 3 :: 4 :: Nil

empty.isEmpty = true
fruit.isEmpty = false
fruit.head = "apples"
fruit.tail.head = "oranges"
diag3.head = List(1, 0, 0)
/*list good*/
def isort(xs: List[Int]): List[Int] = {
	if (xs.isEmpty) Nil
	else insert(xs.head, isort(xs.tail))
}
def isort(xs: List[Int]): List[Int] = xs match {
	case List() => List()
	case x :: xs1 => insert(x, isort(xs1))
}
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
	case List() => List(x)
	case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}