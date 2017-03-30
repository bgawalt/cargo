package com.gawalt.cargo

import scala.collection.mutable
import scala.collection.immutable

/**
 * This source file created by Brian Gawalt, Mar 29, 2017.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */
object HelloWorld {
  def main(args: Array[String]) {
    val tree = mutable.TreeSet[Double]()
    val hash = mutable.HashSet[Double]()


    val arr = (0 until 10000).toVector
    for (i <- arr) {
      tree.add(i)
      hash.add(i)
    }
    if (args.contains("tree")) {
      for (i <- 1 until 1000) {
        for (j <- 10 to 800) {
          assert(tree.range(j - 5, j + 5 ).size == 10)
        }
      }
    }
    if (args.contains("hash")) {
      for (i <- 1 until 1000) {
        for (j <- 10 to 800) {
          assert(hash.filter(hi => hi >= j - 5 && hi < j + 5 ).size == 10)
        }
      }
    }
    if (args.contains("arr")) {
      for (i <- 1 until 1000) {
        for (j <- 10 to 800) {
          assert(arr.filter(hi => hi >= j - 5 && hi < j + 5 ).size == 10)
        }
      }
    }
  }
}
