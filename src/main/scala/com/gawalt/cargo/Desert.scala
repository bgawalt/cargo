package com.gawalt.cargo

/**
 * This source file created by Brian Gawalt, 3/29/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */
case class DesertItem(isCar: Boolean, distance: Double, amount: Double) {
  def toArray: Array[Double] = Array(if (isCar) 1.0 else 0.0, distance, amount)
}

case class Desert(items: Seq[DesertItem]) {

  def sort: Desert = Desert(items.sortBy(_.distance))

  def moveCar(move: Move) : Desert = {
    val newItems = for (item <- items) yield {
      if (item.isCar) {
        if (move.forward) item.copy(distance = item.distance + move.amount)
        else item.copy(distance = item.distance - move.amount)
      } else {
        item
      }
    }
    Desert(newItems).sort
  }

  def dumpFuel(dump: DumpFuel) {
    Desert(
      Seq(DesertItem(isCar = false, distance = dump.distance, amount = dump.amount)) ++ items
    ).sort
  }
}
