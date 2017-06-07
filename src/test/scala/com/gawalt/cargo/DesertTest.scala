package com.gawalt.cargo

import org.scalatest.FunSuite
import org.scalatest.Matchers

/**
 * This source file created by Brian Gawalt, 6/3/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */
class DesertTest extends FunSuite with Matchers {

  test("Initial Values") {
    val desert = new Desert()
    desert.tankRoom shouldBe 0
    desert.carPosition shouldBe 0
    desert.fuelCans.size shouldBe 0
    desert.itemSequence().size shouldBe 1
    desert.carToDesertItem shouldBe DesertItem(isCar = true, 0, 100)
  }


}
