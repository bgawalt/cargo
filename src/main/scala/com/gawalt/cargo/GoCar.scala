package com.gawalt.cargo

/**
 * This source file created by Brian Gawalt, 3/29/17.
 * It is subject to the MIT license bundled with this package in the file LICENSE.txt.
 * Copyright (c) Brian Gawalt, 2017
 */
case class GoCar(decisionWeights: Array[Array[Double]],
                 memoryWeights: Array[Array[Double]]
                  ) {

}

trait Decision {
  val amount: Double
}
case class Move(forward: Boolean, amount: Double) extends Decision
case class GasCan(distance: Double, amount: Double) extends Decision