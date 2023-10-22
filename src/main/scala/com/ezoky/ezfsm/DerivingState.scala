package com.ezoky.ezfsm

import scala.deriving.*

/**
 * @since 0.1.0
 * @author gweinbach on 16/03/2023
 */
trait DerivingState[S] extends State[S]

object DerivingState:
  inline given derived[T](using Mirror.Of[T]): DerivingState[T] =
    new DerivingState[T] {}

