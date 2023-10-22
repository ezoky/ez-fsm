package com.ezoky.ezfsm

import org.scalatest.flatspec.AnyFlatSpec

import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/**
 * @since 0.1.0
 * @author gweinbach on 05/03/2023
 */
class StateTest extends AnyFlatSpec:

  object StaticState:
    trait LightStatus

    case object On extends LightStatus

    case object Off extends LightStatus

    // Two next lines are useless when using DerivingState instead of State as they are automatically derived
    given State[On.type]()

    given State[Off.type]()

    val TurnOn = Transition[LightStatus, Off.type, On.type](On)
    val TurnOff = Transition[LightStatus, On.type, Off.type](Off)

  "Transition" should "enable to change state" in {

    import StaticState.*

    assert(TurnOff(On) === Off)
    assert(TurnOn(Off) === On)
  }

  "FSM" should "be used to trigger transition" in {

    import StaticState.*

    val initial = SimpleFSM[LightStatus, On.type](On)
    val next = initial.trigger(TurnOff)
    val last = next.trigger(TurnOn)

    assert(initial.currentState === On)
    assert(next.currentState === Off)
    assert(last.currentState === On)
  }

  "FSM" can "record state history" in {

    import StaticState.*

    val initial = HistorizingFSM[LightStatus, On.type](On)
    val next = initial.trigger(TurnOff)
    val last = next.trigger(TurnOn)

    assert(initial.currentState === On)
    assert(initial.previousState === None)
    assert(initial.initialState === On)
    assert(initial.stateHistory.isEmpty)

    assert(next.currentState === Off)
    assert(next.previousState === Some(On))
    assert(next.initialState === On)
    assert(next.stateHistory === On :: Nil)

    assert(last.currentState === On)
    assert(last.previousState === Some(Off))
    assert(last.initialState === On)
    assert(last.stateHistory === Off :: On :: Nil)
  }

  object DynamicState:

    sealed abstract class Level[T: Numeric]:
      val value: T

      def evolve(evolution: T): Level[T] =
        Level(value + evolution)

    case class NegativeLevel[T: Numeric](override val value: T) extends Level[T]

    case class PositiveLevel[T: Numeric](override val value: T) extends Level[T]

    case class NullLevel[T: Numeric]() extends Level[T]:
      override val value: T = Numeric[T].zero

    type NonNullLevel[T] = NegativeLevel[T] | PositiveLevel[T]

    object Level:
      def apply[T: Numeric](value: T): Level[T] =
        val _zero = Numeric[T].zero
        if value > _zero then
          PositiveLevel(value)
        else if value < _zero then
          NegativeLevel(value)
        else
          NullLevel()

    given[T: Numeric]: State[Level[T]]()

    given[T: Numeric]: State[NullLevel[T]]()

    def Nullify[T: Numeric]: Transition[Level[T], Level[T], NullLevel[T]] =
      Transition[Level[T], Level[T], NullLevel[T]](NullLevel[T]())

    def Evolve[T: Numeric](evolution: T): Transition[Level[T], Level[T], Level[T]] =
      Transition((s1: Level[T]) =>
        s1.evolve(evolution)
      )

  "FSM" can "be used to govern dynamic state transitions" in {

    import DynamicState.*

    val initial = SimpleFSM(Level(10))
    val second = initial.trigger(Evolve(10))
    val third = second.trigger(Evolve(-30))
    val fourth = third.trigger(Nullify)
    val fifth = fourth.trigger(Evolve(-20))

    assert(initial.currentState === PositiveLevel(10))
    assert(second.currentState === PositiveLevel(20))
    assert(third.currentState === NegativeLevel(-10))
    assert(fourth.currentState === NullLevel[Int]())
    assert(fifth.currentState === NegativeLevel(-20))
  }

  object InternalState:

    sealed abstract class Account(val balance: Int):
      def credit(amount: Int): Account =
        Account(balance + amount)

    final case class CreditAccount(override val balance: Int) extends Account(balance)

    final case class DebitAccount(override val balance: Int) extends Account(balance)

    final case class NullAccount(override val balance: Int) extends Account(balance)

    object Account:
      def apply(balance: Int): Account =
        if (balance > 0) then
          new CreditAccount(balance)
        else if balance < 0 then
          new DebitAccount(balance)
        else
          new NullAccount(balance)