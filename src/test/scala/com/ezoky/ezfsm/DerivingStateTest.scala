package com.ezoky.ezfsm

import com.ezoky.ezfsm.DerivingState.derived
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @since 0.1.0
 * @author gweinbach on 05/03/2023
 */
class DerivingStateTest extends AnyFlatSpec:

  object StaticState:
    // Automatic State Typeclass derivation
    sealed trait LightStatus derives DerivingState

    case object On extends LightStatus

    case object Off extends LightStatus

    val TurnOn = Transition[LightStatus, Off.type, On.type](On)
    val TurnOff = Transition[LightStatus, On.type, Off.type](Off)


  "Transition" should "enable to change derived state" in {

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
