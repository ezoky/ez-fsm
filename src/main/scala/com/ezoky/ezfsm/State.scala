package com.ezoky.ezfsm

/**
 * @since 0.1.0
 * @author gweinbach on 01/03/2023
 */
trait State[S]

/**
 * A Transition from a value of state S1 to a value of state S2
 *
 * @tparam T  Parent type of S1 and S2 (the type governed by the state machine)
 *            TODO check if it has sense to keep this type an explicit type here
 * @tparam S1 type of transition source
 * @tparam S2 type of transition destination
 */
abstract class Transition[T, -S1 <: T : State, +S2 <: T : State]:
  def apply(s1: S1): S2

object Transition:
  /**
   * Creates a simple transition that goes from any state of type S1 to a value "to" of type S2.
   *
   * @param to Target (unique) value of the transition
   */
  def apply[T, S1 <: T : State, S2 <: T : State](to: S2): Transition[T, S1, S2] =
    new Transition[T, S1, S2]:
      def apply(s1: S1): S2 = to

  /**
   * Creates a transition from a function
   *
   * @param transitionFunction The function that computes target state from source state
   */
  def apply[T, S1 <: T : State, S2 <: T : State](transitionFunction: (S1 => S2)): Transition[T, S1, S2] =
    new Transition[T, S1, S2]:
      def apply(s1: S1): S2 = transitionFunction(s1)


abstract class FSM[T, S <: T : State](val currentState: S):

  /**
   * F-Bound type member
   *
   * @tparam S2 any subtype of T
   */
  protected type Self[S2 <: T] <: FSM[T, S2]

  final def trigger[S2 <: T : State](transition: Transition[T, S, S2]): Self[S2] =
    evolveTo(transition(currentState))

  protected def evolveTo[S2 <: T : State](nextState: S2): Self[S2]

case class SimpleFSM[T, S <: T : State](override val currentState: S) extends FSM[T, S](currentState):

  override protected type Self[S2 <: T] = SimpleFSM[T, S2]

  override protected def evolveTo[S2 <: T : State](nextState: S2): Self[S2] =
    SimpleFSM[T, S2](nextState)

case class HistorizingFSM[T, S <: T : State](override val currentState: S,
                                             stateHistory: List[T] = Nil) extends FSM[T, S](currentState):

  override protected type Self[S2 <: T] = HistorizingFSM[T, S2]

  override protected def evolveTo[S2 <: T : State](nextState: S2): Self[S2] =
    HistorizingFSM[T, S2](nextState, currentState :: stateHistory)

  lazy val previousState: Option[T] = stateHistory.headOption

  lazy val initialState: T = stateHistory.lastOption.getOrElse(currentState)


