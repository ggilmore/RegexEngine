package Core

import scala.annotation.tailrec

/**
 * Created by gmgilmore on 4/2/15.
 */
object MachineRunner {

  def nextStep(input:Char, machine: Machine, validStates: Set[State]):Set[State] = {

    def handleNTransitions(beforeStates:Set[State]):Set[State] = {
      @tailrec
      def loop(beforeStates:Set[State]):Set[State] = {
        val newStates = beforeStates.foldLeft(Set[State]()){case (set, state) => {
          if (machine.nTransitions.contains(state)) set ++ machine.nTransitions(state)
          else set + state
        }}
        if (newStates == beforeStates) newStates else loop(newStates)
      }
      val finalStates = loop(beforeStates)
      finalStates
    }

    def handleDTransitions(beforeStates:Set[State]):Set[State] = {
      beforeStates.foldLeft(Set[State]()){case (set, state) => {
        if (machine.dTransitions.contains(state) && machine.dTransitions(state).contains(input)) set ++ machine.dTransitions(state)(input)
        else set
      }}
    }
    val firstRoundNTransitions = handleNTransitions(validStates)
    val allDTransitions = handleDTransitions(firstRoundNTransitions)
    val lastRoundNTransitions = handleNTransitions(allDTransitions)
    lastRoundNTransitions

  }

  def testInput(machine:Machine, inputString:String) = {
    def loop(testStr:String, validStates:Set[State]):Set[State] = {
      if (testStr.isEmpty) validStates
      else loop(testStr.drop(1), nextStep(testStr.charAt(0), machine, validStates))
    }
    val finalStates = loop(inputString, Set(machine.initialState))
    finalStates.contains(machine.finalState)
  }

}