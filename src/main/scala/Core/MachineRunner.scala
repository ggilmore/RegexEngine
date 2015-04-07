package Core

import scala.annotation.tailrec

/**
 * Contains methods that are using to "run" machines and test input to see if the input matches the regex that is
 * describing a particular machine
 */
object MachineRunner {

  /**
   * @param input the input that we are using to advance the machine's state
   * @param machine the machine that we are using to attempt to match the input string
   * @param validStates the set of states that we are currently evaluating
   * @return a new set of states that we are currently evaluating based on the new input "input" to the
   *         Machine "machine"
   */
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

  /**
   * @param machine that machine that we are using to attempt to match the input "inputString"
   * @param inputString the input that we are evaluating to see if it matches the regex that is described by the machine
   * @return
   */
  def testInput(machine:Machine, inputString:String):Boolean = {
    def loop(testStr:String, validStates:Set[State]):Set[State] = {
      if (testStr.isEmpty) validStates
      else loop(testStr.drop(1), nextStep(testStr.charAt(0), machine, validStates))
    }
    val finalStates = loop(inputString, Set(machine.initialState))
    finalStates.contains(machine.finalState)
  }

}