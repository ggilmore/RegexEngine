package Core

/**
 * Created by gmgilmore on 4/2/15.
 */
object MachineRunner {

  def nextStep(input:Char, machine: Machine, validStates: Set[State]):Set[State] = {
    def handleNTransitions(beforeStates:Set[State]):Set[State] = {
      println(s"initial: $beforeStates")
      def loop(beforeStates:Set[State]):Set[State] = {
        val newStates = beforeStates.foldLeft(Set[State]()){case (set, state) => {
          if (machine.nTransitions.contains(state)) set ++ machine.nTransitions(state)
          else set + state
        }}
        if (newStates == beforeStates) newStates else loop(newStates)
      }
      val finalStates = loop(beforeStates)
      println(s"final: $finalStates")
      finalStates
    }
    def handleDTransitions(beforeStates:Set[State]):Set[State] = {
      beforeStates.foldLeft(Set[State]()){case (set, state) => {
        if (machine.dTransitions.contains(state)) {
          if (machine.dTransitions(state).contains(input)) set ++ machine.dTransitions(state)(input)
          else set
        }
        else set
      }}
    }
    handleNTransitions(handleDTransitions(handleNTransitions(validStates)))
  }

  def testInput(machine:Machine, inputString:String) = {
    def loop(testStr:String, validStates:Set[State]):Set[State] = {
      if (testStr.isEmpty) validStates
      else loop(testStr.drop(1), nextStep(testStr.charAt(0), machine, validStates))
    }
    val finalStates = loop(inputString, Set(machine.finalState))
    finalStates.foreach(x => println(x))
    finalStates.contains(machine.finalState)
  }

}