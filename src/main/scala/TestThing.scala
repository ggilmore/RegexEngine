/**
 * Created by gmgilmore on 3/31/15.
 */

import Machine._


class TestThing extends App{

  val aI = State("aI")
  val aF = State("aF")
  val testMachine = singleChar(aI, aF, 'a')

}







