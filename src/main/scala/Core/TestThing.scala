package Core

/**
 * Created by gmgilmore on 3/31/15.
 */

import Core.Machine._


object TestThing extends App{
 val myP = new RegexParser("""(a|b)?""")
  println(myP.parse)

}







