package Core

/**
 * Created by gmgilmore on 3/31/15.
 */

import Core.Machine._


object TestThing extends App{
 new RegexParser("""(a|b+)""").parse match {
   case Some(m) => {
     println(Machine.toDot(m))
     println(MachineRunner.testInput(m,"a"))
   }

   case None => println("invalid regex")
 }




}







