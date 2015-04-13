package Core

/**
 * Created by gmgilmore on 3/31/15.
 */

import Core.Machine._


object TestThing extends App{
 new RegexParser("""w?\+""").parse match {
   case Some(m) => {
     println(Machine.toDOTFileFormat(m))
     println(MachineRunner.testInput(m, ""))
//     println(MachineRunner.testInput(m,"d"))
   }
   case None => println("invalid regex")
 }

}







