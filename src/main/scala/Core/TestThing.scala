package Core

/**
 * Created by gmgilmore on 3/31/15.
 */

import Core.Machine._


object TestThing extends App{
 new RegexParser("""((a*)?|b)?c+d""").parse match {
   case Some(m) => {
     println(Machine.toDOTFileFormat(m))
     println(MachineRunner.testInput(m,"bcccd"))
   }
   case None => println("invalid regex")
 }




}







