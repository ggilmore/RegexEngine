package com.ggilmore.core

/**
 * Created by gmgilmore on 3/31/15.
 */

object TestThing extends App{
 new RegexParser("""((ab|ab?)c)""").parse match {
   case Some(m) => {
     val result = MachineRunner.testInput(m, "ac")
     println(result._1)
     result._2.foreach(progWithChar =>println("Input: " + progWithChar.input + "\n" + Machine.toDOTFileFormat(m,
       Some(progWithChar.progression)) + "\n ----------------\n"))
   }
   case None => println("invalid regex")
 }
}






