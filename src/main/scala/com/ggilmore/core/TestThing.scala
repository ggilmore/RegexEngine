package com.ggilmore.core

/**
 * Created by gmgilmore on 3/31/15.
 */

object TestThing extends App{
 new RegexParser("""((ab|ab?))""").parse match {
   case Some(m) => {
     val result = MachineRunner.testInput(m, "ab")
     println(result._1)
     result._2.foreach(set =>println(Machine.toDOTFileFormat(m, Some(set)) + "\n ----------------\n"))
   }
   case None => println("invalid regex")
 }
}







