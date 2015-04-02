import Core.Machine
import Machine._

class RParser(val tokens:String) {
  var upTo = 0
  def end:Boolean = this.upTo == this.tokens.size
  def next = this.upTo  = this.upTo + 1
  def peek:Option[String] = {
    if (this.end) None
    else Some(this.tokens.charAt(upTo).toString)
  }

  /**
   * Grammar (as seen from matt might)
   *
   * <regex> ::= <term> '|' <regex>
            |  <term>

   <term> ::= { <factor> }

   <factor> ::= <base> ('*'| '?'|'+')?

   <base> ::= <char>
           |  '\' <char>
           |  '(' <regex> ')'
   */

  private def parseRegex:Machine = {
    val term = parseTerm
    this.peek match {
      case Some("|") => {
        this.next
        val regex = parseRegex
        or(term, regex)
      }
      case _ => term
    }
  }

  private def parseTerm:Machine = {
    val factor = parseFactor
    if (!this.end && this.peek != Some("|") && this.peek != Some(")")) {
      and(factor, parseTerm)
    }
    else factor
  }

  private def parseFactor:Machine = {
    val base = parseBase
    this.peek match {
      case Some("*") => {
        this.next
        someOrNone(base)
      }
      case Some("+") => {
        this.next
        oneOrMore(base)
      }
      case Some("?") => {
        this.next
        zeroOrOne(base)
      }
      case _ => base
    }
  }

  private def parseBase:Machine = {
    this.peek match {
      case Some("(") => {
        this.next
        val regex = this.parseRegex
        if (this.peek == Some(")")){
          this.next
          regex
        }
        else {
          throw new Exception("closing parens not found")
        }
      }

      case Some("""\""") => {
        this.next
        parseBase
      }
      case Some(char) => singleChar(char)
    }
  }

  def parse:Option[Machine] = {
    val machine = parseRegex
    if (this.end) Some(machine)
    else None
  }

}

println("hell!")


