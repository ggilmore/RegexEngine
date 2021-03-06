package com.ggilmore.core

import Machine._


/**
 * Created by gmgilmore on 3/31/15.
 */
class RegexParser(val tokens:String) {
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
   *        ::=  <term>
   *
   *<term> ::= <factor>+
   *
   *<factor> ::= <factor> ('*'| '?'|'+')?
   *         ::= <base> ('*'| '?'|'+')?
   *
   *<base> ::= <char>
   *        ::=  '\' <char>
   *        ::= '(' <regex> ')'
   *
   *
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
    var factor = parseFactor
    while (!this.end && this.peek != Some("|") && this.peek != Some(")")) {
      val nextFactor = parseFactor
      factor = and(factor, nextFactor)
    }
    factor
  }

  private def parseFactor:Machine = {
    val base = parseBase
    if (!this.end) {
      this.peek match {
        case Some("*") => {
          this.next
          if (this.peek == Some("*") | this.peek ==Some("+") | this.peek==Some("?")) throw new Exception(s"double operators")
          someOrNone(base)
        }
        case Some("+") => {
          this.next
          if (this.peek == Some("+") | this.peek ==Some("*") | this.peek==Some("?")) throw new Exception(s"double operators")
          oneOrMore(base)
        }
        case Some("?") => {
          this.next
          if (this.peek == Some("?") | this.peek ==Some("*") | this.peek==Some("+")) throw new Exception(s"double operators's")
          zeroOrOne(base)
        }
        case _ => base
      }
    }
    else {
      base
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
      case Some(char) => {
        this.next
        singleChar(char)}
    }
  }

  def parse:Option[Machine] = {
    val machine = parseRegex
    if (this.end) Some(machine)
    else None
  }

}