sealed trait Node{
  val left:Node
  val right:Node

  def eval:Int
}

case class AddNode(left:Node,right:Node) extends Node{
  def eval = left.eval + right.eval
}

case class MultNode(left:Node, right:Node) extends Node {
  def eval = left.eval * right.eval
}

case class LiteralNode(value:Int) extends Node{
  def eval = value
  val left = null
  val right = null
}

class CalParser(val tokens:String) {
  val RE_NUMBER = s"""-?[0-9]+""".r
  val length = tokens.size
  var upTo = 0

  def end: Boolean = this.upTo == this.length

  def peek: Option[String] = {
    if (this.end) None
    else Some(tokens.charAt(this.upTo).toString)
  }

  def next = if (!this.end) this.upTo = upTo + 1

  private def parseE3: Node = {
    this.peek match {
      case Some("(") => {
        this.next
        val node = this.parseE1
        if (this.peek != Some(")")) throw new Exception
        else {
          this.next
          node
        }
      }

      case Some(num) if RE_NUMBER.pattern.matcher(s"$num").matches => {
        val node = LiteralNode(num.toInt)
        this.next
        node
      }

      case _ => null
    }
  }

  private def parseE2: Node = {
    val node = this.parseE3
    this.peek match {
      case Some("*") => {
        this.next
        val node2 = this.parseE2
        MultNode(node, node2)
      }
      case _ => node
    }
  }

  private def parseE1: Node = {
    val node = this.parseE2
    this.peek match {
      case Some("+") => {
        this.next
        val node2 = this.parseE1
        AddNode(node, node2)
      }
      case _ => node
    }
  }

  def parse: Node = {
    val node = this.parseE1
    if (!this.end) throw new Exception("extra input found at end of string")
    node
  }
}

val myP = new CalParser(s"""3*(3+2)""")
val result = myP.parse.eval





