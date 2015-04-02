class Parser(val tokens:String){
  val length = tokens.size
  var upTo = 0

  def end:Boolean = this.upTo == this.length

  def peek:Option[Char] = {
    if (this.end) None
    else Some(tokens.charAt(this.upTo))
  }

  def next = if (!this.end) this.upTo = upTo + 1

  private def myParse:Boolean = {
    this.peek match {
      case Some(char) => char match {
        case 'a' => {
          this.next
          if (!this.myParse) false
          else if (this.peek != Some('b')) false
          else {
            this.next
            true
          }
        }
        case 'e' => {
          this.next
          true
        }

        case _ => false

      }

      case None => false
    }

  }

  def parse:Boolean = myParse && end


}

val myP = new Parser("aaebb")
myP.parse
myP.upTo
