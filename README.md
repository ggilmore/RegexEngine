# RegexEngine

##Intro: 
This is a simple regex engine written in Scala that I made following [this article](http://perl.plover.com/Regex/article.html). 

##Supported Grammar: 
([borrowed and modified from Matt Might](http://matt.might.net/articles/parsing-regex-with-recursive-descent/))


     <regex> ::= <term> '|' <regex>
             ::=  <term>
     
     <term> ::= <factor>+
     
     <factor> ::= <factor> ('*'| '?'|'+')?
             ::= <base> ('*'| '?'|'+')?
     
     <base>  ::= <char>
             ::=  '\' <char>
             ::= '(' <regex> ')'

##Basic Usage: 
The `RegexParser` class is used to parse a string that represents a regex. Create a new `RegexParser` instance and use its `parse()`
method in order to create a [Machine](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) instance that represents that
regex. 

The `MachineRunner` object has a method called `testInput` that takes a `Machine` and some `inputString` and returns `true` if the
`inputString` matches the regex that the `Machine` represents, and false otherwise. 

The `Machine` companion object also has a helper function, called `toDOTFileFormat` that takes a `Machine`, and returns a representation 
of that machine using the [DOT graph description language](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29).
This graph has nodes as states, unlabeled edges representing transitions that aren't contingent on any character, and labeled edges 
representing transitions that are contingent on the character that the edge is labeled with. 
  
