
def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) REPEAT(command)(condition)
}


var i = 0
REPEAT({ i = i + 1; println(i) })(i < 5)
