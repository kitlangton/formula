def uppercase(string: String): String =
  string
    .split("(?=[A-Z])")
    .map(_.capitalize)
    .mkString(" ")

uppercase("whatTheHell")
