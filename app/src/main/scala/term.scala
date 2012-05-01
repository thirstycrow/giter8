package giter8

object Term {
  def support: Term =
    if (System.getProperty("os.name").toLowerCase().startsWith("win")) NoTerm
    else ClrTerm
}

sealed trait Term {
  def bold: String => String
  def reversed: String => String
  def reset: String => String
  def clear: String
}

object NoTerm extends Term {
  def bold        = { s => s }
  def reversed    = { s => s }
  def reset       = { s => s }
  def clear       = ""
}

object ClrTerm extends Term {
  def reversed = Console.REVERSED + _
  def reset = _ + Console.RESET
  def bold = Console.BOLD + _
  def clear = "\033[2J\033[1;1H"
}
