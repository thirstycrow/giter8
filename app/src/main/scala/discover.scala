package giter8

sealed trait Term {
  def bold: String => String
  def reversed: String => String
  def reset: String => String
}

object NoTerm extends Term {
  def bold        = { s => s }
  def reversed    = { s => s }
  def reset       = { s => s }
}

object ClrTerm extends Term {
  def reversed = Console.REVERSED + _
  def reset = _ + Console.RESET
  def bold = Console.BOLD + _
}

trait Discover { self: Giter8 =>
  import dispatch._
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  case class Template(user: String, name: String, desc: String)

  val RepoNamed = """(\S+)\.g8""".r

  val Limit = 10

  def term: Term = ClrTerm

  def template(in: String) = (term.bold andThen term.reset)(in)

  def description(in: String) = in

  def pager(q: Option[String], page: Int, more: Boolean): Unit =
    Console.readLine(
      if(more) ":"
      else (term.reversed andThen term.reset)("(END)")) match {
      case "b" | "B" | "u" | "U" if(page > 0) =>
        discover(q, page - 1)
      case _ if(more) =>
        discover(q, page + 1)
      case _  => ()
    }

  def show(ps: Seq[Template], more: Boolean) =
    ps.map { t =>
      (template(t.user +"/"+ t.name)
        + " \n\t "
        + description(if(t.desc.isEmpty) "-" else t.desc))
    } mkString(" ", "\n ","")

  def discover(query: Option[String] = None, page: Int = 0) =
    remoteTemplates(query, page).right.flatMap { templates =>
      templates match {
        case Nil =>
          Right("No templates matching %s" format query)
        case templates =>
          println(show(templates.init, templates.size > Limit))
          pager(query, page, templates.size > Limit)
          Right("")
      }
    }
  
  def remoteTemplates(query: Option[String], page: Int = 0) =
    http x (ls(query, page) ># { j => j }) {
      case (200, _, _, js) =>
        Right(for {
          JArray(repos) <- js()
          JObject(fields) <- repos
          JField("name", JString(name)) <- fields
          JField("username", JString(userName)) <- fields
          JField("description", JString(desc)) <- fields
        } yield Template(userName, name, desc))
      case (404, _, _, _) =>
        Left("Unable to find github repositories like : %s" format query)
    }

  def ls(query: Option[String], page: Int = 0) = :/("ls.implicit.ly") / "api" / "1" / "g8" <<? Map(
    "page" -> page.toString, "limit" -> (Limit + 1).toString
  ) ++ query.map("query" -> _)
}
