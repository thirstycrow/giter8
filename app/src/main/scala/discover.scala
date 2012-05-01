package giter8

trait Discover { self: Giter8 =>
  import dispatch._
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._
  import java.net.UnknownHostException

  case class Template(user: String, name: String, desc: String)

  val RepoNamed = """(\S+)\.g8""".r

  val Limit = 10

  def term: Term = Term.support

  def bold(in: String) = (term.bold andThen term.reset)(in)

  def reversed(in: String) = (term.reversed andThen term.reset)(in)

  def clear = println(term.clear)

  def pager(user: Option[String],
            name: Option[String],
            page: Int,
            more: Boolean): Unit =
    Console.readLine(
      if(more) ":"
      else reversed("(END)")) match {
      case "b" | "B" | "u" | "U" if(page > 0) =>
        clear
        discover(user, name, page - 1)
      case _ if(more) =>
        clear
        discover(user, name, page + 1)
      case _  => ()
    }

  def show(ps: Seq[Template], more: Boolean) =
    ps.map { t =>
      (bold(t.user +"/"+ t.name)
        + " \n\t "
        + (if (t.desc.isEmpty) "-" else t.desc))
    } mkString(" ", "\n ","")

  def discover(user: Option[String] = None,
               name: Option[String] = None,
               page: Int = 1) =
    remoteTemplates(user, name, page).right.flatMap { templates =>
      templates match {
        case Nil =>
          Right("No templates matching %s/%s" format(user.getOrElse("*"),
                                                     name.getOrElse("*")))
        case template :: Nil =>
          println(show(templates, false))
          pager(user, name, page, false)
          Right("")
        case templates =>
          println(show(templates.init, templates.size > Limit))
          pager(user, name, page, templates.size > Limit)
          Right("")
      }
    }
  
  def remoteTemplates(user: Option[String], name: Option[String], page: Int = 0) =
    try { http x (ls(user, name, page) ># { j => j }) {
      case (200, _, _, js) =>
        Right(for {
          JArray(repos) <- js()
          JObject(fields) <- repos
          JField("name", JString(name)) <- fields
          JField("username", JString(userName)) <- fields
          JField("description", JString(desc)) <- fields
        } yield Template(userName, name, desc))
      case (404, _, _, _) =>
        Left("Unable to find templates like : %s/%s" format(user.getOrElse("*"),
                                                            name.getOrElse("*")))
    } } catch {
      case e: UnknownHostException =>
        Left("Failed to resolve ls host %s" format e.getMessage)
    }

  def api = :/("ls.implicit.ly") / "api" / "1" / "g8"

  def ls(user: Option[String], name: Option[String], page: Int = 0) =
    (user, name) match {
      case (None, None) =>
        api <<? Map(
          "page" -> page.toString, "limit" -> (Limit + 1).toString
        )
      case _ =>
        api / user.getOrElse("*") / name.getOrElse("*") <<? Map(
          "page" -> page.toString, "limit" -> (Limit + 1).toString
        )
    }
}
