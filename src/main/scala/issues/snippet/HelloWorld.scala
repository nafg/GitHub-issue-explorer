package issues.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import issues.lib._
import Helpers._

import net.liftweb.http.{S,SHtml, StatefulSnippet}

case class Issue(
  number: Int,
  votes: Int,
  created: Date,
  updated: Date,
  closed: Option[Date],
  title: String,
  body: String,
  labels: Seq[String],
  user: String,
  state: String
) {
  var showBody = false
}

class HelloWorld extends StatefulSnippet {
  override def dispatch = {
    case "howdy" => howdy _
  }
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
  def parseDate(date: String) = {
    date.substring(20).toList match {
      case h1 :: h2 :: ':' :: m1 :: m2 :: Nil =>
        format.parse(date.substring(0,20) + h1 + h2 + m1 + m2)
      case _ =>
        format.parse(date)
    }
  }

  val ticketXML = xml.XML.load(new java.net.URL("file:///D:/docs/downloads/open%20lift%20issues.xml").openStream)
  var issues = ticketXML \ "issue" map { issueXML =>
    Issue(
      (issueXML\"number").text.toInt,
      (issueXML\"votes").text.toInt,
      parseDate(issueXML\"created-at" text),
      parseDate(issueXML\"updated-at" text),
      {
        val closedAt = (issueXML\"closed-at").first
        closedAt.attributes.get("type") match {
          case Some(Seq(t @ Text("datetime"))) => Some(parseDate(t.text))
          case None => None
        }
      },
      issueXML \ "title" text,
      issueXML \ "body" text,
      issueXML \ "labels" \ "label" map {_.text.trim},
      issueXML \ "user" text,
      issueXML \ "state" text
    )
  } toList
  val labels = issues.flatMap(_.labels).removeDuplicates.sort(_ < _)
  val users = issues.map(_.user).removeDuplicates.sort(_ < _)
  val states = issues.map(_.state).removeDuplicates.sort(_ < _)
  var displayedLabels = labels
  var displayedUsers = users
  var displayedStates = states
  
  def howdy(in: NodeSeq): NodeSeq = {
    <fieldset><legend>Filter</legend>
      <table>
        <tr>
          <th>Users</th>
          <td>{
            users.flatMap {user =>
              SHtml.checkbox(
                displayedUsers contains user,
                (v: Boolean) => v match {
                  case true => displayedUsers = user :: displayedUsers removeDuplicates
                  case false => displayedUsers -= user
                },
                "id" -> ("user_" + user)
              ) ++ <label for={"user_" + user}>{user}</label> ++ Text(" ")
            } ++ <br /> ++ SHtml.submit("All", () => {displayedUsers = users}) ++ SHtml.submit("None", () => {displayedUsers = Nil})
          }</td>
        </tr>
        <tr>
        <th>States</th>
        <td>{
            states.flatMap {state =>
              SHtml.checkbox(
                displayedStates contains state,
                (v: Boolean) => v match {
                  case true => displayedStates = state :: displayedStates removeDuplicates
                  case false => displayedStates -= state
                },
                "id" -> ("state_" + state)
              ) ++ <label for={"state_" + state}>{state}</label> ++ Text(" ")
            } ++ <br /> ++ SHtml.submit("All", () => {displayedStates = states}) ++ SHtml.submit("None", () => {displayedStates = Nil})
          }</td>
        </tr>
        <tr>
        <th>Labels</th>
        <td>{
            labels.flatMap {label =>
              SHtml.checkbox(
                displayedLabels contains label,
                (v: Boolean) => v match {
                  case true => displayedLabels = label :: displayedLabels removeDuplicates
                  case false => displayedLabels -= label
                },
                "id" -> ("label_" + label)
              ) ++ <label for={"label_" + label}>{label}</label> ++ Text(" ")
            } ++ <br /> ++ SHtml.submit("All", () => {displayedLabels = labels}) ++ SHtml.submit("None", () => {displayedLabels = Nil})
          }</td>
        </tr>
      </table>{SHtml.submit("Update", () => {})}
    </fieldset> ++
    <table>
    <tr>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.number < _.number},
        Text("number")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.title < _.title},
        Text("title")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.votes < _.votes},
        Text("votes")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.created before _.created},
        Text("created")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.updated before _.updated},
        Text("updated")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort {(a,b) =>
          (a.closed, b.closed) match {
            case (None, None) => false
            case (None, Some(_)) => false
            case (Some(_), None) => true
            case (Some(a), Some(b)) => a before b
          }
        },
        Text("closed")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.user < _.user},
        Text("user")
      )}</th>
      <th>{link(
        S.uri,
        () => issues = issues.sort { _.state < _.state},
        Text("state")
      )}</th>
      <th>labels</th>
    </tr>
    {
      def line(s: String) = Text(s) ++ <br />
      issues.filter{i =>
        displayedUsers.contains(i.user) && displayedStates.contains(i.state) && i.labels.exists(displayedLabels.contains)
      }.flatMap {i =>
        <tr>
          <td>{i.number}</td>
          <td>{i.title}</td>
          <td>{i.votes}</td>
          <td>{i.created}</td>
          <td>{i.updated}</td>
          <td>{i.closed getOrElse ""}</td>
          <td>{i.user}</td>
          <td>{i.state}</td>
          <td>{i.labels map line}</td>
        </tr>
        <tr><td></td><td colspan="7">{
          if(i.showBody) { 
            i.body.lines.collect.map(line) ++
              link(
                S.uri,
                () => i.showBody = false,
                Text("(less)")
              )
          } else {
            Text(i.body.take(30) + " ") ++
              link(
                S.uri,
                () => i.showBody = true,
                Text("(more...)")
              )
          }
        }</td></tr>
      }
    }</table>
  }
}



