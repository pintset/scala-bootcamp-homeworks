package async

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Try

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object AsyncHomework extends App {
  // Waits 60 seconds because of newCachedThreadPool
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def pipeline(url: String): Future[List[String]] =
    fetchPageBody(url)
      .flatMap(findLinkUrls)
      .flatMap(Future.traverse(_)(fetchServerName))
      .map(_.flatten.distinct.sorted)

  def print(url: String, result: Try[List[String]]): Unit = {
    val names = {
      result.fold(t => s"Fetching failed: ${t.getMessage}", {
        case List() => "None"
        case xs => xs.mkString(", ")
      })
    }

    synchronized {
      println(s"Server names for $url: $names")
    }
  }

//  val args2 = Array(
//    "https://google.com", "https://microsoft.com", "https://yandex.ru", "https://amazon.com", "https://github.com")
  args.map(url => (url, pipeline(url))).foreach { case (url, f) => f.onComplete(print(url, _)) }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }


  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
