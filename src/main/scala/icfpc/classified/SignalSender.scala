package icfpc.classified

import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

trait SignalSender {
  def send(signal: String): String
}

class HttpSignalSender(serverUrl: String, apiKey: String) extends SignalSender {

  private val client = HttpClient
    .newBuilder()
    .followRedirects(HttpClient.Redirect.ALWAYS)
    .build()

  override def send(signal: String): String = {
    val request = HttpRequest.newBuilder
      .uri(URI.create(s"$serverUrl/aliens/send?apiKey=$apiKey"))
      .version(HttpClient.Version.HTTP_1_1)
      .POST(HttpRequest.BodyPublishers.ofString(signal))
      .build
    val response = client
      .send(request, HttpResponse.BodyHandlers.ofString)
    val status = response.statusCode
    if (status != HttpURLConnection.HTTP_OK) {
      println("Unexpected server response:")
      println("HTTP code: " + status)
      println("Response body: " + response.body)
      System.exit(2)
    }
    response.body
  }
}

object IdentitySignalSender extends SignalSender {
  override def send(signal: String): String = signal
}

object SignalSender {

  val live: HttpSignalSender =
    new HttpSignalSender("https://icfpc2020-api.testkontur.ru", "8d26edd4434c42df82127c1640bed928")
}
