package icfpc.classified

import java.net.{HttpURLConnection, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class SignalSender(serverUrl: String, apiKey: String) {

  private val base = URI.create(serverUrl)
  private val client = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.ALWAYS)
    .build()

  def send(signal: String): String = {
    val request = HttpRequest.newBuilder
      .uri(base.relativize(URI.create(s"/aliens/send?apiKey=$apiKey")))
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
