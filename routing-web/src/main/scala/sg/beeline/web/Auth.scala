package sg.beeline.web

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.{AuthenticationDirective, Credentials}
import sg.beeline.web.Auth.{BeelineCredentials, User}

object Auth {
  sealed trait BeelineCredentials
  case class User(userId: Int) extends BeelineCredentials

  def apply(authSettings: E2EAuthSettings): Auth = new Auth(authSettings)
}

class Auth(authSettings: E2EAuthSettings) {
  private val credentialsToBeelineCredentials: Credentials => Option[BeelineCredentials] = {
    case p @ Credentials.Provided(s) =>
      import pdi.jwt._
      val decoded = JwtCirce.decodeJson(
        p.identifier,
        key = sys.env("AUTH0_SECRET"),
        algorithms = List(
          JwtAlgorithm.HS256,
          JwtAlgorithm.HS384,
          JwtAlgorithm.HS512
        )
      )

      for {
        json <- decoded.toOption
        claims <- json.asObject
        userIdJson <- claims.apply("userId")
        userIdNumber <- userIdJson.asNumber
        userId <- userIdNumber.toInt
      } yield User(userId)

    case _ => None
  }

  val authDirective: AuthenticationDirective[BeelineCredentials] =
    authenticateOAuth2("beeline.sg", credentialsToBeelineCredentials)
}
