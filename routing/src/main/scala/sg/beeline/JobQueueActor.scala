package sg.beeline

import java.util.UUID

import akka.actor.{ActorRef, Actor}
import akka.pattern.ask
import akka.util.Timeout
import sg.beeline.JobQueueActor.{ResultPendingException, PollResult, InitRequest}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object JobQueueActor {
  case class InitRequest[T](t: T)
  case class PollResult(uuid: UUID)

  case class ResultPendingException() extends Exception
}

class JobQueueActor(actor: ActorRef)
                   (implicit val timeout: Timeout, implicit val executionContext : ExecutionContext)
  extends Actor {
  val jobQueue = new scala.collection.mutable.HashMap[UUID, Future[Any]]

  def receive = {
    case InitRequest(request) =>
      val uuid = UUID.randomUUID
      val future = actor ? request

      jobQueue.put(uuid, future)

      future onComplete {
        case _ =>
          import scala.concurrent.duration._
          akka.pattern.after(5 minutes, context.system.scheduler)(Future {
            jobQueue.remove(uuid)
          })
      }

      sender ! uuid.toString

    case PollResult(uuid) =>
      sender ! Try {
        val future = jobQueue(uuid)
        if (future.isCompleted)
          future.value.get.get
        else
          throw ResultPendingException()
      }

  }
}