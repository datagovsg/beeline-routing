package sg.beeline.jobs

import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import sg.beeline.jobs.JobResultActor._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class JobQueue[JobSpec, ResultType](actorRef: ActorRef,
                                    validityDuration: FiniteDuration,
                                    timeoutDuration: FiniteDuration)
                                   (implicit executionContext: ExecutionContext) {
  val system = ActorSystem()
  val jobResultActor = system.actorOf(Props[JobResultActor[ResultType]])

  def enqueueJob(a: JobSpec): UUID = {
    implicit val timeout = new Timeout(timeoutDuration.toMillis, TimeUnit.MILLISECONDS)
    val uuid = UUID.randomUUID

    jobResultActor ! AddJob(uuid)

    val future = actorRef ? (uuid, a)

    future.onComplete { tryResult =>
      tryResult match {
        case Success(result) =>
          jobResultActor ! UpdateResult(uuid, result)
        case Failure(exc) =>
          jobResultActor ! UpdateException(uuid, exc)
      }
      implicit val timeout = Timeout(timeoutDuration.toMillis, TimeUnit.MILLISECONDS)
      akka.pattern.after(validityDuration, system.scheduler) {
        Future { jobResultActor ! RemoveJob(uuid) }
      }
    }

    uuid
  }

  def getStatus(uuid: UUID): Future[JobStatus[ResultType]] = {
    implicit val timeout = Timeout(timeoutDuration.toMillis, TimeUnit.MILLISECONDS)
    (jobResultActor ? GetStatus(uuid)).mapTo[JobStatus[ResultType]]
  }
}

object JobResultActor {
  sealed trait JobStatus[A]
  case class JobQueued[A]() extends JobStatus[A]
  case class JobNotFound[A]() extends JobStatus[A]
  // FIXME: currently we don't have anything to determine when the job
  // starts running. Is that even necessary?
  case class JobRunning[A]() extends JobStatus[A]
  case class JobFailed[A](e: Throwable) extends JobStatus[A]
  case class JobSucceeded[A](result: A) extends JobStatus[A]

  sealed trait JobStatusOps
  case class AddJob(uuid: UUID) extends JobStatusOps
  case class RemoveJob(uuid: UUID) extends JobStatusOps
  case class GetStatus(uuid: UUID) extends JobStatusOps
  case class UpdateResult(uuid: UUID, result: Any) extends JobStatusOps
  case class UpdateException(uuid: UUID, exception: Throwable) extends JobStatusOps
}

class JobResultActor[ResultType] extends Actor {
  val jobs = mutable.HashMap[UUID, JobStatus[ResultType]]()

  override def receive: Receive = {
    case AddJob(uuid) => jobs.put(uuid, JobQueued())
    case RemoveJob(uuid) => jobs.remove(uuid)
    case GetStatus(uuid) => sender ! jobs.getOrElse(uuid, JobNotFound())
    case UpdateResult(uuid, result: ResultType) => jobs.put(uuid, JobSucceeded(result))
    case UpdateException(uuid, exc: Throwable) => jobs.put(uuid, JobFailed(exc))
  }
}