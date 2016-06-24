package com.example

import akka.actor.Actor
import spray.http.{StatusCode, HttpResponse}
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.routing._
import scala.util.Random
import spray.json._
import spray.util._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

  val MAX_BUDGET = 150000
  val rangeMin = 0.035
  val rangeMax = 0.055

  var remainingBudget: Float = MAX_BUDGET

  var lastBidTime: Long = _

  val rnd = new Random

  def uuid = java.util.UUID.randomUUID.toString

  def campaignBudgetRemaining(bid: Float) = {
    remainingBudget = remainingBudget - bid
  }


  case class BidResponse(auctionId: String, result: String, bid: String, currrency: String, creative: String, winning_notification: String)
  case class NoBidResponse(auctionId: String, result: String)


  object BidResponseJsonSupport extends DefaultJsonProtocol {
    implicit val bidResponseFormat = jsonFormat6(BidResponse.apply)
  }

  object NoBidResponseJsonSupport extends DefaultJsonProtocol {
    implicit val noBidResponseFormat = jsonFormat2(NoBidResponse.apply)
  }

  def getNextRandomValue = rangeMin + (rangeMax - rangeMin) * rnd.nextFloat()

  import BidResponseJsonSupport._
  import NoBidResponseJsonSupport._

  val myRoute =
    path("bid_request") {
      get {
        parameters('auction_id, 'ip, 'bundle_name, 'connection_type) { (auction_id, ip, bundle_name, connection_type) =>
          complete {
            val currentTime = System.currentTimeMillis()
            // Check the last bid time is no lesser than 3 secs. If the bid time is lesser than 3 secs,
            // then there will be no bid response. 
            if(remainingBudget > 0 && connection_type == "wifi" && (currentTime - lastBidTime) > 3000){
              lastBidTime = System.currentTimeMillis()
              val randBid = getNextRandomValue
              campaignBudgetRemaining(randBid.toFloat)
              HttpResponse(status = 200, entity =
                BidResponse(
                  auction_id,
                  "bid",
                  randBid.toString,
                  "USD",
                  "http://videos­bucket.com/video" + rnd.nextInt().toString + ".mov",
                  "http://my_dsp.com/winner/" + uuid
                ).toJson.prettyPrint
                )
            }else{
              HttpResponse(status = 200, entity =
                NoBidResponse(auction_id, "no_bid").toJson.prettyPrint
              )
            }
          }
        }
      }
    } ~ path("winner" / Segment) { auction_id =>
      get {
        complete {
          HttpResponse(status = 200, entity = "")
        }
      }
    }
}