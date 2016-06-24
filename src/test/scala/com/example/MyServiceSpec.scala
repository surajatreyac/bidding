package com.example

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._

class MyServiceSpec extends Specification with Specs2RouteTest with MyService {
  def actorRefFactory = system
  
  "MyService" should {

    "return a proper bid response for bid_request" in {
      Get("/bid_request") ~> myRoute ~> check {
        responseAs[String] must contain("Say hello")
      }
    }.pendingUntilFixed

    "return a proper winner response for winner request" in {
      Get("/winner/6c831376-c1df-43ef-a377-85d83aa3314c") ~> myRoute ~> check {
        responseAs[String] must contain("Say hello")
      }
    }.pendingUntilFixed
  }
}
