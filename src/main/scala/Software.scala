/*
 * This file has code presenting basic approach for modeling software
 * delivery process as a programming.
 *
 * Purpose of this code is not to make anything runtime-useful, but to:
 * * document the process (explicitly)
 * * valdiate it against outcomes
 * * consider edge cases that might happen
 */
trait Artifact

trait Software extends Artifact

trait Specification extends Artifact

trait Requirement extends Artifact

trait Feedback extends Artifact
case class WellDone() extends Feedback
case class CriticalIssue() extends Feedback
case class MinorIssues() extends Feedback

trait SoftwareDeliveryProcess[Input] {
  def deliver(input: Input): Software
}

object Undone extends Software with Specification with Requirement with Feedback

object Waterfall extends SoftwareDeliveryProcess[List[Requirement]] {
  /* This is a simplest and quite easy-to-implement approach.
   * Essentialy its splitted down to 3 phases:
   *
   * 1. Specifying (creating a specification document)
   * 2. Implementing (and tested against specifiation what is missing)
   * 3. Integration testing as a whole (getting ready to ship it)
   */

 def deliver(requirements: List[Requirement]): Software = {
   val specification = specify(requirements)
   val software = implement(specification)

   test(specification, software) match {
     case WellDone() => software
     case testFeedback => fix(software, testFeedback, specification)
   }
 }

 def specify(requirement: List[Requirement]): Specification = Undone

 def implement(specification: Specification): Software = {
   def initialImplementation(specification: Specification): Software = Undone

   def implementIter(software: Software, feedback: Feedback): Software = {
     feedback match {
       case WellDone() => software
       case _: Feedback => {
         val fixedSoftware = fix(software, feedback, specification)
         implementIter(fixedSoftware, test(specification, fixedSoftware))
       }
     }
   }

   val initial = initialImplementation(specification)
   implementIter(initial, test(specification, initial)) 
 }

 def fix(software: Software, feedback: Feedback, specification: Specification): Software = Undone

 def test(specification: Specification, software: Software): Feedback = Undone
}
