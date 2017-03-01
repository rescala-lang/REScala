package rescala.pipelining.tests

import org.scalatest.FlatSpec
import rescala.pipelining.PipelineEngine
import rescala.reactives.{Signals, Var}

class TransitiveResolvingTest extends FlatSpec {

  ignore should "resolve Transitive Conflict" in {
    implicit val engine = new PipelineEngine()

    /*
     * This tests uses the following topology which allows three
     * parallel updated with a transitive waiting relation
     *
     * S1    S2   S3
     * | \  / \  / |
     * |  \/   \/  |
     * |  /\   /\  |
     * | /  \ /  \ |
     * D1   D2    D3
     */

    val s1 = Var(0)
    val s2 = Var(0)
    val s3 = Var(0)
    val d1 = Signals.static(s1, s2) { implicit t => s1.pulse.get - s2.pulse.get }
    val d2 = Signals.static(s1, s3) { implicit t => s1.pulse.get - s3.pulse.get }
    val d3 = Signals.static(s2, s3) { implicit t => s2.pulse.get - s3.pulse.get }
  }


}
