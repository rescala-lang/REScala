package lofi_acl

import lofi_acl.ardt.datatypes.{AddWinsLastWriterWinsMap, Counter}
import org.scalacheck.{Arbitrary, Gen}
import test.rdts.DataGenerator as rdtGen

object DataGenerator {
  given arbCounter: Arbitrary[Counter] = Arbitrary(
    for
      pos <- Gen.mapOf(Gen.zip(rdtGen.arbId.arbitrary, rdtGen.smallNum))
      neg <- Gen.mapOf(Gen.zip(rdtGen.arbId.arbitrary, rdtGen.smallNum))
    yield Counter(pos, neg)
  )
}
