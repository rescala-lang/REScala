package rescala.operator

import rescala.extra.lenses.LensBundle

/** To support virtual State types, everything is put into the bundle traits.
  * But because the operators all have cyclic dependencies to each other,
  * we need this combining bundle, which all other operator bundles use as a self type
  * this is then the actual combinator of those bundles, ensuring that they
  * can access each other
  */
trait Operators extends AnyRef // to make the below more symmetric
    with EventBundle
    with SignalBundle
    with FoldBundle
    with FlattenBundle
    with SourceBundle
    with LensBundle {
  type BundleState[_]
}
