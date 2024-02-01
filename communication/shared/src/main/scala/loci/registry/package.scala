package loci

package registry {
  class BindingCreator[T] private[registry] {
    def apply[R](name: String)(implicit builder: BindingBuilder[T, R]) = builder(name)
    def Value[R](name: String)(implicit builder: BindingBuilder.Value[T, R]) = builder(name)
  }
}

package object registry {
  def Binding[T] = new BindingCreator[T]
}
