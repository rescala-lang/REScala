package loci
package compatibility

import scala.collection.convert.{AsJavaExtensions, AsScalaExtensions}

private[loci] object jdkCollectionConverters extends AsJavaExtensions with AsScalaExtensions
