package lore
import lore.ast.*
import lore.backends.ViperBackend
import munit.FunSuite

class ViperBackendSuite extends FunSuite {
  test("Imports") {
    import java.nio.charset.StandardCharsets
    import java.nio.file.Path

    val compiled = ViperBackend.compileAsSingleFile(List(TViperImport(Path.of("deps/calendar_header.vpr"))))
    val expectation =
      String(getClass.getClassLoader.getResourceAsStream("importstest.viper").readAllBytes, StandardCharsets.UTF_8)

    assertEquals(compiled, expectation)
  }
}
