package lore
import munit.FunSuite
import lore.AST.*
import lore.backends.ViperBackend

class ViperBackendSuite extends FunSuite:
  test("Imports"):
    import java.nio.file.{Path}
    import java.nio.charset.StandardCharsets

    val compiled = ViperBackend.compileAsSingleFile(List(TViperImport(Path.of("deps/calendar_header.vpr"))))
    val expectation = String(getClass.getClassLoader.getResourceAsStream("importstest.viper").readAllBytes,  StandardCharsets.UTF_8)

    assertEquals(compiled, expectation)