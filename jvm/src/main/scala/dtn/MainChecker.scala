package dtn


@main def start_checker_server() = {
  val checker = DotsConvergenceChecker(5000, "0.0.0.0")
  checker.run()
}
