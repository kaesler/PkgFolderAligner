import ammonite.ops._

// Code goes here.

@main
def main(scalaProjectRootDir: String, reallyDoIt: Boolean): Unit = {
  val projectRoot = Path(new java.io.File(scalaProjectRootDir).getAbsoluteFile)
  println(projectRoot)
  PackageFolderFixer.repair(projectRoot, reallyDoIt)
}
