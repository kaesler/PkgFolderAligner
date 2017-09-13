package org.kae.pkgfolderaligner

object AlignerApp {

  def main(argv: Array[String]): Unit = {
    val projectRoot = home / 'mds / "api"
    PackageFolderAligner.alignProject(
      projectRoot,
      Some(ParsedPkgDecl(Vector("com", "banno", "api"))),
      reallyDoIt = false)
  }


}
