package org.kae.pkgfolderaligner

import ammonite.ops._

object AlignerApp {

  def main(argv: Array[String]): Unit = {
    val projectRoot = home / 'mds / "api"
    val containingPackagePath = "com.banno.api"

    PackageFolderAligner.alignProject(
      projectRoot,
      PackageFolderAligner.ParsedPkgDecl.parsePackagePath(
        containingPackagePath),
      dryRun = false)
  }

}
