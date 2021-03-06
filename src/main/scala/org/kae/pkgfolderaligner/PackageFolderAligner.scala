package org.kae.pkgfolderaligner

import scala.io.Codec

import scalaz._

import ammonite.ops._

/**
  * Function to align the directory structure of an SBT project with
  * the package structure. It will not edit files but only move
  * them. If any files need editing before it can proceed it will
  * report the problems requiring manual repair, and do nothing.
  *
  * TODO: Use "git mv" when appropriate
  */
object PackageFolderAligner {

  /**
    * If possible without changing any source files, align the
    * directory structure of an SBT project with the package structure.
    * It will not edit files but only move them.  If any files need
    * editing before it can proceed it will report the problems
    * requiring manual repair, and do nothing.
    *
    * @param projectRoot the root of the project upon which to operate
    * @param requiredRootPackage the package path under which all code should reside
    * @param dryRun if true then dry run only
    */
  def alignProject(projectRoot: Path,
                   requiredRootPackage: Option[ParsedPkgDecl],
                   dryRun: Boolean): Unit = {

    requireToBeAScalaProject(projectRoot)

    val attemptedMoves =
      for {
        tree <- List(
          projectRoot / 'src / 'main / 'scala,
          projectRoot / 'src / 'test / 'scala,
          projectRoot / 'src / 'it / 'scala
        )
        if tree.toIO.isDirectory
        move <- movesForScalaSrcTree(tree, requiredRootPackage)
      } yield move

    val moves = attemptedMoves collect { case \/-(move)       => move }
    val badMoves = attemptedMoves collect { case -\/(problem) => problem }

    val problems = badMoves ++
      clashingMoves(moves) ++
      mkdirProblems(moves)

    if (problems.nonEmpty) {
      println(
        s"Cannot move about ${moves.size} files until these problems are repaired manually:"
      )
      println(
        problems
          .map(_.show)
          .mkString("\n"))
    } else {
      println(moves.mkString("\n"))
      if (!dryRun) {
        println(s"Moving ${moves.size} files...")
        moves foreach {
          _.execute()
        }
        println(s"Moving ${moves.size} files...done")
      } else {
        println(s"${moves.size} files to be moved")
      }
    }
  }

  private def clashingMoves(
      moves: List[Move]
  ): Iterable[MultipleSourcesForDestination] = {
    moves
      .groupBy { case Move(_, dst) => dst }
      .collect {
        case (dst, ms) if ms.size > 1 =>
          MultipleSourcesForDestination(dst, moves.map(_.source))
      }
  }

  private def mkdirProblems(moves: List[Move]): List[Problem] = {
    for {
      move <- moves
      dst = move.destination
      if dst.toIO.isFile
    } yield MkDirWouldFailDueToFileInTheWay(dst)
  }

  private def requireToBeAScalaProject(dir: Path): Unit = {
    if (!dir.toIO.isDirectory) {
      fail(s"Not an existing directory: $dir")
    }

    val expectedDirs = List(
      dir / 'src / 'main / 'scala,
      dir / 'src / 'test / 'scala,
      dir / 'src / 'it / 'scala
    ).distinct

    val foundDirs = expectedDirs filter { _.toIO.isDirectory }
    if (foundDirs.isEmpty) {
      fail(
        s"Not a Scala project because there is no directory src/main/scala, src/test/scala nor" +
          s" src/it/scala"
      )
    }
  }

  private def movesForScalaSrcTree(
      tree: Path,
      requiredRootPackage: Option[ParsedPkgDecl]
  ): Seq[Problem \/ Move] = {
    for {
      scalaFile <- ls.rec ! tree |? (_.ext == "scala")
      moveOrProblem <- moveFor(scalaFile, tree, requiredRootPackage)
    } yield moveOrProblem
  }

  private def moveFor(
      file: Path,
      tree: Path,
      requiredRootPackage: Option[ParsedPkgDecl]
  ): Option[Problem \/ Move] = {
    canonicalParentFor(file, tree, requiredRootPackage)
      .fold(
        problem => Some(-\/(problem)),
        canonicalParent => {
          val currentParent = file / up
          if (canonicalParent != currentParent) {
            Some(\/-(Move(file, canonicalParent / file.segments.last)))
          } else {
            None
          }
        }
      )
  }

  private def canonicalParentFor(
      srcPath: Path,
      tree: Path,
      requiredRootPackage: Option[ParsedPkgDecl]
  ): Problem \/ Path = {
    for {
      pkg <- homePackage(srcPath)
      correctlyScopedPkg <- requiredRootPackage match {
        case None => \/-(pkg)
        case Some(requiredRoot) =>
          if (requiredRoot.isPrefixOf(pkg)) {
            \/-(pkg)
          } else {
            -\/(PackageNotUnderRequiredRoot(srcPath, pkg))
          }
      }

    } yield tree / correctlyScopedPkg.parts
  }

  private def homePackage(path: Path): Problem \/ ParsedPkgDecl = {
    val lines = path.getLines(Codec.UTF8)

    val pkgDecls = packageDeclarations(lines)
    val pkgObjectDecls = packageObjectDeclarations(lines)

    if (pkgDecls.isEmpty) {
      -\/(NoPackageDeclarations(path))
    } else {

      // Handle this correctly:
      //  package a
      //  package b
      //  package c
      // ==> belongs in a.b.c

      val netPackageDecl = pkgDecls.reduce(_ ++ _)

      if (path.segments.last == "package.scala") {

        // Case: it is called package.scala

        if (pkgObjectDecls.isEmpty) {
          -\/(PackageFileLacksPackageObject(path))
        } else if (pkgObjectDecls.size > 1) {
          -\/(PackageFileWithMultiplePackageObjects(path))
        } else {
          \/-(netPackageDecl + pkgObjectDecls.head.name)
        }
      } else {

        // Case: it is not called package.scala

        \/-(netPackageDecl)
      }
    }
  }

  private def packageDeclarations(
      lines: Vector[String]
  ): Vector[ParsedPkgDecl] = {
    for {
      line <- lines
      decl <- ParsedPkgDecl.parseLine(line)
    } yield decl
  }

  private def packageObjectDeclarations(
      lines: Vector[String]
  ): Vector[ParsedPkgObjectDecl] = {
    (for {
      line <- lines
      decl <- ParsedPkgObjectDecl.parseLine(line)
    } yield decl).distinct
  }

  private case class Move(source: Path, destination: Path) {
    def execute(): Unit = {
      val parent = destination / up
      if (!parent.toIO.isDirectory) {
        mkdir ! parent
        println(s"Created $parent")
      }
      mv(source, destination)
      println(s"Moved $source to $destination")
    }
  }

  case class ParsedPkgDecl(parts: Vector[String]) {
    def ++(other: ParsedPkgDecl): ParsedPkgDecl = copy(
      parts = this.parts ++ other.parts
    )

    def +(name: String): ParsedPkgDecl = copy(parts = this.parts :+ name)

    def isPrefixOf(that: ParsedPkgDecl): Boolean = {
      val zipped = this.parts zip that.parts
      zipped.size == this.parts.size &&
      zipped.forall { case (p, q) => p == q }
    }

    override def toString: String = {
      s""""package ${parts.mkString(".")}""""
    }

    def toShortString: String = s"""${parts.mkString(".")}"""
  }

  object ParsedPkgDecl {
    private val regexp = """^package\s+([a-zA-Z][a-zA-Z0-9\._]*)$""".r

    /**
      * Parse a line containing a package decl
      * @param line something like "package a.b.c"
      * @return the [[ParsedPkgDecl]] if found
      */
    def parseLine(line: String): Option[ParsedPkgDecl] = line match {
      case regexp(path) => parsePackagePath(path)
      case _            => None
    }

    /**
      * Parse a string containing a package path
      * @param path something like "a.b.c"
      * @return
      */
    def parsePackagePath(path: String): Option[ParsedPkgDecl] = {
      val parts = path.split('.')
      if (parts.isEmpty) {
        None
      } else {
        Some(ParsedPkgDecl(parts.toVector))
      }
    }
  }

  private case class ParsedPkgObjectDecl(name: String)

  private object ParsedPkgObjectDecl {
    private val regexp =
      """^package\s+(object\s+)?([a-zA-Z][a-zA-Z0-9_]*).*\{$""".r

    def parseLine(line: String): Option[ParsedPkgObjectDecl] = line match {
      case regexp(_, name) => Some(ParsedPkgObjectDecl(name))
      case _               => None
    }
  }

  sealed trait Problem {
    def show: String
  }
  private case class MkDirWouldFailDueToFileInTheWay(path: Path)
      extends Problem {
    override def show: String =
      s"This needs to be a directory but is a file: $path"
  }
  private case class MultipleSourcesForDestination(destination: Path,
                                                   sources: List[Path])
      extends Problem {

    override def show: String =
      s"Multiple files need to be moved to the same destination, $destination: " +
        s"${sources.mkString(", ")}"
  }
  private case class NoPackageDeclarations(path: Path) extends Problem {
    override def show: String =
      s"No package declarations found in $path"
  }
  private case class PackageFileLacksPackageObject(path: Path) extends Problem {
    override def show: String =
      s"package file lacks a package object: $path"
  }
  private case class PackageFileWithMultiplePackageObjects(path: Path)
      extends Problem {
    override def show: String =
      s"package file has multiple package objects: $path"
  }
  private case class PackageNotUnderRequiredRoot(path: Path,
                                                 parsedPkgDecl: ParsedPkgDecl)
      extends Problem {

    override def show: String =
      s"""Package for file $path is "${parsedPkgDecl.toShortString}", """ +
        s"which is outside of the expected containing package"
  }

  private def fail(msg: String): Nothing =
    sys.error(s"$msg. Exiting. Repair and rerun.")
}
