package org.jetbrains.plugins.scala
package lang.refactoring.introduceField

import java.{util => ju}

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.namesSuggester.NameSuggester
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.lang.refactoring.util.{ScalaRefactoringUtil, _}

/**
 * Nikolay.Tropin
 * 7/15/13
 */
class IntroduceFieldContext[T <: PsiElement](val project: Project,
                                             val editor: Editor,
                                             val file: PsiFile,
                                             val element: T,
                                             val types: Array[ScType],
                                             val aClass: ScTemplateDefinition) {

  import IntroduceFieldContext._

  val occurrences: Array[TextRange] = element match {
    case expr: ScExpression =>
      getOccurrenceRanges(ScalaRefactoringUtil.unparExpr(expr), aClass.extendsBlock)
    case _ => null
  }

  private implicit val validator: ScalaVariableValidator = ScalaVariableValidator(file, element, occurrences)

  val reporter: ValidationReporter = new ValidationReporter(project, new DialogConflictsReporter {})

  val canBeInitInDecl: Boolean = element match {
    case expr: ScExpression => canBeInitializedInDeclaration(expr, aClass)
    case _ => throw new IntroduceException
  }

  val possibleNames: ju.Set[String] = element match {
    case expr: ScExpression =>
      import scala.collection.JavaConversions._
      NameSuggester.suggestNames(expr).toSet[String]
    case _ => throw new IntroduceException
  }

  def canBeInitLocally(replaceAll: Boolean): Boolean = {
    val occurrences = if (replaceAll) this.occurrences else Array(element.getTextRange)

    val parExpr: ScExpression = findParentExpr(commonParent(file, occurrences: _*))
    val stmtsAndMmbrs = statementsAndMembersInClass(aClass)

    val containerIsLocal = container(parExpr, file).withParentsInFile
      .exists(stmtsAndMmbrs.contains(_))
    if (!containerIsLocal) false
    else {
      element match {
        case expr: ScExpression => checkForwardReferences(expr, parExpr)
        case _ => false
      }
    }
  }
}

object IntroduceFieldContext {

  private def canBeInitializedInDeclaration(expr: ScExpression, aClass: ScTemplateDefinition): Boolean = {
    val stmtsAndMmbrs = statementsAndMembersInClass(aClass)
    expr.withParentsInFile
      .find(stmtsAndMmbrs.contains(_))
      .forall(checkForwardReferences(expr, _))
  }
}
