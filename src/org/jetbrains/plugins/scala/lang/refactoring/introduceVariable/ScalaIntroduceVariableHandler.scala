package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceVariable

import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.command.impl.StartMarkAction
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.editor.{Editor, SelectionModel}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util._
import com.intellij.psi._
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.refactoring.HelpID
import com.intellij.refactoring.util.CommonRefactoringUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.types._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.lang.refactoring.util.{DialogConflictsReporter, ScalaRefactoringUtil}


/**
  * User: Alexander Podkhalyuzin
  * Date: 23.06.2008
  */

class ScalaIntroduceVariableHandler extends ScalaRefactoringActionHandler with DialogConflictsReporter with IntroduceExpressions with IntroduceTypeAlias {
  var occurrenceHighlighters = Seq.empty[RangeHighlighter]

  override def invoke(file: PsiFile)
                     (implicit project: Project, editor: Editor, dataContext: DataContext): Unit = {
    val offset = editor.getCaretModel.getOffset

    implicit val selectionModel: SelectionModel = editor.getSelectionModel
    implicit val psiFile: PsiFile = file

    def hasSelection = selectionModel.hasSelection

    trimSpacesAndComments(editor, file)

    val selectedElement: Option[PsiElement] = findSelectedTypeElement.orElse(findSelectedExpression)

    def getTypeElementAtOffset = {
      def isExpression = {
        val element: PsiElement = file.findElementAt(offset) match {
          case w: PsiWhiteSpace if w.getTextRange.getStartOffset == offset &&
            w.getText.contains("\n") => file.findElementAt(offset - 1)
          case p => p
        }
        ScalaRefactoringUtil.getExpressions(element).nonEmpty
      }

      def findTypeElement(offset: Int) =
        if (!hasSelection && !isExpression)
          Option(PsiTreeUtil.findElementOfClassAtOffset(file, offset, classOf[ScTypeElement], false))
        else
          None

      file.findElementAt(offset) match {
        case w: PsiWhiteSpace if w.getTextRange.getStartOffset == offset => findTypeElement(offset - 1)
        case _ => findTypeElement(offset)
      }
    }

    if (hasSelection && selectedElement.isEmpty) {
      val message = ScalaBundle.message("cannot.refactor.not.expression.nor.type")
      CommonRefactoringUtil.showErrorHint(project, editor, message, INTRODUCE_VARIABLE_REFACTORING_NAME, HelpID.INTRODUCE_VARIABLE)
      return
    }

    //clear data on startRefactoring, if there is no marks, but there is some data
    if (StartMarkAction.canStart(project) == null) {
      editor.putUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO, new IntroduceTypeAliasData())
    }

    val maybeTypeElement = selectedElement match {
      case Some(te: ScTypeElement) => Option(te)
      case _ => getTypeElementAtOffset
    }

    if (maybeTypeElement.isDefined) {
      val typeElement = maybeTypeElement.get
      if (editor.getUserData(IntroduceTypeAlias.REVERT_TYPE_ALIAS_INFO).isData) {
        invokeOnTypeElement(typeElement)
      } else {
        afterTypeElementChoosing(project, editor, file, dataContext, typeElement, INTRODUCE_TYPEALIAS_REFACTORING_NAME) {
          invokeOnTypeElement
        }
      }
    } else {
      afterExpressionChoosing(project, editor, file, dataContext, INTRODUCE_VARIABLE_REFACTORING_NAME, checkCanBeIntroduced(_)) {
        invokeOnSelection(file)
      }
    }
  }
}

object ScalaIntroduceVariableHandler {
  val REVERT_INFO: Key[ScalaRefactoringUtil.RevertInfo] = new Key("RevertInfo")
}