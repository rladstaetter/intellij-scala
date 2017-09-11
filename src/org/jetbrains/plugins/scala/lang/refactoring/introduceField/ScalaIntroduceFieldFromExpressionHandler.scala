package org.jetbrains.plugins.scala
package lang.refactoring.introduceField

import com.intellij.codeInsight.navigation.NavigationUtil
import com.intellij.ide.util.PsiClassListCellRenderer
import com.intellij.internal.statistic.UsageTrigger
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.markup.RangeHighlighter
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.search.PsiElementProcessor
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.refactoring.HelpID
import com.intellij.refactoring.util.CommonRefactoringUtil
import org.jetbrains.plugins.scala.extensions.childOf
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScEarlyDefinitions
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{ScExtendsBlock, ScTemplateParents}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.refactoring.ScalaRefactoringActionHandler
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.ScalaUtils


/**
  * Nikolay.Tropin
  * 6/27/13
  */
class ScalaIntroduceFieldFromExpressionHandler extends ScalaRefactoringActionHandler {

  private var occurrenceHighlighters = Seq.empty[RangeHighlighter]

  val REFACTORING_NAME = ScalaBundle.message("introduce.field.title")

  import ScalaIntroduceFieldFromExpressionHandler._

  override def invoke(file: PsiFile)
                     (implicit project: Project, editor: Editor, dataContext: DataContext): Unit = {
    afterExpressionChoosing(project, editor, file, dataContext, REFACTORING_NAME, checkCanBeIntroduced(_)) {
      trimSpacesAndComments(editor, file)
      invokeOnSelection(file)
    }
  }

  def invokeOnSelection(file: PsiFile)
                       (implicit project: Project, editor: Editor): Unit = {
    try {
      UsageTrigger.trigger(ScalaBundle.message("introduce.field.id"))
      PsiDocumentManager.getInstance(project).commitAllDocuments()

      val scalaFile = checkFile(file, REFACTORING_NAME)
      val selectionModel = editor.getSelectionModel
      val (expr, types) = getExpression(project, editor, scalaFile, selectionModel.getSelectionStart, selectionModel.getSelectionEnd).getOrElse {
        showErrorMessage(ScalaBundle.message("cannot.refactor.not.expression"))
        return
      }

      afterClassChoosing[ScExpression](expr, types, project, editor, file, "Choose class for Introduce Field") {
        convertExpressionToField
      }
    }
    catch {
      case _: IntroduceException =>
    }
  }

  def convertExpressionToField(ifc: IntroduceFieldContext[ScExpression]) {
    implicit val project: Project = ifc.project
    implicit val editor: Editor = ifc.editor

    val possiblePlace = checkCanBeIntroduced(ifc.element, showErrorMessage)
    if (!possiblePlace) return

    def runWithDialog() {
      val settings = new IntroduceFieldSettings(ifc)
      if (!settings.canBeInitInDeclaration && !settings.canBeInitLocally) {
        showErrorMessage("Cannot create field from this expression")
      } else {
        val dialog = getDialog(ifc, settings)
        if (dialog.isOK) {
          runRefactoring(ifc, settings)
        }
      }
    }

    runWithDialog()
  }

  private def afterClassChoosing[T <: PsiElement](elem: T, types: Array[ScType], project: Project, editor: Editor, file: PsiFile, title: String)
                                                 (action: IntroduceFieldContext[T] => Unit) {
    try {
      val classes = ScalaPsiUtil.getParents(elem, file).collect {
        case t: ScTemplateDefinition => t
      }.toArray[PsiClass]
      classes.length match {
        case 0 =>
        case 1 => action(new IntroduceFieldContext[T](project, editor, file, elem, types, classes(0).asInstanceOf[ScTemplateDefinition]))
        case _ =>
          val selection = classes(0)
          val processor = new PsiElementProcessor[PsiClass] {
            def execute(aClass: PsiClass): Boolean = {
              action(new IntroduceFieldContext[T](project, editor, file, elem, types, aClass.asInstanceOf[ScTemplateDefinition]))
              false
            }
          }
          NavigationUtil.getPsiElementPopup(classes, new PsiClassListCellRenderer() {
            override def getElementText(element: PsiClass): String = super.getElementText(element).replace("$", "")
          }, title, processor, selection).showInBestPositionFor(editor)
      }
    }
    catch {
      case _: IntroduceException =>
    }
  }

  private def runRefactoringInside(ifc: IntroduceFieldContext[ScExpression], settings: IntroduceFieldSettings[ScExpression]) {
    implicit val project: Project = ifc.project
    implicit val editor: Editor = ifc.editor

    val expression = expressionToIntroduce(ifc.element)
    val mainOcc = ifc.occurrences.filter(_.getStartOffset == editor.getSelectionModel.getSelectionStart)
    val occurrencesToReplace = if (settings.replaceAll) ifc.occurrences else mainOcc
    val aClass = ifc.aClass
    val checkAnchor: PsiElement = anchorForNewDeclaration(expression, occurrencesToReplace, aClass)
    if (checkAnchor == null) {
      showErrorMessage("Cannot find place for the new field")
      return
    }
    implicit val projectContext: ProjectContext = aClass.projectContext
    val name = settings.name
    val typeName = Option(settings.scType).map(_.canonicalText).getOrElse("")
    val replacedOccurences = replaceOccurences(occurrencesToReplace, name, ifc.file)

    val anchor = anchorForNewDeclaration(expression, replacedOccurences, aClass)
    val initInDecl = settings.initInDeclaration
    var createdDeclaration: PsiElement = null
    if (initInDecl) {
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, expression)
    } else {
      val underscore = createExpressionFromText("_")
      createdDeclaration = createDeclaration(name, typeName, settings.defineVar, underscore)

      anchorForInitializer(replacedOccurences, ifc.file) match {
        case Some(anchorForInit) =>
          val parent = anchorForInit.getParent
          val assignStmt = createExpressionFromText(s"$name = ${expression.getText}")
          parent.addBefore(assignStmt, anchorForInit)
          parent.addBefore(createNewLine(), anchorForInit)
        case None => throw new IntroduceException

      }
    }

    settings.visibilityLevel match {
      case "" =>
      case other =>
        val modifier = createModifierFromText(other)
        createdDeclaration.asInstanceOf[ScMember].getModifierList.add(modifier)
    }

    lazy val document: Document = editor.getDocument

    anchor match {
      case (_: ScTemplateParents) childOf (extBl: ScExtendsBlock) =>
        val earlyDef = extBl.addEarlyDefinitions()
        createdDeclaration = earlyDef.addAfter(createdDeclaration, earlyDef.getFirstChild)
      case _ childOf (ed: ScEarlyDefinitions) if onOneLine(document, ed.getTextRange) =>
        def isBlockStmtOrMember(elem: PsiElement) = elem != null && (elem.isInstanceOf[ScBlockStatement] || elem.isInstanceOf[ScMember])

        var declaration = createdDeclaration.getText
        if (isBlockStmtOrMember(anchor)) declaration += "; "
        if (isBlockStmtOrMember(anchor.getPrevSibling)) declaration = "; " + declaration
        document.insertString(anchor.getTextRange.getStartOffset, declaration)
        PsiDocumentManager.getInstance(project).commitDocument(document)
      case _ childOf parent =>
        createdDeclaration = parent.addBefore(createdDeclaration, anchor)
        parent.addBefore(createNewLine(), anchor)
    }

    ScalaPsiUtil.adjustTypes(createdDeclaration)
  }

  def runRefactoring(ifc: IntroduceFieldContext[ScExpression], settings: IntroduceFieldSettings[ScExpression]) {
    val runnable = new Runnable {
      def run(): Unit = runRefactoringInside(ifc, settings)
    }
    ScalaUtils.runWriteAction(runnable, ifc.project, REFACTORING_NAME)
    ifc.editor.getSelectionModel.removeSelection()
  }

  protected def getDialog(ifc: IntroduceFieldContext[ScExpression], settings: IntroduceFieldSettings[ScExpression]): ScalaIntroduceFieldDialog = {
    val occCount = ifc.occurrences.length
    // Add occurrences highlighting
    if (occCount > 1)
      occurrenceHighlighters = highlightOccurrences(ifc.project, ifc.occurrences, ifc.editor)

    val dialog = new ScalaIntroduceFieldDialog(ifc, settings)
    dialog.show()
    if (!dialog.isOK) {
      if (occCount > 1) {
        occurrenceHighlighters.foreach(_.dispose())
        occurrenceHighlighters = Seq.empty
      }
    }
    dialog
  }

  private def showErrorMessage(text: String)
                              (implicit project: Project, editor: Editor): Unit = {
    CommonRefactoringUtil.showErrorHint(project, editor, text, REFACTORING_NAME, HelpID.INTRODUCE_FIELD)
  }
}

object ScalaIntroduceFieldFromExpressionHandler {

  private def anchorForInitializer(occurences: Array[TextRange], file: PsiFile): Option[PsiElement] = {
    var firstRange = occurences(0)
    val commonParent = ScalaRefactoringUtil.commonParent(file, occurences: _*)

    val parExpr = ScalaRefactoringUtil.findParentExpr(commonParent)
    if (parExpr == null) return None
    val container: PsiElement = ScalaRefactoringUtil.container(parExpr, file)
    val needBraces = !parExpr.isInstanceOf[ScBlock] && ScalaRefactoringUtil.needBraces(parExpr, ScalaRefactoringUtil.nextParent(parExpr, file))
    val parent =
      if (needBraces) {
        firstRange = firstRange.shiftRight(1)
        parExpr.replaceExpression(createExpressionFromText(s"{${parExpr.getText}}")(file.getManager),
          removeParenthesis = false)
      } else container
    if (parent == null) None
    else parent.getChildren.find(_.getTextRange.contains(firstRange))
  }

  private def anchorForNewDeclaration(expr: ScExpression, occurrences: Array[TextRange], aClass: ScTemplateDefinition): PsiElement = {
    val commonParent = ScalaRefactoringUtil.commonParent(aClass.getContainingFile, occurrences: _*)
    val firstOccOffset = occurrences.map(_.getStartOffset).min
    val anchor = ScalaRefactoringUtil.statementsAndMembersInClass(aClass).find(_.getTextRange.getEndOffset >= firstOccOffset)
    anchor.getOrElse {
      if (PsiTreeUtil.isAncestor(aClass.extendsBlock.templateBody.orNull, commonParent, false)) null
      else {
        aClass.extendsBlock match {
          case ScExtendsBlock.EarlyDefinitions(earlyDef) => earlyDef.getLastChild
          case extBl => extBl.templateParents.orNull
        }
      }
    }
  }

  private def onOneLine(document: Document, range: TextRange): Boolean =
    document.getLineNumber(range.getStartOffset) == document.getLineNumber(range.getEndOffset)
}