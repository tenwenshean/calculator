import scala.swing.MenuBar.NoMenuBar.revalidate
import scala.swing._
import scala.swing.event._

object Calculator extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Scala Calculator"
    preferredSize = new Dimension(300, 400)

    // Main screen buttons
    val normalCalcButton = new Button("Normal Calculation")
    val dataStructButton = new Button("Data Structures and Algorithms")
    val discreteMathButton = new Button("Simple Discrete Math")

    val mainPanel = new GridPanel(3, 1) {
      contents += normalCalcButton
      contents += dataStructButton
      contents += discreteMathButton
      preferredSize = new Dimension(300, 400)
    }

    // Calculator UI elements
    val display = new TextField {
      columns = 10
      editable = false
      preferredSize = new Dimension(280, 50)
    }

    val buttons = List(
      "7", "8", "9", "/",
      "4", "5", "6", "*",
      "1", "2", "3", "-",
      "0", ".", "=", "+"
    ).map(new Button(_))

    var currentNumber: String = ""
    var previousNumber: String = ""
    var operator: String = ""

    val buttonPanel = new GridPanel(4, 4) {
      buttons.foreach(contents += _)
      preferredSize = new Dimension(280, 300)
    }

    // Back button
    val backButton = new Button("Back")

    // Menu bar
    var myMenuBar = new MenuBar {
      contents += new Menu("Options") {
        contents += new MenuItem("Normal Calculation") {
          action = Action("Normal Calculation") {
            showCalculator()
          }
        }
        contents += new MenuItem("Data Structures and Algorithms") {
          action = Action("Data Structures and Algorithms") {
            showDataStructuresAndAlgorithmsMenu()
          }
        }
        contents += new MenuItem("Simple Discrete Math") {
          action = Action("Simple Discrete Math") {
            showDiscreteMathMenu()
          }
        }
        contents += new MenuItem("Exit") {
          action = Action("Exit") {
            sys.exit(0)
          }
        }
      }
    }
    menuBar = myMenuBar

    def showMainMenu(): Unit = {
      contents = mainPanel
      revalidate()
    }

    def showCalculator(): Unit = {
      contents = new BorderPanel {
        layout(new BorderPanel {
          layout(display) = BorderPanel.Position.North
          layout(buttonPanel) = BorderPanel.Position.Center
          layout(backButton) = BorderPanel.Position.South
        }) = BorderPanel.Position.Center
      }
      revalidate()
    }

    def showDataStructuresAndAlgorithmsMenu(): Unit = {
      contents = new BorderPanel {
        layout(new Label("Data Structures and Algorithms Menu")) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }
      revalidate()
    }

    def showDiscreteMathMenu(): Unit = {
      contents = new BorderPanel {
        layout(new Label("Simple Discrete Math Menu")) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }
      revalidate()
    }

    // Button actions
    listenTo(normalCalcButton, dataStructButton, discreteMathButton, backButton)
    listenTo(buttons: _*)

    reactions += {
      case ButtonClicked(`normalCalcButton`) =>
        showCalculator()

      case ButtonClicked(`dataStructButton`) =>
        showDataStructuresAndAlgorithmsMenu()

      case ButtonClicked(`discreteMathButton`) =>
        showDiscreteMathMenu()

      case ButtonClicked(`backButton`) =>
        showMainMenu()

      case ButtonClicked(b) if b.text.forall(_.isDigit) || b.text == "." =>
        currentNumber += b.text
        display.text = currentNumber

      case ButtonClicked(b) if "+-*/".contains(b.text) =>
        if (operator.isEmpty) {
          operator = b.text
          previousNumber = currentNumber
          currentNumber = ""
        } else {
          computeResult()
          operator = b.text
        }

      case ButtonClicked(b) if b.text == "=" =>
        computeResult()

      case ButtonClicked(b) if b.text == "C" =>
        currentNumber = ""
        previousNumber = ""
        operator = ""
        display.text = ""
    }

    def computeResult(): Unit = {
      if (previousNumber.nonEmpty && currentNumber.nonEmpty && operator.nonEmpty) {
        val result = operator match {
          case "+" => previousNumber.toDouble + currentNumber.toDouble
          case "-" => previousNumber.toDouble - currentNumber.toDouble
          case "*" => previousNumber.toDouble * currentNumber.toDouble
          case "/" =>
            if (currentNumber.toDouble == 0) {
              Dialog.showMessage(top, "Cannot divide by zero!", title = "Error")
              0.0
            } else {
              previousNumber.toDouble / currentNumber.toDouble
            }
        }
        display.text = result.toString
        currentNumber = result.toString
        previousNumber = ""
        operator = ""
      }
    }

    // Initial screen
    contents = mainPanel
  }
}
