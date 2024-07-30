import scala.swing._
import scala.swing.event._
import java.awt.{Color, Font, Graphics2D, RenderingHints}
import scala.swing.MenuBar.NoMenuBar.revalidate


abstract class Calculator {
  def calculate(a: Double, b: Double, operator: String): Double
}

class BasicCalculator extends Calculator {
  override def calculate(a: Double, b: Double, operator: String): Double = {
    operator match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" =>
        if (b == 0) throw new ArithmeticException("Cannot divide by zero")
        else a / b
      case _ => throw new IllegalArgumentException("Unknown operator")
    }
  }
}

class CalculatorUI(calculator: Calculator) extends SimpleSwingApplication {
  private var currentNumber: String = ""
  private var previousNumber: String = ""
  private var operator: String = ""
  private var displayText: String = ""
  private var isResultShown: Boolean = false

  val dataStructuresAndAlgorithms = new DataStructuresAndAlgorithms()
  val sorting = new dataStructuresAndAlgorithms.Sorting()

  val travel = new Travel()

  def top = new MainFrame {
    title = "Scala Calculator"
    preferredSize = new Dimension(1000, 800)

    // Main screen buttons
    val normalCalcButton = new Button("Normal Calculation")
    val dataStructButton = new Button("Data Structures and Algorithms")

    val mainPanel = new GridPanel(2, 1) {
      contents += normalCalcButton
      contents += dataStructButton
      preferredSize = new Dimension(600, 700)
    }

    // Calculator UI elements
    val display = new TextField {
      columns = 10
      editable = false
      preferredSize = new Dimension(580, 50)
    }

    val buttons = List(
      "7", "8", "9", "/",
      "4", "5", "6", "*",
      "1", "2", "3", "-",
      "0", ".", "=", "+",
      "C"
    ).map(new Button(_))

    val buttonPanel = new GridPanel(5, 4) {
      buttons.foreach(contents += _)
      preferredSize = new Dimension(580, 300)
    }

    // Back button
    val backButton = new Button("Back")

    // Menu bar
    val myMenuBar = new MenuBar {
      contents += new Menu("Options") {
        contents += new MenuItem(Action("Normal Calculation") {
          showCalculator()
        })
        contents += new MenuItem(Action("Data Structures and Algorithms") {
          showDataStructuresAndAlgorithmsMenu()
        })
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    menuBar = myMenuBar

    def showMainMenu(): Unit = {
      contents = mainPanel
      revalidate()
      repaint()
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

    def computeResult(): Unit = {
      if (previousNumber.nonEmpty && currentNumber.nonEmpty && operator.nonEmpty) {
        try {
          val result = calculator.calculate(previousNumber.toDouble, currentNumber.toDouble, operator)
          displayText += s" = ${result.toString}"
          display.text = displayText
          currentNumber = result.toString
          previousNumber = ""
          operator = ""
          isResultShown = true
        } catch {
          case e: Exception => Dialog.showMessage(top, e.getMessage, title = "Error")
        }
      }
    }

    def clearCalculator(): Unit = {
      displayText = ""
      currentNumber = ""
      previousNumber = ""
      operator = ""
      display.text = displayText
      isResultShown = false
    }

    // Initial screen
    contents = mainPanel

    // Button actions
    listenTo(normalCalcButton, dataStructButton, backButton)
    listenTo(buttons: _*)

    reactions += {
      case ButtonClicked(`normalCalcButton`) =>
        showCalculator()

      case ButtonClicked(`dataStructButton`) =>
        showDataStructuresAndAlgorithmsMenu()

      case ButtonClicked(`backButton`) =>
        showMainMenu()

      case ButtonClicked(b) if b.text.forall(_.isDigit) || b.text == "." =>
        if (isResultShown) {
          clearCalculator()
        }
        currentNumber += b.text
        displayText += b.text
        display.text = displayText
        isResultShown = false

      case ButtonClicked(b) if "+-*/".contains(b.text) =>
        if (isResultShown) {
          previousNumber = currentNumber
          displayText = currentNumber
          isResultShown = false
        }
        if (operator.isEmpty) {
          operator = b.text
          previousNumber = currentNumber
          currentNumber = ""
          displayText += s" ${b.text} "
          display.text = displayText
        } else {
          computeResult()
          operator = b.text
          displayText += s" ${b.text} "
          display.text = displayText
        }

      case ButtonClicked(b) if b.text == "=" =>
        computeResult()

      case ButtonClicked(b) if b.text == "C" =>
        clearCalculator()
    }

    def showDataStructuresAndAlgorithmsMenu(): Unit = {
      val sortingButton = new Button("Sorting")
      val dataStructuresButton = new Button("DataStructure")
      val graphButton = new Button("Graph")

      contents = new BorderPanel {
        layout(new GridPanel(3, 1) {
          contents += sortingButton
          contents += dataStructuresButton
          contents += graphButton
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(sortingButton, dataStructuresButton, graphButton)
      reactions += {
        case ButtonClicked(`sortingButton`) => showSortingMenu()
        case ButtonClicked(`dataStructuresButton`) => showDataStructuresMenu()
        case ButtonClicked(`graphButton`) => showBSTMenu()
      }

      revalidate()
    }
    def showSortingMenu(): Unit = {
      val bubbleSortButton = new Button("Bubble Sort")    // button for bubble sort
      val selectionSortButton = new Button("Selection Sort") // button for selection sort
      val insertionSortButton = new Button("Insertion Sort") // button for insertion sort
      val shellSortButton = new Button("Shell Sort") // button for shell sort
      val mergeSortButton = new Button("Merge Sort") // button for merge sort


      contents = new BorderPanel {
        layout(new GridPanel(6, 1) {
          contents += bubbleSortButton
          contents += selectionSortButton
          contents += insertionSortButton
          contents += shellSortButton
          contents += mergeSortButton
          contents += new Label("Select a sorting algorithm")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }
      listenTo(bubbleSortButton, selectionSortButton, insertionSortButton, shellSortButton, mergeSortButton)
      reactions += {
        case ButtonClicked(`bubbleSortButton`) => showBubbleSort()
        case ButtonClicked(`selectionSortButton`) => showSelectionSort()
        case ButtonClicked(`insertionSortButton`) => showInsertionSort()
        case ButtonClicked(`shellSortButton`) => showShellSort()
        case ButtonClicked(`mergeSortButton`) => showMergeSort()

      }

      revalidate()
    }


    //UI FOR BUBBLE SORT

    def showBubbleSort(): Unit = {
      val inputField = new TextField {
        columns = 30
      }
      val sortButton = new Button("Start Bubble Sort")
      val previousButton = new Button("Previous")
      val nextButton = new Button("Next")
      val resultLabel = new Label("Enter up to 20 numbers and click 'Start Bubble Sort'")
      val explanationLabel = new Label("Explanation will appear here")

      var numbers: Array[Int] = Array()
      var sortingSteps: List[(Array[Int], String)] = List() // Now includes explanations
      var currentStep = 0

      val chart = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          if (sortingSteps.nonEmpty && currentStep < sortingSteps.length) {
            val (step, _) = sortingSteps(currentStep)
            val maxValue = step.max
            val minValue = step.min
            val width = size.width / step.length

            g.setFont(new Font("Arial", Font.BOLD, 14))

            for (i <- step.indices) {
              val heightRatio = (step(i) - minValue).toDouble / (maxValue - minValue).max(1)
              val height = (heightRatio * (size.height - 120)).toInt.max(40) // Reduced max height to make room for position labels

              g.setColor(new Color(100, 149, 237))
              g.fillRect(i * width, size.height - height - 20, width - 1, height) // Moved up by 20 pixels
              g.setColor(Color.BLACK)
              g.drawRect(i * width, size.height - height - 20, width - 1, height) // Moved up by 20 pixels

              // Draw the value inside the bar
              val valueStr = step(i).toString
              val fontMetrics = g.getFontMetrics
              val valueWidth = fontMetrics.stringWidth(valueStr)
              val valueHeight = fontMetrics.getHeight

              val valueX = i * width + (width - valueWidth) / 2
              val valueY = size.height - (height / 2) + (valueHeight / 2) - 20 // Moved up by 20 pixels

              g.setColor(Color.WHITE)
              for (dx <- -1 to 1; dy <- -1 to 1) {
                g.drawString(valueStr, valueX + dx, valueY + dy)
              }

              g.setColor(Color.BLACK)
              g.drawString(valueStr, valueX, valueY)

              // Draw the array position below the bar
              val positionStr = i.toString
              val positionWidth = fontMetrics.stringWidth(positionStr)
              val positionX = i * width + (width - positionWidth) / 2
              val positionY = size.height - 5 // 5 pixels from the bottom

              g.setColor(Color.BLACK)
              g.drawString(positionStr, positionX, positionY)
            }
          }
        }
      }

      contents = new BorderPanel {
        layout(new GridPanel(7, 1) { // Increased to 7 to accommodate the explanation label
          contents += new Label("Enter up to 20 numbers separated by spaces:")
          contents += inputField
          contents += sortButton
          contents += chart
          contents += new FlowPanel(previousButton, nextButton)
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(sortButton, previousButton, nextButton)
      reactions += {
        case ButtonClicked(`sortButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(20)
            if (numbers.length > 20) {
              resultLabel.text = "Only the first 20 numbers were used."
            } else {
              resultLabel.text = s"Sorting ${numbers.length} numbers."
            }
            sortingSteps = sorting.bubbleSort(numbers)
            currentStep = 0
            resultLabel.text += s" Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          } catch {
            case _: NumberFormatException =>
              resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }
        case ButtonClicked(`previousButton`) =>
          if (currentStep > 0) {
            currentStep -= 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }
        case ButtonClicked(`nextButton`) =>
          if (currentStep < sortingSteps.length - 1) {
            currentStep += 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }
      }

      revalidate()
    }



    //UI FOR SELECTION SORT

    def showSelectionSort(): Unit = {
      val inputField = new TextField { columns = 30 }
      val sortButton = new Button("Start Selection Sort")
      val previousButton = new Button("Previous")
      val nextButton = new Button("Next")
      val resultLabel = new Label("Enter up to 20 numbers and click 'Start Selection Sort'")
      val explanationLabel = new Label("Explanation will appear here")
      var numbers: Array[Int] = Array()
      var sortingSteps: List[(Array[Int], String)] = List()
      var currentStep = 0



      val chart = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          if (sortingSteps.nonEmpty && currentStep < sortingSteps.length) {
            val (step, _) = sortingSteps(currentStep)
            val maxValue = step.max
            val minValue = step.min
            val width = size.width / step.length
            g.setFont(new Font("Arial", Font.BOLD, 14))
            for (i <- step.indices) {
              val heightRatio = (step(i) - minValue).toDouble / (maxValue - minValue).max(1)
              val height = (heightRatio * (size.height - 120)).toInt.max(40)
              g.setColor(new Color(100, 149, 237))
              g.fillRect(i * width, size.height - height - 20, width - 1, height)
              g.setColor(Color.BLACK)
              g.drawRect(i * width, size.height - height - 20, width - 1, height)

              // Draw the value inside the bar
              val valueStr = step(i).toString
              val fontMetrics = g.getFontMetrics
              val valueWidth = fontMetrics.stringWidth(valueStr)
              val valueHeight = fontMetrics.getHeight
              val valueX = i * width + (width - valueWidth) / 2
              val valueY = size.height - (height / 2) + (valueHeight / 2) - 20
              g.setColor(Color.WHITE)
              g.drawString(valueStr, valueX, valueY)

              // Draw the array position below the bar
              val positionStr = i.toString
              val positionWidth = fontMetrics.stringWidth(positionStr)
              val positionX = i * width + (width - positionWidth) / 2
              val positionY = size.height - 5
              g.setColor(Color.BLACK)
              g.drawString(positionStr, positionX, positionY)
            }
          }
        }
      }

      contents = new BorderPanel {
        layout(new GridPanel(7, 1) {
          contents += new Label("Enter up to 20 numbers separated by spaces:")
          contents += inputField
          contents += sortButton
          contents += chart
          contents += new FlowPanel(previousButton, nextButton)
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(sortButton, previousButton, nextButton)
      reactions += {
        case ButtonClicked(`sortButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(20)
            if (numbers.length > 20) {
              resultLabel.text = "Only the first 20 numbers were used."
            } else {
              resultLabel.text = s"Sorting ${numbers.length} numbers."
              sortingSteps = sorting.selectionSort(numbers)
              currentStep = 0
              resultLabel.text += s" Step ${currentStep + 1} of ${sortingSteps.length}"
              explanationLabel.text = sortingSteps(currentStep)._2
              chart.repaint()
            }
          } catch {
            case _: NumberFormatException => resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }

        case ButtonClicked(`previousButton`) =>
          if (currentStep > 0) {
            currentStep -= 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }

        case ButtonClicked(`nextButton`) =>
          if (currentStep < sortingSteps.length - 1) {
            currentStep += 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }
      }
    }


    //UI FOR INSERTION SORT

    def showInsertionSort(): Unit = {
      val inputField = new TextField { columns = 30 }
      val sortButton = new Button("Start Insertion Sort")
      val previousButton = new Button("Previous")
      val nextButton = new Button("Next")
      val resultLabel = new Label("Enter up to 20 numbers and click 'Start Insertion Sort'")
      val explanationLabel = new Label("Explanation will appear here")
      var numbers: Array[Int] = Array()
      var sortingSteps: List[(Array[Int], String)] = List()
      var currentStep = 0

      val chart = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          if (sortingSteps.nonEmpty && currentStep < sortingSteps.length) {
            val (step, _) = sortingSteps(currentStep)
            val maxValue = step.max
            val minValue = step.min
            val width = size.width / step.length
            g.setFont(new Font("Arial", Font.BOLD, 14))
            for (i <- step.indices) {
              val heightRatio = (step(i) - minValue).toDouble / (maxValue - minValue).max(1)
              val height = (heightRatio * (size.height - 120)).toInt.max(40)
              g.setColor(new Color(100, 149, 237))
              g.fillRect(i * width, size.height - height - 20, width - 1, height)
              g.setColor(Color.BLACK)
              g.drawRect(i * width, size.height - height - 20, width - 1, height)

              // Draw the value inside the bar
              val valueStr = step(i).toString
              val fontMetrics = g.getFontMetrics
              val valueWidth = fontMetrics.stringWidth(valueStr)
              val valueHeight = fontMetrics.getHeight
              val valueX = i * width + (width - valueWidth) / 2
              val valueY = size.height - (height / 2) + (valueHeight / 2) - 20
              g.setColor(Color.WHITE)
              g.drawString(valueStr, valueX, valueY)

              // Draw the array position below the bar
              val positionStr = i.toString
              val positionWidth = fontMetrics.stringWidth(positionStr)
              val positionX = i * width + (width - positionWidth) / 2
              val positionY = size.height - 5
              g.setColor(Color.BLACK)
              g.drawString(positionStr, positionX, positionY)
            }
          }
        }
      }

      contents = new BorderPanel {
        layout(new GridPanel(7, 1) {
          contents += new Label("Enter up to 20 numbers separated by spaces:")
          contents += inputField
          contents += sortButton
          contents += chart
          contents += new FlowPanel(previousButton, nextButton)
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(sortButton, previousButton, nextButton)
      reactions += {
        case ButtonClicked(`sortButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(20)
            if (numbers.length > 20) {
              resultLabel.text = "Only the first 20 numbers were used."
            } else {
              resultLabel.text = s"Sorting ${numbers.length} numbers."
              sortingSteps = sorting.insertionSort(numbers)
              currentStep = 0
              resultLabel.text += s" Step ${currentStep + 1} of ${sortingSteps.length}"
              explanationLabel.text = sortingSteps(currentStep)._2
              chart.repaint()
            }
          } catch {
            case _: NumberFormatException => resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }

        case ButtonClicked(`previousButton`) =>
          if (currentStep > 0) {
            currentStep -= 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }

        case ButtonClicked(`nextButton`) =>
          if (currentStep < sortingSteps.length - 1) {
            currentStep += 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }
      }
    }

    //UI FOR SHELLSORT

    def showShellSort(): Unit = {
      val inputField = new TextField { columns = 30 }
      val sortButton = new Button("Start Shell Sort")
      val previousButton = new Button("Previous")
      val nextButton = new Button("Next")
      val resultLabel = new Label("Enter up to 20 numbers and click 'Start Shell Sort'")
      val explanationLabel = new Label("Explanation will appear here")
      var numbers: Array[Int] = Array()
      var sortingSteps: List[(Array[Int], String)] = List()
      var currentStep = 0

      val chart = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          if (sortingSteps.nonEmpty && currentStep < sortingSteps.length) {
            val (step, _) = sortingSteps(currentStep)
            val maxValue = step.max
            val minValue = step.min
            val width = size.width / step.length
            g.setFont(new Font("Arial", Font.BOLD, 14))
            for (i <- step.indices) {
              val heightRatio = (step(i) - minValue).toDouble / (maxValue - minValue).max(1)
              val height = (heightRatio * (size.height - 120)).toInt.max(40)
              g.setColor(new Color(100, 149, 237))
              g.fillRect(i * width, size.height - height - 20, width - 1, height)
              g.setColor(Color.BLACK)
              g.drawRect(i * width, size.height - height - 20, width - 1, height)

              // Draw the value inside the bar
              val valueStr = step(i).toString
              val fontMetrics = g.getFontMetrics
              val valueWidth = fontMetrics.stringWidth(valueStr)
              val valueHeight = fontMetrics.getHeight
              val valueX = i * width + (width - valueWidth) / 2
              val valueY = size.height - (height / 2) + (valueHeight / 2) - 20
              g.setColor(Color.WHITE)
              g.drawString(valueStr, valueX, valueY)

              // Draw the array position below the bar
              val positionStr = i.toString
              val positionWidth = fontMetrics.stringWidth(positionStr)
              val positionX = i * width + (width - positionWidth) / 2
              val positionY = size.height - 5
              g.setColor(Color.BLACK)
              g.drawString(positionStr, positionX, positionY)
            }
          }
        }
      }

      contents = new BorderPanel {
        layout(new GridPanel(7, 1) {
          contents += new Label("Enter up to 20 numbers separated by spaces:")
          contents += inputField
          contents += sortButton
          contents += chart
          contents += new FlowPanel(previousButton, nextButton)
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(sortButton, previousButton, nextButton)
      reactions += {
        case ButtonClicked(`sortButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(20)
            if (numbers.length > 20) {
              resultLabel.text = "Only the first 20 numbers were used."
            } else {
              resultLabel.text = s"Sorting ${numbers.length} numbers."
              sortingSteps = sorting.shellSort(numbers)
              currentStep = 0
              resultLabel.text += s" Step ${currentStep + 1} of ${sortingSteps.length}"
              explanationLabel.text = sortingSteps(currentStep)._2
              chart.repaint()
            }
          } catch {
            case _: NumberFormatException => resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }

        case ButtonClicked(`previousButton`) =>
          if (currentStep > 0) {
            currentStep -= 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }

        case ButtonClicked(`nextButton`) =>
          if (currentStep < sortingSteps.length - 1) {
            currentStep += 1
            resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
            explanationLabel.text = sortingSteps(currentStep)._2
            chart.repaint()
          }
      }
    }

    //UI FOR MERGESORT

    def showMergeSort(): Unit = {
      val inputField = new TextField { columns = 30 }
      val sortButton = new Button("Start Merge Sort")
      val previousButton = new Button("Previous")
      val nextButton = new Button("Next")
      val clearButton = new Button("Clear")
      val resultLabel = new Label("Enter up to 8 numbers and click 'Start Merge Sort'")
      val explanationLabel = new Label("Explanation will appear here")
      var numbers: Array[Int] = Array()
      var sortingSteps: List[(Array[Int], String)] = List()
      var currentStep = 0

      val chart = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          if (sortingSteps.nonEmpty && currentStep < sortingSteps.length) {
            val stepHeight = size.height / sortingSteps.length
            val barHeight = (stepHeight * 0.6).toInt // Fixed bar height
            g.setFont(new Font("Arial", Font.BOLD, 10))

            for ((step, stepIndex) <- sortingSteps.take(currentStep + 1).zipWithIndex) {
              val (array, explanation) = step
              val stepWidth = size.width / array.length

              for (i <- array.indices) {
                val x = i * stepWidth
                val y = stepIndex * stepHeight + (stepHeight - barHeight) / 2

                // Set color based on whether it's the current step
                g.setColor(if (stepIndex == currentStep) new Color(100, 149, 237) else new Color(200, 200, 200))
                g.fillRect(x.toInt, y.toInt, stepWidth.toInt - 1, barHeight)
                g.setColor(Color.BLACK)
                g.drawRect(x.toInt, y.toInt, stepWidth.toInt - 1, barHeight)

                val valueStr = array(i).toString
                val fontMetrics = g.getFontMetrics
                val valueX = x + (stepWidth - fontMetrics.stringWidth(valueStr)) / 2
                val valueY = y + barHeight / 2 + fontMetrics.getHeight / 2
                g.setColor(Color.BLACK)
                g.drawString(valueStr, valueX.toInt, valueY.toInt)
              }

              // Draw explanation
              g.setColor(Color.BLACK)
              g.drawString(explanation, 10, stepIndex * stepHeight + stepHeight - 5)
            }
          }
        }
      }
      val scrollPane = new ScrollPane(chart)
      scrollPane.preferredSize = new Dimension(600, 400)


      contents = new BorderPanel {
        layout(new GridPanel(1, 1) {
          contents += new GridPanel(2, 1) {
            contents += new FlowPanel(new Label("Enter up to 8 numbers separated by spaces:"), inputField)
            contents += new FlowPanel(sortButton, clearButton, previousButton, nextButton)
          }
        }) = BorderPanel.Position.North
        layout(scrollPane) = BorderPanel.Position.Center
        layout(new GridPanel(2, 1) {
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.South
        layout(backButton)  = BorderPanel.Position.South
      }

      listenTo(sortButton, previousButton, nextButton, clearButton)
      reactions += {
        case ButtonClicked(`sortButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(8)
            if (numbers.length > 8) {
              resultLabel.text = "Only the first 8 numbers were used."
            } else {
              resultLabel.text = s"Sorting ${numbers.length} numbers."
              sortingSteps = sorting.mergeSort(numbers)
              currentStep = 0
              updateVisualization()
            }
          } catch {
            case _: NumberFormatException => resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }

        case ButtonClicked(`previousButton`) =>
          if (currentStep > 0) {
            currentStep -= 1
            updateVisualization()
          }

        case ButtonClicked(`nextButton`) =>
          if (currentStep < sortingSteps.length - 1) {
            currentStep += 1
            updateVisualization()
          }

        case ButtonClicked(`clearButton`) =>
          inputField.text = ""
          numbers = Array()
          sortingSteps = List()
          currentStep = 0
          resultLabel.text = "Enter up to 8 numbers and click 'Start Merge Sort'"
          explanationLabel.text = "Explanation will appear here"
          chart.repaint()
      }

      def updateVisualization(): Unit = {
        resultLabel.text = s"Step ${currentStep + 1} of ${sortingSteps.length}"
        explanationLabel.text = sortingSteps(currentStep)._2
        chart.repaint()
      }
    }
    def showDataStructuresMenu(): Unit = {
      val stackButton = new Button("Stack Visualization")
      val queueButton = new Button("Queue Visualization")

      contents = new BorderPanel {
        layout(new GridPanel(3, 1) {
          contents += stackButton
          contents += queueButton
          contents += new Label("Select a data structure")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South

      }
      listenTo(stackButton, queueButton )
      reactions += {
        case ButtonClicked(`stackButton`) => showStackVisualization()
        case ButtonClicked(`queueButton`) => showQueueVisualization()

      }

      revalidate()
    }

    def showStackVisualization(): Unit = {
      var stack: List[Int] = List()
      val inputField = new TextField {
        columns = 10
      }
      val pushButton = new Button("Push")
      val popButton = new Button("Pop")
      val clearButton = new Button("Clear")
      val resultLabel = new Label("Stack operations will be shown here")

      val stackPanel = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          val boxWidth = 60
          val boxHeight = 60
          var startX = 20 // Starting X position
          val startY = (size.height - boxHeight) / 2 - 20 // Centered Y position, moved up to make room for position labels

          g.setFont(new Font("Arial", Font.BOLD, 14))

          stack.zipWithIndex.foreach { case (item, index) =>
            g.setColor(new Color(100, 149, 237))
            g.fillRect(startX, startY, boxWidth, boxHeight)
            g.setColor(Color.BLACK)
            g.drawRect(startX, startY, boxWidth, boxHeight)

            val itemStr = item.toString
            val fontMetrics = g.getFontMetrics
            val textWidth = fontMetrics.stringWidth(itemStr)
            val textHeight = fontMetrics.getHeight
            g.drawString(itemStr, startX + (boxWidth - textWidth) / 2, startY + boxHeight - (boxHeight - textHeight) / 2)

            // Draw array position below the box
            val positionStr = index.toString
            val positionWidth = fontMetrics.stringWidth(positionStr)
            g.drawString(positionStr, startX + (boxWidth - positionWidth) / 2, startY + boxHeight + 20)

            startX += boxWidth + 5 // Move right for the next box
          }

          // Draw arrow pointing to the right side of the rightmost element
          if (stack.nonEmpty) {
            g.setColor(Color.RED)
            val arrowX = startX - 5 // Just to the right of the last box
            val arrowY = startY + boxHeight / 2
            val arrowLength = 30

            // Horizontal line
            g.drawLine(arrowX, arrowY, arrowX + arrowLength, arrowY)

            // Arrowhead
            g.drawLine(arrowX + arrowLength, arrowY, arrowX + arrowLength - 5, arrowY - 5)
            g.drawLine(arrowX + arrowLength, arrowY, arrowX + arrowLength - 5, arrowY + 5)

            // "Top" label
            g.drawString("Top", arrowX + arrowLength + 5, arrowY + 5)
          }
        }
      }

      def updateStackVisualization(): Unit = {
        stackPanel.repaint()
        resultLabel.text = s"Current stack: ${stack.reverse.mkString(", ")}"
      }

      contents = new BorderPanel {
        layout(new GridPanel(6, 1) {
          contents += new Label("Enter a number to push onto the stack:")
          contents += inputField
          contents += new FlowPanel(pushButton, popButton, clearButton)
          contents += stackPanel
          contents += resultLabel
          contents += new Label("Note: Stack grows from left to right. Rightmost element is the top.")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(pushButton, popButton, clearButton)
      reactions += {
        case ButtonClicked(`pushButton`) =>
          try {
            val num = inputField.text.toInt
            stack = stack :+ num // Append to the end of the list
            resultLabel.text = s"Pushed $num onto the stack"
            inputField.text = ""
            updateStackVisualization()
          } catch {
            case _: NumberFormatException =>
              resultLabel.text = "Invalid input. Please enter a valid integer."
          }

        case ButtonClicked(`popButton`) =>
          stack match {
            case init :+ last =>
              stack = init
              resultLabel.text = s"Popped $last from the stack"
              updateStackVisualization()
            case Nil =>
              resultLabel.text = "Cannot pop from an empty stack"
          }

        case ButtonClicked(`clearButton`) =>
          stack = List()
          resultLabel.text = "Stack cleared"
          updateStackVisualization()
      }
      updateStackVisualization()
      revalidate()
    }

    def showQueueVisualization(): Unit = {
      var queue: List[Int] = List()
      val inputField = new TextField {
        columns = 10
      }
      val enqueueButton = new Button("Enqueue")
      val dequeueButton = new Button("Dequeue")
      val clearButton = new Button("Clear")
      val resultLabel = new Label("Queue operations will be shown here")

      val queuePanel = new Panel {
        preferredSize = new Dimension(580, 400)

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          val boxWidth = 60
          val boxHeight = 60
          var startX = 20 // Starting X position
          val startY = (size.height - boxHeight) / 2 - 20 // Centered Y position, moved up to make room for position labels

          g.setFont(new Font("Arial", Font.BOLD, 14))

          queue.zipWithIndex.foreach { case (item, index) =>
            g.setColor(new Color(100, 149, 237))
            g.fillRect(startX, startY, boxWidth, boxHeight)
            g.setColor(Color.BLACK)
            g.drawRect(startX, startY, boxWidth, boxHeight)

            val itemStr = item.toString
            val fontMetrics = g.getFontMetrics
            val textWidth = fontMetrics.stringWidth(itemStr)
            val textHeight = fontMetrics.getHeight
            g.drawString(itemStr, startX + (boxWidth - textWidth) / 2, startY + boxHeight - (boxHeight - textHeight) / 2)

            // Draw array position below the box
            val positionStr = index.toString
            val positionWidth = fontMetrics.stringWidth(positionStr)
            g.drawString(positionStr, startX + (boxWidth - positionWidth) / 2, startY + boxHeight + 20)

            startX += boxWidth + 5 // Move right for the next box
          }

          // Draw arrow pointing to the right side of the rightmost element
          if (queue.nonEmpty) {
            g.setColor(Color.RED)
            val arrowX = startX - 5 // Just to the right of the last box
            val arrowY = startY + boxHeight / 2
            val arrowLength = 30

            // Horizontal line
            g.drawLine(arrowX, arrowY, arrowX + arrowLength, arrowY)

            // Arrowhead
            g.drawLine(arrowX + arrowLength, arrowY, arrowX + arrowLength - 5, arrowY - 5)
            g.drawLine(arrowX + arrowLength, arrowY, arrowX + arrowLength - 5, arrowY + 5)

            // "Rear" label
            g.drawString("Rear", arrowX + arrowLength + 5, arrowY + 5)
          }

          // Draw arrow pointing to the left side of the leftmost element
          if (queue.nonEmpty) {
            g.setColor(Color.GREEN)
            val arrowX = 20 - 5 // Just to the left of the first box
            val arrowY = startY + boxHeight / 2
            val arrowLength = 30

            // Horizontal line
            g.drawLine(arrowX, arrowY, arrowX - arrowLength, arrowY)

            // Arrowhead
            g.drawLine(arrowX - arrowLength, arrowY, arrowX - arrowLength + 5, arrowY - 5)
            g.drawLine(arrowX - arrowLength, arrowY, arrowX - arrowLength + 5, arrowY + 5)

            // "Front" label
            g.drawString("Front", arrowX - arrowLength - 40, arrowY + 5)
          }
        }
      }

      def updateQueueVisualization(): Unit = {
        queuePanel.repaint()
        resultLabel.text = s"Current queue: ${queue.mkString(", ")}"
      }

      contents = new BorderPanel {
        layout(new GridPanel(6, 1) {
          contents += new Label("Enter a number to enqueue into the queue:")
          contents += inputField
          contents += new FlowPanel(enqueueButton, dequeueButton, clearButton)
          contents += queuePanel
          contents += resultLabel
          contents += new Label("Note: Queue grows from left to right. Leftmost element is the front.")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(enqueueButton, dequeueButton, clearButton)
      reactions += {
        case ButtonClicked(`enqueueButton`) =>
          try {
            val num = inputField.text.toInt
            queue = queue :+ num // Append to the end of the list
            resultLabel.text = s"Enqueued $num into the queue"
            inputField.text = ""
            updateQueueVisualization()
          } catch {
            case _: NumberFormatException =>
              resultLabel.text = "Invalid input. Please enter a valid integer."
          }

        case ButtonClicked(`dequeueButton`) =>
          queue match {
            case head :: tail =>
              queue = tail
              resultLabel.text = s"Dequeued $head from the queue"
              updateQueueVisualization()
            case Nil =>
              resultLabel.text = "Cannot dequeue from an empty queue"
          }

        case ButtonClicked(`clearButton`) =>
          queue = List()
          resultLabel.text = "Queue cleared"
          updateQueueVisualization()
      }
      updateQueueVisualization()
      revalidate()
    }

    //BST UI PART

    def showBSTMenu(): Unit = {
      var bst: TreeNode = null
      val inputField = new TextField {
        columns = 30
      }
      val insertButton = new Button("Insert")
      val clearButton = new Button("Clear")
      val showInorderButton = new Button("Show Inorder")
      val showPreorderButton = new Button("Show Preorder")
      val showPostorderButton = new Button("Show Postorder")
      val resultLabel = new Label("Enter up to 7 numbers separated by spaces and click 'Insert'")


      def drawTree(g: Graphics2D, node: TreeNode, x: Int, y: Int, hGap: Int): Unit = {
        if (node != null) {
          g.setColor(new Color(100, 149, 237))
          g.fillOval(x - 15, y - 15, 30, 30)
          g.setColor(Color.BLACK)
          g.drawOval(x - 15, y - 15, 30, 30)
          g.drawString(node.value.toString, x - 7, y + 5)

          if (node.left != null) {
            g.drawLine(x - 10, y + 10, x - hGap + 10, y + 50)
            drawTree(g, node.left, x - hGap, y + 60, hGap / 2)
          }
          if (node.right != null) {
            g.drawLine(x + 10, y + 10, x + hGap - 10, y + 50)
            drawTree(g, node.right, x + hGap, y + 60, hGap / 2)
          }
        }
      }

      val treePanel = new Panel {
        override def preferredSize: Dimension = {
          val depth = travel.treeDepth(bst)
          val width = math.pow(2, depth).toInt * 80 // Adjust width based on depth
          new Dimension(width, depth * 80) // Adjust height based on depth
        }

        override def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawTree(g, bst, size.width / 2, 30, size.width / 4)
        }
      }

      def showTraversal(traversalType: String, traversal: List[Int]): Unit = {
        if (traversal.isEmpty) {
          Dialog.showMessage(contents.head, "The BST is empty", s"$traversalType Traversal", Dialog.Message.Info)
        } else {
          val traversalWithPositions = traversal.zipWithIndex.map { case (value, index) => s"${index + 1}: $value" }
          val message = s"$traversalType Traversal:\n${traversalWithPositions.mkString("\n")}"
          Dialog.showMessage(contents.head, message, s"$traversalType Traversal", Dialog.Message.Info)
        }
      }

      contents = new BorderPanel {
        layout(new GridPanel(3, 1) {
          contents += new Label("Enter up to 7 numbers separated by spaces:")
          contents += inputField
          contents += new FlowPanel(insertButton, clearButton)
          contents += new FlowPanel(showInorderButton, showPreorderButton, showPostorderButton)
          contents += treePanel
          contents += resultLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(insertButton, clearButton, showInorderButton, showPreorderButton, showPostorderButton)
      reactions += {
        case ButtonClicked(`insertButton`) =>
          try {
            val numbers = inputField.text.split(" ").map(_.trim.toInt).take(7)
            bst = null
            for (num <- numbers) {
              if (travel.countNodes(bst) < 7) {
                bst = sorting.insert(bst, num)
              }
            }
            if (numbers.length > 7) {
              resultLabel.text = "Only the first 7 numbers were used."
            } else {
              resultLabel.text = s"Inserted ${numbers.length} numbers into the BST."
            }
            treePanel.revalidate() // Ensure the panel revalidates to update the size
            treePanel.repaint()
          } catch {
            case _: NumberFormatException =>
              resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }
        case ButtonClicked(`clearButton`) =>
          bst = null
          inputField.text = ""
          resultLabel.text = "BST cleared"
          treePanel.revalidate() // Ensure the panel revalidates to update the size
          treePanel.repaint()
        case ButtonClicked(`showInorderButton`) =>
          showTraversal("Inorder", travel.inorderTraversal(bst))
        case ButtonClicked(`showPreorderButton`) =>
          showTraversal("Preorder", travel.preorderTraversal(bst))
        case ButtonClicked(`showPostorderButton`) =>
          showTraversal("Postorder", travel.postorderTraversal(bst))
      }

      revalidate()
    }

    // Add button action listeners here
    buttons.foreach { button =>
      button.reactions += {
        case ButtonClicked(`button`) =>
        // Handle button clicks
      }
    }
  }
}

class DataStructuresAndAlgorithms extends  {

  class Sorting {
    def bubbleSort(arr: Array[Int]): List[(Array[Int], String)] = {
      var steps = List((arr.clone(), "Initial array"))
      val n = arr.length
      for (i <- 0 until n - 1) {
        for (j <- 0 until n - i - 1) {
          if (arr(j) > arr(j + 1)) {
            val temp = arr(j)
            arr(j) = arr(j + 1)
            arr(j + 1) = temp
            steps = (arr.clone(), s"Swapped ${arr(j)} and ${arr(j + 1)} at positions $j and ${j + 1}") :: steps
          } else {
            steps = (arr.clone(), s"Compared ${arr(j)} and ${arr(j + 1)} at positions $j and ${j + 1}, no swap needed") :: steps
          }
        }
        steps = (arr.clone(), s"Completed pass ${i + 1}. Largest unsorted element (${arr(n - i - 1)}) is now in its correct position.") :: steps
      }
      steps.reverse
    }

    def selectionSort(arr: Array[Int]): List[(Array[Int], String)] = {
      var steps = List((arr.clone(), "Initial array"))
      val n = arr.length
      for (i <- 0 until n - 1) {
        var minIndex = i
        for (j <- i + 1 until n) {
          if (arr(j) < arr(minIndex)) {
            minIndex = j
          }
        }
        if (minIndex != i) {
          val temp = arr(i)
          arr(i) = arr(minIndex)
          arr(minIndex) = temp
          steps = (arr.clone(), s"Swapped ${arr(i)} and ${arr(minIndex)} at positions $i and $minIndex") :: steps
        }
        steps = (arr.clone(), s"Completed pass ${i + 1}") :: steps
      }
      steps.reverse
    }

    def insertionSort(arr: Array[Int]): List[(Array[Int], String)] = {
      var steps = List((arr.clone(), "Initial array"))
      val n = arr.length
      for (i <- 1 until n) {
        val key = arr(i)
        var j = i - 1
        while (j >= 0 && arr(j) > key) {
          arr(j + 1) = arr(j)
          steps = (arr.clone(), s"Moved ${arr(j)} from position $j to position ${j + 1}") :: steps
          j -= 1
        }
        arr(j + 1) = key
        steps = (arr.clone(), s"Inserted ${key} at position ${j + 1}") :: steps
      }
      steps.reverse
    }

    def shellSort(arr: Array[Int]): List[(Array[Int], String)] = {
      var steps = List((arr.clone(), "Initial array"))
      val n = arr.length
      var gap = n / 2

      while (gap > 0) {
        for (i <- gap until n) {
          val temp = arr(i)
          var j = i
          while (j >= gap && arr(j - gap) > temp) {
            arr(j) = arr(j - gap)
            steps = (arr.clone(), s"Moved ${arr(j - gap)} from position ${j - gap} to position $j with gap $gap") :: steps
            j -= gap
          }
          arr(j) = temp
          steps = (arr.clone(), s"Inserted ${temp} at position $j") :: steps
        }
        steps = (arr.clone(), s"Reduced gap to ${gap / 2}") :: steps
        gap /= 2
      }
      steps.reverse
    }

    def mergeSort(arr: Array[Int]): List[(Array[Int], String)] = {
      def merge(left: Array[Int], right: Array[Int]): (Array[Int], String) = {
        var result = Array[Int]()
        var i = 0
        var j = 0

        while (i < left.length && j < right.length) {
          if (left(i) <= right(j)) {
            result :+= left(i)
            i += 1
          } else {
            result :+= right(j)
            j += 1
          }
        }

        result ++= left.drop(i)
        result ++= right.drop(j)
        (result, s"Merged ${left.mkString(", ")} and ${right.mkString(", ")}")
      }

      def sort(arr: Array[Int]): List[(Array[Int], String)] = {
        if (arr.length <= 1) {
          List((arr.clone(), s"Single element or empty array: ${arr.mkString(", ")}"))
        } else {
          val mid = arr.length / 2
          val left = arr.take(mid)
          val right = arr.drop(mid)
          val leftSteps = sort(left)
          val rightSteps = sort(right)
          val (merged, mergeExplanation) = merge(leftSteps.last._1, rightSteps.last._1)

          val combinedSteps = leftSteps.dropRight(1) ++ rightSteps.dropRight(1)
          val sortedLeft = leftSteps.last._1
          val sortedRight = rightSteps.last._1
          val partialSort = sortedLeft ++ sortedRight

          combinedSteps :+
            (partialSort, s"Sorted left (${sortedLeft.mkString(", ")}) and right (${sortedRight.mkString(", ")})") :+
            (merged, mergeExplanation)
        }
      }

      List((arr.clone(), "Initial unsorted array")) ++ sort(arr)
    }


    def insert(root: TreeNode, value: Int): TreeNode = {
      if (root == null) new TreeNode(value)
      else {
        if (value < root.value) root.left = insert(root.left, value)
        else if (value > root.value) root.right = insert(root.right, value)
        root
      }
    }

  }
}

class TreeNode(var value: Int) {
  var left: TreeNode = null
  var right: TreeNode = null

}

class Travel {

  def inorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else inorderTraversal(root.left) ++ List(root.value) ++ inorderTraversal(root.right)
  }

  def preorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else List(root.value) ++ preorderTraversal(root.left) ++ preorderTraversal(root.right)
  }

  def postorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil
    else postorderTraversal(root.left) ++ postorderTraversal(root.right) ++ List(root.value)
  }

  def countNodes(root: TreeNode): Int = {
    if (root == null) 0
    else 1 + countNodes(root.left) + countNodes(root.right)
  }

  def treeDepth(root: TreeNode): Int = {
    if (root == null) 0
    else 1 + math.max(treeDepth(root.left), treeDepth(root.right))
  }
}

object CalculatorApp {
    def main(args: Array[String]): Unit = {
      val calculator = new BasicCalculator()
      new CalculatorUI(calculator).main(args)
    }
  }


/*references
https://github.com/suliatis/scala-calculator
https://otfried.org/scala/index_30.html
https://github.com/ggsoft/guicalc
https://stackoverflow.com/questions/21734961/scala-gui-event-handling
https://stackoverflow.com/questions/1570175/scala-and-swing-gui-applications
https://stackoverflow.com/questions/30769983/making-a-very-basic-binary-tree-in-scala
https://www.geeksforgeeks.org/class-and-object-in-scala/?ref=lbp
https://www.geeksforgeeks.org/scala-this-keyword/?ref=lbp
https://www.geeksforgeeks.org/packages-in-scala/?ref=lbp
https://stackoverflow.com/questions/14226252/scala-code-bubble-sort-for-loop
https://www.educative.io/answers/how-to-clone-an-array-in-scala
https://stackoverflow.com/questions/10406064/insertion-sort-implementation-in-scala
https://stackoverflow.com/questions/1672074/selection-sort-in-functional-scala
https://alvinalexander.com/scala/how-to-sort-scala-collections-sortwith-sorted-ordered-ordering/
https://gist.github.com/Lytol/225870
https://www.geeksforgeeks.org/shell-sort/
https://stackoverflow.com/questions/1131925/how-do-i-sort-an-array-in-scala
https://www.youtube.com/watch?v=x6eDiEoIqns
https://stackoverflow.com/questions/10321252/mergesort-in-scala
https://dzone.com/articles/how-could-scala-do-merge-sort
https://codereview.stackexchange.com/questions/21575/merge-sort-in-scala
https://stackoverflow.com/questions/12772964/binary-tree-traversal-in-scala
https://www.geeksforgeeks.org/pre-order-post-order-and-in-order-traversal-of-a-binary-tree-in-one-traversal-using-recursion/
https://www.freecodecamp.org/news/binary-search-tree-traversal-inorder-preorder-post-order-for-bst/
https://www.youtube.com/watch?v=yUVpaSnzETo
https://stackoverflow.com/questions/36342456/scala-swing-when-dialog-accesses-parent-parent-content-clears
https://github.com/scala/scala-swing/blob/work/docs/SIP-8.md
https://otfried.org/scala/drawing.html

 */
