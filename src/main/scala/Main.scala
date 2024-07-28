import Calculator.listenTo

import scala.collection.mutable
import scala.swing._
import scala.swing.event._
import java.awt.{Color, Font, Graphics2D, RenderingHints}
import scala.swing.MenuBar.NoMenuBar.{contents, revalidate}

object Calculator extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Scala Calculator"
    preferredSize = new Dimension(600, 700) // Increased overall size

    // Main screen buttons
    val normalCalcButton = new Button("Normal Calculation")
    val dataStructButton = new Button("Data Structures and Algorithms")
    val graphVisualizationButton = new Button("Graph Visualization")

    val mainPanel = new GridPanel(3, 1) {
      contents += normalCalcButton
      contents += dataStructButton
      contents += graphVisualizationButton
      preferredSize = new Dimension(600, 700) // Adjusted size
    }


    // Calculator UI elements
    val display = new TextField {
      columns = 10
      editable = false
      preferredSize = new Dimension(580, 50) // Adjusted width
    }

    val buttons = List(
      "7", "8", "9", "/",
      "4", "5", "6", "*",
      "1", "2", "3", "-",
      "0", ".", "=", "+",
      "C"
    ).map(new Button(_))

    var currentNumber: String = ""
    var previousNumber: String = ""
    var operator: String = ""
    var displayText: String = ""
    var isResultShown: Boolean = false

    val buttonPanel = new GridPanel(5, 4) {
      buttons.foreach(contents += _)
      preferredSize = new Dimension(580, 300) // Adjusted width
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
        contents += new MenuItem("Graph Visualization") {
          action = Action("Graph Visualization") {
            showGraphVisualizationMenu()
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

    // Updated Data Structures and Algorithms Menu
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
        case ButtonClicked(`graphButton`) => showGraphMenu()
      }

      revalidate()
    }


    def showSortingMenu(): Unit = {
      val bubbleSortButton = new Button("Bubble Sort")

      contents = new BorderPanel {
        layout(new GridPanel(2, 1) {
          contents += bubbleSortButton
          contents += new Label("Select a sorting algorithm")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }
      listenTo(bubbleSortButton)
      reactions += {
        case ButtonClicked(`bubbleSortButton`) => showBubbleSort()
      }

      revalidate()
    }

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
            sortingSteps = bubbleSort(numbers)
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

    def showDataStructuresMenu(): Unit = {
      val stackButton = new Button("Stack Visualization")

      contents = new BorderPanel {
        layout(new GridPanel(2, 1) {
          contents += stackButton
          contents += new Label("Select a data structure")
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South

      }
      listenTo(stackButton)
      reactions += {
        case ButtonClicked(`stackButton`) => showStackVisualization()
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


    def showGraphMenu(): Unit = {
      contents = new BorderPanel {
        layout(new Label("Graph Menu - To be implemented")) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }
      revalidate()
    }


    def showGraphVisualizationMenu(): Unit = {
      val inOrderTraversalButton = new Button("In-Order Tree Traversal")

      contents = new BorderPanel {
        layout(new GridPanel(1, 1) {
          contents += inOrderTraversalButton
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(inOrderTraversalButton)
      reactions += {
        case ButtonClicked(`inOrderTraversalButton`) => showInOrderTraversal()
      }

      revalidate()
    }

    def showInOrderTraversal(): Unit = {
      val inputField = new TextField {
        columns = 30
      }
      val traverseButton = new Button("Start In-Order Traversal")
      val resultLabel = new Label("Enter up to 15 numbers and click 'Start In-Order Traversal'")
      val explanationLabel = new Label("Explanation will appear here")

      var numbers: Array[Int] = Array()
      var traversalResult: List[Int] = List()

      contents = new BorderPanel {
        layout(new GridPanel(6, 1) {
          contents += new Label("Enter up to 15 numbers separated by spaces:")
          contents += inputField
          contents += traverseButton
          contents += resultLabel
          contents += explanationLabel
        }) = BorderPanel.Position.Center
        layout(backButton) = BorderPanel.Position.South
      }

      listenTo(traverseButton)
      reactions += {
        case ButtonClicked(`traverseButton`) =>
          try {
            numbers = inputField.text.split(" ").map(_.trim.toInt).take(15)
            if (numbers.length > 15) {
              resultLabel.text = "Only the first 15 numbers were used."
            } else {
              resultLabel.text = s"Processing ${numbers.length} numbers."
            }
            val root = buildTree(numbers)
            traversalResult = inOrderTraversal(root)
            explanationLabel.text = s"In-Order Traversal Result: ${traversalResult.mkString(", ")}"
            drawTree(root)
          } catch {
            case _: NumberFormatException =>
              resultLabel.text = "Invalid input. Please enter valid integers separated by spaces."
          }
      }

      def buildTree(arr: Array[Int]): TreeNode = {
        var root: TreeNode = null
        for (num <- arr) {
          root = insert(root, num)
        }
        root
      }

      def insert(node: TreeNode, value: Int): TreeNode = {
        if (node == null) {
          new TreeNode(value)
        } else {
          if (value < node.value) {
            node.left = insert(node.left, value)
          } else {
            node.right = insert(node.right, value)
          }
          node
        }
      }

      def inOrderTraversal(node: TreeNode): List[Int] = {
        if (node == null) {
          List()
        } else {
          inOrderTraversal(node.left) ++ List(node.value) ++ inOrderTraversal(node.right)
        }
      }

      def drawTree(root: TreeNode): Unit = {
        val treePanel = new Panel {
          preferredSize = new Dimension(800, 600)

          override def paintComponent(g: Graphics2D): Unit = {
            super.paintComponent(g)
            if (root != null) {
              drawNode(g, root, 400, 30, 200)
            }
          }

          def drawNode(g: Graphics2D, node: TreeNode, x: Int, y: Int, offset: Int): Unit = {
            g.drawOval(x - 15, y - 15, 30, 30)
            g.drawString(node.value.toString, x - 5, y + 5)
            if (node.left != null) {
              g.drawLine(x, y, x - offset, y + 50)
              drawNode(g, node.left, x - offset, y + 50, offset / 2)
            }
            if (node.right != null) {
              g.drawLine(x, y, x + offset, y + 50)
              drawNode(g, node.right, x + offset, y + 50, offset / 2)
            }
          }
        }
        contents = new BorderPanel {
          layout(new BorderPanel {
            layout(treePanel) = BorderPanel.Position.Center
          }) = BorderPanel.Position.Center
          layout(backButton) = BorderPanel.Position.South
        }
        revalidate()
      }

      revalidate()
    }

    class TreeNode(var value: Int) {
      var left: TreeNode = null
      var right: TreeNode = null
    }





    // Button actions
    listenTo(normalCalcButton, dataStructButton, graphVisualizationButton, backButton)
    listenTo(buttons: _*)

    reactions += {
      case ButtonClicked(`normalCalcButton`) =>
        showCalculator()

      case ButtonClicked(`dataStructButton`) =>
        showDataStructuresAndAlgorithmsMenu()

      case ButtonClicked(`graphVisualizationButton`) =>
        showGraphVisualizationMenu()

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
        displayText += s" = ${result}"
        display.text = displayText
        currentNumber = result.toString
        previousNumber = ""
        operator = ""
        isResultShown = true
      }
    }

    def clearCalculator(): Unit = {
      currentNumber = ""
      previousNumber = ""
      operator = ""
      displayText = ""
      display.text = ""
      isResultShown = false
    }

    // Initial screen
    contents = mainPanel

  }
}