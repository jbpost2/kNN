library(shiny)
library(class)
library(plotrix)

load("mixture.example.RData")
train <- mixture.example$x
trainclass <- mixture.example$y
test <- mixture.example$xnew
pts1 <- mixture.example$px1
pts2 <- mixture.example$px2

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('k-Nearest Neighbours Classification'),
    sidebarPanel(
      sliderInput('k', 'Select the Number of Nearest Neighbours', value = 25, min = 1, max = 150),
      checkboxInput('showN', label = "Show the neighbourhood for one point (click to select a point)"),
      a("App credit: https://github.com/schoonees/kNN", href = "https://github.com/schoonees/kNN")
    ),
    mainPanel(
      plotOutput('plot1', width = "600px", height = "600px",  click = "click_plot")
    )
  )
)

server <- function(input, output, session) {
  idx  <- NULL
  dmat <- NULL
  ## ID the point clicked on
  xy  <- reactive(c(input$click_plot$x, input$click_plot$y))

  id <- observe({
    if (!is.null(xy())) {
      dmat <- as.matrix(dist(rbind(xy(), train)))
      idx <<- which.min(dmat[1, -1])
      dmat <<- dmat[-1, -1]
    }
  })

  output$plot1 <- renderPlot({
    xy()
    ## Fit model
    fit <- knn(train = train, test = test, cl = trainclass, k = input$k, prob = TRUE)
    probs <- matrix(fit, length(pts1), length(pts2))

    ## Plot create empty plot
    plot(train, asp = 1, type = "n", xlab = "x1", ylab = "x2",
         xlim = range(pts2), ylim = range(pts2), main =  paste0(input$k, "-Nearest Neighbours"))

    ## Get neighbourhood, draw circle, if needed
    if (input$showN & !is.null(idx)) {
      rad <- sort(dmat[, idx])[1 + input$k]
      draw.circle(x = train[idx, 1], y = train[idx, 2], radius= rad, col = "lightgoldenrod1")
    }

    ## Plot the grid
    grid <- expand.grid(x = pts1, y = pts2)
    points(grid, pch = 20, cex = 0.2, col = ifelse(probs > 0.5, "coral", "cornflowerblue"))
    points(train, col = ifelse(trainclass == 1, "coral", "cornflowerblue"), cex = 1.5, pch = 21, lwd = 2)

    ## Add decision boundary
    contour(pts1, pts2, probs, levels = 0.5, labels = "", lwd = 1.5, add = TRUE)

    ## ID points within neighbourhood
    if (input$showN & !is.null(idx)) {
      points(train[which(dmat[, idx] <= rad), ], col = "red", pch = 20, cex = 0.75)
      points(train[idx, , drop = FALSE], pch = 3, cex = 1.5, lwd = 2)
    }

  })
}

# Run the application
shinyApp(ui = ui, server = server)
