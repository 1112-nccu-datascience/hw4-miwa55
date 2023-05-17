library(shiny)
library(FactoMineR)
#library(ggbiplot)
library(ggvis)
data(iris)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("PCA analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # sliderInput(inputId = "n",
      #             label = "Number of points:",
      #             min = 1,
      #             max = nrow(iris),
      #             value = 30,
      #             step = 1)
      
      selectInput(inputId = 'nn',
                  label = 'Select PCs:(PC1 refers to dim1...)',
                  choices = c('PC1 and PC2' = '1:2', 'PC1 and PC3' = '1:3', 'PC1 and PC4' = '1:4', 'PC2 and PC3' = '2:3', 'PC2 and PC4' = '2:4', 'PC3 and PC4' = '3:4'),
                  selected = 'PC1 and PC2',
                  multiple = F)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = 'PCA'), 
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$PCA <- renderPlot({
    # library(ggbiplot)
    palette(c("#E41A1C", "#377EB8"))
    library(factoextra)
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    fviz_pca_biplot(ir.pca, 
                    repel = TRUE, 
                    axes = c(as.integer(substring(input$nn, 1, 1)), as.integer(substring(input$nn, 3, 3))),
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969"  # Individuals color
                    , label ="var"
                    ,habillage=iris$Species,
                    addEllipses=TRUE, ellipse.level=0.95
    ) 
    #   fviz_ca_biplot(ir.ca, map ="rowprincipal",
    #  arrow = c(FALSE, TRUE))
    # labs(title ="PCA", x = "PC1", y = "PC2")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

