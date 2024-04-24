#load libraries 
library(ggplot2)
library(dplyr)
library(shiny)
library(corrgram)
#read Diamonds dataset and save as diamonds
diamonds<-as.data.frame(read.csv("~/Downloads/Diamonds.csv"))
head(diamonds)

#select the 5 variable and only those that are distict and save as diamonds_df
diamonds_df <-select(diamonds, c("carat","cut","color","clarity", "price")) %>% 
  distinct()

#do correlation test for carat and price using cor test
correlation_test <-cor.test(diamonds_df$price, diamonds_df$carat)
correlation_test

#using ggplot visualize correlation w best fit line
diamonds_df_cor <- diamonds_df %>%
  ggplot(aes(price,carat))+
  geom_point()+
  geom_smooth(method = "lm", color = "blue")+
  ggtitle("carat vs price")
diamonds_df_cor

# color by clarity
diamonds_df_clarity <- diamonds_df %>%
  ggplot(aes(price,carat,color= clarity))+
  geom_point()+ 
  ggtitle("carat vs price by clarity")
diamonds_df_clarity

#color by cut
diamonds_df_cut <- diamonds_df %>%
  ggplot(aes(price,carat, col= cut))+
  geom_point()+
  ggtitle("carat vs price by cut")
diamonds_df_cut

#color by color of diamond
diamonds_df_color <- diamonds_df %>%
  ggplot(aes(price,carat, col= color))+
  geom_point()+
  ggtitle("carat vs price by color")
diamonds_df_color


ui <- fluidPage(
  titlePanel("Diamonds"),
  sidebarLayout(
    sidebarPanel(
      #select variables for y-axis
      selectInput(inputId = "y",
                  label = "Y-axis",
                  choices = c("carat", "price"),
                  selected = "carat"),
      
      #Select variables for x-axis        
      selectInput(inputId = "x",
                  label = "X-axis",
                  choices = c("carat", "price"),
                  selected = "price"),
      
      #Set alpha level
      sliderInput(inputId = "alpha",
                  label = "Alpha",
                  min = 0, max = 1,
                  value = 0.5),
      
      #select variable for color
      selectInput(inputId = "color",
                  label = "Color by: ",
                  choices = c("cut","color","clarity"),
                  selected = "clarity"),
    ),
    #Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"))))

server <- function(input, output) {
  output$scatterplot <- renderPlot({ 
    ggplot(diamonds_df, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha)
  })
}

shinyApp(ui = ui, server = server)
