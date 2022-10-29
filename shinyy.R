library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(readr)
library(ijtiff)
library(raster)
library(imager)
library(shinyWidgets)
library(RMySQL)
library(DBI)


makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = '1234',
                 dbname = 'tourism', host = 'localhost')

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Tour Management Service",
                  tabPanel("Home" , tabName = "Home", icon = icon("house-user"),
                           mainPanel("Home",
                                     h1("Welcome to the website check this out L0L!?!?"),
                                     h2("India Tourism"),
                                     h3("INDIA is rich in heritage, culture, traditions and history! India Tourisms has something in store for everybody, a complete tourist destination!")
                           ),
                  ),
                  tabPanel("India" , tabName = "India", icon = icon("train"),
                           sidebarPanel(
                             div(p(h3("The All India tour packages"))),
                             div(p(h3("Select one"))),
                             div(p(actionLink("do", h4("Goa-Mumbai" ),width=300),)),
                             div(p(actionLink("do", h4("Kerala"),width=300))),
                             div(p(actionLink("do", h4("Mumbai-Pune"),width=300))),
                             div(p(actionLink("do", h4("Kulu_manali"),width=300))),
                             div(p(actionLink("do", h4("Delhi-Agra"),width=300))),
                             div(p(actionLink("do", h4("Jaipur-Delhi"),width=300))),
                             div(p(actionLink("do", h4("Calicut-wayanad"),width=300))),
                             div(p(actionLink("do", h4("Coorg-Banglore"),width=300))),
                             div(p(actionLink("do", h4("Coimbatore-Nilgiri"),width=300)))
                             
                           )
                  ),
                  tabPanel("International",
                           sidebarPanel(
                             div(p(h3("The All India tour packages"))),
                             div(p(h3("Select one"))),
                             div(p(actionLink("do", h4("United Kingdom" ),width=300),)),
                             
                             
                             fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                                                 <br>")
                             ),
                             fluidRow(
                               column(3),
                               column(6,
                                      tags$div(align = "center", 
                                               tags$a("Start", 
                                                      onclick="fakeClick('Package_Booking')", 
                                                      class="btn btn-primary btn-lg")
                                      )
                               ),
                               column(3)
                             ),
                             
                             
                             div(p(actionLink("do", h4("South Korea"),width=300))),
                             div(p(actionLink("do", h4("United Arab Emirates"),width=300))),
                             div(p(actionLink("do", h4("Sri Lanka"),width=300))),
                             div(p(actionLink("do", h4("France"),width=300))),
                             div(p(actionLink("do", h4("Germany"),width=300))),
                             div(p(actionLink("do", h4("USA"),width=300))),
                             div(p(actionLink("do", h4("Canada"),width=300))),
                             div(p(actionLink("do", h4("Australia"),width=300)))
                             
                           ), tabName = "International", icon = icon("plane")),
                  tabPanel("Package_Booking", value =  "Packages",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("uid", "Enter your unique id: ", ""),
                             textInput("name", "Enter your name: ",""),
                             numericInput("mobile", "Enter your mobile no: ",""),
                             dateInput("date", "Enter the date of the travel: ", ""),
                             textInput("boarding_point", "Enter your boarding point: ",""),
                             textInput("destination", "Enter your boarding destination: ",""),
                             actionButton('writetodb', 'Save'),
                             tableOutput('dbtable')
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Your choice: "),
                             
                             h4("Confirm your details!!"),
                             verbatimTextOutput("txtout"),
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Review", tabName = "Review", icon = icon("user"),
                           fluidPage(
                             fluidRow(
                               sidebarPanel(h2("Throw us your review"),width = 100,
                                            textInput("userid", "Enter your user id: "),
                                            textInput("username", "Enter your name: "),
                                            textInput("email", "Enter your email id: "),
                                            textInput("review", "Enter your review: ")),
                               sliderInput("stars", "Star rating for us out of 5: ", min = 1, max = 5, value = 1),
                               radioButtons("rb", "Your Booking experience with us:",
                                            choiceNames = list(
                                              icon("angry"),
                                              icon("smile"),
                                              icon("sad-tear")
                                            ),
                                            choiceValues = list("angry", "happy", "sad")
                                            
                                            
                               ),
                               
                               actionButton('writetoreview', 'Save'),
                               tableOutput('reviewtable'),
                               
                               mainPanel(
                                 h1("Thank you for your review"),
                                 
                                 h4("Please confirm your review !!"),
                                 verbatimTextOutput("reviewout"),
                                 
                               )
                               
                             )
                           ),),
                  tabPanel("Help & Support", tabName = "Help", icon = icon("phone"),
                           h1("Contact us on:"),
                           h1("Mobile: 9339336464"),
                           h1("Email: rapidbookings@gmail.com")),
                  tabPanel("About us" , tabName = "About", icon = icon("award"),
                           h1("Rapid is a leading business travel and expense solution that automates and streamlines processes for high-growth companies and enterprises. At Rapid we save you a lifetime. We free companies and their employees from travel and expense friction. From searching flights and hotels, booking a seat on a train or expensing a cab from the airport. Our end-to-end solution covers your whole journey. Period"))
                  
                  
                  
                ),
                
                # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  
  output$txtout <- renderText({
    paste0("Your id: ", input$uid, "\nYour name: ", input$name, "\nYour mobile no: ", input$mobile, "\nYour date: ", input$date,"\nYour travel boarding point: ", input$boarding_point,"\nYour travel destination: ",input$destination, sep = "\n" )
  })
  
  output$reviewout <- renderText({
    paste0("Your user id: ", input$userid ,"\nYour username: ", input$username, "\nYour email: ", input$email,"\nYour Review: ", input$review,"\nStar rating for us: ", input$stars,"\nYour experience with us: ",input$rb, sep = "\n" )
  })
  
  packageinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT uid, name, mobile, date, boarding_point, destination from package')
  })
  reviewinshiny <- reactive({
    dbtrigger$depend()
    dbGetQuery(con, 'SELECT userid, username, email, review, stars, rb from review')
  })
  
  observeEvent(input$writetodb, {
    sql1 = "INSERT INTO package (uid, name, mobile, date, boarding_point, destination) VALUES (?uid, ?name, ?mobile, ?date, ?boarding_point, ?destination)"
    sql3 <- sqlInterpolate(con,sql1, uid=input$uid, name=input$name, mobile = input$mobile, date=input$date, boarding_point=input$boarding_point, destination = input$destination)
    dbExecute(con, sql3)
    dbtrigger$trigger()
  })
  observeEvent(input$writetoreview,{
    
    sql2 = "INSERT INTO review (userid, username, email,review, stars, rb) VALUES (?userid, ?username, ?email, ?review, ?stars, ?rb)"
    sql4 <- sqlInterpolate(con, sql2, userid=input$userid, username=input$username, email = input$email, review=input$review, stars=input$stars, rb = input$rb)
    dbExecute(con, sql4)
    dbtrigger$trigger()
    
    
  })
  output$dbtable <- renderTable({
    packageinshiny()
  })
  
  output$reviewtable <- renderTable({
    reviewinshiny()
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)