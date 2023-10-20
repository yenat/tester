library(shiny)
library(shinyjs)
library(httr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(grid)
library(extrafont)
library(DT)



Title <- "School Level Report"
Href <- paste0("<a href=\"mailto:data-analytics@iohk.io?subject='feedback on ",
               Title, "'\"><i class='far fa-envelope fa-2x'></i></a>&nbsp;&nbsp;<a href=\"https://da.iog.solutions\"><i class='fas fa-globe fa-2x'></i></a>")
LengthMenu <- c("10", "20", "30", "50", "100", "200")

da_theme <- theme(plot.title = element_text(size = 14, face = "bold"))
#da_theme <- theme(plot.title = element_text(size = 14, face = "bold"), text = element_text(family = "Helvetica"))

ui <- fluidPage(
  useShinyjs(),
  tags$head(HTML(paste0("
            <link rel='preconnect' href='https://fonts.gstatic.com' />
            <link href='https://fonts.googleapis.com/css2?family=Chivo:wght@300;400;700&display=swap' rel='stylesheet' />
            <link rel='stylesheet' href='styles.css' />
            "))),
  # fix for labels showing on same row
  tags$head(
    tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}"),
    tags$style(type="text/css", "#busymessage {position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #bbee44; z-index: 105;}")
  ),
  
  # Application title
  #div(id="brandHeader", tags$img(src="moe_logo.png"), titlePanel(Title)),
  uiOutput("loginPage"),
  uiOutput("welcomePage")
)

server <- function(input, output, session) {
  values <- reactiveValues(token = NULL,insitution = NULL, status=NULL)
  
  output$loginPage <- renderUI({
    if (is.null(values$token)) {
      fluidPage( class = "loginn",
                 p("Username"),
                 textInput("username",NULL, value="MeronSA2TT"),
                 p(""),
                 p("Password"),
                 passwordInput("password", NULL, value="Admin@123"),
                 actionButton("login", "Login")
      )
    }
  })
  
  output$welcomePage <- renderUI({
    if (!is.null(values$token)) {
      
   
          
      dashboardPage(
        dashboardHeader(
                        # Set height of dashboardHeader
                       # tags$li(class = "dropdown",
                        #        tags$style(".main-header {max-height: 150px}"),
                         #       tags$style(".main-header .logo {height: 20px}")
                        #),
                        # Use image in title
                        # = tags$a(tags$img(src='moe_logo.png'))
        ),
        dashboardSidebar( 
          selectInput("selectInput","",  choices = unique(values$status)  ),
          selectInput("selectInput","",  choices = unique(values$stud_learningStatus),   width = 300  ),
          selectInput("selectInput","Hi",  label = "Hi", choices = unique(values$stud_grade),   width="120px"  ),
          
          checkboxGroupButtons(
            inputId = "somevalue1",
            label = "Make a choice: ",
            choices = c("Natural Science", "Social Science", "Common")
          )
          

        ),
        dashboardBody(
          
          fluidPage(id="viewPane",
                    div(
                      div( id="brandHeader", tags$img(src="moe_logo.png"), titlePanel(h2(paste(values$insitution))),style = "width: 95%; float: left;"),
                      div(id="logoutt", actionButton("logout", "Logout"),style = "width: 5%; float: left;")),
                    #div(id="region", h2(paste(values$insitution))),
                    p(),
          tabsetPanel(
            tabPanel( "Students",
                      
                      fluidRow(class="numbers",
                               column(2,textOutput("students"), p("Students")),
                               column(2,textOutput("activeStudents"),p("Active Students")),
                               column(2,textOutput("PromotedStudents"),p("Promoted Students")),
                               column(2,textOutput("failedStudents"),p("Failed Students")),
                               column(2,textOutput("dropoutStudents"), p("Dropout Students")),
                               
                               column(2,textOutput("specialNeeds"),p("Special Needs Students"))
                              
                      ),
                      
                     
                      fluidRow(class = "secCol",
                               column(6, plotOutput("bar21")),
                               column(6,plotOutput("bar22"))),
                      fluidRow(class = "secCol",           
                               column(6, plotOutput("bar23")),
                               column(6,plotOutput("bar24")),
                      ),
                      fluidRow(class = "secCol",
                               column(3, plotOutput("bar43")),
                               column(3, plotOutput("bar44")),
                               
                               column(3,plotOutput("bar41")),
                               column(3,plotOutput("bar42"))),
                      fluidRow(class = "secCol",
                               column(6, plotOutput("bar45")),
                               column(6, plotOutput("bar46"))),
                      fluidRow(class = "secCol",
                               column(6, plotOutput("bar47")),
                               column(6, plotOutput("bar48"))
                               ),
                      fluidRow(class="secCol",
                               column(1.5, DTOutput("table1"))
                      ),
            ),
            
            tabPanel( "Teachers",
                      fluidRow(class="numbers",
                               column(3,textOutput("teachers"), p("Total Teachers")),
                               column(3,textOutput("activeTeachers"),p("Active Teachers")),
                               column(3,textOutput("leaders"),p("Total School Leaders")),
                               column(3,textOutput("activeLeaders"),p("Active School Leaders"))
                      ),
                      fluidRow(class = "secCol",
                               column(6, plotOutput("bar31")),
                               column(6, plotOutput("bar32"))),
                      fluidRow(class = "secCol",
                               column(4,plotOutput("bar33")),
                               column(4,plotOutput("bar34")),
                               column(4,plotOutput("pie31")),
                               column(4,plotOutput("pie32"))),
            ),
            tabPanel( "Transcript",
                      fluidRow(class = "secCol",
                               column(12,plotOutput("bar51")),
                               column(12,plotOutput("bar52")) ,                            
                               column(12,plotOutput("bar53"))),
                      
            )
          ) # tabsetPanel
          ) # fluidPage with in the dashboardBody
      
       ) # dashboardBody
) # dashboardPage 
  
      
      
    }
  })
  
  
  
  observeEvent(input$login, {
    # url = https://api.dev.moe.iohk.io/da/login zzz
    res <- POST(
      "https://api.dev.moe.iohk.io/da/login",
      body = list(
        username = input$username,
        password = input$password
      ),
      encode = "json"
    )
    
    if (http_error(res)) {
      # Handle error
      error_message <- content(res)$message
      showModal(modalDialog(
        title = "Login Error",
        paste("Error:", error_message)
      ))
      values$token <- NULL
    } else {
      # Extract token from response
      response <- content(res)
      if ("message" %in% names(response) && response$message == "Invalid username or password") {
        # Invalid username or password
        showModal(modalDialog(
          title = "Login Error",
          paste("Error:", response$message)
        ))
        values$token <- NULL
      } else {
        # Successful login
        values$token <- response$token
        runjs("document.getElementById('loginPage').style.backgroundImage = 'none'")
        runjs("document.getElementById('loginPage').style.height = '0px'")
      }
    }
    ins <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institution/name")
    values$insitution = ins$name
    student_filter <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    values$status =student_filter$status 
    #studentlearningStatus <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    values$stud_learningStatus =student_filter$learningStatus
    #studentstudGrade <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    values$stud_grade =student_filter$grade
    
    
  })
  
  # this should be moved to helper file
  endpointCall <- function(endpoint){
    headers <- c("Authorization" = paste("Bearer", values$token))
    res <- httr::GET(endpoint, httr::add_headers(.headers = headers))
    if (http_type(res) == "application/json") {
      
      json_data <- content(res, as = "text")
      parsed_data <- jsonlite::fromJSON(json_data)
      return(parsed_data)
    }
    else {
      showNotification("Error: Failed to retrieve JSON data from the API", type = "warning")
    }
  }
  
  

  output$stud_status <- ({
    renderText(values$status)
  })
    
  # Second tab
  output$students <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  output$PromotedStudents <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/pass")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  
  output$failedStudents <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/fail")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  output$dropoutStudents <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/dropout")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  
  output$activeStudents <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- sum(result$status == "ACTIVE")
    paste0(result_sum)
  })
  
  output$specialNeeds <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- sum(result$specialNeeds != "NONE") 
    paste0(result_sum)
  })
  
  
  output$table1 <- renderDT(
    endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts"), options = list(lengthChange = FALSE,       caption = "All Student Data")
    
  )
  
  output$bar21 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      group_by(gender, stream) %>%
      summarize(total_count = sum(count))
    ggplot(data = student, aes(x = stream, y = count, fill=gender)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x = stream , y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Stream") +
      ylab("# Students") +
      labs(fill = "Gender") +
      ggtitle("Students By Stream By Gender") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar22 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      group_by(gender, learningStatus) %>%
      summarize(total_count = sum(count))
    ggplot(data = student, aes(x = learningStatus, y = count, fill=gender)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x = learningStatus , y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Learning Status") +
      ylab("# Students") +
      labs(fill = "Gender") +
      ggtitle("Students By Learning Status By Gender") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar23 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    
    result_sum <- result %>%
      group_by(specialNeeds) %>%
      summarize(total_count = sum(result$specialNeeds != "NONE"))
    ggplot(result_sum, aes(x = "", y = total_count, fill = specialNeeds)) +
      geom_text(data = result_sum, aes(x = "" , y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(x = NULL, y = NULL, fill = "specialNeeds")+
      labs(fill = "Special Need") +
      ggtitle("Students By Special Need") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
   
  })
  
  output$bar24 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      mutate(year=year(as.Date(createdDate))) %>%
      group_by(grade,year) %>%
      summarize(total_count = sum(count))
    
    ggplot(data = student, aes(x = factor(year(as.Date(createdDate))), y = count, fill=grade)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x =  factor(year), y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Created Year") +
      ylab("Students") +
      labs(fill = "Grade") +
      ggtitle("Students By Created year") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$teachers <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/teachers/counts")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  
  output$activeTeachers <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/teachers/counts")
    result_sum <- sum(result[result$status == "ACTIVE", "count"])
    paste0(result_sum)
  })
  
  output$leaders <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/leaders/counts")
    result_sum <- sum(result$count) 
    paste0(result_sum)
  })
  
  output$activeLeaders <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/leaders/counts")
    result_sum <- sum(result$status =="ACTIVE") 
    paste0(result_sum)
  })
  
  
  output$bar31 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/teachers/counts")
    result_sum <- result %>%
      group_by(gender,levelEducation) %>%
      summarize(total_count = sum(count))
    
      ggplot(data = result_sum, aes(x = gender, y = total_count, fill=levelEducation)) +
      geom_bar(stat = "identity") +
        # geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = -0.5,size=4, color="white") +
      geom_text(data = result_sum, aes(x = "" , y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      
      xlab("Gender") +
      ylab("Teachers") +
      labs(fill = "Level of Education") +
      ggtitle("Teachers By Gender By Education Level") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar32 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/teachers/counts")
    result_sum <- result %>%
      group_by(gender,status) %>%
      summarize(total_count = sum(count))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=status)) +
      geom_bar(stat = "identity",width = 0.7,position = "dodge") +
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Teachers") +
      labs(fill = "Status") +
      ggtitle("Teachers By Gender By Status") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar33 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/leaders/counts")
    result_sum <- result %>%
      group_by(gender,levelEducation) %>%
      summarize(total_count = sum(count))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=levelEducation)) +
      geom_bar(stat = "identity",width = 0.7,position = "dodge") +
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") + 
      labs(fill = "Level of Education") +
      ggtitle("School Leaders By Gender") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar34 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/teachers/counts")
    result_sum <- result %>%
      group_by(gender,specialNeeds) %>%
      summarize(total_count = sum(count))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=specialNeeds)) +
      geom_bar(stat = "identity",width = 0.7,position = "dodge") +
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Teachers") +
      labs(fill = "special Needs") +
      ggtitle("Teachers By Gender By Spcial Needs") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$pie31 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/leaders/counts")
    result_sum <- result %>%
      group_by(hasTeachingExperience) %>%
      summarize(total_count = sum(count))
    ggplot(result_sum, aes(x = "", y = total_count, fill = hasTeachingExperience)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(x = NULL, y = NULL, fill = "hasTeachingExperience")+
      labs(fill = "Teaching Experience") +
      ggtitle("School Leaders vs Experience") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$pie32 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/leaders/counts")
    result_sum <- result %>%
      group_by(schoolLeadershipQualification) %>%
      summarize(total_count = sum(count))
    ggplot(result_sum, aes(x = "", y = total_count, fill = schoolLeadershipQualification)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(x = NULL, y = NULL, fill = "schoolLeadershipQualification")+
      labs(fill = "Leadership Qualification") +
      ggtitle("School Leaders vs Leadership qualification") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar41 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/dropout")
    result_sum <- result %>%
      group_by(gender, reason) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=reason)) +
      geom_bar(stat = "identity",width = 0.7) +
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Reason") +
      ggtitle("Dropout Students By Reason") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar42 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/dropout")
    result_sum <- result %>%
      group_by(gender, grade) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=grade)) +
      geom_bar(stat = "identity",width = 0.7) +
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Grade") +
      ggtitle("Dropout Students By Grade") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar43 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/pass")
    result_sum <- result %>%
      group_by(gender, stream) %>%
      summarize(total_count = base::sum(counts))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=stream)) +
      geom_bar(stat = "identity",width = 0.7) +
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Stream") +
      ggtitle("Students passed By Gender") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar44 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/fail")
    result_sum <- result %>%
      group_by(gender, stream) %>%
      summarize(total_count = base::sum(counts))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=stream)) +
      geom_bar(stat = "identity",width = 0.7) +
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Stream") +
      ggtitle("Students failed By Gender") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar45 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/results")
    result_sum <- result %>%
      group_by(year, stream) %>%
      summarize(total_count = base::mean(results))
    
    ggplot(data = result_sum, aes(x = stream, y = total_count, fill=stream)) +
      geom_bar(stat = "identity",width = 0.7) +
      
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Stream") +
      ylab("Count") +
      labs(fill = "stream") +
      ggtitle("Students Average Result By Stream") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar46 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/results")
    result_sum <- result %>%
      group_by(year, term) %>%
      summarize(total_count = base::mean(results))
    
    ggplot(data = result_sum, aes(x = term, y = total_count, fill=term)) +
      geom_bar(stat = "identity",width = 0.7) +
      
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Term") +
      ylab("Count") +
      labs(fill = "Term") +
      ggtitle("Students Average Result By Term") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar47 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/trasfer/out")
    result_sum <- result %>%
      group_by(gender, reason) %>%
      summarize(total_count = base::sum(count))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=reason)) +
      geom_bar(stat = "identity",width = 0.7) +
      
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Reason") +
      ggtitle("Students Transfered-out By Reason") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar48 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/trasfer/in")
    result_sum <- result %>%
      group_by(gender, reason) %>%
      summarize(total_count = base::sum(count))
    
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=reason)) +
      geom_bar(stat = "identity",width = 0.7) +
      
      geom_text(aes(label = total_count),position = position_stack(vjust = 0.5),vjust = -0.5,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Reason") +
      ggtitle("Students Transfered-in By Reason") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })

  
  output$bar51 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/absences")
    result_sum <- result %>%
      group_by(section,grade) %>%
      summarize(total_count = sum(totalSemesterAbsences))
    ggplot(data = result_sum, aes(x = grade, y = total_count, fill=section)) +
      geom_bar(stat = "identity") +
      geom_text( aes(x = grade, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Grade") +
      ylab("Count") +
      labs(fill = "Section") +
      ggtitle("Students Absense By Grade By Section") +
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  
  
  output$bar52 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/behavior")
    result_sum <- result %>%
      group_by(semester,behavior) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = semester, y = total_count, fill=behavior)) +
      geom_bar(stat = "identity") +
      geom_text( aes(x = semester, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Semester") +
      ylab("Count") +
      labs(fill = "Behavior") +
      ggtitle("Students Behavior By Semester") +
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  output$bar53 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/transcript")
    result_sum <- result %>%
      group_by(gender,status) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = gender, y = total_count, fill=status)) +
      geom_bar(stat = "identity") +
      geom_text( aes(x = gender, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Gender") +
      ylab("Count") +
      labs(fill = "Status") +
      ggtitle("Students Received Transcript By Gender") +
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  observeEvent(input$logout, {
    values$token <- NULL
    runjs("document.getElementById('loginPage').style.backgroundImage = 'url('background.png')'")
  })
}

shinyApp(ui, server)
