library(shiny)
library(shinyjs)
library(httr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(grid)
library(extrafont)

Title <- "Ministry Level Report"
Href <- paste0("<a href=\"mailto:data-analytics@iohk.io?subject='feedback on ",
               Title, "'\"><i class='far fa-envelope fa-2x'></i></a>&nbsp;&nbsp;<a href=\"https://da.iog.solutions\"><i class='fas fa-globe fa-2x'></i></a>")
LengthMenu <- c("10", "20", "30", "50", "100", "200")

da_theme <- theme(plot.title = element_text(size = 14, face = "bold"))
forgot_password_link <- "https://frontend.dev.moe.iohk.io/recover"
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
  values <- reactiveValues(token = NULL,insitution = NULL)
  
  output$loginPage <- renderUI({
    if (is.null(values$token)) {
      fluidPage( 
        column(6,class = "loginp", h2("Welcome Back")),
        column(6,
        class = "loginn",
        img(src='moe_logo.png', height="20%", width="20%", align = "center"),
        p("Username"),
        textInput("username",NULL),
        p(""),
        p("Password"),
        passwordInput("password", NULL),
        actionButton("login", "Login"),
        a(href = forgot_password_link, "Forgot password?"))
      )
    }
  })
  
  output$welcomePage <- renderUI({
    if (!is.null(values$token)) {
      fluidPage(id="viewPane",
        div(
        div( id="brandHeader", tags$img(src="moe_logo.png"), titlePanel(Title),style = "width: 93%; float: left;"),
        div(id="logoutt",actionButton("logout", "Logout"),style = "width: 7%; float: left;")),
        div(id="region", h4(paste(values$insitution))),
        p(),
        tabsetPanel(
          tabPanel( "Institutions",
                    fluidRow(class="numbers",
                      column(1,textOutput("institutions"), p("Institutions")),
                      column(1,textOutput("activeInstitutions"),p("Active Institutions")),
                      column(1,textOutput("school"),p("Schools")),
                      column(1,textOutput("regions"),p("Regions")),
                      column(1,textOutput("city"),p("City Administration")),
                      column(1,textOutput("subcity"),p("Sub City")),
                      column(1,textOutput("woreda"),p("Woreda")),
                      column(1,textOutput("users"),p("Total Users")),
                             ),
                    fluidRow(class = "secCol",
                      column(6, plotOutput("bar11")),
                      column(6, plotOutput("bar12"))
                    ),
        fluidRow(class = "secCol",           
          column(6,plotOutput("bar13")),
          column(6, plotOutput("line")))),
        tabPanel( "Students",
                  fluidRow(class="numbers",
                           column(2,textOutput("students"), p("Students")),
                           column(2,textOutput("activeStudents"),p("Active Students")),
                           column(2,textOutput("specialNeeds"),p("Special Needs Students")),
                           column(2,textOutput("PromotedStudents"),p("Promoted Students")),
                           column(2,textOutput("failedStudents"),p("Failed Students")),
                           column(2,textOutput("dropoutStudents"), p("Dropout Students")),
                           
                           
                  ),
                  fluidRow(class = "secCol",
                    column(6, plotOutput("bar21")),
             column(6,plotOutput("bar22"))),
             fluidRow(class = "secCol",           
                    column(6, plotOutput("bar23")),
                    column(6,plotOutput("bar24")),
             ),
             fluidRow(class = "secCol",
               column(3, plotOutput("bar41")),
               column(3, plotOutput("bar42")),
        
              column(3,plotOutput("bar43")),
              column(3,plotOutput("bar44"))),
             
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
                      column(4,plotOutput("pie31")),
                      column(4,plotOutput("pie32"))),
        ),
        tabPanel( "Logins",
                  fluidRow(
                    column(12,plotOutput("bar51")))
                    ,
                  fluidRow(class = "secCol",
                           column(12,plotOutput("bar52")),
                    ),
                  
        )
        )
        )
        
    }
  })
  
  
  
  observeEvent(input$login, {
    # url = https://api.dev.moe.iohk.io/da/login 
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
  output$institutions <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    print(result)
    result_sum <- nrow(result) 
    paste0(result_sum)
    })
  output$activeInstitutions <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    sum_column1 <- sum(result$status == "ACTIVE")
    paste0(sum_column1)
  })
  output$users <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/users/count")
    result_sum <- nrow(result)
    paste0(result_sum)
  })
  
  output$school <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    result_sum <- sum(result$type == "SCHOOL")
    paste0(result_sum)
  })
  
  output$regions <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    result_sum <- sum(result$type == "REGION")
    paste0(result_sum)
  })
  
  output$city <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    result_sum <- sum(result$type == "CITY_ADMINISTRATION")
    paste0(result_sum)
  })
  
  output$subcity <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    result_sum <- sum(result$type == "SUB_CITY")
    paste0(result_sum)
  }) 
  output$woreda <- renderText({ 
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/institutions/count")
    result_sum <- sum(result$type == "WOREDA")
    paste0(result_sum)
  }) 
  
  output$bar11 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/registration/institutions")
    result_sum <- result %>%
      mutate(month = format(as.Date(date), "%B, %Y")) %>%
      group_by(month, institutionsStatus) %>%
      summarize(total_count = sum(count))
    
      ggplot(data = result, aes(x = format(as.Date(date), "%B, %Y"), y = count, fill=institutionsStatus)) +
        geom_bar(stat = "identity") +
        geom_text(data = result_sum, aes(x = month, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
        xlab("Months") +
        ylab("Institutions") +
        labs(fill = "Status") +
        ggtitle("Institution registration status") +
        theme_classic() + da_theme +
        scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
        

  })
  
  output$bar12 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/registration/institutions_bydiddate")
    result_sum <- result %>%
      mutate(month = format(as.Date(date), "%B, %Y")) %>%
      group_by(month, type) %>%
      summarize(total_count = sum(DIDCount))
    result_sum$month = factor(result_sum$month, levels = month.abb)
    
    result$month_year = format(as.Date(result$date), "%B, %Y")
    result$month_year = factor(result$month_year, levels = unique(result$month_year))
    
    ggplot(data = result, aes(x = month_year, y = DIDCount, fill=type)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x = month, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Month") +
      ylab("Institutions") +
      labs(fill = "Type") +
      ggtitle("Institution Digital ID(DID) creation") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#E28E63", "#EDB655", "#5DC7B6", "#417D8D"))
  })
  
  output$bar13 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/registration/users")
    result_sum <- result %>%
      mutate(month = format(as.Date(date), "%B, %Y")) %>%
      group_by(month, userStatus) %>%
      summarize(total_count = sum(count)) 
    
    ggplot(data = result, aes(x = format(as.Date(date), "%B, %Y"), y = count, fill=userStatus)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x = month, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Month") +
      ylab("Users") +
      labs(fill = "Status") +
      ggtitle("User registration status") + 
      theme_classic() +  da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$line <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/registration/users")
    
    ggplot(data = result, aes(x = as.Date(date), y = count, color=role)) +
      geom_line(stat = "identity",size=1.1) +
      xlab("Month") +
      ylab("Count") +
      labs(color = "Role") +
      ggtitle("User registration Role") + theme_classic() + da_theme +
      scale_color_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
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
  
  output$bar21 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      group_by(gender, stream) %>%
      summarize(total_count = sum(count))
    ggplot(data = student, aes(x = gender, y = count, fill=stream)) +
      geom_bar(stat = "identity") +
      geom_text(data = result_sum, aes(x = gender, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Gender") +
      ylab("Students") +
      labs(fill = "Stream") +
      ggtitle("Students in a stream") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar22 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      group_by(status) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = status, y = total_count)) +
      geom_bar(stat = "identity",fill="#417D8D") +
      geom_text(aes(label = total_count),vjust = 2,size=4, color="white") +
      xlab("Status") +
      ylab("Students") +
      ggtitle("Students By Registration status") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar23 <- renderPlot({
    student <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/students/counts")
    result_sum <- student %>%
      group_by(region) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = region, y = total_count)) +
      geom_bar(stat = "identity",fill="#417D8D") +
      geom_text(aes(label = total_count), vjust = 2,size=4, color="white") +
      xlab("Region") +
      ylab("Students") +
      ggtitle("Number of students in Regions") + 
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
      ggtitle("Students By created year") + 
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
      geom_bar(stat = "identity",width = 0.7,position = "dodge") +
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = 2,size=4, color="white") +
      xlab("Gender") +
      ylab("Teachers") +
      labs(fill = "Level of Education") +
      ggtitle("Number of Teachers per level of education") + 
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
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = 2,size=4, color="white") +
      xlab("Gender") +
      ylab("Teachers") +
      labs(fill = "Status") +
      ggtitle("Number of Teachers per status") + 
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
      geom_text(aes(label = total_count), position = position_dodge(width = 0.7),vjust = 2,size=4, color="white") +
      xlab("Gender") +
      ylab("Count") + 
      labs(fill = "Level of Education") +
      ggtitle("School Leaders By Gender") + 
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
      ggtitle("Dropout Students per Reason") + 
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
      ggtitle("Dropout Students per Grade") + 
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
  
  output$bar51 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/logins")
    result_sum <- result %>%
      mutate(month = format(as.Date(date), "%B, %Y")) %>%
      mutate(cyl_am = paste(month, action, sep = "\n")) %>%
      group_by(cyl_am,role) %>%
      summarize(total_count = sum(count))
    ggplot(data = result_sum, aes(x = cyl_am, y = total_count, fill=role)) +
      geom_bar(stat = "identity") +
      geom_text( aes(x = cyl_am, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Month") +
      ylab("Count") +
      labs(fill = "Role") +
      ggtitle("Login by date") +
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  
  output$bar52 <- renderPlot({
    result <- endpointCall("https://reporting-api.moe.iohk.io/api/v1/logins/durations")
    result_sum <- result %>%
      mutate(month = format(as.Date(date), "%B, %Y")) %>%
      group_by(month,role) %>%
      summarize(total_count = sum(sessionDuration))
    ggplot(data = result_sum, aes(x = month, y = total_count, fill=role)) +
      geom_bar(stat = "identity") +
      geom_text( aes(x = month, y = total_count, label = total_count),position = position_stack(vjust = 0.5),size=4, color="white")  +
      xlab("Month") +
      ylab("Count") +
      labs(fill = "Role") +
      ggtitle("Login duration in seconds") + 
      theme_classic() + da_theme +
      scale_fill_manual(values = c("#417D8D", "#5DC7B6" , "#EDB655", "#E28E63","#D86778", "#696EB0","#1F4987","#6B3922","#5E94BA","#B6F34F"))
  })
  observeEvent(input$logout, {
    values$token <- NULL
    runjs("document.getElementById('loginPage').style.backgroundImage = 'url('background.png')'")
  })
}
# Export the values of your output objects
exportTestValues (ui = ui, server = server)
session$exportTestValues()

# Save them as a PNG file
png ("dashboard.png")

shinyApp(ui, server)
