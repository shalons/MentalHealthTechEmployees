#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(cowplot)
library(shinydashboard)
library(shinycssloaders)
library(fmsb)
library(memisc)
library(psych)
library(ggplot2)
library('rsconnect')

survey <- read.csv("survey.csv", header = T)

nrow(survey)
ncol(survey)
survey <- survey[-c(1,27)]

describe(survey)

table(survey$Gender)
survey$Gender <- tolower(survey$Gender)

Fe <- c('female','F','Femake', 'cis female', 'f','Female (cis)', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail','Female ')
M <-  c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man','Mle') 
Queer <- c('queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer', 'androgyne', 'agender', 'guy (-ish) ^_^', 
           'male leaning androgynous', 'neuter', 'queer', 'ostensibly male, unsure what that really means',
           'a little about you','p','all','All','Unknown')
Trans <- c('trans-female', 'trans woman','Female (trans)', 'female (trans)','something kinda male?')

survey$Gender <- sapply(survey$Gender, function(x) if(x %in% M) "Male" else x)
survey$Gender <- sapply(survey$Gender, function(x) if(x %in% Fe) "Female" else x)
survey$Gender <- sapply(survey$Gender, function(x) if(x %in% Queer) "Genderqueer" else x)
survey$Gender <- sapply(survey$Gender, function(x) if(x %in% Trans) "Transgender" else x)

table(survey$Gender)

survey[which(survey$Age <= 15), "Age"] <- median(survey$Age)
survey[which(survey$Age > 100), "Age"] <- median(survey$Age)

colSums(is.na(survey))

# Convert blanks to "No Response"
survey <- survey %>% mutate(.,
                            work_interfere = ifelse(is.na(work_interfere), median(survey$work_interfere, na.rm = T), work_interfere),
                            no_employees = ifelse(is.na(no_employees), median(survey$no_employees, na.rm = T), no_employees),
                            self_employed = ifelse(is.na(self_employed), median(survey$self_employed, na.rm = T), self_employed))

survey <- survey[-c(4)]

colSums(is.na(survey))

table(survey$no_employees)

survey <- survey %>% mutate(Agency_Size = case_when(
  no_employees %in% c('1-5','6-25','26-100')  ~ "Small",
  no_employees %in% c('100-500','500-1000')  ~ "Medium",
  no_employees %in% c('More than 1000')  ~ "Large"
))

str(survey)
survey[-c(1)] <- lapply(survey[-c(1)] , factor)

ui <- shinyUI(navbarPage(title = "Exploring Mental Health of Tech Employees", 
                   
                   tabPanel("Overview",
                              box(
                                width = 20,  solidHeader = TRUE,
                                collapsible = F, 
                                tags$h1("Data Overview"),
                                tags$h3("The Mental Health in Tech dataset is the survey data from 2014 regarding the mental health and 
                                        well-being of the tech employees (taken from Kaggle).This dashboard contains various visualizations 
                                        from this dataset giving an overview/visualizations of all responses w.r.t empployees as well as employers."),
                                setBackgroundColor(color = c("#F7FBFF", "#2171B5"),
                                                   gradient = "linear",
                                                   direction = "bottom")
                              ),
                            br(),
                            tags$h1("Data Demographic"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("age_range1", "Age:",width = 560,dragRange = F, min = 10,max = 75,value = c(10, 100)),br(),
                                dropdownButton(inputId = "CountryInput", 
                                               label = "Country",
                                               circle = F,
                                               size = "lg",
                                               checkboxGroupInput(inputId = "chk_box",
                                                                  label = "Select", 
                                                                  choices = c(unique(survey$Country))
                                               ))),
                              mainPanel(
                                splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                            fluidRow(withSpinner(plotOutput("gender_plot")), br(),
                                                     withSpinner(plotOutput("remote_plot"))), 
                                            fluidRow(withSpinner(plotOutput("agency_size_plot")),br(),
                                                     withSpinner(plotOutput("tech_company_plot")))
                                            
                                ),
                                box(
                                  tags$h1("Age Distribution w.r.t Gender and Agency Size"),
                                  splitLayout(style = "border: 1px solid silver:", cellWidths = c("100%","100%"),
                                              withSpinner(plotOutput("age_gender_plot")),
                                              withSpinner(plotOutput("age_gender_plot2"))
                                             
                                              
                                  )
                                  
                                  
                                )
                              )
                            )
                   ),
                   
                   tabPanel("Employer Support",
                            tags$h1("Employer Support Data"),
                            setBackgroundColor(color = c("mistyrose", "#2171B5","aqua"),
                                                 gradient = "linear",
                                                 direction = "bottom"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("age_range2", h3("Age"),width = 560,dragRange = F, min = 10,max = 75,value = c(10, 100)),br(),
                                radioButtons("radio2",h3("Remote Work"), inline = T,choices = list("Both" = 1, "Yes" = 2,"No" = 3),selected = 1),br(),
                                dropdownButton(inputId = "CountryInput2", label = "Country",circle = F,size = "lg",
                                               checkboxGroupInput(inputId = "chk_box2",label = "Select", choices = c(unique(survey$Country)))),br(),
                                checkboxGroupInput("checkGroup2", h3("Gender:"), choices = c(unique(survey$Gender)), selected = c(unique(survey$Gender))),br(),
                                checkboxGroupButtons(inputId = "checkboxGroupButtons2",h3("Agency Size"),choices = c('Small','Medium','Large')),br(),
                                tags$h6("Note: Agency Size = Small if number of employees = 1 to 100\n
                                        Agency Size = Medium if number of employees = 101 to 1000\n
                                        Agency Size = Large if number of employees = more than 1000\n
                                        \nAll values selected if no input selection"),br()
                                
                                ),
                              mainPanel(
                                splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                            fluidRow(withSpinner(plotOutput("treatment_plot")),br(),
                                                     withSpinner(plotOutput("family_hist_plot"))),
                                            fluidRow(withSpinner(plotOutput("work_interfere_plot")),br(),
                                                     withSpinner(plotOutput("mental_vs_physical_plot")))
                                )
                              )
                            )
                            ),

                   tabPanel("Employee Support",
                            tags$h1("Employee Support Data"),
                            setBackgroundColor(color = c("mistyrose", "#2171B5","aqua"),
                                               gradient = "linear",
                                               direction = "bottom"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("age_range3", h3("Age"),width = 560,dragRange = F, min = 10,max = 75,value = c(10, 100)),br(),
                                radioButtons("radio3",h3("Remote Work"), inline = T,choices = list("Both" = 1, "Yes" = 2,"No" = 3),selected = 1),br(),
                                dropdownButton(inputId = "CountryInput3", label = "Country",circle = F,size = "lg",
                                               checkboxGroupInput(inputId = "chk_box3",label = "Select", choices = c(unique(survey$Country)))),br(),
                                checkboxGroupInput("checkGroup3", h3("Gender:"), choices = c(unique(survey$Gender)), selected = c(unique(survey$Gender))),br(),
                                checkboxGroupButtons(inputId = "checkboxGroupButtons3",h3("Agency Size"),choices = c('Small','Medium','Large')),br(),
                                tags$h6("Note: Agency Size = Small if number of employees = 1 to 100\n
                                        Agency Size = Medium if number of employees = 101 to 1000\n
                                        Agency Size = Large if number of employees = more than 1000\n
                                        \nAll values selected if no input selection"),br()
                                
                              ),
                              mainPanel(
                                splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                            fluidRow(withSpinner(plotOutput("benefit_plot")),br(),
                                                     withSpinner(plotOutput("care_options_plot"))),
                                            fluidRow(withSpinner(plotOutput("wellness_program_plot")),br(),
                                                     withSpinner(plotOutput("seek_help_plot")))
                                )
                              )
                            )
                   )
                   )
              )


server <- shinyServer(function(input, output, session) {
  
  survey_filtered <- reactive(
    if (is.null(input$chk_box) || input$chk_box == "Top Ten") {
      
      survey_filter <- survey %>% filter(Age >= input$age_range1[1],Age <= input$age_range1[2]) %>% arrange(Country) 
      
      survey_filter <- droplevels(survey_filter)
      
    } else {
      
      survey %>% filter(Age >= input$age_range1[1],Age <= input$age_range1[2], Country %in% input$chk_box)
    }
  )
  
  update_plot = function() {
    
    output$gender_plot <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = Gender)) + geom_bar(color = 'green', fill = 'springgreen') + 
        geom_text(stat = 'Count', aes(label=..count..), vjust=-1) + ggtitle("Gender Distribution") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orangered")) +
        theme(panel.background = element_blank(),  
              panel.border = element_rect(colour = "midnightblue", fill=NA, size=1)) +
        theme(axis.line = element_line(colour = "grey"))
    )
    

    output$agency_size_plot <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = Agency_Size)) + geom_bar(color = 'green', fill = 'yellow1') + 
        geom_text(stat = 'Count', aes(label=..count..), vjust=-1) + ggtitle("Agency SIze Distribution") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orangered")) +
        theme(panel.background = element_blank(),  
              panel.border = element_rect(colour = "midnightblue", fill=NA, size=1)) +
        theme(axis.line = element_line(colour = "grey"))
    )
    
    output$remote_plot <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = remote_work)) + geom_bar( fill = 'plum4') + 
        geom_text(stat = 'Count', aes(label=..count..), vjust=-1) + ggtitle("Remote Work or Not?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orangered")) +
        theme(panel.background = element_blank(),  
              panel.border = element_rect(colour = "midnightblue", fill=NA, size=1)) +
        theme(axis.line = element_line(colour = "grey"))
    )
    
    output$tech_company_plot <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = tech_company)) + geom_bar( fill = 'violetred') + 
        geom_text(stat = 'Count', aes(label=..count..), vjust=-1) + ggtitle("Works in Tech Company or Not?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orangered")) +
        theme(panel.background = element_blank(),  
              panel.border = element_rect(colour = "midnightblue", fill=NA, size=1)) +
        theme(axis.line = element_line(colour = "grey"))
    )
    
    output$age_gender_plot <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = Gender, y = Age)) + 
        geom_boxplot() +
        scale_fill_manual(values = c("#003366", "#339999","#99CC66")) +
        coord_cartesian(ylim = c(0, 80)) +  
        theme_bw() +
        xlab("") +
        ylab("Age") +
        theme(legend.position = "none") +
        ggtitle(("Age Distribution"))
    )
    
    output$age_gender_plot2 <- renderPlot(
      survey_filtered() %>%
        ggplot(aes(x = Agency_Size, y = Age)) + 
        geom_boxplot() +
        scale_fill_manual(values = c("#003366", "#339999","#99CC66")) +
        coord_cartesian(ylim = c(0, 80)) +  
        theme_bw() +
        xlab("") +
        ylab("Age") +
        theme(legend.position = "none") +
        ggtitle(("Age Distribution"))
    )
    
  }

  observeEvent(eventExpr = input$chk_box, ignoreNULL = FALSE, {
    update_plot()
  })
  
  # Employer Page 
  survey_filtered_employer <- reactive(
    
    if (is.null(input$chk_box2) || input$chk_box2 == "Top Ten") {
      
      if(input$radio2==1){ survey_filter <- survey}
      if(input$radio2==2){ survey_filter <- survey %>% filter(remote_work == 'Yes')}
      if(input$radio2==3){ survey_filter <- survey %>% filter(remote_work == 'No')}
      
      if(is.null(input$checkGroup2)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Gender %in% input$checkGroup2)}
      
      if(is.null(input$checkboxGroupButtons2)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Agency_Size %in% input$checkboxGroupButtons2)}
      
      survey_filter <- survey_filter %>% filter(Age >= input$age_range2[1],Age <= input$age_range2[2]) %>% arrange(Country) 
      survey_filter <- droplevels(survey_filter)

    } else {
      
      if(input$radio2==1){ survey_filter <- survey}
      if(input$radio2==2){ survey_filter <- survey %>% filter(remote_work == 'Yes')}
      if(input$radio2==3){ survey_filter <- survey %>% filter(remote_work == 'No')}
      
      if(is.null(input$checkGroup2)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Gender %in% input$checkGroup2)}
      
      if(is.null(input$checkboxGroupButtons2)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Agency_Size %in% input$checkboxGroupButtons2)}
      
      survey_filter %>% filter(Age >= input$age_range2[1],Age <= input$age_range2[2], Country %in% input$chk_box2)
      
      
    }
  )
  
  update_plot_employer = function() {

    output$treatment_plot <- renderPlot({
      
      pc_treatment <- survey_filtered_employer() %>%
        group_by(., treatment) %>%
        count() %>%
        ungroup() %>%
        mutate(per=`n`/sum(`n`)) %>%
        arrange(desc(per))
      pc_treatment['label'] <-round(pc_treatment$per*100,2)
      
      ggplot(data=pc_treatment) +
        geom_bar(aes(x="", y=per, fill=treatment), stat="identity") +
        coord_polar("y", start=0)+
        scale_fill_manual(values = c("#99CC66", "#339999", "#FF9966")) +
        theme_void()+
        guides(fill=guide_legend(title="Response")) +
        geom_text(aes(x=1, y = cumsum(per) - per/2, label=label), size=5) +
        ggtitle("Have you sought treatment for a mental health condition?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "maroon")) +
        theme(panel.border = element_rect(colour = "maroon", fill=NA, size=1))

    })
    
    
    output$family_hist_plot <- renderPlot({
      
      pc_fam_hist <- survey_filtered_employer() %>%
        group_by(., family_history) %>%
        count() %>%
        ungroup() %>%
        mutate(per=`n`/sum(`n`)) %>%
        arrange(desc(per))
      pc_fam_hist['label'] <-round(pc_fam_hist$per*100,2)
      
      ggplot(data=pc_fam_hist) +
        geom_bar(aes(x="", y=per, fill=family_history), stat="identity") +
        coord_polar("y", start=0)+
        scale_fill_manual(values = c("salmon" ,"rosybrown1")) +
        theme_void()+
        guides(fill=guide_legend(title="Response")) +
        geom_text(aes(x=1, y = cumsum(per) - per/2, label=label), size=5) +
        ggtitle("Do you have a family history of mental illness?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "maroon")) +
        theme(panel.border = element_rect(colour = "maroon", fill=NA, size=1))
      
    })
    
    output$work_interfere_plot <- renderPlot({
      
      pc_work_interfere <- survey_filtered_employer() %>%
        group_by(., work_interfere) %>%
        count() %>%
        ungroup() %>%
        mutate(per=`n`/sum(`n`)) %>%
        arrange(desc(per))
      pc_work_interfere['label'] <-round(pc_work_interfere$per*100,2)
      
      ggplot(data=pc_work_interfere) +
        geom_bar(aes(x="", y=per, fill=work_interfere), stat="identity") +
        coord_polar("y", start=0)+
        scale_fill_manual(values = c("#99CC66", "#339999", "#FF9966","#336666")) +
        theme_void()+
        guides(fill=guide_legend(title="Response")) +
        geom_text(aes(x=1, y = cumsum(per) - per/2, label=label), size=5) +
        ggtitle("If you have a mental health condition, \ndo you feel that it interferes with your work?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "maroon")) +
        theme(panel.border = element_rect(colour = "maroon", fill=NA, size=1))
      
    })
    

    
    output$mental_vs_physical_plot <- renderPlot({
      
      pc_mental_vs_physical <- survey_filtered_employer() %>%
        group_by(., mental_vs_physical) %>%
        count() %>%
        ungroup() %>%
        mutate(per=`n`/sum(`n`)) %>%
        arrange(desc(per))
      pc_mental_vs_physical['label'] <-round(pc_mental_vs_physical$per*100,2)
      
      ggplot(data=pc_mental_vs_physical) +
        geom_bar(aes(x="", y=per, fill=mental_vs_physical), stat="identity") +
        coord_polar("y", start=0)+
        scale_fill_manual(values = c("springgreen","mediumseagreen" ,"aquamarine")) +
        theme_void()+
        guides(fill=guide_legend(title="Response")) +
        geom_text(aes(x=1, y = cumsum(per) - per/2, label=label), size=5) +
        ggtitle("Do you feel that your employer takes mental health \nas seriously as physical health?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "maroon")) +
        theme(panel.border = element_rect(colour = "maroon", fill=NA, size=1))
      
    })

  }
  observeEvent(eventExpr = input$chk_box2, ignoreNULL = FALSE, {
    update_plot_employer()
  })
  
  
  # Employee Page 
  survey_filtered_employee <- reactive(
    
    if (is.null(input$chk_box3) || input$chk_box3 == "Top Ten") {
      
      if(input$radio3==1){ survey_filter <- survey}
      if(input$radio3==2){ survey_filter <- survey %>% filter(remote_work == 'Yes')}
      if(input$radio3==3){ survey_filter <- survey %>% filter(remote_work == 'No')}
      
      if(is.null(input$checkGroup3)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Gender %in% input$checkGroup3)}
      
      if(is.null(input$checkboxGroupButtons3)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Agency_Size %in% input$checkboxGroupButtons3)}
      
      survey_filter <- survey_filter %>% filter(Age >= input$age_range3[1],Age <= input$age_range3[2]) %>% arrange(Country) 
      survey_filter <- droplevels(survey_filter)
      
    } else {
      
      if(input$radio3==1){ survey_filter <- survey}
      if(input$radio3==2){ survey_filter <- survey %>% filter(remote_work == 'Yes')}
      if(input$radio3==3){ survey_filter <- survey %>% filter(remote_work == 'No')}
      
      if(is.null(input$checkGroup3)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Gender %in% input$checkGroup3)}
      
      if(is.null(input$checkboxGroupButtons3)){survey_filter <- survey_filter}
      else{survey_filter <- survey_filter %>% filter(Agency_Size %in% input$checkboxGroupButtons3)}
      
      survey_filter %>% filter(Age >= input$age_range3[1],Age <= input$age_range3[2], Country %in% input$chk_box3)
      
      
    }
  )
  
  update_plot_employee = function() {
    
    output$benefit_plot <- renderPlot({
      
      benefit_dc <- survey_filtered_employee() %>% group_by(., benefits) %>% count(.,)
      benefit_dc$per <- round((benefit_dc$n/sum(benefit_dc$n)),2)
      benefit_dc$ymax <- cumsum(benefit_dc$per)
      benefit_dc$ymin <- c(0, head(benefit_dc$ymax, n=-1))
      benefit_dc$labelPosition <- (benefit_dc$ymax + benefit_dc$ymin) / 2
      benefit_dc['label'] <- paste0(benefit_dc$benefits, "\n", round((benefit_dc$per*100),2),"%")
      
      ggplot(data = benefit_dc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = benefits)) +
        geom_rect() +
        geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4.5) +
        scale_fill_manual(values = c("yellow", "blue", "green","#336699")) +
        coord_polar(theta = "y") +
        xlim(c(2,4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Does your employer provide mental health benefits?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orchid4")) +
        theme(panel.border = element_rect(colour = "thistle", fill=NA, size=1))
      
    })
    
    
    output$care_options_plot <- renderPlot({
      
      care_options_dc <- survey_filtered_employee() %>% group_by(., care_options) %>% count(.,)
      care_options_dc$per <- round((care_options_dc$n/sum(care_options_dc$n)),2)
      care_options_dc$ymax <- cumsum(care_options_dc$per)
      care_options_dc$ymin <- c(0, head(care_options_dc$ymax, n=-1))
      care_options_dc$labelPosition <- (care_options_dc$ymax + care_options_dc$ymin) / 2
      care_options_dc['label'] <- paste0(care_options_dc$care_options, "\n", round((care_options_dc$per*100),2),"%")
      
      ggplot(data = care_options_dc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = care_options)) +
        geom_rect() +
        geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4.5) +
        scale_fill_manual(values =  c("yellow", "blue", "green","#336699")) +
        coord_polar(theta = "y") +
        xlim(c(2,4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Do you know the options for mental health \n care your employer provides?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orchid4")) +
        theme(panel.border = element_rect(colour = "thistle", fill=NA, size=1))
      
    })
    
    output$wellness_program_plot <- renderPlot({
      
      wellness_program_dc <- survey_filtered_employee() %>% group_by(., wellness_program) %>% count(.,)
      wellness_program_dc$per <- round((wellness_program_dc$n/sum(wellness_program_dc$n)),2)
      wellness_program_dc$ymax <- cumsum(wellness_program_dc$per)
      wellness_program_dc$ymin <- c(0, head(wellness_program_dc$ymax, n=-1))
      wellness_program_dc$labelPosition <- (wellness_program_dc$ymax + wellness_program_dc$ymin) / 2
      wellness_program_dc['label'] <- paste0(wellness_program_dc$wellness_program, "\n", round((wellness_program_dc$per*100),2),"%")
      
      ggplot(data = wellness_program_dc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = wellness_program)) +
        geom_rect() +
        geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4.5) +
        scale_fill_manual(values = c("yellow", "blue", "green","#336699")) +
        coord_polar(theta = "y") +
        xlim(c(2,4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Has your employer ever discussed mental health \n as part of an employee wellness program?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orchid4")) +
        theme(panel.border = element_rect(colour = "thistle", fill=NA, size=1))
      
    })
    
    
    output$seek_help_plot <- renderPlot({
      
      seek_help_dc <- survey_filtered_employee() %>% group_by(., seek_help) %>% count(.,)
      seek_help_dc$per <- round((seek_help_dc$n/sum(seek_help_dc$n)),2)
      seek_help_dc$ymax <- cumsum(seek_help_dc$per)
      seek_help_dc$ymin <- c(0, head(seek_help_dc$ymax, n=-1))
      seek_help_dc$labelPosition <- (seek_help_dc$ymax + seek_help_dc$ymin) / 2
      seek_help_dc['label'] <- paste0(seek_help_dc$seek_help, "\n", round((seek_help_dc$per*100),2),"%")
      
      ggplot(data = seek_help_dc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = seek_help)) +
        geom_rect() +
        geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4.5) +
        scale_fill_manual(values =  c("yellow", "blue", "green","#336699")) +
        coord_polar(theta = "y") +
        xlim(c(2,4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Does your employer provide resources to learn more \n about mental health issues and how to seek help?") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", colour = "orchid4")) +
        theme(panel.border = element_rect(colour = "thistle", fill=NA, size=1))
      
    })
    
  }
  observeEvent(eventExpr = input$chk_box3, ignoreNULL = FALSE, {
    update_plot_employee()
  })
  
  
})

shinyApp(ui = ui, server = server)



