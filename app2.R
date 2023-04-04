#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Dashboard




# Connect with data file

source("ml_models.R", local = T)
options(scipen=999)

##The actual board

# Define the UI
ui <- navbarPage(
    title = "Salary Comparison Dashboard",
    tabPanel("Mean Salary", sidebarLayout(   #mean salary
        sidebarPanel(
            selectInput("company_input", "Select company(s):", choices = c("All", unique(salaries$company)), multiple = TRUE),
            selectInput("job_title_input", "Select job title(s):", choices = c("All", unique(salaries$job_title)), multiple = TRUE)
        ),
        mainPanel(
            plotOutput("salary_plot_mean"),
            plotOutput("salary_plot_median")
        )
    )),
    #  tabPanel("Median Salary", 
    #    mainPanel(
    #      plotOutput("salary_plot_median")
    #    )
    #  ),
    tabPanel("Time Series Salary", sidebarLayout(   #time series
        sidebarPanel(
            selectInput("company_input_ts", "Select company(s):", choices = c("All", unique(salaries$company)), multiple = FALSE),
            selectInput("job_title_input_ts", "Select job title(s):", choices = c("All", unique(salaries$job_title)), multiple = FALSE)
        ), 
        mainPanel(
            plotOutput("ts_plot")
        )
    ) 
    ),
    tabPanel("Salary Predictor", sidebarLayout(   #xgboost
        sidebarPanel(
            selectInput("job_tag", "Job Tag",  #job tag
                        choices = c("analytics", "finance", "engineering", "economics", "marketing", 
                                    "director", "UX", "mobile", "security", "production")),
            selectInput("education", "Education",  #education
                        choices = c("Masters_Degree", "Bachelors_Degree", "Doctorate_Degree", "Highschool", 
                                    "Some_College")),
            selectInput("job_title", "Job Title",  #job title
                        choices = levels(as.factor(final_data$title))),
            selectInput("company", "Company",      #company
                        choices = levels(as.factor(final_data$company))),
            selectInput("gender", "Gender",        #gender
                        choices = c("Male", "Female")),
            selectInput("race", "Race",         #race
                        choices = c("Asian", "White", "Two_Or_More", "Black", "Hispanic")),
            selectInput("state", "State",       #state
                        choices = levels(as.factor(final_data$state))),
            numericInput("years_of_experience", "Years of Experience", 0), #years experience
            numericInput("years_at_company", "Years at Company", 0), #years company
            actionButton("submit", "Submit"), #submit button
            # textOutput("predicted_salary")  #ayyy 
        ), 
        mainPanel(
            textOutput("predicted_salary")
        )
    ) 
    )  
    
    
    
)

# Define the server
server <- function(input, output) {
    
    # Create a reactive filtered dataset based on the user input
    filtered_salaries <- reactive({
        if ("All" %in% input$company_input & "All" %in% input$job_title_input) {
            return(salaries)
        }
        else if (!"All" %in% input$company_input & "All" %in% input$job_title_input) {
            return(salaries %>% filter(company %in% input$company_input))
        }
        else if ("All" %in% input$company_input & !"All" %in% input$job_title_input) {
            return(salaries %>% filter(job_title %in% input$job_title_input))
        }
        else {
            return(salaries %>% filter(company %in% input$company_input & job_title %in% input$job_title_input))
        }
    })
    
    # Reactive summary table based on the filtered data -- mean
    summary_table_mean <- reactive({
        filtered_salaries() %>% #only gimme the filtered salary info
            group_by(company, job_title) %>% #group by the two sections
            summarise(avg_salary = mean(salary), .groups = "keep") #summarise by mean. -could change to median if I wanted less outlier influence
    })
    
    # Reactive plot from the summary_table -- mean
    output$salary_plot_mean <- renderPlot({
        ggplot(summary_table_mean(), aes(x = job_title, y = avg_salary, fill = company)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Average Salary by Job Title and Company", x = "Job Title", y = "Average Salary") +
            ylim(0, 500000) + theme(panel.grid = element_blank())
    })
    #Reactive summary table based on the filtered data-- median
    summary_table_median <- reactive({
        filtered_salaries() %>% #only gimme the filtered salary info
            group_by(company, job_title) %>% #group by the two sections
            summarise(avg_salary = median(salary), .groups = "keep") #summarize by median
    })
    #Reactive plot from the summary_table -- median
    output$salary_plot_median <- renderPlot({
        ggplot(summary_table_median(), aes(x = job_title, y = avg_salary, fill = company)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Median Salary by Job Title and Company", x = "Job Title", y = "Median Salary") +
            ylim(0, 500000) + theme(panel.grid = element_blank())
        #  + geom_text(stat = "summary", fun = "median", aes(label = sprintf("$%.2f", ..y..)), vjust = -1)
    })
    
    # Create a reactive filtered dataset based on the user input for time series
    filtered_salaries_ts <- reactive({
        if ("All" %in% input$company_input_ts & "All" %in% input$job_title_input_ts) {
            return(salaries)
        }
        else if (!"All" %in% input$company_input_ts & "All" %in% input$job_title_input_ts) {
            return(salaries %>% filter(company %in% input$company_input_ts))
        }
        else if ("All" %in% input$company_input_ts & !"All" %in% input$job_title_input_ts) {
            return(salaries %>% filter(job_title %in% input$job_title_input_ts))
        }
        else {
            return(salaries %>% filter(company %in% input$company_input_ts & job_title %in% input$job_title_input_ts))
        }
    })
    
    # aggregate data by month
    monthly.data <- reactive({
        filtered_salaries_ts() %>%
            mutate(year_month = list(format(date, "%Y-%m"))) %>%
            group_by(year_month, company, job_title) %>%
            summarise(avg_salary = median(salary),  .groups = "keep")
    })
    
    # create time series object
    monthly.ts <- reactive({
        ts(monthly.data()$avg_salary, freq = 12, start = c(2020,7), end = c(2021,8))
    })
    
    # create plot
    output$ts_plot <- renderPlot({
        ggplot(data.frame(time = time(monthly.ts()), value = monthly.ts()), aes(x = time, y = value)) +
            geom_line() +
            xlab("Time") +
            ylab("Median Salary") +
            ggtitle("Median Salary Over Time by Company or Jobtitle") +
            ylim(0,500000) + theme(panel.grid = element_blank())
    })
    
    #XGBOOST
    # When the Submit button is clicked
    observeEvent(input$submit, {
        
        # Create the user input data frame
        user_entry <- data.frame(
            company = match(input$company, levels(as.factor(final_data$company))),
            title = match(input$job_title, levels(as.factor(final_data$title))),
            totalyearlycompensation = 0, #place holder
            yearsofexperience = input$years_of_experience,
            yearsatcompany = input$years_at_company,
            gender = ifelse(input$gender == "Male", 1, 0),
            Masters_Degree = ifelse(input$education == 'Masters_Degree', 1, 0),
            Bachelors_Degree = ifelse(input$education == 'Bachelors_Degree', 1, 0),
            Doctorate_Degree = ifelse(input$education == 'Doctorate_Degree', 1, 0),
            Highschool = ifelse(input$education == 'Highschool', 1, 0),
            Some_College = ifelse(input$education == 'Some_College', 1, 0),
            Race_Asian = ifelse(input$race == 'Asian', 1, 0),
            Race_White = ifelse(input$race == 'White', 1, 0),
            Race_Two_Or_More = ifelse(input$race == 'Two_Or_More', 1, 0),
            Race_Black = ifelse(input$race == 'Black', 1, 0),
            Race_Hispanic = ifelse(input$race == 'Hispanic', 1, 0),
            state = match(input$state, levels(as.factor(final_data$state))),
            analytics_tag = ifelse(input$job_tag == 'analytics', 1, 0),
            finance_tag = ifelse(input$job_tag == 'finance', 1, 0),
            analytics_tag = ifelse(input$job_tag == 'analytics', 1, 0), 
            finance_tag = ifelse(input$job_tag == 'finance', 1, 0), 
            engineering_tag = ifelse(input$job_tag == 'engineering', 1, 0), 
            economics_tag = ifelse(input$job_tag == 'economics', 1, 0), 
            marketing_tag = ifelse(input$job_tag == 'marketing', 1, 0), 
            director_tag = ifelse(input$job_tag == 'director', 1, 0), 
            UX_tag = ifelse(input$job_tag == 'UX', 1, 0), 
            mobile_tag = ifelse(input$job_tag == 'mobile', 1, 0), 
            security_tag = ifelse(input$job_tag == 'security', 1, 0), 
            production_tag = ifelse(input$job_tag == 'production', 1, 0),
            stringsAsFactors = FALSE
        )
        
        # Use the trained model to predict salary
        features <- names(model_data)
        user_entry <- user_entry[,features]
        colnames(user_entry) <- colnames(bst$model)
        user_entry <- user_entry[,c(1:2, 4:27)]
        user_test <- xgb.DMatrix(data = as.matrix(user_entry))
        prediction <- predict(bst, newdata = user_test)
        
        # Output the predicted salary
        output$predicted_salary <- renderText({
            paste0("Your predicted salary is: $", round(prediction, 2))
        })
    })
}

shinyApp(ui, server)
