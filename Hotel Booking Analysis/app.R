
library(dplyr)
library(ggplot2)
library(caret)
library(superml)
library(reshape2)
library(reshape)
library(e1071)
library(caret)
library(pROC)
library(DALEX)
library(rpart)
library(rpart.plot)
library(randomForest)
library(PerformanceAnalytics)
library(gridExtra)
library(purrr)
library(rattle)
library(RColorBrewer)
library(shiny)
library(DT)
library(readr)
library(tidyverse)
library(shinythemes)
library(bslib)
library(lubridate)

# Define UI for application
ui <- fluidPage(theme = bs_theme(bootswatch = "litera"),
                navbarPage(tags$b(a("Hotel Bookings Analysis",style = "color:#7ec0ee")),id = "inTabset",
                           tabPanel(title = "Data", value = "panel1", 
                                    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("file", "Choose CSV File",
                    multiple = FALSE,
                    accept = c(".csv")),
          helpText("The analysis may take several minutes"),
          br(),
          br(),
          actionButton("button", "Start estimation", icon("magnifying-glass-chart"), 
                       style="color: #FFFFFF; background-color: #6495ED"),
          hr(),
          helpText("Made with", a("Shiny",
                           href = "http://shiny.rstudio.com"), ".")
        ),

        # Show a plot of the generated distribution
        mainPanel(DTOutput(outputId = "table")))),
    tabPanel(title = "Exploratory Data Analysis", value = "panel2",
             navlistPanel("Variables",
                          tabPanel("Bookings",
                     br(),
                     plotOutput(outputId = "countries"),
                     br(),
                     plotOutput(outputId = "circle"),
                     actionButton("nexxt", "Next", icon("magnifying-glass-chart"), 
                                  style="color: #FFFFFF; background-color: #6495ED"),
                     br(),
                     plotOutput(outputId = "reservation"),
                     br(),
                     plotOutput(outputId = "type"),
                     br(),
                     plotOutput(outputId = "revenue"),
                     br()),
                     tabPanel("Average Daily Rate",
                              br(),
                     plotOutput(outputId = "adr"),
                     br(),
                     plotOutput(outputId = "adr2"),
                     br(),
                     plotOutput(outputId = "adr3"),
                     br(),
                     plotOutput(outputId = "adr4"),
                     br(),
                     plotOutput(outputId = "adr5"),
                     br(),
                     plotOutput(outputId = "adr6"),
                     br(),
                     plotOutput(outputId = "adr7"),
                     br(),
                     plotOutput(outputId = "adr8"),
                     br(),
                     plotOutput(outputId = "adr9"),
                     br(),
                     plotOutput(outputId = "adr10"),
                     br(),
                     plotOutput(outputId = "adr11"),
                     br(),
                     plotOutput(outputId = "adr12"),
                     br(),
                     plotOutput(outputId = "adr13"),
                     br(),
                     plotOutput(outputId = "adr14"),
                     br(),
                     plotOutput(outputId = "adr15"),
                     br(),
                     plotOutput(outputId = "adr16"),
                     br(),
                     plotOutput(outputId = "adr17"),
                     br())
                     )),
    navbarMenu(title = "Models", 
               tabPanel("Bookings", value = "panel3",
                        navlistPanel("Booking Prediction Options",
                                     tabPanel("ML Model Predictions",
                     br(),
                     plotOutput(outputId = "cancel"),
                     br(),
                     h4(p("Logistic Regression")),
                     verbatimTextOutput("glm"),
                     br(),
                     plotOutput("glmplot"),
                     br(),
                     verbatimTextOutput("confglm"),
                     br(),
                     h4(p("Decision Tree")),
                     plotOutput("dt"),
                     br(),
                     verbatimTextOutput("dt2"),
                     br(), 
                     h4(p("Random Forest")),
                     plotOutput("randomf1"),
                     br(),
                     verbatimTextOutput("randomf2"),
                     br()),
                     tabPanel("DALEX Interpretation",
                     plotOutput(outputId = "glmdalex1"),
                     br(),
                     plotOutput(outputId = "glmdalex2"),
                     br(),
                     plotOutput(outputId = "glmdalex3"),
                     br(),
                     h4(p("Decision Tree")),
                     br(),
                     plotOutput(outputId = "dtdalex1"),
                     br(),
                     plotOutput(outputId = "dtdalex2"),
                     br(),
                     plotOutput(outputId = "dtdalex3"),
                     br(),
                     h4(p("Random Forest")),
                     br(),
                     plotOutput(outputId = "randomfdalex1"),
                     br(),
                     plotOutput(outputId = "randomfdalex2"),
                     br(),
                     plotOutput(outputId = "randomfdalex3")))))
          )
        )
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) #file size extension
  
  data <- reactive({ # file input
    req(input$file)
    if(is.null(file)){return()} 
    read_csv(input$file$datapath)
  })
  
  observeEvent(input$button, {
  # Data Processing
  output$table <- renderDT({ # data table
    data <- data()
    df = data
    # eliminate NA and other undefined value
    df$meal[df$meal=='Undefined'] <- 'SC'
    df$children[is.na(df$children)] <- 0
    df <- subset(df, market_segment!='Undefined')
    df <- subset(df, distribution_channel!='Undefined')
    df$is_agent <- ifelse(is.na(df$agent), 0, 1)
    df$is_company <- ifelse(is.na(df$company), 0, 1)
    df = df %>% select(-agent, -company)
    # New variables
    # Total guests
    df <- mutate(df,total_guests = adults + children + babies)
    # In each row, number of adults, children and babies cannot be 0 together.
    df = df %>% filter(total_guests>0)
    # ADR
    df = df %>% filter(adr>0)
    # Total Costs
    df <- mutate(df,total_costs = (stays_in_weekend_nights+stays_in_week_nights)*adr)
    #Number of stays in weekend and weekdays also cannot be 0, considering hotel booking is count per night
    df<-df[!(df$stays_in_weekend_nights== 0 & df$stays_in_week_nights == 0),]
    df<-df[!(df$adults== 0),]
    
    df = df %>% select(-reservation_status, -reservation_status_date, -total_guests)
    df
    })
  

  output$countries <- renderPlot({
    country_df <- df[df$is_canceled == 0,]
    country_df <- head(country_df %>% count(country,sort=TRUE),n=10)
    
    barplot(country_df$n,names.arg=country_df$country,
            xlab="Country",ylab="Number of Reservation",
            col="#87CEFA", main="Top 10 Reservation based on Guest's Country",border="black")
  })
  
  
  output$circle <- renderPlot({
  # Extract data where the booking is canceled but deposit is non-refundable
  ms1 <- df[df$is_canceled == 1 & df$deposit_type == "Non Refund",]
  ms1 <- select(df, hotel, arrival_date_year, market_segment, adr)

  # Extract data where guest completed their stay in hotel
  ms2 <- df[df$is_canceled == 0,]
  ms2 <- select(df, hotel, arrival_date_year, market_segment, adr)
  
  # Combine dataframe ms1 and ms2
  ms <- rbind(ms1,ms2)
  
  ms <- ms %>%
    group_by(market_segment) %>%
    summarise(Revenue = sum(adr)) %>% arrange(desc(Revenue)) %>% head(5)
  
  # Compute percentages
  ms$fraction <- round(ms$Revenue / sum(ms$Revenue)*100,2)
  
  # Compute the cumulative percentages (top of each rectangle)
  ms$ymax <- cumsum(ms$fraction)
  
  # Compute the bottom of each rectangle
  ms$ymin <- c(0, head(ms$ymax, n=-1))
  
  # Compute label position
  ms$labelPosition <- (ms$ymax + ms$ymin) / 2
  
  # Compute a good label
  ms$label <- paste0(ms$market_segment, "\n", ms$Revenue, "\n", ms$fraction, " %")
  
  ggplot(ms, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=market_segment)) +
    geom_rect() +
    geom_label(x=4, aes(y=labelPosition, label=label), size=3.5) +
    scale_fill_brewer(palette="Blues") +
    coord_polar(theta="y") +
    xlim(c(1, 4)) +
    theme_void()+ 
    theme(legend.position = "none")+
    ggtitle("Hotel Revenue By Market Segment")
  })
  
  observeEvent(input$nexxt, {
    removeUI(selector='#countries', immediate=TRUE)
    removeUI(selector='#circle', immediate=TRUE)
    removeUI(selector='#nexxt', immediate=TRUE)

  output$reservation <- renderPlot({
    ggplot(df, aes(arrival_date_month, fill = factor(is_canceled))) +
      geom_bar() +
      geom_text(stat = "count",
                aes(label = after_stat(count)),
                position = position_stack(vjust = 0.5)) +
      scale_fill_discrete(
        name = "Reservation Status",
        breaks = c("0", "1"),
        label = c("Not Cancelled", "Cancelled")
      ) +
      labs(title = "Reservation Status by Month", x = "Month", y = "Count") +
      scale_x_discrete(labels = month.abb)+
      scale_fill_brewer(palette="Blues")+
      theme_minimal()+
      labs(fill='Is canceled') 
  })
  }, autoDestroy=TRUE)
  
  output$type <- renderPlot({
    ggplot(data = df,
           aes(
             x = hotel,
             y = prop.table(stat(count)),
             fill = factor(is_canceled),
             label = scales::percent(prop.table(after_stat(count)))
           )) +
      geom_bar(position = position_dodge()) +
      geom_text(
        stat = "count",
        position = position_dodge(.9),
        vjust = -0.5,
        size = 3
      ) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Reservation Status by Hotel Type", x = "Hotel Type", y = "Count") +
      theme_classic() +
      scale_fill_discrete(
        name = "Reservation Status",
        breaks = c("0", "1"),
        labels = c("Guest", "Cancelled")
      ) +
      scale_fill_brewer(palette="Blues") + theme_minimal()+
      labs(fill='Is canceled')
  })
  
  
  output$revenue <- renderPlot({
    # filter data of non-canceled booking
    revenue_by_month <- df %>%
      filter(is_canceled == 0) %>%
      group_by(hotel, arrival_date_year, arrival_date_month) %>%
      summarise(total_revenue = sum(adr * (stays_in_week_nights + stays_in_weekend_nights))) %>%
      mutate(month_num = match(arrival_date_month, month.name),
             months = factor(substr(arrival_date_month, 1, 3), 
                             levels = month.abb)) %>%
      arrange(hotel, arrival_date_year, month_num)
    
    #lowest revenue
    lowest_revenue <- revenue_by_month %>%
      group_by(hotel, arrival_date_year) %>%
      slice_min(total_revenue) %>%
      ungroup()
    
    #highest revenue
    highest_revenue <- revenue_by_month %>%
      group_by(hotel, arrival_date_year) %>%
      slice_max(total_revenue) %>%
      ungroup()
    
    # create a new data frame to store the highest and lowest revenue.
    highest_lowest_revenue <- revenue_by_month %>%
      group_by(arrival_date_year, hotel) %>%
      slice_min(total_revenue, n = 1) %>% # get the lowest point
      bind_rows(revenue_by_month %>% 
                  group_by(arrival_date_year, hotel) %>%
                  slice_max(total_revenue, n = 1)) %>% # get the highest point
      arrange(arrival_date_year)
    
    #plot the chart
    ggplot(revenue_by_month, aes(x = months, y = total_revenue, color = hotel, group = hotel)) +
      facet_wrap(~arrival_date_year, ncol = 1) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("City Hotel" = "darkslateblue", "Resort Hotel" = "skyblue2")) +
      labs(title = "Revenue 2018 - 2020",
           x = "Arrival month",
           y = "Total Revenue",
           color = "Hotel Type",
           fill = "Hotel Type") +
      scale_y_continuous(labels = scales::dollar) +
      theme(plot.title = element_text(colour = "#3A3A3A"),
            plot.subtitle = element_text(colour = "#3A3A3A"),
            plot.caption = element_text(colour = "#3A3A3A"),
            panel.background = element_rect(fill = "white"),
            panel.grid.minor.x = element_line(size = 0),
            legend.position = "top",
            axis.title.y = element_text(colour = "#3A3A3A", vjust = 3, hjust = 0.95, size = 10),
            axis.title.x = element_text(colour = "#3A3A3A", hjust = 0.04,vjust = 3, size = 10),
            axis.line = element_line(linewidth = 0.4, colour = "#8E8783")) +
      geom_point(data = highest_lowest_revenue, aes(x = months, y = total_revenue), 
                 size = 3)
  })
  
  output$cancel <- renderPlot({
    #reserved_room_type and assigned_room_type is similar feature. We can combine these into one feature only. We will put 1 if the guest was assigned to the same room that they reserved and 0 if its different.
    df2 <- df
    df2["room_given"]=0
    df2 <- df2 %>% 
      mutate(room_given = if_else(reserved_room_type == assigned_room_type, 1, 0))
    
    #previous_cancellations and previous_bookings_not_canceled describes about cancellation. We can combine these into one feature only. We will put 1 if previous_cancellations is higher than previous_bookings_not_canceled and 0 if its not
    df2 <- df2 %>% 
      mutate(cancellation_tendency = if_else(previous_cancellations > previous_bookings_not_canceled, 1, 0))
    
    df2 <- subset(df2, select = -c(reserved_room_type, assigned_room_type,previous_cancellations, previous_bookings_not_canceled, country ))
    
    ggplot(df2, aes(x=is_canceled)) +
      geom_bar(fill="skyblue2") +theme_minimal() +
      ggtitle("Cancellation Distribution") +
      xlab("Is canceled") +
      ylab("Count")
  })
  
  
  output$glm <- renderPrint({
  # Data Splitting
  set.seed(50)
  df2$is_canceled <-as.factor(df2$is_canceled)
  trainIndex <- createDataPartition(df2$is_canceled, p = .7,list = FALSE,times = 1)
  train_data<- df2[ trainIndex,]
  test_data <- df2[-trainIndex,]
  
  #DownSampling
  #set.seed(50)
  #train_data_down<-downSample(x=train_data[,-ncol(train_data)],
  #                            y=train_data$is_canceled)
  #train_data_down <- subset(train_data_down, select = -c(Class))
  
  modelLR = glm(is_canceled ~ . -total_costs , family= "binomial", data = train_data)
  summary(modelLR)
  })
  
  output$glmplot <- renderPlot({
    plot(modelLR, main = "Error rate of Logistic Regression")
  })
  
  output$confglm <- renderPrint({
    predictionLR = predict(modelLR, newdata = test_data, type = "response")
    predictionLR = ifelse(predictionLR < 0.3, 0, 1)
    
    AUCLR = auc(test_data$is_canceled,as.numeric(predictionLR))
    AUCLR
    
    
    tab_Test = table(actual=test_data$is_canceled, predicted=predictionLR);
    confusionMatrix(tab_Test, mode = "everything", positive = "1")
  })
  
  # Dalex for Logistic Regression
  output$glmdalex1 <- renderPlot({
  df2$is_canceled = as.vector(as.numeric(as.character(df2$is_canceled)))
  df_exLR = explain(modelLR,
                    data  = df2,
                    y     = df2$is_canceled,
                    label = "Logistic Regression")
  
  # instance level
  (single_row = df2[1,])
  # prediction
  df_exLR %>% predict(single_row)
  
  # prediction parts
  plot(predict_parts(df_exLR, new_observation = single_row))
  })
  
  output$glmdalex2 <- renderPlot({
  plot(model_performance(df_exLR), geom = "roc")
  })
  
  output$glmdalex3 <- renderPlot({
  plot(model_parts(df_exLR), show_boxplots = FALSE)
  })
  
  output$dt <- renderPlot({
    modelDT2 = train(is_canceled ~ . -total_costs , method= "rpart", data = train_data)
    predictionsDT2 = predict(modelDT2, newdata = test_data)
    # plotting tree
    rpart.plot(modelDT2$finalModel)
  })
  
  output$dt2 <- renderPrint({
    AUCDT = auc(test_data$is_canceled,as.numeric(predictionsDT2))
    
    tab_Test = table(actual=test_data$is_canceled, predicted=predictionsDT2);
    confusionMatrix(tab_Test, mode = "everything", positive = "1")
  })
  
  output$dtdalex1 <- renderPlot({
  df_exDT = explain(modelDT2,
                    data  = df2,
                    y     = df2$is_canceled,
                    label = "Decision Tree")
  
  # instance level
  (single_row = df2[1,])
  # prediction
  df_exDT %>% predict(single_row)
  
  # prediction parts
  plot(predict_parts(df_exDT, new_observation = single_row))
  })
  
  output$dtdalex2 <- renderPlot({
  plot(model_performance(df_exLR),
       model_performance(df_exDT),
       geom = "roc")
  })
  
  output$dtdalex3 <- renderPlot({
  plot(model_parts(df_exDT), show_boxplots = FALSE)
  })
  
  output$randomf1 <- renderPlot({
    set.seed(45)
    #For train data
    model.rf = randomForest(is_canceled ~ . -total_costs, data = train_data,
                            keep.forest = TRUE, ntree = 150)
    
    #For train data down
   # model_down.rf = randomForest(is_canceled ~ . -total_costs, data = train_data_down,
    #                             keep.forest = TRUE, ntree = 150)
    
    #plot the train data model
    plot(model.rf, main = "Error rate of random forest")
  })
  
  output$randomf2 <- renderPrint({
    #For train data
    pred_Test_rd = predict(model.rf, test_data[,setdiff(names(test_data),"is_canceled")],
                           type="response", 
                           norm.votes=TRUE)
    tab_Test = table(actual=test_data$is_canceled, predicted=pred_Test_rd);
    confusionMatrix(tab_Test, mode = "everything", positive = "1")
  })
  
  output$randomfdalex1 <- renderPlot({
    df_exRF = explain(model.rf,
                      data  = df2,
                      y     = df2$is_canceled,
                      label = "Random Forest")
    
    # instance level
    (single_row = df2[1,])
    # prediction
    df_exRF %>% predict(single_row)
    
    # prediction parts
    plot(predict_parts(df_exRF, new_observation = single_row))
  })
  
  output$randomfdalex2 <- renderPlot({
    plot(model_performance(df_exDT), 
         model_performance(df_exLR),
         model_performance(df_exRF),
         geom = "roc")
  })
  
  output$randomfdalex3 <- renderPlot({
    plot(model_parts(df_exRF), show_boxplots = FALSE)
  })
  
  output$adr <- renderPlot({
    # Convert some of the numeric variables into factors
    df$is_canceled <- as.factor(df$is_canceled)
    df$arrival_date_year <- as.factor(df$arrival_date_year)
    # Rearrange order of the levels of the arrival_date_month factor 
    df$arrival_date_month <- factor(df$arrival_date_month, levels=c("January", "February", "March", "April", 
                                                                    "May", "June", "July", "August", "September", 
                                                                    "October", "November", "December"))
    # remove unused Resort data
    df_resort <- subset(df, hotel=="Resort Hotel", select= -c(hotel,country,distribution_channel,
                                                              is_repeated_guest,previous_cancellations,
                                                              previous_bookings_not_canceled,assigned_room_type,
                                                              booking_changes,deposit_type,is_agent,is_company,
                                                              days_in_waiting_list))
    df_resort$arrival_date <- with(df_resort, paste(arrival_date_year, 
                                                    arrival_date_month, arrival_date_day_of_month, sep="-"))
    df_resort$arrival_date_formatted <- parse_date_time(df_resort$arrival_date, orders = "ymd")
    
    ggplot(df_resort, aes(x=adr)) + 
      geom_histogram(aes(y=..density..), binwidth=5, colour="skyblue2", fill="skyblue2") + 
      geom_density(alpha=.1, fill="skyblue2") + 
      theme_light() + 
      ggtitle("Histogram of average daily rate (adr)") + 
      xlab("Average daily rate (adr)")
  })
  
  output$adr2 <- renderPlot({
    num_df_resort <- select_if(df_resort, is.numeric) #Pick numeric variables
    # Plot correlations and histograms using PerformanceAnalytics library
    chart.Correlation(num_df_resort, histogram=TRUE, pch=19)
  })
  
  output$adr3 <- renderPlot({
    # Number of adults, number of children, total of special requests
    ggplot(df_resort, aes(x=adults, y=adr)) + geom_point() + theme_minimal()+
      ggtitle("ADR per Adults")
  })
  
  output$adr4 <- renderPlot({
    ggplot(df_resort, aes(x=children, y=adr)) + geom_point() + theme_minimal()+
      ggtitle("ADR per Children")
  })
  
  output$adr5 <- renderPlot({
    ggplot(df_resort, aes(x=total_of_special_requests, y=adr)) + geom_point() + theme_minimal()+
    ggtitle("ADR per Total Special Requests")
  })
  
  output$adr6 <- renderPlot({
    week = ggplot(df_resort, aes(x=arrival_date_week_number, y=adr)) + 
      geom_point() + 
      geom_smooth() + 
      theme_light() + 
      ggtitle("Average daily rate vs. Arrival date week number") + 
      xlab("Arrival date week number") + 
      ylab("Average daily rate (adr)")
    
    month = ggplot(df_resort, aes(x=arrival_date_month, y=adr)) + 
      geom_boxplot() + 
      theme_light() + 
      ggtitle("Average daily rate vs. Arrival month") + 
      xlab("Arrival month") + 
      ylab("Average daily rate (adr)")
    
    year <- ggplot(df_resort, aes(x=arrival_date_year, y=adr)) + 
      geom_boxplot() + 
      theme_light() + 
      ggtitle("Average daily rate vs. Arrival date year") + 
      xlab("Arrival date year") + 
      ylab("Average daily rate (adr)")
    
    # Plot week, monthly, and yearly trends on the same display
    grid.arrange(week, month, year, nrow=3, ncol=1)
  })
  
  output$adr7 <- renderPlot({
    # Explore full arrival date trends
    ggplot(df_resort, aes(x=arrival_date_formatted, y=adr)) + 
      geom_point() + 
      theme_light() + 
      ggtitle("Average daily rate vs. Arrival date") + 
      xlab("Arrival date") + 
      ylab("Average daily rate (adr)")
  })
  
  output$adr8 <- renderPlot({
    # Plot arrival_date_day_of_month for each month separately
    df_resort_adr_by_month_day <- df_resort %>% group_by(arrival_date_month, arrival_date_day_of_month) %>% 
      summarise(mean_adr = mean(adr)) %>% arrange(arrival_date_month, arrival_date_day_of_month)
    
    ggplot(data = df_resort_adr_by_month_day, aes(x=arrival_date_day_of_month, y=mean_adr, colour=arrival_date_month)) +
      geom_point() + 
      geom_smooth(method='lm', se=FALSE) + 
      facet_wrap( ~ arrival_date_month) + 
      ggtitle("Mean average daily rate (adr) per day of month by month") + 
      xlab("Day of month") + ylab("Mean average daily rate (adr)") + 
      scale_color_brewer(palette = "Paired") + 
      theme_light() + 
      theme(legend.position = "none") + 
      theme(strip.background = element_rect(color="black", size=1)) + 
      theme(strip.text.x = element_text(color = "black"))
  })
  
  output$adr9 <- renderPlot({
    # Explore categorical variables 
    cat_df_resort <- select_if(df_resort, negate(is.numeric))
    ggplot(df_resort, aes(x=is_canceled, y=adr, colour=is_canceled)) + 
      geom_boxplot() + 
      geom_jitter(position=position_jitter(0.2)) + 
      scale_color_brewer(palette = "Set2") + 
      theme_light() + 
      ggtitle("Average daily rate vs. Cancellation status") + 
      xlab("Cancellation status") + 
      ylab("Average daily rate (adr)") + 
      scale_x_discrete(labels=c("Not cancelled","Cancelled")) + 
      theme(legend.position = "none") 
  })
  
  output$adr10 <- renderPlot({
    ggplot(df_resort, aes(x=market_segment, y=adr, colour=market_segment)) + 
      geom_boxplot() + 
      geom_jitter(position=position_jitter(0.2)) + 
      scale_color_brewer(palette = "Set2") + 
      theme_light() + 
      ggtitle("Average daily rate vs. Market segment") + 
      xlab("Market segment") + 
      ylab("Average daily rate (adr)") +
      theme(legend.position = "none") 
  })
  
  output$adr11 <- renderPlot({
    ggplot(df_resort, aes(x=meal, y=adr, colour=meal)) + 
      geom_boxplot() + 
      geom_jitter(position=position_jitter(0.2)) + 
      scale_color_brewer(palette = "Set2") + 
      theme_light() + 
      ggtitle("Average daily rate vs. Type of Meal") + 
      xlab("Meal") + 
      ylab("Average daily rate (adr)") + 
      theme(legend.position = "none")
  })
  
  output$adr12 <- renderPlot({
    ggplot(df_resort, aes(x=reserved_room_type, y=adr, colour=reserved_room_type)) + 
      geom_boxplot() + 
      geom_jitter(position=position_jitter(0.2)) + 
      scale_color_brewer(palette = "Set2") + 
      theme_light() + 
      ggtitle("Average daily rate vs. Room Type") + 
      xlab("Reserved Room Type") + 
      ylab("Average daily rate (adr)") + 
      theme(legend.position = "none")
  })
  
  output$adr13 <- renderPlot({
    ggplot(df_resort, aes(x=customer_type, y=adr, colour=customer_type)) + 
      geom_boxplot() + 
      geom_jitter(position=position_jitter(0.2)) + 
      scale_color_brewer(palette = "Set2") + 
      theme_light() + 
      ggtitle("Average daily rate vs. Customer Type") + 
      xlab("Customer Type") + 
      ylab("Average daily rate (adr)") + 
      theme(legend.position = "none") 
  })
  
  output$adr14 <- renderPlot({
    par(mfrow=c(2,1))
    interaction.plot(df_resort$arrival_date_month, df_resort$is_canceled, df_resort$adr, 
                     xlab="Arrival date month", ylab="Mean of adr", legend=TRUE, trace.label="Is cancelled")
    
    interaction.plot(df_resort$arrival_date_month, df_resort$market_segment, df_resort$adr, 
                     xlab="Market segment", ylab="Mean of adr", legend=TRUE, trace.label="Market Segment")
  })
  
  output$adr15 <- renderPlot({
    interaction.plot(df_resort$arrival_date_month, df_resort$meal, df_resort$adr, 
                     xlab="Meal", ylab="Mean of adr", legend=TRUE, trace.label="Meal")
  })
  
  output$adr16 <- renderPlot({
    interaction.plot(df_resort$arrival_date_month, df_resort$reserved_room_type, df_resort$adr, 
                     xlab="Reserved Room Type", ylab="Mean of adr", legend=TRUE, trace.label="Room Type")
  })
  
  output$adr17 <- renderPlot({
    interaction.plot(df_resort$arrival_date_month, df_resort$customer_type, df_resort$adr, 
                     xlab="Customer Type", ylab="Mean of adr", legend=TRUE, trace.label="Customer Type")
  })
  
  output$adrlm1 <- renderPrint({
    df_resort_short <- subset(df_resort, select=c(is_canceled, lead_time, arrival_date_year, arrival_date_month, 
                                                  arrival_date_week_number, arrival_date_day_of_month, stays_in_weekend_nights, 
                                                  stays_in_week_nights, adults, children, babies, meal, market_segment, 
                                                  reserved_room_type, customer_type, total_of_special_requests, adr))
    # Pick rows to be used for training
    set.seed(10)
    train <- sample(1:nrow(df_resort_short), round(nrow(df_resort_short)*0.75, 0))
    
    # Values used for testing
    y <- df_resort_short[-train, "adr"] 
    
    linear.reg <- lm(adr ~ ., data=df_resort_short, subset=train)
    summary(linear.reg)
    
  })
  
  output$adrlm2 <- renderPlot({
    # Check for homoscedasticity
    par(mfrow=c(1,1))
    plot(linear.reg)
  })
  
  output$adrlmdalex1 <- renderPlot({
    # Make predictions on the test dataset
    predict.linear.reg <- predict(linear.reg, newdata=df_resort_short[-train, ])
    # Dalex for Linear Regression
      df_ex_adrLR = explain(linear.reg,
                        data  = df_resort_short,
                        y     = df_resort_short$adr,
                        label = "Linear Regression")
      
      # instance level
      (single_row = df_resort_short[1,])
      # prediction
      df_ex_adrLR %>% predict(single_row)
      
      # prediction parts
      plot(predict_parts(df_ex_adrLR, new_observation = single_row))
    })
    
    output$adrlmdalex2 <- renderPlot({
      plot(model_performance(df_ex_adrLR), geom = "roc")
    })
    
    output$adrlmdalex3 <- renderPlot({
      plot(model_parts(df_ex_adrLR), show_boxplots = FALSE)
    })
    
    output$adrdt1 <- renderPrint({
      # Train model using default regression tree from rpart
      tree <- rpart(adr ~ ., df_resort_short, subset=train)

      # Train model
      tree2 <- rpart(adr ~ ., df_resort_short, subset=train, cp=0.001)
      
      # Print cp (complexity parameter) table to find optimal cp
      printcp(tree2)
    })
    
    output$adrdt2 <- renderPlot({
      myCPtable <- tree2$cptable
      id.min <- which.min(myCPtable[,'xerror']) #22, 22 splits
      my1se.err <- myCPtable[id.min,'xerror'] + myCPtable[id.min, 'xstd'] # 0.4199426
      plotcp(tree2)
      abline(h=my1se.err, col="red") 
    })
    
    output$adrdt3 <- renderPlot({
      # Pick the smallest tree below the red line, find its cp, and prune the tree with that cp value (optimal tree)
      id.1se <- min(which(myCPtable[,'xerror'] < my1se.err)) 
      CP.1se <- myCPtable[id.1se, 'CP'] 
      tree.2.pruned <- prune.rpart(tree2, CP.1se)
      
      # Fancy plot
      fancyRpartPlot(tree.2.pruned, caption = NULL) 
    })
    
    output$adrdt_dalex1 <- renderPlot({
      # Make predictions on the test dataset
      predict.tree.2.pruned <- predict(tree.2.pruned, newdata=df_resort_short[-train, ])
      
      # Dalex for FT
      df_ex_adrDT = explain(tree.2.pruned,
                            data  = df_resort_short,
                            y     = df_resort_short$adr,
                            label = "Decision Tree")
      
      # instance level
      (single_row = df_resort_short[1,])
      # prediction
      df_ex_adrDT %>% predict(single_row)
      
      # prediction parts
      plot(predict_parts(df_ex_adrDT, new_observation = single_row))
    })
      
    output$adrdt_dalex2 <- renderPlot({
      plot(model_performance(df_ex_adrDT), geom = "roc")
    })
    
    output$adrdt_dalex3 <- renderPlot({
      plot(model_parts(df_ex_adrDT), show_boxplots = FALSE)
    })
    
    #output$adrpred <- renderPlot({
    #  predictions <- data.frame(actual = y,linear.reg = predict.linear.reg, tree.2.pruned = predict.tree.2.pruned)
    #  predictions_long <- melt(predictions, 
    #                           id.vars="adr", 
    #                           measure.vars=c("linear.reg",  "tree.2.pruned"), 
    #                           variable.name="model", 
    #                           value.name="prediction")
      
      # Plot actual vs. predicted values for each model
      
      # Create new facet label names for each model to be used for plotting
    #  model.labs <- c( "Linear regression", "Decision tree")
    #  names(model.labs) <- c("linear.reg", "tree.2.pruned")
      
    #  ggplot(data = predictions_long, aes(x=adr, y=prediction, colour=model)) +
    #    geom_point() + 
    #    geom_abline(intercept = 0, slope = 1) + 
    #    facet_wrap( ~ model, labeller = labeller(model = model.labs)) + 
    #   ggtitle("Predicted vs. Actual values for each model") + 
    #    xlab("Actual values") + ylab("Predicted values") + 
    #    scale_colour_viridis_d(option="mako") +  
    #    theme_light() + 
    #    theme(legend.position = "none") + 
    #    theme(strip.background = element_rect(color="black", size=1)) + #Facet label rectangle contour
    #    theme(strip.text.x = element_text(color = "black"))
    #})
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
