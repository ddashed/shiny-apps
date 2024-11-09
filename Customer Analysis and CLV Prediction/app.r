

library(shiny)
library(reshape2)
library(DT)
library(readxl)
library(readr)
library(BTYDplus)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(bslib)
library(CLVTools)
library(Metrics)

# ui object
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "lux"),
                titlePanel(h1("Customer analytics", style = "color:#6565e0")),
                sidebarLayout(
                    sidebarPanel(
                        fileInput("file", "Choose Excel File",
                                  multiple = FALSE,
                                  accept = c(".xlsx")),
                        helpText("File should include 3 columns - customer ID, date, transaction.
                                 It is recommended to upload the data no more than for 5 years."),
                        br(),
                        sliderInput("year", "Choose year cohort",
                                   min = 1990, max = 2100,
                                   value = 2016),
                        br(),
                        submitButton("Update estimation", icon("refresh")),
                        br(),
                        helpText("The estimation on the tabs starts, when you click on particular tab."),
                        helpText("The analysis takes 15-20 minutes."),
                        hr(),
                        p("Made with", a("Shiny",
                                         href = "http://shiny.rstudio.com"), ".")),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Table",
                                     h4(p("Transaction log data")),
                                     DTOutput(outputId = "table")),
                            tabPanel("Regularity",
                                     h4(p("Timing Patterns")),
                                     plotOutput("timepat", width = "100%", height = "300px"),
                                     br(),
                                     textOutput("tim"),
                                     br(),
                                     h4(p("Estimating regularity")),
                                     plotOutput("regularity"),
                                     h5(p("Wheat & Morrison")),
                                     verbatimTextOutput("wheat"),
                                     textOutput("reg"),
                                     h5(p("Maximum Likelihood")),
                                     verbatimTextOutput("mle"),
                                     textOutput("reg2"),
                                     br(),
                                     br()),
                            tabPanel("Prediction",
                                     h4(p("Probabilistic models estimation")),
                                     plotOutput("revenue"),
                                     textOutput("comment"),
                                     br(),
                                     DTOutput(outputId = "predictiontable")
                            ),
                            tabPanel("Cohorts",
                                     h4(p("Cohort analysis")),
                                     DTOutput("cohort"),
                                     br(),
                                     textOutput("cohorttext"),
                                     br(),
                                     h4(p("Weekly transactions")),
                                     plotOutput("weeklypl"),
                                     textOutput("week"),
                                     br(),
                                     br()
                                     #plotOutput("avgtr"),
                                     #plotOutput("prediction")
                            )))
                )
)

# server()
server <- function(input, output, session){
    data <- reactive({
        req(input$file)
        if(is.null(file)){return()} 
        read_excel(input$file$datapath, skip = 1, col_names = c("cust", "date", "sales"),
                   col_types = c("text", "date", "numeric"))
    })
    
    output$table <- renderDT({
        data <- data()
        data$date <- as.Date(data$date, format = "%Y/%m/%d")
        data})
    
    

    #IMPORTANT PART
    output$regularity <- renderPlot({
        data <- data()
        cohorts = data
        cohorts$date <- as.Date(cohorts$date, format = "%Y/%m/%d")
        cohorts$year <- as.numeric(format(cohorts$date, '%Y'))
        join.date <- aggregate(date~cust,cohorts,min, na.rm = TRUE)
        colnames(join.date)[2] <- "join_date"
        cohorts <- merge(cohorts, join.date, by.x = "cust",by.y = "cust", all.x = TRUE)
        cohorts$cohort <- as.numeric(format(cohorts$join_date, "%Y"))
        
        coh16 = cohorts %>% filter(cohort == input$year & sales >= 0) %>% select(1:3)
        
        #CBS format
        dataCBS <- elog2cbs(coh16, T.cal = mean(coh16$date))
        
        op <- par(mfrow = c(1, 2))
        # Wheat & Morrison
        (k.wheat <- estimateRegularity(coh16, method = "wheat",
                                       plot = TRUE, title = "Wheat & Morrison"))
        
        # Maximum Likelihood
        (k.mle <- estimateRegularity(coh16, method = "mle",
                                     plot = TRUE, title = "Maximum Likelihood"))
        par(op)
        
        k.wheat <- estimateRegularity(coh16, method = "wheat")
    output$wheat <- renderPrint({
        k.wheat
    })
    
    output$reg <- renderText({ 
        paste("If the regularity, estimated by Wheat & Morrison method", 
              "1) is less that 1.3 -> your customer base has low purchase regularity", 
              "2) if more than 1.7 -> your customer base has high purchase regularity",
              "3) if more than 1.3 and less than 1.7 -> purchase regularity is medium", sep="\n")
    })
        k.mle <- estimateRegularity(coh16, method = "mle")
    output$mle <- renderPrint({
        k.mle
    })
    
    output$reg2 <- renderText({ 
        paste("Maximum Likelihood method counts regularity in", 
              "high purchase activity group of customers", 
              "who have more than 10 repeated transactions", sep="\n")
    })
    
    output$timepat <- renderPlot({
        #data <- data()
        #data$date = as.Date(data$date)
        da <- coh16[, c(1, 2)]
        
        # timing patterns
        plotTimingPatterns(da, n = 60, headers = c("Past", "Future"), title = "")
    })
    
    output$tim <- renderText({
        paste("* The purchase timing patterns of first 60 customer in your year cohort")
    })
    
    
    clv.coh16 <- clvdata(coh16,
                         date.format="ymd",
                         time.unit = "week",
                         estimation.split = 54,
                         name.id = "cust",
                         name.date = "date",
                         name.price = "sales")
    
    # Customer Spending
    # Gamma/Gamma model
    est.gg<- gg(clv.data = clv.coh16, remove.first.transaction=FALSE)
    # predicted mean spending
    results.spending <- predict(est.gg)
    results.spending <- results.spending %>%  rename(cust = Id)
    
    
    if (k.wheat > 1.7) {
        params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/CNBD-k
        params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG
        row <- function(params, LL) {
            names(params) <- c("k", "r", "alpha", "a", "b")
            c(round(params, 3), LL = round(LL))}
        print(rbind(`BG/CNBD-k` = row(params.bgcnbd, bgcnbd.cbs.LL(params.bgcnbd, dataCBS)),
                    `MBG/CNBD-k` = row(params.mbgcnbd, mbgcnbd.cbs.LL(params.mbgcnbd, dataCBS))))
        
        # BG/CNBD-k
        dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
            params = params.bgcnbd, T.star = dataCBS$T.star,
            x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        
        #dataCBS$palive.bgcnbd <- bgcnbd.PAlive(
        # params.bgcnbd, dataCBS$x, dataCBS$t.x, dataCBS$T.cal)
        
        # MBG/NBD-k
        dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
            params = params.mbgcnbd, T.star = dataCBS$T.star,
            x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        
        #dataCBS$palive.mbgcnbd <- mbgcnbd.PAlive(
        # params.mbgcnbd, dataCBS$x, dataCBS$t.x, dataCBS$T.cal)
        
        # Pareto/GGG
        pggg.draws <- pggg.mcmc.DrawParameters(dataCBS) # ~2mins on 2015 MacBook Pro
        # generate draws for holdout period
        pggg.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pggg.draws, dataCBS$T.star)
        # conditional expectations
        dataCBS$xstar.pggg <- apply(pggg.xstar.draws, 2, mean)
        # P(active)
        #dataCBS$pactive.pggg <- mcmc.PActive(pggg.xstar.draws)
        # P(alive)
        #dataCBS$palive.pggg <- mcmc.PAlive(pggg.draws)
        # report median cohort-level parameter estimates
        round(apply(as.matrix(pggg.draws$level_2), 2, median), 3)
        # report mean over median individual-level parameter estimates
        median.est <- sapply(pggg.draws$level_1, function(draw) {
            apply(as.matrix(draw), 2, median)})
        round(apply(median.est, 1, mean), 3)
        
        # compare predictions with actuals at aggregated level
        rbind(`Actuals` = c(`Holdout` = sum(dataCBS$x.star)),
              `BG/CNBD-k` = (sum(dataCBS$xstar.bgcnbd)),
              `MBG/CNBD-k` = (sum(dataCBS$xstar.mbgcnbd)),
              `Pareto/GGG` = (sum(dataCBS$xstar.pggg)))
        
        cat('Mean absolute error\n')
        mae <- function(act, est) {
            stopifnot(length(act)==length(est))
            sum(abs(act-est)) / sum(act)}
        mae.mbgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        mae.bgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        mae.pggg <- mae(dataCBS$x.star, dataCBS$xstar.pggg)
        
        cat('Root mean square error\n')
        rmse.mbgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        rmse.bgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        rmse.pnbd.ggg <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.ggg)
        
        print(rbind(`BG/CNBD-k` = c(`MAE` = round(mae.bgcnbd, 3)),
                    `MBG/CNBD-k` = c(`MAE` = round(mae.mbgcnbd, 3)),
                    `Pareto/GGG` = c(`MAE` = round(mae.pggg, 3))))
        
        print(rbind(`BG/CNBD-k` = c(`RMSE` = round(rmse.bgcnbd, 3)),
                    `MBG/CNBD-k` = c(`RMSE` = round(rmse.mbgcnbd, 3)),
                    `Pareto/GGG` = c(`RMSE` = round(rmse.pnbd.ggg, 3))))
    
        # Prediction    
    output$revenue <- renderPlot({
        if (rmse.bgcnbd > rmse.mbgcnbd & rmse.pggg > rmse.mbgcnbd) { 
            #MB/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params = params.mbgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is MB/CNBD-k: 
                dropout process is independent from purchasing activity, 
                      customers dropout is discrete during slight time intervals, 
                      noticable individual purchasing regularity and customer heterogeneity,
                      high level of purchasing regularity in a cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.mbgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            

        } else if (rmse.bgcnbd > rmse.pggg) {
            # Pareto/GGG
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            pggg.draws <- pggg.mcmc.DrawParameters(dataCBS)
            pggg.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pggg.draws, T.star = 52)
            dataCBS$xstar.pggg <- apply(pggg.xstar.draws, 2, mean)
            round(apply(as.matrix(pggg.draws$level_2), 2, median), 3)
            median.est <- sapply(pggg.draws$level_1, function(draw) {
                apply(as.matrix(draw), 2, median)})
            round(apply(median.est, 1, mean), 3)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/GGG:
                      dropout process depends on purchasing activity, 
                      high level of customer heterogeinity, 
                      high individual purchasing regularity,
                      high level of purchasing regularity in a cohort.")
                
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pggg, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            

        } else {
            #BG/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/CNBD-k
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
                params = params.bgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is BG/CNBD-k:
                dropout process is independent from purchasing activity,
                      customers dropout is discrete during longer time intervals, 
                      noticable individual purchasing regularity and customer heterogeneity,
                      high level of purchasing regularity in a cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.bgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
        }
    })
    } else if (k.wheat < 1.3) { 
        paret = dataCBS %>% dplyr::select(2,3,8)
        
        # estimate Pareto/NBD parameters
        params.pnbd <- BTYD::pnbd.EstimateParameters(paret)
        cat("Pareto/NBD model parameters\n")  
        names(params.pnbd) <- c("r", "alpha", "s", "beta")
        #print(round(params.pnbd, 3))
        # report log-likelihood
        cat("Pareto/NBD model log-likelihood\n")   
        print(BTYD::pnbd.cbs.LL(params.pnbd, paret))
        dataCBS$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
            params = params.pnbd, dataCBS$T.star,
            x = dataCBS$x, t.x = dataCBS$t.x, T.cal = dataCBS$T.cal)
        #dataCBS$palive.pnbd <- BTYD::pnbd.PAlive(params.pnbd, x = dataCBS$x, dataCBS$t.x, T.cal = 52)
        
        #BG/NBD-k, MBG/NBD-k
        params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/NBD
        params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG/NBD
        row <- function(params, LL) {
            names(params) <- c("k", "r", "alpha", "a", "b")
            c(round(params, 3), LL = round(LL))}
        
        dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
            params = params.bgcnbd, dataCBS$T.star, x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
            params = params.mbgcnbd, dataCBS$T.star, x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        
        # Pareto/NBD (HB)
        pnbd.draws <- pnbd.mcmc.DrawParameters(dataCBS)
        pnbd.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pnbd.draws, dataCBS$T.star)
        dataCBS$xstar.pnbd.hb <- apply(pnbd.xstar.draws, 2, mean)
        #dataCBS$pactive.pnbd.hb <- mcmc.PActive(pnbd.xstar.draws)
        #dataCBS$palive.pnbd.hb <- mcmc.PAlive(pnbd.draws)
        #cohort.draws <- pnbd.draws$level_2
        #cohort.draws <- apply(as.matrix(cohort.draws))
        # Pareto/NBD (Abe)
        first <- aggregate(sales ~ cust, data, function(x) x[1] * 10^-3)
        names(first) <- c("cust", "first.sales")
        dataCBS <- merge(dataCBS, first, by = "cust")
        # estimate with first purchase spend as covariate; model M2 in Abe (2009)
        draws.m2 <- abe.mcmc.DrawParameters(dataCBS, covariates = c("first.sales"),
                                            mcmc = 7500, burnin = 2500) # ~33secs on 2015 MacBook Pro

        xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, draws.m2, dataCBS$T.star)
        xstar.est <- apply(xstar.draws, 2, mean)
        dataCBS$xstar.pnbd.abe <- apply(xstar.draws, 2, mean)
        
        
        # compare predictions with actuals at aggregated level
        rbind(`Actuals` = c(`Holdout` = sum(dataCBS$x.star)),
              `BG/CNBD-k` = (sum(dataCBS$xstar.bgcnbd)),
              `MBG/CNBD-k` = (sum(dataCBS$xstar.mbgcnbd)),
              `Pareto/NBD` = (sum(dataCBS$xstar.pnbd)),
              `Pareto/NBD (HB)` = (sum(dataCBS$xstar.pnbd.hb)),
              `Pareto/NBD (Abe)` = (sum(dataCBS$xstar.pnbd.abe)))
        
        cat('Mean absolute error\n')
        mae <- function(act, est) {
            stopifnot(length(act)==length(est))
            sum(abs(act-est)) / sum(act)}
        mae.mbgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        mae.bgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        mae.pnbd <- mae(dataCBS$x.star, dataCBS$xstar.pnbd)
        mae.pnbd.hb <- mae(dataCBS$x.star, dataCBS$xstar.pnbd.hb)
        mae.pnbd.abe <- mae(dataCBS$x.star, dataCBS$xstar.pnbd.abe)
        
        cat('Root mean square error\n')
        rmse.mbgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        rmse.bgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        rmse.pnbd <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd)
        rmse.pnbd.hb <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.hb)
        rmse.pnbd.abe <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.abe)
        
        rbind(`BG/CNBD-k` = c(`MAE` = round(mae.bgcnbd, 3)),
              `MBG/CNBD-k` = c(`MAE` = round(mae.mbgcnbd, 3)),
              `Pareto/NBD` = c(`MAE` = round(mae.pnbd, 3)),
              `Pareto/NBD (HB)` = c(`MAE` = round(mae.pnbd.hb, 3)),
              `Pareto/NBD (Abe)` = c(`MAE` = round(mae.pnbd.abe, 3)))
        
        rbind(`BG/CNBD-k` = c(`RMSE` = round(rmse.bgcnbd, 3)),
                    `MBG/CNBD-k` = c(`RMSE` = round(rmse.mbgcnbd, 3)),
                    `Pareto/NBD` = c(`RMSE` = round(rmse.pnbd, 3)),
                    `Pareto/NBD (HB)` = c(`RMSE` = round(rmse.pnbd.hb, 3)),
                    `Pareto/NBD (Abe)` = c(`RMSE` = round(rmse.pnbd.abe, 3)))
        
    output$revenue <- renderPlot({
        if (rmse.bgcnbd > rmse.mbgcnbd & rmse.pnbd > rmse.mbgcnbd & rmse.pnbd.abe > rmse.mbgcnbd
            & rmse.pnbd.hb > rmse.mbgcnbd) { 
            #MB/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params = params.mbgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is MB/CNBD-k: 
                dropout process is independent from purchasing activity,
                      customers dropout is discrete during slight time intervals,
                      low individual purchasing regularity and customer heterogeneity,
                      low level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.mbgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else if (rmse.bgcnbd > rmse.pnbd & rmse.mbgcnbd > rmse.pnbd & rmse.pnbd.abe > rmse.pnbd
                   & rmse.pnbd.hb > rmse.pnbd) {
            
            #Pareto/NBD
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            paret = dataCBS %>% dplyr::select(2,3,8)
            params.pnbd <- BTYD::pnbd.EstimateParameters(paret)
            names(params.pnbd) <- c("r", "alpha", "s", "beta")
            print(BTYD::pnbd.cbs.LL(params.pnbd, paret))
            dataCBS$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                params = params.pnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x, T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD:
                dropout process is independent from purchasing activity, 
                      low level of individual purchasing regularity and customer heterogeinity,
                      low level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
                })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
            
        } else if (rmse.bgcnbd > rmse.pnbd.abe & rmse.mbgcnbd > rmse.pnbd.abe & rmse.pnbd > rmse.pnbd.abe
                   & rmse.pnbd.hb > rmse.pnbd.abe) {
            
            # Pareto/NBD (Abe) Prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            first <- aggregate(sales ~ cust, data, function(x) x[1] * 10^-3)
            names(first) <- c("cust", "first.sales")
            dataCBS <- merge(dataCBS, first, by = "cust")
            draws.m2 <- abe.mcmc.DrawParameters(dataCBS, covariates = c("first.sales"),
                                                mcmc = 7500, burnin = 2500) # ~33secs on 2015 MacBook Pro
            xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, draws.m2, T.star = 52)
            xstar.est <- apply(xstar.draws, 2, mean)
            dataCBS$xstar.pnbd.abe <- apply(xstar.draws, 2, mean)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD (Abe):
                dropout process depends on purchasing activity,
                      high level of customer heterogeinity, low individual purchasing regularity,
                      low level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd.abe, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else if (rmse.bgcnbd > rmse.pnbd.hb & rmse.mbgcnbd > rmse.pnbd.hb & rmse.pnbd > rmse.pnbd.hb
                   & rmse.pnbd.abe > rmse.pnbd.hb) {
            
            # Pareto/NBD (HB) Prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            pnbd.draws <- pnbd.mcmc.DrawParameters(dataCBS)
            pnbd.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pnbd.draws, T.star = 52)
            dataCBS$xstar.pnbd.hb <- apply(pnbd.xstar.draws, 2, mean)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD (HB):
                dropout process is independent from purchasing activity, 
                      increased level of customer heterogeinity, low individual purchasing regularity,
                      low level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd.hb, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else {
            #BG/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/CNBD-k
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
                params = params.bgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is BG/CNBD-k:
                      dropout process is independent from purchasing activity, 
                      customers dropout is discrete during longer time intervals, 
                      low individual purchasing regularity and customer heterogeneity,
                      low level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.bgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
        }
    })
        
    } else { 
        #Pareto/NBD
        paret = dataCBS %>% dplyr::select(2,3,8)
        # estimate Pareto/NBD parameters
        params.pnbd <- BTYD::pnbd.EstimateParameters(paret)
        cat("Pareto/NBD model parameters\n")  
        names(params.pnbd) <- c("r", "alpha", "s", "beta")
        #print(round(params.pnbd, 3))
        # report log-likelihood
        cat("Pareto/NBD model log-likelihood\n") 
        print(BTYD::pnbd.cbs.LL(params.pnbd, paret))
        dataCBS$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
            params = params.pnbd, T.star = dataCBS$T.star,
            x = dataCBS$x, t.x = dataCBS$t.x, T.cal = dataCBS$T.cal)
        #dataCBS$palive.pnbd <- BTYD::pnbd.PAlive(params.pnbd, x = dataCBS$x, dataCBS$t.x, T.cal = 52)
        #BG/NBD-k, MBG/NBD-k
        params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/NBD
        params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG/NBD
        row <- function(params, LL) {
            names(params) <- c("k", "r", "alpha", "a", "b")
            c(round(params, 3), LL = round(LL))}
        
        dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
            params = params.bgcnbd, T.star = dataCBS$T.star, x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
            params = params.mbgcnbd, T.star = dataCBS$T.star, x = dataCBS$x, t.x = dataCBS$t.x,
            T.cal = dataCBS$T.cal)
        
        # Pareto/NBD (HB)
        pnbd.draws <- pnbd.mcmc.DrawParameters(dataCBS)
        pnbd.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pnbd.draws, dataCBS$T.star)
        dataCBS$xstar.pnbd.hb <- apply(pnbd.xstar.draws, 2, mean)
        #dataCBS$pactive.pnbd.hb <- mcmc.PActive(pnbd.xstar.draws)
        #dataCBS$palive.pnbd.hb <- mcmc.PAlive(pnbd.draws)
        #cohort.draws <- pnbd.draws$level_2
        #cohort.draws <- apply(as.matrix(cohort.draws))
        # Pareto/NBD (Abe)
        first <- aggregate(sales ~ cust, data, function(x) x[1] * 10^-3)
        names(first) <- c("cust", "first.sales")
        dataCBS <- merge(dataCBS, first, by = "cust")
        # estimate with first purchase spend as covariate; model M2 in Abe (2009)
        draws.m2 <- abe.mcmc.DrawParameters(dataCBS, covariates = c("first.sales"),
                                            mcmc = 7500, burnin = 2500) # ~33secs on 2015 MacBook Pro
        
        xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, draws.m2, dataCBS$T.star)
        xstar.est <- apply(xstar.draws, 2, mean)
        dataCBS$xstar.pnbd.abe <- apply(xstar.draws, 2, mean)
        
        # Pareto/GGG
        pggg.draws <- pggg.mcmc.DrawParameters(dataCBS) # ~2mins on 2015 MacBook Pro
        # generate draws for holdout period
        pggg.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pggg.draws, dataCBS$T.star)
        # conditional expectations
        dataCBS$xstar.pggg <- apply(pggg.xstar.draws, 2, mean)
        # P(active)
        #dataCBS$pactive.pggg <- mcmc.PActive(pggg.xstar.draws)
        # P(alive)
        #dataCBS$palive.pggg <- mcmc.PAlive(pggg.draws)
        # report median cohort-level parameter estimates
        round(apply(as.matrix(pggg.draws$level_2), 2, median), 3)
        # report mean over median individual-level parameter estimates
        median.est <- sapply(pggg.draws$level_1, function(draw) {
            apply(as.matrix(draw), 2, median)})
        round(apply(median.est, 1, mean), 3)
        
        # compare predictions with actuals at aggregated level
        rbind(`Actuals` = c(`Holdout` = sum(dataCBS$x.star)),
              `BG/CNBD-k` = (sum(dataCBS$xstar.bgcnbd)),
              `MBG/CNBD-k` = (sum(dataCBS$xstar.mbgcnbd)),
              `Pareto/NBD` = (sum(dataCBS$xstar.pnbd)),
              `Pareto/NBD (HB)` = (sum(dataCBS$xstar.pnbd.hb)),
              `Pareto/NBD (Abe)` = (sum(dataCBS$xstar.pnbd.abe)))
        
        
        cat('Mean absolute error\n')
        mae <- function(act, est) {
            stopifnot(length(act)==length(est))
            sum(abs(act-est)) / sum(act)}
        
        mae.mbgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        mae.bgcnbd <- mae(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        mae.pnbd <- mae(dataCBS$x.star, dataCBS$xstar.pnbd)
        mae.pnbd.hb <- mae(dataCBS$x.star, dataCBS$xstar.pnbd.hb)
        mae.pnbd.abe <- mae(dataCBS$x.star, dataCBS$xstar.pnbd.abe)
        mae.pnbd.ggg <- mae(dataCBS$x.star, dataCBS$xstar.pnbd.ggg)
        
        cat('Root mean square error\n')
        rmse.mbgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.mbgcnbd)
        rmse.bgcnbd <- rmse(dataCBS$x.star, dataCBS$xstar.bgcnbd)
        rmse.pnbd <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd)
        rmse.pnbd.hb <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.hb)
        rmse.pnbd.abe <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.abe)
        rmse.pnbd.ggg <- rmse(dataCBS$x.star, dataCBS$xstar.pnbd.ggg)
        
        rbind(`BG/CNBD-k` = c(`MAE` = round(mae.bgcnbd, 3)),
              `MBG/CNBD-k` = c(`MAE` = round(mae.mbgcnbd, 3)),
              `Pareto/NBD` = c(`MAE` = round(mae.pnbd, 3)),
              `Pareto/NBD (HB)` = c(`MAE` = round(mae.pnbd.hb, 3)),
              `Pareto/NBD (Abe)` = c(`MAE` = round(mae.pnbd.abe, 3)),
              `Pareto/GGG` = c(`MAE` = round(mae.pnbd.ggg, 3)))
        
        rbind(`BG/CNBD-k` = c(`RMSE` = round(rmse.bgcnbd, 3)),
              `MBG/CNBD-k` = c(`RMSE` = round(rmse.mbgcnbd, 3)),
              `Pareto/NBD` = c(`RMSE` = round(rmse.pnbd, 3)),
              `Pareto/NBD (HB)` = c(`RMSE` = round(rmse.pnbd.hb, 3)),
              `Pareto/NBD (Abe)` = c(`RMSE` = round(rmse.pnbd.abe, 3)),
              `Pareto/GGG` = c(`RMSE` = round(rmse.pnbd.ggg, 3)))
        
    output$revenue <- renderPlot({
        if (rmse.bgcnbd > rmse.mbgcnbd & rmse.pnbd > rmse.mbgcnbd & rmse.pnbd.abe > rmse.mbgcnbd
            & rmse.pnbd.hb > rmse.mbgcnbd & rmse.pnbd.ggg > rmse.mbgcnbd) { 
            #MB/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.mbgcnbd <- mbgcnbd.EstimateParameters(dataCBS) # MBG
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
                params = params.mbgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is MB/CNBD-k:
                dropout process is independent from purchasing activity, 
                customers dropout is discrete during slight time intervals, 
                noticable individul purchasing regularity and customer heterogeneity,
                      increased level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.mbgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
        } else if (rmse.bgcnbd > rmse.pnbd & rmse.mbgcnbd > rmse.pnbd & rmse.pnbd.abe > rmse.pnbd
                   & rmse.pnbd.hb > rmse.pnbd & rmse.pnbd.ggg > rmse.pnbd) {
            
            #Pareto/NBD
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            paret = dataCBS %>% dplyr::select(2,3,8)
            params.pnbd <- BTYD::pnbd.EstimateParameters(paret)
            names(params.pnbd) <- c("r", "alpha", "s", "beta")
            print(BTYD::pnbd.cbs.LL(params.pnbd, paret))
            dataCBS$xstar.pnbd <- BTYD::pnbd.ConditionalExpectedTransactions(
                params = params.pnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x, T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD:
                dropout process is independent from purchasing activity, 
                low level of individual purchasing regularity and customer heterogeinity,
                      increased level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else if (rmse.bgcnbd > rmse.pnbd.abe & rmse.mbgcnbd > rmse.pnbd.abe & rmse.pnbd > rmse.pnbd.abe
                   & rmse.pnbd.hb > rmse.pnbd.abe & rmse.pnbd.ggg > rmse.pnbd.abe) {
            
            # Pareto/NBD (Abe) Prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            first <- aggregate(sales ~ cust, data, function(x) x[1] * 10^-3)
            names(first) <- c("cust", "first.sales")
            dataCBS <- merge(dataCBS, first, by = "cust")
            draws.m2 <- abe.mcmc.DrawParameters(dataCBS, covariates = c("first.sales"),
                                                mcmc = 7500, burnin = 2500) # ~33secs on 2015 MacBook Pro
            xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, draws.m2, T.star = 52)
            xstar.est <- apply(xstar.draws, 2, mean)
            dataCBS$xstar.pnbd.abe <- apply(xstar.draws, 2, mean)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD (Abe):
                dropout process depends on purchasing activity, 
                high level of customer heterogeinity, low individual purchasing regularity,
                      increased level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd.abe, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else if (rmse.bgcnbd > rmse.pnbd.hb & rmse.mbgcnbd > rmse.pnbd.hb & rmse.pnbd > rmse.pnbd.hb
                   & rmse.pnbd.abe > rmse.pnbd.hb & rmse.pnbd.ggg > rmse.pnbd.hb) {
            
            # Pareto/NBD (HB) Prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            pnbd.draws <- pnbd.mcmc.DrawParameters(dataCBS)
            pnbd.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pnbd.draws, T.star = 52)
            dataCBS$xstar.pnbd.hb <- apply(pnbd.xstar.draws, 2, mean)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/NBD (HB):
                dropout process is independent from purchasing activity, 
                increased level of customer heterogeinity, low individual purchasing regularity,
                      increased level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pnbd.hb, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else if (rmse.bgcnbd > rmse.pnbd.ggg & rmse.mbgcnbd > rmse.pnbd.ggg
                   & rmse.pnbd > rmse.pnbd.ggg & rmse.pnbd.abe > rmse.pnbd.ggg
                   & rmse.pnbd.hb > rmse.pnbd.ggg) {
            
            # Pareto/GGG
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            pggg.draws <- pggg.mcmc.DrawParameters(dataCBS)
            pggg.xstar.draws <- mcmc.DrawFutureTransactions(dataCBS, pggg.draws, T.star = 52)
            dataCBS$xstar.pggg <- apply(pggg.xstar.draws, 2, mean)
            round(apply(as.matrix(pggg.draws$level_2), 2, median), 3)
            median.est <- sapply(pggg.draws$level_1, function(draw) {
                apply(as.matrix(draw), 2, median)})
            round(apply(median.est, 1, mean), 3)
            
            output$comment <- renderText({ 
                paste("The best model for your data is Pareto/GGG:
                dropout process depends on purchasing activity, 
                high level of customer heterogeinity, high individual purchasing regularity,
                      increased level of purchasing regularity in cohort.")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.pggg, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
            
        } else {
            #BG/CNBD-k prediction
            dataCBS <- elog2cbs(coh16, T.cal = max(coh16$date))
            params.bgcnbd <- bgcnbd.EstimateParameters(dataCBS) # BG/CNBD-k
            row <- function(params, LL) {
                names(params) <- c("k", "r", "alpha", "a", "b")
                c(round(params, 3), LL = round(LL))}
            dataCBS$xstar.bgcnbd <- bgcnbd.ConditionalExpectedTransactions(
                params = params.bgcnbd, T.star = 52,
                x = dataCBS$x, t.x = dataCBS$t.x,
                T.cal = dataCBS$T.cal)
            
            output$comment <- renderText({ 
                paste("The best model for your data is BG/CNBD-k")
            })
            
            full = left_join(dataCBS, results.spending, by = "cust")
            full$mean.future.revenue = round(full$xstar.bgcnbd, 0) * full$predicted.mean.spending
            
            output$predictiontable <- renderDT({
                full = full %>% select("cust", "x", "sales", "first", 
                                       "predicted.mean.spending", "mean.future.revenue")
                full
            })
            
            ggplot(full, aes(x=mean.future.revenue)) +  geom_histogram(colour="black", fill="white", bins = 50) +
                xlab("Future revenue") + ylab("Number of transactions") + theme_minimal() + scale_x_log10()
            
        }
    })
        
    }
    
    
    
    
    
    
    

    
    output$cohort <- renderDT({
        data <- data()
        cohorts = data
        cohorts$date <- as.Date(cohorts$date, format = "%Y/%m/%d")
        cohorts$year <- as.numeric(format(cohorts$date, '%Y'))
        join.date <- aggregate(date~cust,cohorts,min, na.rm = TRUE)
        colnames(join.date)[2] <- "join_date"
        cohorts <- merge(cohorts, join.date, by.x = "cust",by.y = "cust", all.x = TRUE)
        cohorts$cohort <- as.numeric(format(cohorts$join_date, "%Y"))
        
        coh16 = cohorts %>% filter(cohort == min(cohorts$cohort) & sales >= 0) %>% select(1:3)
        clv.coh16 <- clvdata(coh16,
                             date.format="ymd",
                             time.unit = "week",
                             estimation.split = 54,
                             name.id = "cust",
                             name.date = "date",
                             name.price = "sales")
        # Pareto/NBD
        est.pnbd <- pnbd(clv.data = clv.coh16)
        
        # GGom/NBD model
        est.ggomnbd <- ggomnbd(clv.data = clv.coh16,
                               start.params.model = c(r=0.7, alpha=5, b=0.005, s=0.02, beta=0.001),
                               optimx.args = list(control=list(trace=5),
                                                  method="Nelder-Mead"))
        #est.ggomnbd
        
        # Predict Customer Behavior
        results <- predict(est.pnbd)
        #print(results)
        
        #predict(est.pnbd, prediction.end = 30)
        # Weekly tracking plot
        output$weeklypl <- renderPlot({
            clv.coh16 <- clvdata(coh16,
                                 date.format="ymd",
                                 time.unit = "week",
                                 name.id = "cust",
                                 name.date = "date",
                                 name.price = "sales")
            
            est.pnbd <- pnbd(clv.data = clv.coh16)
            plot(est.pnbd)
        })
        
        output$week <- renderText({
            paste("* Weekly trends of transaction numbers in comparison of estimation of Pareto/NBD model")
        })
        
       # output$prediction <- renderPlot({
        #    clv.coh16 <- clvdata(coh16,
         #                        date.format="ymd",
          #                       time.unit = "week",
           #                      estimation.split = 156,
            #                     name.id = "cust",
             #                    name.date = "date",
              #                   name.price = "sales")
        #    est.pnbd <- pnbd(clv.data = clv.coh16)
         #   print(plot(est.pnbd, prediction.end = 200, cumulative = TRUE))
        #})
        
        # Customer Spending
        # Gamma/Gamma model

        
        
        
        cohorts$age_by_day <- as.numeric(difftime(cohorts$date,cohorts$join_date,units = c("days")))
        
        # Dividing the days by 30 to get the number of months
        cohorts$age_by_month <- floor(cohorts$age_by_day/30)
        
        groups <- c(unique(cohorts$cohort),ordered = T)
        
        for(i in 1:5){
            cohorts[cohorts$cohort==i,"cohort"] <- groups[i]
        }
        
        #unique(cohorts$cust)
        cohorts$cohort <- factor(cohorts$cohort,ordered = T)
        
        cohorts.wide <- reshape2::dcast(cohorts,cohort~year,
                                        value.var="cust",
                                        fun.aggregate = length)
        
        cw.retention <- cohorts.wide
        cw.churn <- cohorts.wide
        
        # Creating 19 breaks and 20 rgb color values ranging from blue to white
        #breaks <- quantile(cohorts.wide[,3:5], probs = seq(.05, .95, .05), na.rm = TRUE)
        colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                         function(x){ rgb(x,x,155, maxColorValue = 155) } )
        
        # The Retention Mixpanel with counts
        DT::datatable(cohorts.wide,
                      class = 'cell-border stripe',
                      rownames = FALSE,
                      options = list(
                          ordering=F,
                          dom = 't',
                          pageLength = 5) ) %>%
            formatStyle("cohort",
                        backgroundColor = 'lightgrey',
                        fontWeight = 'bold') %>%
            formatStyle(names(cohorts.wide[c(-1)]),fontWeight = 'bold',color = 'white', 
                        backgroundColor = styleInterval(breaks,colors))
    })
    
    output$cohorttext <- renderText({
        paste("* Cohort analysis shows the distribution of number of transactions ",
        "in particular customer year cohorts by year", sep="\n")
    })
    })

    }
    


# shinyApp()
shinyApp(ui = ui, server = server)
