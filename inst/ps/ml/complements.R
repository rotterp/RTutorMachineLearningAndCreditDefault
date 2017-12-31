##########################
#        presets         ####################################################################################################################
##########################

data = readRDS("data.rds")

library(dplyr)
select = dplyr::select

# Suppressing Scientific Notation
options(scipen = 999)

####################################################
# RStudio - dyGraphs:
# See: http://rstudio.github.io/dygraphs/index.html
dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("examples/plotters/barchart.js",
                               package = "dygraphs"))
}

####################################################

##########################
# user-defined functions ####################################################################################################################
##########################

convert = function(.data, .what = ""){
  
  library(dplyr)
  
  if(.what == "dygraph"){
    # Coerce to data.frame() 
    .data <- as.data.frame(.data)
    # Assign the date as row name
    row.names(.data) <- .data$issue_d
    # Drop the previous date column
    .data$issue_d <- NULL
  }
  
  if(.what == "character"){
    # Replace all special characters with "_"
    colnames(.data) <- gsub("[.]|<|>| |/", "_", colnames(.data)) 
  }
  
  return(.data)
  
}

f = function(x){
  7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4
}

draw_random_y = function(){
  # Draw a random vector of 25 uniform distributed values between 0 and 30
  X = runif(n=25, min=0, max=30)
  # Draw a vector of 25 normal distributed residuals with zero mean and standard
  # deviation 500
  eps = rnorm(n = 25, mean = 0, sd = 500)
  # Create a vector of observations y based on our function f(x) and our residuals
  Y = f(X)+ eps
  # Assign both variables to a data.frame called dat
  dat = data.frame(X=X, Y=Y)
}

test = data.frame(X = 27)

genr = function(.what = ""){
  
  if(.what == "random"){
    .data <- data.frame(x = 1:40*10 + rnorm(40,sd=10),
                        y = (1:40)^2 + rnorm(40,sd=150))
  }
  
  if(.what == "spline"){
    .data <- data.frame(x=1:10, y=sample(genr("random")$x1,10))
  }
  
  return(.data)
  
}

simulation_study = function(){
  .dat = draw_random_y()
  predict(lm(Y ~ X, data = .dat), test)
}

simulation_study_extended = function(model = NULL){
  
  .dat = draw_random_y()
  
  # case_when follows a simple if this then that logic:
  # if our model correspons to linear, then predict following
  # this logic
  case_when(model == "linear" ~ predict(lm(Y~X,data=.dat),test),
            model == "constant" ~ predict(lm(Y~1,data=.dat),test),
            model == "quadratic" ~ predict(lm(Y~poly(X,2),data=.dat),test),
            model == "septic" ~ predict(lm(Y~poly(X,7),data=.dat),test))
}


#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################

htable = function(.data = NULL, .what = ""){
  
  library(stargazer)

  if(.what == "transpose"){
    stargazer(t(.data),
              title = "Content",
              align = TRUE,
              type = "html",
              style = "aer",
              colnames = NULL,
              digits = 1)
  }
  
  if(.what == "regular"){
    stargazer(.data,
              title = "Content",
              align = FALSE,
              type = "html",
              style = "aer",
              rownames = FALSE,
              digits = 1,
              summary = FALSE)
  }
}

stargaze = function(.what = "", model1, model2=NULL, model3=NULL, model4=NULL){
  library(stargazer)
  
  if(.what == "primus"){
    stargazer(model1,
              title="Logit Regression",
              # omit = c(3,4,5,6),
              covariate.labels = c("Loan amount", "Debt to Income Ratio", "Annual Income", "Term[60 months]", "Home Ownership[Own]", "Home Ownership[Rent]"),
              # column.labels = c("Base model","Private seller sample", "Dealer sample", "Book value sample"),
              # dep.var.labels = c("","","",""),
              model.names = FALSE,
              align=TRUE,
              digits  = 3,
              digits.extra = 3,
              type = "html",
              style = "aer"
    )
  }
  
  if(.what == "iterum"){
    stargazer(model1, model2,
              title="Logit Regression",
              # omit = c(3,4,5,6),
              covariate.labels = c("Loan amount", "Debt to Income Ratio", "Annual Income", "Term[60 months]", "Home Ownership[Own]", "Home Ownership[Rent]",
                                   "FICO Upper Boundary", "Last FICO Upper Boundary", "Loan Grade[B]", "Loan Grade[C]",
                                   "Loan Grade[D]", "Loan Grade[E]", "Loan Grade[F]", "Loan Grade[G]"),
              model.names = FALSE,
              align=TRUE,
              digits  = 3,
              digits.extra = 3,
              type = "text",
              style = "aer"
    )
  }
}

gridPlotly = function(.what = "", .data = NULL){
  
  library(ggplot2)
  library(plotly)
  
  if(.what == "models"){
    # Create the four plots p1 to p4
    p1 <- ggplot(.data) +
      geom_point(aes(x,y) ,size = 1, color = "#ff0080") +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y), method=lm, formula = y ~ 1, 
                  se=FALSE, color = "#bf00ff") +
      labs(x = "X", y = "Y") +
      theme(legend.position = "none")
    p2 <- ggplot(.data) + 
      geom_point(aes(x,y) ,size = 1, color = "#ff0080") +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y), method=lm, se=FALSE, color = "#FFC100") +
      labs(x = "X", y = "Y") + 
      theme(legend.position = "none") 
    p3 <- ggplot(.data) +
      geom_point(aes(x,y) ,size = 1, color = "#ff0080") +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y), method=lm, 
                  formula = y ~ poly(x,2), se=FALSE, color = "#cc3300") +
      labs(x = "X", y = "Y") +
      theme(legend.position = "none") 
    p4 <- ggplot(.data) +
      geom_point(aes(x,y) ,size = 1, color = "#ff0080") +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y), method=lm, formula = y ~ poly(x,7), 
                  se=FALSE, color = "#483699") +
      labs(x = "X", y = "Y") +
      theme(legend.position = "none")
    
    # Combine the four plots
    subplot(p1, p2, p3, p4, nrows = 2, 
            heights = c(0.5, 0.5), titleX = TRUE, titleY = TRUE) -> .p
  }
  
  if(.what == "var-bias"){
    # Create the observations
    x = runif(n=5000, min=0, max=30)
    # Draw a vector of 5000 normal distributed residuals with zero mean and standard
    # deviation 500
    eps = rnorm(n = 5000, mean = 0, sd = 500)
    # Create a vector of observations y based on our function f(x) and our residuals
    y = f(x) + eps
    
    # The rep() statement is tricky, but easy to understand:
    # rep(1:25, each = 50) will create 50 times 1 followed by 50 times 2...
    # rep(c("con","sep"), each = 25, times = 10) will create "con" 25 times followed by "sep" 25 times (100 times)
    # rep(1:25, times = 100) will simply create the number 1 to 25 followed by 1 to 25 a hundred times
    .data = data.frame(instance = rep(1:25, each = 50),
                       model =    rep(c("constant", "septic"), each = 25, times = 100),
                       obs = rep(1:25, times = 100),
                       x = x,
                       y = y)
    
    
    # Create the plots
    p5 <- ggplot(.data) +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y, group = instance), method=lm, 
                  formula = y ~ 1, se=FALSE, color = "#bf00ff")
    p6 <- ggplot(.data) +
      stat_function(fun=function(x)
      {7500 + 2*x - 0.5*x^2 + 0.7*(x-20)^3 - 0.011 *(x-9)^4}, 
      geom="line", size = 2, color="#12C7A6") +
      geom_smooth(aes(x, y, group = instance), method=lm, 
                  formula = y ~ poly(x,7), se=FALSE, color = "#483699")
    
    # Combine the plots
    subplot(p5, p6, nrows = 1, titleX = TRUE, titleY = TRUE) -> .p
  }
  
  return(.p)
  
}

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################

choropleth = function(.data = NULL, .what = ""){
  
  library(leaflet)
  library(dplyr)
  library(geojsonio)
  library(geojson)
  library(htmltools)
  
  .data %>% 
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat

  .data %>%
    filter(period == 1) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat1
 
  dat1[51,] <- c("North Dakota", as.numeric(0), as.numeric(0), as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat1[,2] <- as.numeric(dat1[,2])
  dat1[,3] <- as.numeric(dat1[,3])
  dat1[,4] <- as.numeric(dat1[,4])
  dat1[,5] <- as.numeric(dat1[,5])
  dat1[,6] <- as.numeric(dat1[,6])
  
  .data %>%
    filter(period == 2) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat2
  
  dat2[50,] <- c("Maine", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat2[51,] <- c("North Dakota", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat2[,2] <- as.numeric(dat2[,2])
  dat2[,3] <- as.numeric(dat2[,3])
  dat2[,4] <- as.numeric(dat2[,4])
  dat2[,5] <- as.numeric(dat2[,5])
  dat2[,6] <- as.numeric(dat2[,6])

  .data %>%
    filter(period == 3) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat3
  
  dat3[50,] <- c("Nebraska", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat3[51,] <- c("North Dakota", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat3[,2] <- as.numeric(dat3[,2])
  dat3[,3] <- as.numeric(dat3[,3])
  dat3[,4] <- as.numeric(dat3[,4])
  dat3[,5] <- as.numeric(dat3[,5])
  dat3[,6] <- as.numeric(dat3[,6])
  
  .data %>%
    filter(period == 4) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat4
  
  dat4[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat4[51,] <- c("Idaho", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat4[,2] <- as.numeric(dat4[,2])
  dat4[,3] <- as.numeric(dat4[,3])
  dat4[,4] <- as.numeric(dat4[,4])
  dat4[,5] <- as.numeric(dat4[,5])
  dat4[,6] <- as.numeric(dat4[,6])
  
  .data %>%
    filter(period == 5) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat5
  
  dat5[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat5[51,] <- c("Wyoming", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat5[,2] <- as.numeric(dat5[,2])
  dat5[,3] <- as.numeric(dat5[,3])
  dat5[,4] <- as.numeric(dat5[,4])
  dat5[,5] <- as.numeric(dat5[,5])
  dat5[,6] <- as.numeric(dat5[,6])
  
  .data %>%
    filter(period == 6) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat6
  
  dat6[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat6[51,] <- c("Wyoming", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat6[,2] <- as.numeric(dat6[,2])
  dat6[,3] <- as.numeric(dat6[,3])
  dat6[,4] <- as.numeric(dat6[,4])
  dat6[,5] <- as.numeric(dat6[,5])
  dat6[,6] <- as.numeric(dat6[,6])
  
  .data %>%
    filter(period == 7) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat7
  
  dat7[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat7[51,] <- c("Wyoming", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat7[,2] <- as.numeric(dat7[,2])
  dat7[,3] <- as.numeric(dat7[,3])
  dat7[,4] <- as.numeric(dat7[,4])
  dat7[,5] <- as.numeric(dat7[,5])
  dat7[,6] <- as.numeric(dat7[,6])
  
  .data %>%
    filter(period == 8) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat8
  
  dat8[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat8[51,] <- c("West Virginia", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat8[,2] <- as.numeric(dat8[,2])
  dat8[,3] <- as.numeric(dat8[,3])
  dat8[,4] <- as.numeric(dat8[,4])
  dat8[,5] <- as.numeric(dat8[,5])
  dat8[,6] <- as.numeric(dat8[,6])
  
  .data %>%
    filter(period == 9) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat9
  
  dat9[50,] <- c("Iowa", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat9[51,] <- c("West Virginia", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat9[,2] <- as.numeric(dat9[,2])
  dat9[,3] <- as.numeric(dat9[,3])
  dat9[,4] <- as.numeric(dat9[,4])
  dat9[,5] <- as.numeric(dat9[,5])
  dat9[,6] <- as.numeric(dat9[,6])
  
  .data %>%
    filter(period == 10) %>%
    group_by(state) %>%
    summarise(loan_book = (sum(loan_amnt/1e+06)), 
              loans_per_state = n(),
              fico = as.integer(mean(fico_range_high)),
              default = mean(ifelse(status_group == "Default", 1, 0)*100),
              paid = mean(ifelse(status_group == "Paid", 1, 0))*100) %>% 
    as.data.frame() -> dat10
  
  dat10[50,] <- c("IA", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat10[51,] <- c("WV", 0, 0, as.numeric(NA), as.numeric(NA), as.numeric(NA))
  dat10[,2] <- as.numeric(dat10[,2])
  dat10[,3] <- as.numeric(dat10[,3])
  dat10[,4] <- as.numeric(dat10[,4])
  dat10[,5] <- as.numeric(dat10[,5])
  dat10[,6] <- as.numeric(dat10[,6])
  
  #dfs <- list(dat, dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10)
  #dfs <- lapply(dfs, function(x) as.factor(x$state))
  #dfs <- lapply(dfs, function(x) arrange(x, state))
  
  dat$state <- as.factor(dat$state)
  dat1$state <- as.factor(dat1$state)
  dat2$state <- as.factor(dat2$state)
  dat3$state <- as.factor(dat3$state)
  dat4$state <- as.factor(dat4$state)
  dat5$state <- as.factor(dat5$state)
  dat6$state <- as.factor(dat6$state)
  dat7$state <- as.factor(dat7$state)
  dat8$state <- as.factor(dat8$state)
  dat9$state <- as.factor(dat9$state)
  dat10$state <- as.factor(dat10$state)
  
  
  arrange(dat, state) -> dat
  arrange(dat1, state) -> dat1
  arrange(dat2, state) -> dat2
  arrange(dat3, state) -> dat3
  arrange(dat4, state) -> dat4
  arrange(dat5, state) -> dat5
  arrange(dat6, state) -> dat6
  arrange(dat7, state) -> dat7
  arrange(dat8, state) -> dat8
  arrange(dat9, state) -> dat9
  arrange(dat10, state) -> dat10
  
  us <- geojson_read("us.json", what = "sp")
  
  levels(us$name) <- c("Alabama",              "Alaska"   ,            "Arizona"  ,            "Arkansas"  ,           "California"  ,         "Colorado" ,           
                       "Connecticut" ,         "Delaware"  ,           "District of Columbia", "Florida"   ,           "Georgia"  ,            "Hawaii" ,             
                       "Idaho"     ,           "Illinois"  ,           "Indiana" ,             "Iowa"          ,       "Kansas"  ,             "Kentucky" ,           
                       "Louisiana"   ,         "Maine"  ,              "Maryland"  ,           "Massachusetts"   ,     "Michigan"  ,           "Minnesota" ,          
                       "Mississippi" ,         "Missouri" ,            "Montana"  ,            "Nebraska"   ,          "Nevada"  ,             "New Hampshire" ,      
                       "New Jersey" ,          "New Mexico"  ,         "New York"  ,           "North Carolina"  ,     "North Dakota" ,        "Ohio",                
                       "Oklahoma"  ,           "Oregon"   ,            "Pennsylvania" ,        "Rhode Island"   ,      "South Carolina" ,      "South Dakota"  ,      
                       "Tennessee" ,           "Texas" ,               "Utah"   ,              "Vermont" ,             "Virginia"  ,           "Washington"   ,       
                       "West Virginia" ,       "Wisconsin"  ,          "Wyoming" , "Puerto Rico"   )
  
  
  label <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat$state, dat$loan_book, dat$loans_per_state, dat$fico, dat$default, dat$paid
  ) %>% lapply(htmltools::HTML)
  label1 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
    <strong>loan book [Million]:</strong> %1.1f<br/>
    <strong>loans:</strong> %i<br/>
    <strong>fico [avg]:</strong> %i<br/>
    <strong>default rate [%%]:</strong> %1.1f<br/>
    <strong>paid loans [%%]:</strong> %1.1f",
    dat1$state, dat1$loan_book, dat1$loans_per_state, dat1$fico, dat1$default, dat1$paid
  ) %>% lapply(htmltools::HTML)
  label2 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat2$state, dat2$loan_book, dat2$loans_per_state, dat2$fico, dat2$default, dat2$paid
  ) %>% lapply(htmltools::HTML)
  label3 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
    <strong>loan book [Million]:</strong> %1.1f<br/>
    <strong>loans:</strong> %i<br/>
    <strong>fico [avg]:</strong> %i<br/>
    <strong>default rate [%%]:</strong> %1.1f<br/>
    <strong>paid loans [%%]:</strong> %1.1f",
    dat3$state, dat3$loan_book, dat3$loans_per_state, dat3$fico, dat3$default, dat3$paid
  ) %>% lapply(htmltools::HTML)
  label4 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat4$state, dat4$loan_book, dat4$loans_per_state, dat4$fico, dat4$default, dat$paid
  ) %>% lapply(htmltools::HTML)
  label5 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat5$state, dat5$loan_book, dat5$loans_per_state, dat5$fico, dat5$default, dat5$paid
  ) %>% lapply(htmltools::HTML)
  label6 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat6$state, dat$loan_book, dat6$loans_per_state, dat6$fico, dat6$default, dat6$paid
  ) %>% lapply(htmltools::HTML)
  label7 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
    <strong>loan book [Million]:</strong> %1.1f<br/>
    <strong>loans:</strong> %i<br/>
    <strong>fico [avg]:</strong> %i<br/>
    <strong>default rate [%%]:</strong> %1.1f<br/>
    <strong>paid loans [%%]:</strong> %1.1f",
    dat7$state, dat7$loan_book, dat7$loans_per_state, dat7$fico, dat7$default, dat7$paid
  ) %>% lapply(htmltools::HTML)
  label8 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
    <strong>loan book [Million]:</strong> %1.1f<br/>
    <strong>loans:</strong> %i<br/>
    <strong>fico [avg]:</strong> %i<br/>
    <strong>default rate [%%]:</strong> %1.1f<br/>
    <strong>paid loans [%%]:</strong> %1.1f",
    dat8$state, dat8$loan_book, dat8$loans_per_state, dat8$fico, dat8$default, dat8$paid
  ) %>% lapply(htmltools::HTML)
  label9 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat9$state, dat9$loan_book, dat9$loans_per_state, dat9$fico, dat9$default, dat9$paid
  ) %>% lapply(htmltools::HTML)
  label10 <- sprintf(
    "<span style='color: #454347; font-size: 11pt'><strong>%s</strong><br/>
     <strong>loan book [Million]:</strong> %1.1f<br/>
     <strong>loans:</strong> %i<br/>
     <strong>fico [avg]:</strong> %i<br/>
     <strong>default rate [%%]:</strong> %1.1f<br/>
     <strong>paid loans [%%]:</strong> %1.1f",
    dat10$state, dat10$loan_book, dat10$loans_per_state, dat10$fico, dat10$default, dat10$paid
  ) %>% lapply(htmltools::HTML)
  
  if(grepl("^loanbook$|^loan book$", .what)){
    .bins = c(0, 20, 40, 60, 80, 100, 250, 400, 550, Inf)
    palette <- colorBin("YlOrRd", bins = .bins)
    
    leaflet() %>% 
      setView(-95, 40, 4, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat$loan_book), 
                  fillOpacity = 0.7,         
                  color = "white",       
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat1$loan_book), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>",
                  label = label1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat2$loan_book), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                  label = label2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat3$loan_book), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                  label = label3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat4$loan_book), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                  label = label4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat5$loan_book), 
                  fillOpacity = 0.7,       
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                  label = label5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat6$loan_book), 
                  fillOpacity = 0.7,       
                  color = "white",     
                  weight = 1.5,         
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                  label = label6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat7$loan_book), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                  label = label7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat8$loan_book), 
                  fillOpacity = 0.7,      
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                  label = label8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat9$loan_book), 
                  fillOpacity = 0.7,       
                  color = "white",      
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                  label = label9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat10$loan_book), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>",
                  label = label10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addLayersControl(baseGroups = c("<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>", 
                                      "<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>"), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = palette, 
                values = .bins,
                opacity = 0.7, 
                title = "<span style='color: #454347; font-size: 11pt'><strong>Loan book [Million]</strong></span>",
                position = "bottomleft") -> ch
  }
  
  if(grepl("^loans$|^loan$", .what)){
    .bins = c(0, 2500, 5000, 7500, 10000, 20000, 30000, 40000, 50000, Inf)
    palette <- colorBin("PuBu", bins = .bins)
    
    leaflet() %>% 
      setView(-95, 40, 4, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat$loans_per_state), 
                  fillOpacity = 0.7,         
                  color = "white",       
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat1$loans_per_state), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>",
                  label = label1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat2$loans_per_state), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                  label = label2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat3$loans_per_state), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                  label = label3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat4$loans_per_state), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                  label = label4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat5$loans_per_state), 
                  fillOpacity = 0.7,       
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                  label = label5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat6$loans_per_state), 
                  fillOpacity = 0.7,       
                  color = "white",     
                  weight = 1.5,         
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                  label = label6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat7$loans_per_state), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                  label = label7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat8$loans_per_state), 
                  fillOpacity = 0.7,      
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                  label = label8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat9$loans_per_state), 
                  fillOpacity = 0.7,       
                  color = "white",      
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                  label = label9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat10$loans_per_state), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>",
                  label = label10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addLayersControl(baseGroups = c("<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>", 
                                      "<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>"), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = palette, 
                values = .bins,
                opacity = 0.7, 
                title = "<span style='color: #454347; font-size: 11pt'><strong>Loans per state</strong></span>",
                # topleft or bottomleft seemed most appropiate
                position = "bottomleft") -> ch
  }
  
  if(grepl("^default$", .what)){
    # Alternative palette
    #.bins = c(0, 2, 4, 6, 8, 10, 15, 100)
    #palette <- colorBin(c("#006837", "#31a354", "#78c679", "#2c7fb8", "#d7b5d8", "#df65b0", "#dd1c77", "#980043"), bins = .bins)

    # RColorBrewer palette
    .bins = c(0, 2, 4, 6, 8, 10, 12.5, 15, 20, 100)
    palette <- colorBin("YlOrRd", bins = .bins)
    
    
    leaflet() %>% 
      setView(-95, 40, 4, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat$default), 
                  fillOpacity = 0.7,         
                  color = "white",       
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat1$default), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>",
                  label = label1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat2$default), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                  label = label2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat3$default), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                  label = label3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat4$default), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                  label = label4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat5$default), 
                  fillOpacity = 0.7,       
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                  label = label5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat6$default), 
                  fillOpacity = 0.7,       
                  color = "white",     
                  weight = 1.5,         
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                  label = label6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat7$default), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                  label = label7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat8$default), 
                  fillOpacity = 0.7,      
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                  label = label8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat9$default), 
                  fillOpacity = 0.7,       
                  color = "white",      
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                  label = label9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat10$default), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>",
                  label = label10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addLayersControl(baseGroups = c("<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>", 
                                      "<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>"), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = palette, 
                values = .bins,
                opacity = 0.8, 
                title = "<span style='color: #454347; font-size: 11pt'><strong>Default rate [%]</strong></br>Inaccurate for non-</br>terminated loans</span>",
                position = "bottomleft") -> ch
  }
  
  if(grepl("^fico$|^ficos$|^score$", .what)){
    .bins = c(300, 640, 660, 680, 690, 700, 710, 720, 730, 740, 800, 850)
    palette <- colorBin("RdYlGn", bins = .bins)
 
    leaflet() %>% 
      setView(-95, 40, 4, zoom = 3) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat$fico), 
                  fillOpacity = 0.7,         
                  color = "white",       
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                  label = label,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat1$fico), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>",
                  label = label1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%  
      addPolygons(data = us, 
                  fillColor = ~palette(dat2$fico), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                  label = label2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat3$fico), 
                  fillOpacity = 0.7,         
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                  label = label3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat4$fico), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                  label = label4,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat5$fico), 
                  fillOpacity = 0.7,       
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                  label = label5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat6$fico), 
                  fillOpacity = 0.7,       
                  color = "white",     
                  weight = 1.5,         
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                  label = label6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat7$fico), 
                  fillOpacity = 0.7,        
                  color = "white",       
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                  label = label7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat8$fico), 
                  fillOpacity = 0.7,      
                  color = "white",      
                  weight = 1.5,          
                  group="<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                  label = label8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat9$fico), 
                  fillOpacity = 0.7,       
                  color = "white",      
                  weight = 1.5,            
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                  label = label9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addPolygons(data = us, 
                  fillColor = ~palette(dat10$fico), 
                  fillOpacity = 0.7,        
                  color = "white",      
                  weight = 1.5,           
                  group="<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>",
                  label = label10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>% 
      addLayersControl(baseGroups = c("<span style='color: #454347; font-size: 11pt'><strong>2007 - 2017Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2007 - 2011</strong></span>", 
                                      "<span style='color: #454347; font-size: 11pt'><strong>2012 - 2013</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2014</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2015</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q2</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q3</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2016Q4</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q1</strong></span>",
                                      "<span style='color: #454347; font-size: 11pt'><strong>2017Q2</strong></span>"), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = palette, 
                values = .bins,
                opacity = 0.7, 
                title = "<span style='color: #454347; font-size: 11pt'><strong>FICO average</strong></span>",
                position = "bottomleft") -> ch
  }
  
  return(ch) 
  
}

