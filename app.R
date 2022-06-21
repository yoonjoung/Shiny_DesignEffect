library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to calculate sample size for a survey. 
# There are four parts in this document:
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP 

#******************************
# 0. Database update 
#******************************

dta <- read.csv("ICCfromDHS.csv")

countrylist<-unique(as.vector(dta$country))
indicatorlist<-unique(as.vector(dta$indicator))

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    headerPanel("What is likely design effect in my survey?"),

    # Title panel 
    titlePanel("Interactive application to estimate potential design effect in survey design",
               em("with real world ICC and potential cluster take sizes")),
    
    # Side panel: define input and output   
    sidebarLayout(
        
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            h5(strong("First, find ICC from real world:")),
            selectInput("indicator", 
                        "What is the primary indicator that your survey is designed to measure?",
                        choices = indicatorlist, 
                        selected = "% women using modern contraceptives"),        
            selectInput("country", 
                        "What is the country where your survey will be conducted?",
                        choices = countrylist),        
            h5("(More indicators to be added. Stay tuned!)"),
            br(),
            br(),
            br(),
            br(),
            br(),
            h5(strong("Second, provide your cluster take size:")),
            sliderInput("clustersize", 
                        "What is the number of households in your sampling cluster?",
                        min = 10, max = 40, value = 30, ticks = TRUE) 
            
        ),
        
        # Main page for output display 
        mainPanel(
            h4(strong("Design effect and finding a sweet spot in survey design")), 
            h5("This interactive tool is for those who try to determine", 
               a("sample size", href="https://isquared.shinyapps.io/SampleSize/"), 
               "in cluster sample surveys - balancing operation reality, budget, and precision of survey estimates.", 
               "It will assists you to estimate potential design effect, based on select indicators and their observed ICC from real world,", 
               a("Demographic and Health Surveys (DHS)", href="www.dhsprogram.com"), 
               "conducted in 60 countries, and potential cluster size."),
            
            hr(),                        
            h5(strong("Part 1: What is and what determines deign effect? Very, very briefly...")),
            
                h5("Simple random sample (SRS) is rarely possible, without unlimited human, time, and financial resources.", 
                   "An approach balancing real world feasibility and desired precision of estimates from an unbiased sample is", 
                   strong(a("cluster sampling.", href="https://en.wikipedia.org/wiki/Cluster_sampling")),
                   "We first sample groups that our sampling units belong to (aka cluster)",  
                   "and then randomly select the ultimate sample units from each group."),

                h5("A downside of this operationally smart approach is that: our sampled units tend to form similarities in each group, variance in our sample decreases, and we lose precision.",
                   "For example, even if we sample 300 students based on cluster sampling, statistical variance among the 300 students may be same with variance in a SRS of 100 students."), 
                h5("This relative inefficiency in sample size compared to that in SRS", 
                   "(or relative inflation of sample size required in cluster sample in order to have same precision from SRS) is", 
                   strong("design effect."),
                   "It is an important factor to determine",
                   a("sample size for cluster surveys.", href="https://isquared.shinyapps.io/SampleSize/") ), 

                h5("Two factors determine design effect:"),             
                h5("- Underlying similarity of individuals within cluster,", strong(a("Intraclass Correlation (ICC)",href="https://en.wikipedia.org/wiki/Intraclass_correlation")) ),             
                h5("- The number of units that need to sampled from each cluster,", strong("cluster take size")),
                h5("While we cannot control ICC in a population, we can choose the take size."), 
                h5("The bigger the cluster size, field operation becomes more manageable and possibly even less expensive.", 
                   "But, it will instead increase design effect, losing precision,", 
                   "and we may need a more number of clusters to maintain any required level of precision.", style="color: #ad1d28"),
                h5("Our goal is to find", strong("a sweet spot, balancing operation reality, budget, and precision"), "of survey estimates." ),

            hr(),       
            h5(strong("Part 2: How do we know potential design effect?") ),
            
                h5("If we know ICC of our study population, we can calculate design effect -",
                   a("check out this example to see how to calculate ICC.", href="https://rpubs.com/YJ_Choi/icc_deff"), "But, often, we don't.",
                   "One approach is to `borrow` ICC from a survey that covered similar population and indicators of our interest,",
                   "with an assumption that our study population has ICC that is similar to one observed in the survey."),
                h5("In this exercise, we use illustrative examples of commonly measured indicators from the latest DHS conducted in select countries."),
                h5(em("To get started provide three input values on the left panel."), align="center"),

            hr(),    
            textOutput("text_indicator"),
            textOutput("text_country"),
            h5("ICC from the country's latest DHS is:"),
            verbatimTextOutput("texticc"), 
            textOutput("text_clustersize"),            
            h5("Design effect is:"),
            verbatimTextOutput("textdeff"),     
            
            br(),    
            h5("See how design effect changes based on the cluster size, given ICC from DHS in your study country. Now it's your call to decide the cluster take size, based on your budget and desired level of precision!"),
            plotlyOutput("plot1"),       

            hr(),
            h6("See", a("GitHub",href="https://github.com/yoonjoung/Shiny_DesignEffect"),"for more information - especially calculation of ICC by select indicator."),
            h6("Application initially published on Macrh 3, 2020."),
            h6("For typos, errors, and questions:", a("contact me",href="https://www.isquared.global/YJ"))
        )
    )
)

#******************************
# 2. SERVER
#******************************

server<-function(input, output) {

    #################################### 
    # text output of inputs
    output$text_indicator <- renderText({
        paste("If we want to measure",input$indicator) 
        })
    output$text_country <- renderText({
        paste("in",input$country) 
        })    
    output$text_clustersize <- renderText({
        paste("And, based on the selected cluster size of",input$clustersize) 
        })        

    #################################### 
    # output: ICC 
    output$texticc <- renderText({
        
        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)
        
        paste0(round(dtaselected$icc, 3),"")
        })    

    #################################### 
    # output: DEFF 
    output$textdeff <- renderText({
        
        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)    
        icc <- dtaselected$icc
        mydeff  <- 1 + icc*(input$clustersize - 1)
        
        paste0(round(mydeff, 1),"")
        })    
    
    ####################################             
    # plot DEFF
    output$plot1 <- renderPlotly({

        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)  
        
            mydeff  <- 1 + dtaselected$icc*(input$clustersize - 1) 
        
        clustersize<-c(seq(10, 40, 1))
        deff    <- 1 + dtaselected$icc*(clustersize - 1)
        
        plotdta<-data.frame(clustersize,deff) 

        plot_ly(plotdta, x=plotdta$clustersize, y=plotdta$deff, 
            mode="markers"
            )  %>%
            layout(
                autosize = F, width = 600, height = 400, 
                xaxis = list(title = "Number of households per cluster"),
                yaxis = list(title = "Design effect"),
                shapes= list(
                    type="circle",
                    xref="x", x0=input$clustersize-0.5,x1=input$clustersize+0.5,  
                    yref="y", y0=mydeff-0.1,y1=mydeff+0.1,
                    fillcolor='green',
                    opacity=0.4
                )
            )
            
        })
    

    

}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)