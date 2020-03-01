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
    titlePanel("Interactive application to estimate potential design effect in survey design"),
    
    # Side panel: define input and output   
    sidebarLayout(
        
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            selectInput("indicator", 
                        "What is the primary indicator that your survey is designed to measure?",
                        choices = indicatorlist),        

            selectInput("country", 
                        "What is the country where your survey will be conducted?",
                        choices = countrylist),        
            
            sliderInput("clustersize", 
                        "What is the number of households in your sampling cluster?",
                        min = 20, max = 40, value = 30, ticks = TRUE)
        ),
        
        # Main page for output display 
        mainPanel(
            h4(strong("Design effect and finding a sweet spot in survey design")), 
            h5("This interactive tool is for those who try to determine", 
               a("sample size", href="https://yj-choi.shinyapps.io/Shiny_SampleSize/"), 
               "in surveys using cluster sampling. This tool assists you to estimate potential design effect, based on select indicators and their observed ICC from real world,", 
               a("Demographic and Health Surveys,", href="www.dhsprogram.com"), 
               "and potential cluster size. There are two parts - brief background on design effect and interactive application.",
               "Well, we start from the second part."),
            
            hr(),                        
            h5(strong("Part 2: Calculation of potential design effect") ),
            
                h5("If we know ICC of our study population, we can calculate design effect",
                   a("(see this example to see how to calculate ICC - coming soon).", href="xxx"), "But, often, we don't.",
                   "An approach is to `borrow` ICC from a survey that covered similar population and indicators of our interest."),
                h5("In this exercise, we use illustrative examples of commonly measured indicators.", 
                   "We also assume that our study population has ICC that is similar to ICC observed in the latest DHS conducted in the country."),
                h5(strong("To get started provide three input values on the left panel.")),

            h5("If we want to measure"), 
            verbatimTextOutput("text_indicator"),
            h5("in"),
            verbatimTextOutput("text_country"),
            h5("ICC from the country's latest DHS is:"),
            verbatimTextOutput("texticc"), 
                    
            hr(),    
            h5("Then, based on your selected cluster size of "),
            verbatimTextOutput("text_clustersize"),            
            h5("Your design effect is:"),
            verbatimTextOutput("textdeff"),     

            h5("See how design effect changes based on the cluster size, given ICC from DHS in your study country. Your call to decide on the cluster size!"),
            plotlyOutput("plot1"),       
            
            hr(),                        
            h5(strong("Part 1: What is and what determines deign effect? Very, very briefly...")),
            
                h5("Simple random sample (SRS) is rarely possible, without unlimited human, time, and financial resources.", 
                   "An approach balancing real world feasibility and desired precision of estimates from an unbiased sample is", 
                   strong(a("cluster sampling.", href="https://en.wikipedia.org/wiki/Cluster_sampling"))),
                h5("We first sample groups that our sampling units belong to (aka cluster)",  
                   "and then randomly select the ultimate sample units from each group."),

                h5("A downside of this operationally smart approach is that: our sampled units tend to form similarities in each group, variance in our sample decreases, and we lose precision.",
                   "For example, even if we sample 300 students based on cluster sampling, statistical variance among the 300 students may be same with variance in a SRS of 100 students."), 
                h5("This relative inefficiency in sample size compared to that in SRS", 
                   "(or relative inflation of sample size required in cluster sample in order to have same precision from SRS) is", 
                   strong("design effect."),
                   "It is an important factor to determine",
                   a("sample size for a cluster survey.", href="https://yj-choi.shinyapps.io/Shiny_SampleSize/") ), 

                h5("It is determined by two factors:"),             
                h5("- Underlying similarity of individuals within cluster,", strong(a("Intraclass Correlation (ICC)",href="https://en.wikipedia.org/wiki/Intraclass_correlation")) ),             
                h5("- The number of units that need to sampled from each cluster (take size)"),
                h5("While we cannot control ICC in the population, we can choose the cluster size.", 
                   "The bigger the cluster, field operation will be more manageable and somewhat less expensive.", 
                   "But, it will in turn increase design effect, which will eventually require more number of clusters for the survey."),
                h5("Our goal is to find a sweet spot,", strong("balancing operation reality, budget, and precision"), "of survey estimates." ),

            hr(),
            h6("See", a("GitHub",href="https://github.com/yoonjoung/Shiny_SampleSize"),"for more information."),
            h6("(Application last updated on:", as.Date(Sys.time(	), format='%d%b%Y'),")"),
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
        paste(input$indicator) 
        })
    output$text_country <- renderText({
        paste0(input$country,",") 
        })    
    output$text_clustersize <- renderText({
        paste0(input$clustersize,",") 
        })        

    #################################### 
    # output: ICC 
    output$texticc <- renderText({
        
        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)
        
        paste0(round(dtaselected$icc, 3),".")
        })    

    #################################### 
    # output: DEFF 
    output$textdeff <- renderText({
        
        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)    
        icc <- dtaselected$icc
        mydeff  <- 1 + icc*(input$clustersize - 1)
        
        paste0(round(mydeff, 1),".")
        })    
    
    ####################################             
    # plot DEFF
    output$plot1 <- renderPlotly({

        dtaselected<-filter(dta, country==input$country & indicator==input$indicator)  
        
            mydeff  <- 1 + dtaselected$icc*(input$clustersize - 1) 
        
        clustersize<-c(seq(20, 40, 1))
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
                    yref="y", y0=mydeff-0.2,y1=mydeff+0.2,
                    fillcolor='green',
                    opacity=0.5
                )
            )
            
        })
    

    

}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)