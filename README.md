Design effect calculation app 

This repository is to create an app to calculate and illustrate design effect for surveys. The App is available at https://yj-choi.shinyapps.io/Shiny_DesignEffect/.

Thre are four Key files:
1. HowToCalculateICCandDesignEffect.do: Stata do file to calculate ICC and DEFF using latest DHS data in 60+ countires
2. ICCfromDHS.csv: Dataset with ICC by country and indicator, created using the Stata do file. This file is needed for the Shiny App. 
3. HowToCalculateICC.rmd: R markdown file to explain the Stata do file and present distributions of eestimated ICC and DEFF among the 60+ countries. See here for the knitted output: https://rpubs.com/YJ_Choi/icc_deff
4. app.R: to created Shiny app. 

A companion app is about calculating sample size, and available here: https://yj-choi.shinyapps.io/Shiny_SampleSize/
