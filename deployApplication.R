library(rsconnect)
# rsconnect::setAccountInfo(name='foodandagricultureorganization', token='901F406B80A902BE7F111999B8605DD4', secret='NiUx3/KDGh4UDfNj7N9LQTurmRzWbfcsJh/Oe1Je')

rsconnect::setAccountInfo(name='foodandagricultureorganization', token='AD612DD08AE4B6CD0987A491387F86E0', secret='TconB5nNzwzUjwDTE/TnBRjLsyN05Fd32qODFtyJ')

files <- c(
   
  "ui.R",
  "server.R",
  "global.R",
  "makeRegionData.R",
  "landingPageText.R",
#  "www/gleam2.css",
  "www/gleam.css",
  "www/footer.html",
  "googleAnalytics.html",
  "html/regions.htm",
  # "CountrySummaryReport_HTML.Rmd",
  #"CountrySummaryReport_DOC.Rmd",
  #"CountrySummaryReport_PDF.Rmd",
  "plottingFunctions.R",
  "dashboardData/ParameterALLNoCohorts.csv",
  "dashboardData/feedIntakeCat.csv",
  "dashboardData/GLEAMDataALL.csv",
  "dashboardData/GAULListUsedByGLEAM_20230508.csv", #only so it is there in the dir. 
  "dashboardData/GLEAMColorsAndDashboardTexts.xlsx",
  "html/aboutGLEAM.html",
  "html/changeLog.html",
  "html/readme.html",
  "dashboardData/paramListSupplement.xlsx"
#  "dashboardData/CAIT_TotalGHGEmissionsHistorical.xlsx"
)

fileCheck <- lapply (files, file.exists)
XX <-  unlist (fileCheck)
XX[ XX == FALSE]
 
# appDir <- "/Users/dwisser/Source/GLEAMv3Dashboard"
appDir <- "~/Gleam3dashboard_deployment"

#deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "GLEAMV3_Private", logLevel = "verbose",
#          account = "foodandagricultureorganization")


deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "Gleam_test_JP", logLevel = "verbose",
          account = "foodandagricultureorganization")


# deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "GLEAMV3_Private", logLevel = "verbose",
          # account = "foodandagricultureorganization")



deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "GLEAMV3_Public", logLevel = "verbose",
          account = "foodandagricultureorganization")



deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "GLEAMV3_Public_dev1268101890", logLevel = "verbose",
          account = "foodandagricultureorganization")


deployApp(appDir = appDir , appFiles = files, forceUpdate = T,appName = "GLEAMV3_Private", logLevel = "verbose",
          account = "foodandagricultureorganization")




rsconnect::showLogs(streaming = TRUE ,appName = "GLEAMV3_Public", account = "foodandagricultureorganization")

