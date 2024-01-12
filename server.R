library (shiny)
#source ("scripts/global.R")
#cat ("globbal \n")
source ("plottingFunctions.R")
#source ("subsetTableFunctions.R") # this will be obsolete with a new data structure

#source ("infoBoxes.R")


shinyServer <- function (input, output, session){
  
# landing page events #################
  
  observeEvent(input$switch_tab_population_new, {
    updateTabItems(session, "sidebar", "tab_population")
  })
  observeEvent(input$switch_tab_production_new, {
    updateTabItems(session, "sidebar", "tab_production")
  })
  observeEvent(input$switch_tab_emissions_new, {
    updateTabItems(session, "sidebar", "tab_emissions")
  })
  observeEvent(input$switch_tab_emIntensity_new, {
    updateTabItems(session, "sidebar", "tab_emissionIntensity")
  })
  observeEvent(input$switch_tab_comparison_new, {
    updateTabItems(session, "sidebar", "tab_comparison")
  })
  observeEvent(input$switch_tab_inputdata_new, {
    updateTabItems(session, "sidebar", "tab_inputData")
  })
  observeEvent(input$switch_tab_aboutgleam_new, {
    updateTabItems(session, "sidebar", "tab_about")
  })
  observeEvent(input$switch_tab_regions_new, {
    updateTabItems(session, "sidebar", "tab_Regions")
  })
  observeEvent(input$switch_tab_changes_new, {
    updateTabItems(session, "sidebar", "tab_changes")
  })
  
  
  observeEvent(input$tab_switchMRV, {
    updateTabItems(session, "sidebar", "tab_resources")
  })
  observeEvent(input$tab_switchReports, {
    updateTabItems(session, "sidebar", "tab_report")
  })
  
  data_rv <- reactiveValues(data = iris, names = "iris") 
  
  # data = allData.dt[ COUNTRY %in% input$ply_selCountry], name = "iris") # set to some dummy value !? 
  
  observeEvent(input$ply_selCountry, {
    #if (input$sel_indicator == "Forest land") {
    cat (input$ply_selCountry, "\n")
    data_rv$data <- allData.dt[ COUNTRY %in% input$ply_selCountry   & Animal %in% input$ply_selAnimal ]
    data_rv$name <- "iris"
    
  })
  
  
  esquisse_server(
    id = "esquisse", 
    data_rv = data_rv, 
    #default_aes = reactive(input$aes)
  )
  
  
  # help_on the maps ################################################## 
  observeEvent(input$help_emissions_map, {
    showModal(modalDialog(
      title = "Map of GHG emissions from animals",
      infoTextHiHMaps,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_population_map, {
    showModal(modalDialog(
      title = "Map of animal population",
      infoTextHiHMaps,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_production_map, {
    showModal(modalDialog(
      title = "Map of animal products",
      infoTextHiHMaps,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emissionIntensity_map, {
    showModal(modalDialog(
      title = "Map of animal emission intensity",
      infoTextHiHMaps,
      easyClose = TRUE
    ))
  })
  
  
  ######################################################################################################################################################### 

  observeEvent(input$help_production, {
    showModal(modalDialog(
      #h2("Data Guidelines"),
      title = "Production statistics",
      infoTextProduction,
      HTML('<a href="http://www.fao.org/faostat">FAOSTAT</a>'),
      # tags$a(href="www.fao.org", target= "_blank", "Click here!"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_productionsystems, {
    showModal(modalDialog(
      title = "Animal population statistics",
      infoTextProductionSystem,
      HTML('<a href="https://dataverse.harvard.edu/dataverse/glw">Gridded Livestock of the World (GLW)</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emissions, {
    showModal(modalDialog(
      title = "Greenhouse gas emissions",
      infoTextEmissions,
      HTML('<a href=" http://www.fao.org/gleam/en/">GLEAM documentation</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emissionintensity, {
    showModal(modalDialog(
      title = "Emission intensity",
      infoTextEmissionIntensity,
      HTML('<a href=" http://www.fao.org/gleam/en/">GLEAM documentation</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emission_country_report, {
    showModal(modalDialog(
      title = "Country summary report",
      infoTextEmissionIntensity,
      HTML('<a href=" http://www.fao.org/gleam/en/">GLEAM webpages</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emission_input_report, {
    showModal(modalDialog(
      title = "Input data summary report",
      infoTextEmissionIntensity,
      HTML('<a href=" http://www.fao.org/gleam/en/">GLEAM webpages</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$help_emission_unfccc_report, {
    showModal(modalDialog(
      title = "Livestock emissions UNFCCC compliant",
      infoTextEmissionIntensity,
      HTML('<a href=" http://www.fao.org/gleam/en/">GLEAM webpages</a>'),
      #output$tab,
      easyClose = TRUE
    ))
  })
  
  
  output$emission_country_report_PDF <- downloadHandler(
    #cat (input$selectCountry)
    # For PDF output, change this to "report.pdf"
    filename = paste ("LivestockImpactReport_", input$selectCountry, ".pdf"),
    #filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("CountrySummaryReport_PDF.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(curCountry = input$selectCountry)
      print (params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params, pdf_document(),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$emission_country_report_DOC <- downloadHandler(
    #cat (input$selectCountry)
    # For PDF output, change this to "report.pdf"
    filename = paste ("LivestockImpactReport_", input$selectCountry, ".docx"),
    #filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("CountrySummaryReport_DOC.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(curCountry = input$selectCountry)
      print (params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,word_document(),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$emission_country_report_HTML <- downloadHandler(
    #cat (input$selectCountry)
    # For PDF output, change this to "report.pdf"
    filename = paste ("LivestockImpactReport_", input$selectCountry, ".html"),
    #filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("CountrySummaryReport_HTML.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(curCountry = input$selectCountry)
      print (params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadCountryReport<-downloadHandler(
    filename = function (){
      
      paste(paste0 ('GLEAMCountrySummaryReport_',input$selectCountry),  sep = '.',  switch(
        input$country_report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
      
      
    } ,
    #filename = "report.pdf",
    content = function(file) {
      #showModal(modalDialog("Creating country summary report. This might take a few seconds. ", footer=NULL))
      
      src <-  normalizePath (paste('CountrySummaryReport', sep = '',  switch(
        input$country_report_format, PDF = '_PDF', HTML = '_HTML', Word = '_DOC'
      ), ".Rmd"))
      # Set up parameters to pass to Rmd document
      params <- list(curCountry = input$selectCountry)
      # 
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      cat ("src = ", src, "Tempdir = ",  tempdir(),"\n")
      file.copy( src, 'report.Rmd', overwrite = TRUE)
      out <- render('report.Rmd', switch(
        input$country_report_format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()), params = params, envir = new.env(parent = globalenv()
        ))
      file.rename(out, file)
      on.exit(removeModal())
    }
    
  )
  
#############################  input data section ##################################

  output$inputDatSelAnimal <- renderUI({
    cat ("AN", input$selectCountry,"\n")
    #cat (str (rv$Params))
      anChoices <- unique (paramData.dt[ ISO3 == input$selectCountry]$Animal )
      cat ("AB", anChoices, "\n")
    
    radioButtons ("inData_selSpecies", label= "Species",inline = T, choices = anChoices, selected= anChoices[[1 ]])
  })
  
 output$inputDataHerdParameters <- DT::renderDataTable({
   req (input$inData_selSpecies)
   
   selParams <- paramListSuppl.dt[ VarName == "Herd parameters"]$VarNameShort
#   cat ("SelParams, ",selParams, "\n")
   xx <- paramData.dt[Animal %in% input$inData_selSpecies &  varName %in% selParams & ISO3 == input$selectCountry & varName %in% selParams]
   #xx <- paramData.dt[Animal %in% c("Cattle") &  varName %in% selParams & COUNTRY == "World"]
   xx <- merge (xx, paramListSuppl.dt,by.x = "varName", by.y= "VarNameShort")
   xx[, tableHeader:= paste0 (VarNameText, "<br/> [ ", Unit, " ]" )]  
   #   
   xx$V1 <- signif (xx$V1,3) # [, V1:= signif(V1,3)]
   xx <- xx[ V1 != 0]
  # cat (str (xx),"\n")
   # for some reason parameters are dubplciaged? 
         xx <- xx[!duplicated(xx), ]
  
   xx.w <- dcast (xx,  COUNTRY + Animal + LPS + HerdType ~ tableHeader, value.var =  "V1" )
  # xx.w <- xx.w[ V1 != 0]
   setnames (xx.w,"COUNTRY", "Area")
   ret <-  DT::datatable(xx.w,  caption = "Herd parameters used in GLEAM 3, weighted by population. Rounded to 3 significant digits. " ,
                         extensions=c("Buttons",'Scroller'),escape = F,
                         #colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                         options = list(dom = 'Brtip',
                                        scrollY = 200,
                                        scroller = TRUE,
                                        scrollX= 200,
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        
                                        buttons =
                                          list('copy', 'print', list(
                                            extend = 'collection',
                                            buttons = list(
                                              list(extend = 'csv', filename = paste0("GLEAM3HerdParameters", Sys.Date())   ) ,
                                              list(extend = 'excel', filename = paste0("GLEAM3HerdParameters" , Sys.Date()) ),
                                              list(extend = 'pdf', filename = paste0("GLEAM3HerdParameters", Sys.Date())    )),
                                            text = 'Download'
                                          ))
                         )
                         
   )
   
   
   
   return (ret)
 }) 
 
 
 output$inputDataMMS <- DT::renderDataTable({
   req (input$inData_selSpecies)
   selParams <- paramListSuppl.dt[ VarName == "Manure management"]$VarNameShort
   cat ("SelParams, ",selParams, "\n")
   
   xx <-  paramData.dt[Animal %in% input$inData_selSpecies &  varName %in% selParams & ISO3 == input$selectCountry]
 #  xx <-  paramData.dt[Animal %in%  c("Cattle")  &  varName %in% selParams & COUNTRY == "World"]
   
   xx <- merge (xx, paramListSuppl.dt,by.x = "varName", by.y= "VarNameShort")
   
     
   xx[, tableHeader:= paste0 (VarNameText )]  
   xx [ , V1:=  signif (V1*100, 3)]
   xx <- xx[ V1 != 0]
   xx[ varName == "MMSDEEPLITT"]
   #xx$V1 <- signif (xx$V1,3) # [, V1:= signif(V1,3)]
   # cat (str (xx),"\n")
   # for some reason parameters are dubplciaged? 
   #xx <- xx[duplicated(xx), ]
   
   
   
   
   xx.w <- dcast (xx,  COUNTRY + Animal + LPS + HerdType ~ tableHeader, value.var =  "V1" )
   setnames (xx.w,"COUNTRY", "Area")
  # xx.w <- xx.w[ V1 != 0]
   ret <-  DT::datatable(xx.w,  caption = "Manure management systems percentage, used in GLEAM 3, weighted by population. Rounded to 3 significant digits. " ,
                         extensions=c("Buttons",'Scroller'),escape = F,
                         #colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                         options = list(dom = 'Brtip',
                                        scrollY = 200,
                                        scroller = TRUE,
                                        scrollX=200,
                                        
                                        buttons =
                                          list('copy', 'print', list(
                                            extend = 'collection',
                                            buttons = list(
                                              list(extend = 'csv', filename = paste0("GLEAM3ManureMS", Sys.Date())   ) ,
                                              list(extend = 'excel', filename = paste0("GLEAM3ManureMS" , Sys.Date()) ),
                                              list(extend = 'pdf', filename = paste0("GLEAM3ManureMS", Sys.Date())    )),      
                                            text = 'Download'
                                          ))
                         )
                         
   )

   return (ret)
 })
 
 
 
 output$inputDataLiveWeight <- DT::renderDataTable({
   req (input$inData_selSpecies)
   
   selParams <- paramListSuppl.dt[ VarName == "Live weights"]$VarNameShort
   cat ("SelParams, ",selParams, "\n")
  # xx <-  paramData.dt[Animal %in%  c("Cattle")  &  varName %in% selParams & COUNTRY == "World"]
   
   xx <-  paramData.dt[Animal %in% input$inData_selSpecies & ISO3 == input$selectCountry  & varName %in% selParams]
  # xx <-  paramData.dt[Animal == c("Cattle") &  VarName == "DryMatterIntake"]
   
   xx <- merge (xx, paramListSuppl.dt,by.x = "varName", by.y= "VarNameShort")
   
   
   xx[, tableHeader:= paste0 (VarNameText )]  
   
   xx$V1 <- signif (xx$V1,3) # [, V1:= signif(V1,3)]
   xx <- xx[ V1 != 0]
   cat (str (xx),"\n")
   #xx <- xx[!duplicated(xx), ]
  
   
   xx.w <- dcast (xx,  COUNTRY + Animal + LPS + HerdType ~ tableHeader, value.var =  "V1" )
   
   setnames (xx.w,"COUNTRY", "Area")
   ret <-  DT::datatable(xx.w,  caption = "Live weights used in GLEAM 3, weighted by population in kg. Rounded to 3 significant digits. " ,
                         extensions=c("Buttons",'Scroller'),escape = F,
                         #colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                         options = list(dom = 'Brtip',
                                        scrollY = 200,
                                        scroller = TRUE,
                                        scrollX=200,
                                        
                                        buttons =
                                          list('copy', 'print', list(
                                            extend = 'collection',
                                            buttons = list(
                                              list(extend = 'csv', filename = paste0("GLEAM3LiveWeight", Sys.Date())   ) ,
                                              list(extend = 'excel', filename = paste0("GLEAM3LiveWeight" , Sys.Date()) ),
                                              list(extend = 'pdf', filename = paste0("GLEAM3LiveWeight", Sys.Date())    )),                                          
                                            text = 'Download'
                                          ))
                         )
   )

   return (ret)
 })
 
 output$inputDryMatterIntakeSummary <- DT::renderDataTable({
   req (input$inData_selSpecies)
   
   #selParams <- paramListSuppl.dt[ VarName == "Live weights"]$VarNameShort
   #cat ("SelParams, ",selParams, "\n")
   xx <-  dmIntake.dt[Animal %in% input$inData_selSpecies  & ISO3 == input$selectCountry]
   xx <-  dmIntake.dt[Animal %in% input$inData_selSpecies  & ISO3 == input$selectCountry]
  # xx <- dmIntake.dt[ Animal == "Cattle" & COUNTRY == "World"]
   #xx <- merge (xx, paramListSuppl.dt,by.x = "varName", by.y= "VarNameShort")
   
   
   #xx[, tableHeader:= paste0 (VarNameText, "<br/> [ ", Unit, " ]" )]  
   xx [ , V1:=  as.integer(V1/1000)]
   
   #xx <- rv$DMData[Animal %in% input$inData_selSpecies ]
   #setnames (xx, "feedCat", "Feed category")
   
   #xx$V1 <-  (xx$V1) # [, V1:= signif(V1,3)]
   cat (str (xx),"\n")
   #xx <- xx[!duplicated(xx), ]
   #dmData.dt
   xx$V1 <- format(xx$V1, big.mark=',', scientific=FALSE,  digits = NULL, format = "d") 
   setnames (xx, "HerdType", "Herd")
   #setnames (xx, "ProdSys", "LPS")
   
   xx.w <- dcast (xx,  COUNTRY + Animal + LPS + Herd ~ feedCat, value.var =  "V1" )
   setnames (xx.w,"COUNTRY", "Area")
   #df$x <- format(df$x, big.mark=',', scientific=FALSE) 
   ret <-  DT::datatable(xx.w,  caption = "Total feed dry matter intake calculated with GLEAM 3 [ tonnes ] ." ,
                         extensions=c("Buttons",'Scroller'),escape = F,
                         #colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                         options = list(dom = 'Brtip',
                                        scrollY = 200,
                                        scroller = TRUE,
                                        scrollX=200,
                                        
                                        buttons =
                                          list('copy', 'print', list(
                                            extend = 'collection',
                                            buttons = list(
                                              list(extend = 'csv', filename = paste0("GLEAM3DryMatterIntake", Sys.Date())   ) ,
                                              list(extend = 'excel', filename = paste0("GLEAM3DryMatterIntake" , Sys.Date()) ),
                                              list(extend = 'pdf', filename = paste0("GLEAM3DryMatterIntake", Sys.Date())    )),                                          
                                            text = 'Download'
                                          ))
                         )
                         
   )
   
   return (ret)
 })
 
 
  output$txtSelCountries <- renderText({
    #x1 <- paste ("The following countries are included in the selected region", input$selectCountry)
    #isos <-  specialRegions[[ input$selectCountry ]] 
    #x2  <- gaulList.dt [ ISO3 != "" & !(is.na (ISO3) ) & ISO3 %in% isos]$COUNTRY 
    return ("TODO")
   # return (paste ( x2,";"))
  })
  
  output$dataHerdParameters <- DT::renderDataTable({
    
  })
  
  output$inputData_report_DOC <- downloadHandler(
    #cat (input$selectCountry)
    # For PDF output, change this to "report.pdf"
    filename = paste ("LivestockImpactReport_", input$selectCountry, ".pdf"),
    #filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("emissionReport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(curCountry = input$selectCountry)
      print (params)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  ########################## captions for tabs ##################################
  
  output$emission_caption  <- renderText({ 
    makeTabCaption ("Emissions from livestock",   input$selectCountry     )  })
  
  output$production_caption  <- renderText({ 
    makeTabCaption ("Production of animal products",  input$selectCountry )  })
  
  output$productionsystem_caption  <- renderText({ 
    makeTabCaption ("Animal Population",input$selectCountry) })
  
  output$emissionintensity_caption  <- renderText({ 
    makeTabCaption ("Emission Intensities", input$selectCountry )  })
  
  output$report_caption  <- renderText({ 
    ret <- paste("","<h3>","Download country report for ", names(regionNamesISO) [ regionNamesISO == input$selectCountry], " for 2015</h3>")  })
  
  
  output$emIntensity_downloadPlot <- downloadHandler(
    filename = function(){paste("GLEAM3_EMIntensity_", input$selectCountry,'.png',sep='')},
    content = function(file){
      ggsave(file,plot=data$plot)
    }
  )
  # data access 
  output$Data_EmIntensityHerdType <- renderUI({
    curHerd <- unique (allData.dt[ Animal %in% input$Data_selAnimals]$HerdType)
    
    checkboxGroupInput("Data_select_Herd_emIntensity", "Herd", choices =  curHerd,selected = curHerd, inline = T)
    
  })
  
  output$DataEmIntensityLPS <- renderUI({
    curLPS <- unique (allData.dt[ Animal %in% input$Data_selAnimals]$LPS)
    
    checkboxGroupInput("Data_select_LPS_emIntensity", "Production system", choices =  curLPS,selected = curLPS, inline = T)
    
  }) 
  output$DataEmIntensityCommodity <- renderUI({
    curCom <- unique (allData.dt[ VarName== "Production"& Animal %in% input$Data_selAnimals & LPS %in% input$Data_select_LPS_emIntensity]$Item)
    curCom <- c("Total protein", "Milk", "Meat", "Eggs", "per animal") 
    radioButtons("select_Comm_emIntensity", "Select commmodity", choices = c( curCom), inline = T, selected= "Meat")
    
  })
  
  output$DataEmIntensity_emSource <- renderUI({
    req (input$Data_selAnimals)
    curCom <- unique (allData.dt[ VarName== "Emissions"& Animal %in% input$Data_selAnimals ]$Item)
    #curCom <- c("Total protein", "Milk", "Meat", "Eggs", "per animal") 
    cat ("Emission sources", curCom, "\n")
    checkboxGroupInput("Dataselect_Comm_emSource", "Emission source", choiceValues = c( curCom), choiceNames = names (emSources2Choose) [ emSources2Choose ==curCom ],inline = T, selected= curCom)
    
  })

  

  # ######################## infobox for dashboards ######################################
  # 
  
  
  
  output$infoHerdSizeBUF <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeAll")
    addPopover(session, "infoHerdSizeAll", "Total number of animals", content = 
                 paste0("Total number of animals. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Buffalo", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Buffalo", sum (V1, na.rm = T)]
    
    #totAn <- humanize::count_as_word(as.integer (totAn))
    # xx <- labelFunct(totAn)
    
    if(sum(totAn > 1)){
      xx <- labelFunct(signif(totAn))
    }else{
      xx <- labelFunct(signif(totAn, 2))
    }
    
    
    
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Buffalo", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  output$infoHerdSizeCTL <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Cattle", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Buffalo", sum (V1, na.rm = T)]
    
    # totAn <- humanize::count_as_word(as.integer (totAn))
    xx <- labelFunct(totAn)
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Cattle", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  output$infoHerdSizeGTS <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Goats", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Buffalo", sum (V1, na.rm = T)]
    
    #totAn <- humanize::count_as_word(as.integer (totAn))
    xx <- labelFunct(totAn)
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Goats", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  output$infoHerdSizeSHP <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Sheep", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Buffalo", sum (V1, na.rm = T)]
    
    # totAn <- humanize::count_as_word(as.integer (totAn))
    xx <- labelFunct(totAn)
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Sheep", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  output$infoHerdSizePGS <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Pigs", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Buffalo", sum (V1, na.rm = T)]
    
    # totAn <- humanize::count_as_word(as.integer (totAn))
    # xx <- labelFunct(totAn)
    
    
    if(sum(totAn > 1)){
      xx <- labelFunct(signif(totAn))
    }else{
      xx <- labelFunct(signif(totAn, 2))
    }
    
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Pigs", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  output$infoHerdSizeCHK <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Chickens", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Chickens", sum (V1, na.rm = T)]
    
    
    xx <- labelFunct(totAn)
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Chickens", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  
  output$infoHerdSizeCML <- renderValueBox({ 
    # This is a hack that is only needed once because bsPopover needs to be loaded in the UI and is overwritten here!
    removePopover(session, "infoHerdSizeCTL")
    addPopover(session, "infoHerdSizeCTL", "Total number of Cattle", content = 
                 paste0("Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015. 
  
 "), trigger = 'hover')
    totAn <- allData.dt[VarName == "AnimalNumbers" & ISO3== input$selectCountry & Animal == "Camels", sum (V1, na.rm = T)]
    #totAn <- allData.dt[VarName == "AnimalNumbers" & COUNTRY== "World" & Animal == "Camels", sum (V1, na.rm = T)]
    
    
    xx <- labelFunct(totAn)
    #cat ("Num", totAn, "\n")
    #str ("FUCK")
    #totAn <- allData.dt[  COUNTRY  == input$selectCountry, sum (AnimalNumbers, na.rm = T) ]
    shinydashboard::valueBox(
      value = tags$p(xx, style = "font-size: 100%;"),
      subtitle = tags$p("Camels", style = "font-size: 150%;"),
      
      color = "blue", width= 2
    )
  })
  
  
  output$infoProdMeatTotal <- renderValueBox({
    
    totAn <- allData.dt[ VarName == "Production" & ISO3==  input$selectCountry & Item == "Meat" & Element == "CarcassWeight", sum (V1, na.rm = T) ]
    # xx <- labelFunctProd(signif(totAn))
    
    if(sum(totAn > 1)){
      xx <- labelFunctProd(signif(totAn))
    }else{
      xx <- labelFunctProd(signif(totAn, 2))
    }
    
    shinydashboard:: valueBox(xx,
                              
                              "Of meat",  icon = NULL,
                              color = "blue", width = 2
    )
  })
  
  output$infoProdMilkTotal <- renderValueBox({
    totAn <- allData.dt[ VarName == "Production" & ISO3==  input$selectCountry & Item == "Milk" & Element == "Weight", sum (V1, na.rm = T) ]
    # xx <- labelFunctProd(signif(totAn))
    
    if(sum(totAn > 1)){
      xx <- labelFunctProd(signif(totAn))
    }else{
      xx <- labelFunctProd(signif(totAn, 2))
    }
    
    shinydashboard:: valueBox( xx,
                               "Of milk",  icon = NULL,
                               color = "blue",width =2
    )
  })
  
  output$infoProdEggsTotal <- renderValueBox({
    totAn <- allData.dt[ VarName == "Production" & ISO3==  input$selectCountry & Item == "Eggs" & Element == "Weight", sum (V1, na.rm = T) ]
    # xx <- labelFunctProd(signif(totAn))
    if(sum(totAn > 1)){
      xx <- labelFunctProd(signif(totAn))
    }else{
      xx <- labelFunctProd(signif(totAn, 2))
    }
    
    shinydashboard:: valueBox(xx,
                              
                              "Of eggs",  icon = NULL,
                              color = "blue",width =2
    )
  })
  
  output$infoProdProteinTotal <- renderValueBox({
    totAn <- allData.dt[ VarName == "Production" & ISO3==  input$selectCountry & Element == "Protein", sum (V1, na.rm = T) ]
    #totAn <- allData.dt[ VarName == "Production" & COUNTRY==  "World" & Element == "Protein", sum (V1, na.rm = T) ]
    
    # xx <- labelFunctProd(signif(totAn))
    
    if(sum(totAn > 1)){
      xx <- labelFunctProd(signif(totAn))
    }else{
      xx <- labelFunctProd(signif(totAn, 2))
    }
    
    shinydashboard:: valueBox(xx,
                              
                              "Of animal protein",  icon = NULL,
                              color = "blue", width =2
    )
  })
  
  output$infoEmissionsTotal <- renderValueBox({
    cat ("GWP methane", GWPSets[[ input$selectGWPSet ]] $CH4 , "\n")
    
    co2tot <- allData.dt[ VarName == "Emissions" & Element == "CO2" &  ISO3 == input$selectCountry,  sum (V1 * GWPSets[[ input$selectGWPSet ]] $CO2, na.rm = T) ]
    n2otot <- allData.dt[ VarName == "Emissions"  & Element == "N2O" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]] $N2O, na.rm = T) ]
    ch4tot <- allData.dt[ VarName == "Emissions" & Element == "CH4" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]] $CH4 , na.rm = T) ]
    #ch4tot <- emissionsDetail.dt[ Gas == "CH4" & ADM0_NAME == "Kenya", sum (value_raw  * 21) ]
    cat ("Co2", co2tot, "N#", input$emInGWPNO2, "ch4", ch4tot, "\n")
   
    totEm <- (co2tot +  n2otot + ch4tot)
    
    HTML(paste0("Label (unit = m",tags$sup("2"))) # I think dead code
    # 
    # if (file.exists(dataFile)){
    #   cat ("Data file found!\n")
    #   allData.dt <-  fread (dataFile) # ony the data that will be uploaded when deployed !
    #   
    # }else{
    #   cat ("Data file not found!\n")
    # }
    
    if(sum(totEm > 0)){
      num_format <- labelFunctEm(signif(totEm))
    }else{
      num_format <- labelFunctEm(signif(totEm, 2))
    }

    # num_format <- labelFunctEm(signif(totEm, 2))
    
    # xx <- paste0(signif( as.numeric(substr(num_format, 1, nchar(num_format)-1)), 2 ),
    #              substr(num_format, nchar(num_format), nchar(num_format)))
    
    ret <-  shinydashboard:: valueBox(num_format,
                                      HTML(paste0("Total emissions [ CO",tags$sub("2"), "eq ]")),  icon = NULL,
                                      color = "blue",width =2
    )
    
    return(ret)
    
  })
  
  output$infoEmissionsTotFrac <- renderValueBox({
    
    
    co2tot <- allData.dt[ VarName == "Emissions" & Element == "CO2" & ISO3 == input$selectCountry,  sum (V1 * GWPSets[[ input$selectGWPSet ]]$CO2, na.rm = T) ]
    n2otot <- allData.dt[  VarName == "Emissions"  & Element == "N2O" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]]$N2O, na.rm = T) ]
    ch4tot <- allData.dt[  VarName == "Emissions" & Element == "CH4" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]]$CH4, na.rm = T) ]
    #ch4tot <- emissionsDetail.dt[ Gas == "CH4" & ADM0_NAME == "Kenya", sum (value_raw  * 21) ]
    cat ("-Fraction Co2", co2tot, "N#", input$emInGWPNO2, "ch4", ch4tot, "\n")
    
    totEm <- (co2tot +  n2otot + ch4tot)
    
    # emission_allSectors <- allData.dt[ VarName == "Historial Emissions" & COUNTRY == "World" ,  sum (V1 * 1.0, na.rm = T) ]
    
    emission_allSectors <- IPCCTotals [[ input$selectGWPSet]]
    
    frac <-  totEm /emission_allSectors
    frac_final <- frac * 100
    
    if        (frac_final > 10) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy = 1)
    } else if (frac_final > 1) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.1) 
    } else if (frac_final > 0.1) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.01) 
    } else if (frac_final > 0.01) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.001) 
    } else if (frac_final > 0.001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.0001) 
    } else if (frac_final > 0.0001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.00001)
    } else if (frac_final > 0.00001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.000001)
    } else if (frac_final > 0.000001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.0000001)
    } else if (frac_final > 0.0000001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.00000001)
    } else if (frac_final > 0.00000001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.000000001)
    } else if (frac_final > 0.000000001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.0000000001)
    } else if (frac_final > 0.0000000001) { labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.00000000001)}

    # xx <- paste0(labelFunctEm(signif(frac_final, 2)),(" Percent"))
    
    if(sum(frac_final > 0)){
      xx <- paste0(labelFunctEm(signif(frac_final)),(" Percent"))    
      }else{
      xx <- paste0(labelFunctEm(signif(frac_final, 2)),(" Percent"))    
      }
                 
#here
    # xx <- paste0(signif( as.numeric(substr(num_format, 1, nchar(num_format)-1)), 2 ),
    #              substr(num_format, nchar(num_format), nchar(num_format)))
    
    ret <-  shinydashboard:: valueBox(xx,
                                      HTML(paste0("Of total global GHG emissions" )),  icon = NULL,
                                      color = "blue",width =2

    )
    return (ret)
    
  })
  output$infoEmissionsTotFracDirect <- renderValueBox({
    
    
    co2tot <- allData.dt[ VarName == "Emissions" & Element == "CO2" & ISO3 == input$selectCountry,  sum (V1 * GWPSets[[ input$selectGWPSet ]]$CO2, na.rm = T) ]
    n2otot <- allData.dt[  VarName == "Emissions"  & Element == "N2O" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]]$N2O, na.rm = T) ]
    ch4tot <- allData.dt[  VarName == "Emissions" & Element == "CH4" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]]$CH4, na.rm = T) ]
    #ch4tot <- emissionsDetail.dt[ Gas == "CH4" & ADM0_NAME == "Kenya", sum (value_raw  * 21) ]
    #cat ("Co2", co2tot, "N#", input$emInGWPNO2, "ch4", ch4tot, "\n")
    
    totEm <- (co2tot +  n2otot + ch4tot)

    # emission_allSectors <- allData.dt[ VarName == "Historial Emissions" & COUNTRY == "World" ,  sum (V1 * 1.0, na.rm = T) ]
    
    emission_allSectors <- IPCCTotals [[ input$selectGWPSet]]
    
    frac <-  totEm /emission_allSectors
    # xx <- paste0(labelFunctEm(signif(frac*100, 2)),(" Percent"))
    
    
    if(sum(frac_final > 0)){
      xx <- paste0(labelFunctEm(signif(frac_final*100)),(" Percent"))    
    }else{
      xx <- paste0(labelFunctEm(signif(frac_final*100, 2)),(" Percent"))    
    }
    
    ret <-  shinydashboard:: valueBox(xx,
                                      HTML(paste0("Total global GHG emissions" )),  icon = NULL,
                                      color = "blue", width =2
                                      
                                      #ret <- shinydashboard::valueBox(sprintf ("%.f %%", 100 * frac), "of total global GHG emissions"
                                      #                                ,  icon = NULL,
                                      #                                color = "blue"
    )
    return (ret)
    
  })
  
  output$infoEmissionsRank <- renderValueBox({
    
    # direct emissions
    
    co2tot <- allData.dt[isDirect== TRUE &  Element == "CO2" & ISO3 == input$selectCountry,  sum (V1 * GWPSets[[ input$selectGWPSet ]] $CO2, na.rm = T) ]
    n2otot <- allData.dt[isDirect== TRUE  & Element == "N2O" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]] $N2O, na.rm = T) ]
    ch4tot <- allData.dt[ isDirect== TRUE & Element == "CH4" & ISO3 == input$selectCountry, sum (V1  * GWPSets[[ input$selectGWPSet ]] $CH4, na.rm = T) ]
    #ch4tot <- emissionsDetail.dt[ Gas == "CH4" & ADM0_NAME == "Kenya", sum (value_raw  * 21) ]
    cat ("Co2", co2tot, "N#", input$emInGWPNO2, "ch4", ch4tot, "\n")
    
    totEm <- (co2tot +  n2otot + ch4tot)
    
    # num_format <- labelFunctEm(signif(totEm, 2))
    
    if(sum(totEm > 0)){
      num_format <- labelFunctEm(signif(totEm))
    }else{
      num_format <- labelFunctEm(signif(totEm, 2))
    }
    
    # xx <- paste0(signif( as.numeric(substr(num_format, 1, nchar(num_format)-1)), 2 ),
    #              substr(num_format, nchar(num_format), nchar(num_format)))
    
    ret <-  shinydashboard:: valueBox(num_format,
                                      HTML(paste0("Direct emissions [ CO",tags$sub("2"), "eq ]")),  icon = NULL,
                                      color = "blue",width =2) 
    
    return (ret)
    
  })
  
  
  output$infoIntensityMeat <- renderValueBox({
    selNodes <- "Animal"
    curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
    curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
    curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
    selNodes <- NULL
    selCommodity <- "Meat"
    selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
    bySource <- F
    selUnit <- "Protein"
    
    is.null(selNodes)
    
    xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O, "Protein" )

    totAn <-  xx[ ISO3 == input$selectCountry]$Intensity
    
    ret <-  shinydashboard:: valueBox(sprintf ("%.1f", totAn),
                                      HTML(paste0("kg CO", tags$sub("2"), "eq per kg meat protein" )),  icon = NULL,
                                      color = "blue",width = 2
    )
    
  })
  
  output$infoIntensityEggs <- renderValueBox({
    selNodes <- "Animal"
    curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
    curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
    curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
    selNodes <- NULL
    selCommodity <- "Eggs"
    selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
    bySource <- F
    selUnit <- "Protein"
    
    is.null(selNodes)
    xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O, "Protein" )
    
    #xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, selUnit = NULL )
    totAn <- xx[ ISO3== input$selectCountry]$Intensity
    
    ret <-  shinydashboard:: valueBox(sprintf ("%.1f", totAn),
                                      HTML(paste0("kg CO", tags$sub("2"), "eq per kg egg protein" )),  icon = NULL,
                                      
                                      color = "blue",width =2
    )
    
  })
  
  output$infoIntensityAll <- renderValueBox({
    selNodes <- "Animal"
    curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
    curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
    curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
    selNodes <- NULL
    selCommodity <- c("Eggs", "Meat", "Milk")
    selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
    bySource <- F
    selUnit <- "Protein"
    
    is.null(selNodes)
    xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O,  "Protein" )
    
    #xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, selUnit = NULL )
    totAn <- xx[ ISO3== input$selectCountry]$Intensity
    
    ret <-  shinydashboard:: valueBox(sprintf ("%.1f", totAn),
                                      HTML(paste0("kg CO", tags$sub("2"), "eq per kg animal protein" )),  icon = NULL,
                                      
                                      color = "blue",width =2
    )
  })
  
  
  output$infoIntensityMilk <- renderValueBox({
    
    selNodes <- "Animal"
    curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
    curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
    curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
    selNodes <- NULL
    selCommodity <- "Milk"
    selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
    bySource <- F
    selUnit <- "Protein"
    
    is.null(selNodes)
    xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O, "Protein" )
    
    #xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, selUnit = NULL )
    totAn <- xx[ ISO3== input$selectCountry]$Intensity
    
    
    if (length (totAn) ==0 ){
      totAn <- -1
    }
    
    ret <-  shinydashboard:: valueBox(sprintf ("%.1f", totAn),
                                      HTML(paste0("kg CO", tags$sub("2"), "eq per kg milk protein" )),  icon = NULL,
                                      color = "blue",width =2
                                      
    )
  })

  
  addPopover(session, "infoHerdSizeCTL", "Number of Cattle", content = 
               "Total number of Cattle. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  addPopover(session, "infoHerdSizeBUF", "Number of Buffalo", content = 
               "Total number of Buffalo. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  addPopover(session, "infoHerdSizeSHP", "Number of Sheep", content = 
               "Total number of Sheep. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  addPopover(session, "infoHerdSizeGTS", "Number of Goats", content = 
               "Total number of Goats. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  addPopover(session, "infoHerdSizeCHK", "Number of Chickens", content = 
               "Total number of Chickens. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  addPopover(session, "infoHerdSizePGS", "Number of Pigs", content = 
               "Total number of Pigs. Based on Gridded livestock of the world (GLW), 2015.", trigger = 'hover')
  
  
  
  addPopover(session, "infoProdMeatTotal", "Production of meat", content = 
               "Total production of meat from all animals (expressed in carcass weight), 2015.", trigger = 'hover')
  
  
  addPopover(session, "infoProdMilkTotal", "Production of milk", content = 
               "Total production of milk from ruminants, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoProdEggsTotal", "Production of eggs", content = 
               "Total production of eggs from Chickens, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoProdProteinTotal", "Production of animal protein", content = 
               "Total production of protein from all products, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoEmissionsTotFrac", "Percentage of all GHG emission", content = 
               "Percentage of total global antropogenic GHG emissions, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoEmissionsRank", "Direct emissions", content = 
               "Emissions directly occuring by raising of animals, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoEmissionsTotal", "Total GHG emissions", content = 
               "Total GHG emissions from the livestock sector, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoIntensityMeat", "Emission intensity of meat", content = 
               "Total GHG allocated to eggs divided by total meat production in  protein, 2015.", trigger = 'hover')
  
  
  addPopover(session, "infoIntensityMilk", "Emission intensity of milk", content = 
               "Total GHG allocated to milk divided by total milk production in protein, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoIntensityEggs", "Emission intensity of eggs", content = 
               "Total GHG allocated to eggs divided by total egg production in protein, 2015. ", trigger = 'hover')
  
  
  addPopover(session, "infoIntensityAll", "Emission intensity of animal protein", content = 
               "Total GHG divided by total animal protein production, 2015. ", trigger = 'hover')
  
  
  
  output$HiHFrameEmissions<- renderUI({
    
    my_test <- tags$iframe(src=iframeEmissionSRC, height=600, 
                           width="100%", frameborder = "no", scrolling = "auto" )
    #print(my_test)
    my_test
  })
  
  output$HiHFramePopulation<- renderUI({
    
    my_test <- tags$iframe(src=iframeHerdsSRC, height=600, 
                           width="100%", frameborder = "no", scrolling = "auto" )
    print(my_test)
    my_test
  })
  
  
  output$HiHFrameEmissionIntensity<- renderUI({
    
    my_test <- tags$iframe(src=iframeEmissionIntensitySRC, height=600, 
                           width="100%", frameborder = "no", scrolling = "auto" )
    print(my_test)
    my_test
  })
  
  output$HiHFrameProduction<- renderUI({
    
    my_test <- tags$iframe(src=iframeProductionSRC, height=600, 
                           width="100%", frameborder = "no", scrolling = "auto" )
    print(my_test)
    my_test
  })
  
  
  output$EmIntensityCommodity <- renderUI({
    if (input$emIntensityCommodityOrAnimal == "Commodities"){
      curCom <- unique (allData.dt[ VarName== "Production"& Animal %in% input$int_selAnimals ]$Item)
      checkboxGroupInput("select_Comm_emIntensity", "Commodity", choices = c( curCom), inline = T, selected= curCom)
      
    }
  })
  
  
  output$EmIntensityCommodityAsProtein <- renderUI({
    if (input$emIntensityCommodityOrAnimal == "Commodities"){
      checkboxInput("select_Comm_emIntensityAsProtein", "as protein", value= TRUE)
      
    }
  })
  
  
  output$EmIntensity_emSource <- renderUI({
    req (input$int_selAnimals)
    curCom <- unique (allData.dt[ VarName== "Emissions"& Animal %in% input$int_selAnimals ]$Item)
    cat (curCom, "\n")
    cat ("Emission sources", curCom, "\n")
    checkboxGroupInput("select_Comm_emSource", "Emission source", choiceValues = emSources2Choose, choiceNames =emSources2ChooseNames,inline = T, selected= curCom)
  })
  
  
  output$EmIntensityHerdType <- renderUI({
    curHerd <- unique (allData.dt[ Animal %in% input$int_selAnimals]$HerdType)
    checkboxGroupInput("select_Herd_emIntensity", "Herd", choices =  curHerd,selected = curHerd, inline = T)
  })
  
  output$EmIntensityLPS <- renderUI({
    curLPS <- sort (unique (allData.dt[ Animal %in% input$int_selAnimals]$LPS))
    checkboxGroupInput("select_LPS_emIntensity", "Production system", choices =  curLPS,selected = curLPS, inline = T)
  })

  # output$HerdType_comp <- renderUI({
  #   curHerd <- unique (allData.dt[ Animal %in% input$selectAnimals_comp]$HerdType)
  #   curHerd <- curHerd[which(curHerd%in% c("Chickens", "Pigs") ==F)]
  #   checkboxGroupInput("selectHerd_comp", "Herd type", choices =  curHerd, selected = curHerd, inline = T)
  # })
  
  # output$LPS_comp <- renderUI({
  #   if (("Beef" %in% input$selectHerd_comp) | ("Dairy" %in% input$selectHerd_comp)) {  
  #     curLPS <- sort (unique (allData.dt[ Animal %in% input$selectAnimals_comp]$LPS))
  #   } else { 
  #     animals_no_ruminants <- input$selectAnimals_comp[which(input$selectAnimals_comp %in% c("Chickens", "Pigs"))]
  #     curLPS <- sort (unique (allData.dt[ Animal %in% animals_no_ruminants]$LPS)) 
  #   } 
  #   checkboxGroupInput("selectLPS_comp", "Production system", choices =  curLPS, selected = curLPS, inline = T)
  # })
  
  output$DatatableIntensityAll <- DT::renderDataTable({
    if (input$dataAccess_UnitProtein == T){
      unitProtein = "Protein"
    }else{
      unitProtein = NULL
    }


    ret.dt <- getEmissionIntensityAll_2(input$Data_selAnimals, input$Data_select_Herd_emIntensity,  input$Data_select_LPS_emIntensity, input$Data_selNodes, input$select_Comm_emIntensity,
                                        input$Dataselect_Comm_emSource, input$Data_emIntensityBySource,GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O, unitProtein)
    ret.dt <-  ret.dt[ COUNTRY== input$selectCountry]
    ret.dt[ ,ISO3 := NULL]
    if ("Animal" %in% names (ret.dt)){
      setcolorder(ret.dt, c( "COUNTRY"	,"Animal",	"EmissionSource",	"Production", "Emissions",	"Intensity"))

    }else{
      setcolorder(ret.dt, c( "COUNTRY"	,	"EmissionSource",	"Production", "Emissions",	"Intensity"))

    }



    setnames (ret.dt, "EmissionSource", "Emission Source")
    setnames (ret.dt, "COUNTRY", "Region/Country")





    if (input$select_Comm_emIntensity == "per animal"){
      ret <-  DT::datatable(ret.dt,  caption = "Emissions, animal numbers, and emissions per animal" ,
                            extensions=c("Buttons",'Scroller'),escape = F,
                            colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Herd size [ heads ]" = "Production", "Emission per head [ kg CO2eq / head ]"= "Intensity"),
                            options = list(dom = 'Bfrtip',
                                           scrollY = 500,
                                           scroller = TRUE,
                                           scrollX=TRUE,


                                           buttons = c('copy', 'csv', 'excel','pdf')

                            )

      )

      ret <-  DT::formatRound(ret, columns = c("Herd size [ heads ]","Emissions [ t CO2eq ]"), digits = 0, interval= 3, mark= ",")
      ret <-  DT::formatRound(ret, columns = c("Emission per head [ kg CO2eq / head ]"), digits = 2, interval= 3, mark= ",")
      #ret <- DT::datatable(head(ret), colnames = c('Emissions' = 'Sepal.Width'))

      #   #%>% DT::formatRound(columns = c("Emissions","Head", "Emissions per Animal"), digits = 2, interval= 3, mark= ",")
    }else{
      ret <-  DT::datatable(ret.dt,  caption = "Emissions, animal numbers, and and emissions per unit of product" ,
                            extensions=c("Buttons",'Scroller'),escape = F,
                            colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                            options = list(dom = 'Bfrtip',
                                           scrollY = 500,
                                           scroller = TRUE,
                                           scrollX=TRUE,

                                           buttons =
                                             list('copy', 'print', list(
                                               extend = 'collection',
                                               buttons = list(
                                                 list(extend = 'csv', filename = paste0("GLEAM3Results", Sys.Date())   ) ,
                                                 list(extend = 'excel', filename = paste0("GLEAM3Results", Sys.Date())   ) ,
                                                 list(extend = 'pdf', filename = paste0("GLEAM3Results", Sys.Date())   ) ),
                                               text = 'Download'
                                             ))
                            )
      )

      ret <-  DT::formatRound(ret, columns = c("Production [ t ]","Emissions [ t CO2eq ]"), digits = 0, interval= 3, mark= ",")
      ret <-  DT::formatRound(ret, columns = c("Emission Intensity [ kg CO2eq / kg ]"), digits = 2, interval= 3, mark= ",")
    }
    ret


  })
  
  output$DatatableIntensityAll_2 <- DT::renderDataTable({
    
    selCountry <- input$selectCountry
    selAnimals <- input$int_selAnimals
    selHerds <- input$select_Herd_emIntensity
    selLPS <- input$select_LPS_emIntensity
    if (input$emIntensityCommodityOrAnimal == "Commodities"){
      selCommodity <- input$select_Comm_emIntensity
    }else{
      selCommodity <- "per animal"
    }
    selNodes <- input$EMIntensity_Nodes
    bySource <- input$emIntensityBySource
    selSources <- input$select_Comm_emSource
    cat (selCountry, ":", selAnimals, "\n")
    cat (selHerds, ":", selLPS, "\n")
    cat ("Commodity:", selCommodity, ":", selNodes, "\n")
    cat (bySource,":",bySource, ":\n")
    
    
    
    #unitProtein <- NULL
    if (input$select_Comm_emIntensityAsProtein == T){
      selUnits = "Protein"
    }else{
      selUnits <- c("Weight", "CarcassWeight")
    }
    
    
    #ret.dt <- getEmissionIntensityAll_2(input$Data_selAnimals, input$Data_select_Herd_emIntensity,  input$Data_select_LPS_emIntensity, input$Data_selNodes, input$select_Comm_emIntensity, 
    #                                    input$Dataselect_Comm_emSource, input$Data_emIntensityBySource,GWPSets[[ input$selectGWPSet]]$CH4,GWPSets[[ input$selectGWPSet]]$N2O, selUnits )
    
    GWPCH4 <- GWPSets[[ input$selectGWPSet]]$CH4
    GWPN2O <- GWPSets[[ input$selectGWPSet]]$N2O
    
    
    ret.dt <- getEmissionIntensityAll_2(selAnimals, selHerds,  selLPS, selNodes, selCommodity, 
                                        selSources, bySource,  GWPCH4,
                                        GWPN2O,selUnits)
    
    ret.dt <-  ret.dt[ ISO3  == input$selectCountry]
    cat(input$selectCountry, "\n")
    ret.dt[ ,ISO3 := NULL]
    if ("Animal" %in% names (ret.dt)){
      setcolorder(ret.dt, c( "COUNTRY"	,"Animal",	"EmissionSource",	"Production", "Emissions",	"Intensity"))
      
    }else{
      setcolorder(ret.dt, c( "COUNTRY"	,	"EmissionSource",	"Production", "Emissions",	"Intensity"))
      
    }
    
    
    ret.dt[EmissionSource == "Feed-CO2", EmissionSource:= "Feed (CO2)"]
    ret.dt[EmissionSource == "LandUseChange", EmissionSource:= "LUC: soy and palm (CO2)"]
    ret.dt[EmissionSource == "DirectOnFarmEnergy", EmissionSource:= "Direct on-farm energy (CO2)"]
    ret.dt[EmissionSource == "PastureExpansion", EmissionSource:= "LUC: pasture expansion (CO2)"]
    ret.dt[EmissionSource == "EmbeddedOnFarmEnergy", EmissionSource:= "Embedded on-farm energy (CO2)"]
    ret.dt[EmissionSource == "Postfarm", EmissionSource:= "Post-farm (CO2)"]
    ret.dt[EmissionSource == "Feed-CH4", EmissionSource:= "Feed (CH4)"]
    ret.dt[EmissionSource == "Manure-CH4", EmissionSource:= "Manure (CH4)"]
    ret.dt[EmissionSource == "EntericFermentation", EmissionSource:= "Enteric fermentation (CH4)"]
    ret.dt[EmissionSource == "Feed-N2O", EmissionSource:= "Feed (N2O)"]
    ret.dt[EmissionSource == "Manure-N2O", EmissionSource:= "Manure (N2O)"]
    
    setnames (ret.dt, "EmissionSource", "Emission Source")                       
    setnames (ret.dt, "COUNTRY", "Area")                       
    
    
    
    if (setequal (selUnits,  "Protein")){
      captionW <- "Emissions, animal numbers, and emissions per unit of protein"
    }else{
      captionW <- "Emissions, animal numbers, and emissions per unit of product"
    }
    
    
    
    if (setequal (selCommodity,  "per animal")){
      ret <-  DT::datatable(ret.dt,  caption = "Emissions, animal numbers, and emissions per animal" ,
                            extensions=c("Buttons",'Scroller'),escape = F,
                            colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Herd size [ heads ]" = "Production", "Emission per head [ kg CO2eq / head ]"= "Intensity"),
                            options = list(dom = 'Bfrtip',
                                           scrollY = 500,
                                           scroller = TRUE,
                                           scrollX=TRUE,
                                           
                                           
                                           buttons = c('copy', 'csv', 'excel','pdf')
                                           
                            )
                            
      )
      
      ret <-  DT::formatRound(ret, columns = c("Herd size [ heads ]","Emissions [ t CO2eq ]"), digits = 0, interval= 3, mark= ",")
      ret <-  DT::formatRound(ret, columns = c("Emission per head [ kg CO2eq / head ]"), digits = 2, interval= 3, mark= ",")
      #ret <- DT::datatable(head(ret), colnames = c('Emissions' = 'Sepal.Width'))
      
      #   #%>% DT::formatRound(columns = c("Emissions","Head", "Emissions per Animal"), digits = 2, interval= 3, mark= ",")
    }else{
      ret <-  DT::datatable(ret.dt,  caption = captionW ,
                            extensions=c("Buttons",'Scroller'),escape = F,
                            colnames = c("Emissions [ t CO2eq ]" = "Emissions", "Production [ t ]" = "Production", "Emission Intensity [ kg CO2eq / kg ]"= "Intensity"),
                            options = list(dom = 'Bfrtip',
                                           scrollY = 500,
                                           scroller = TRUE,
                                           scrollX=TRUE,
                                           
                                           buttons =
                                             list('copy', 'print', list(
                                               extend = 'collection',
                                               buttons = list(
                                                 list(extend = 'csv', filename = paste0("GLEAM3Results", Sys.Date())   ) ,
                                                 list(extend = 'excel', filename = paste0("GLEAM3Results", Sys.Date())   ) ,
                                                 list(extend = 'pdf', filename = paste0("GLEAM3Results", Sys.Date())   )) ,
                                                 
                                                # list(extend = 'excel', filename = "GLEAMv3Results"),
                                                # list(extend = 'pdf', filename = "GLEAMv3Results")),
                                               text = 'Download'
                                             ))
                            )
      )
      
      ret <-  DT::formatRound(ret, columns = c("Production [ t ]","Emissions [ t CO2eq ]"), digits = 0, interval= 3, mark= ",")
      ret <-  DT::formatRound(ret, columns = c("Emission Intensity [ kg CO2eq / kg ]"), digits = 2, interval= 3, mark= ",")
      
    }

    
    
    ret 
  },server = FALSE)
  
  
  
  output$emIntensityBar <- renderPlotly({
    req(input$selectCountry) 
    req(input$int_selAnimals)
    req(input$select_Herd_emIntensity)
    req(input$select_LPS_emIntensity)
    req(input$emIntensityCommodityOrAnimal)
    req(input$EMIntensity_Nodes)

    # 
    # cat ("emIntensityBar\n")

    selCountry <- input$selectCountry
    selAnimals <- input$int_selAnimals
    selHerds <- input$select_Herd_emIntensity
    selLPS <- input$select_LPS_emIntensity
    if (input$emIntensityCommodityOrAnimal == "Commodities"){
      selCommodity <- input$select_Comm_emIntensity
    }else{
      selCommodity <- "per animal"
    }
    selNodes <- input$EMIntensity_Nodes
    bySource <- input$emIntensityBySource
    selSources <- input$select_Comm_emSource
    cat (selCountry, ":", selAnimals, "\n")
    cat (selHerds, ":", selLPS, "\n")
    cat ("Commodity:", selCommodity, ":", selNodes, "\n")
    cat (bySource,":",bySource, ":\n")
    
    #unitProtein <- NULL
    if (input$select_Comm_emIntensityAsProtein == T){
      selUnits = "Protein"
    }else{
      selUnits <- c("Weight", "CarcassWeight")
    }
    
    GWPCH4 <- GWPSets[[input$selectGWPSet ]]$CH4
    GWPN2O <- GWPSets[[input$selectGWPSet ]]$N2O
    
    
    gp <-  plot_emissionIntensityBoxplotPlotly_2   (
      selCountry, # this is not the ISO3 code
      selAnimals , 
      selHerds  ,
      selLPS  ,
      selCommodity,  
      selNodes  ,
      bySource  ,
      selSources,
      GWPCH4,
      GWPN2O,
      selUnits)
    
    return (gp)
    
  })
  

  output$SankeyNetwork3D_ProductionAll <- renderSankeyNetwork({
    nodes2show <- sort (input$prod_selSankeyNodes)
    selAnimals <-  sort (input$prod_selAnimals)
    selCommodities <- sort (input$prod_selCommodities)
    nodes2show <- c( nodes2show)
    cat ("UNIT", input$prod_selCommodityUnit,"\n")
    p <- makeSankeyDataProduction(nodes2show, selAnimals, input$selectCountry, selCommodities, input$prod_selCommodityUnit)
    p
  })
  

  output$animalNumberAll <- renderPlotly({
    #data2plot <- allData.dt[ VarName == "AnimalNumbers" &  COUNTRY == input$selectCountry  &  Animal %in%  input$inSelectAnimalsMonogastrics  ]
    fig <- plotBarChartAnimalNumbers (input$selectCountry,input$pop_SelectAnimals,input$pop_SelectHerdType ) 
    
    # fig <- plotBarChartAnimalNumbers(data2plot)
    return (fig)
  })
  
  
  output$ProductionBarChartAll <- renderPlotly({
    fig <- plotBarChartProduction_2(input$selectCountry, input$prod_selAnimals,input$prod_selCommoditiesBar)
  })
  
  
  
### FIGURES IN COMPARISON TOOL ###  
  
  output$AnimalPopulations_Bars_comp <- renderPlotly({
    fig <- plot_AnimalPopulations_comp(input$selectCountry_comp, input$selectAnimals_comp, input$selectHerd_comp, input$selectLPS_comp1, input$selectLPS_comp2, input$selectLPS_comp3) 
  })
  
  output$AnimalProducts_Bars_comp <- renderPlotly({
    fig <- plot_AnimalProducts_comp(input$selectCountry_comp, input$selectAnimals_comp, input$selectHerd_comp, input$selectLPS_comp1, input$selectLPS_comp2, input$selectLPS_comp3, 
                                    input$prod_products_comp, input$prod_unit_comp, input$prod_node_comp)
  })
  
  output$Emissions_Bars_comp <- renderPlotly({
    fig <- plot_Emissions_comp(input$selectCountry_comp, input$selectAnimals_comp, input$selectHerd_comp, input$selectLPS_comp1, input$selectLPS_comp2, input$selectLPS_comp3, 
                               input$em_products_comp, input$em_ghg_comp, input$em_sources_CO2_comp, input$em_sources_CH4_comp, 
                               input$em_sources_N2O_comp, input$em_node_comp, GWPSets[[input$selectGWPSet]]$N2O, GWPSets[[input$selectGWPSet]]$CH4)
  })
  
  output$EmissionIntensities_Bars_comp <- renderPlotly({
    fig <- plot_EmissionIntensities_comp(input$selectCountry_comp, input$selectAnimals_comp, input$selectHerd_comp, input$selectLPS_comp1, input$selectLPS_comp2, input$selectLPS_comp3, 
                                         input$emint_products_comp, input$emint_ghg_comp, input$emint_sources_CO2_comp, input$emint_sources_CH4_comp, 
                                         input$emint_sources_N2O_comp, input$emint_unit_comp, input$emint_node_comp, GWPSets[[input$selectGWPSet]]$N2O, GWPSets[[input$selectGWPSet]]$CH4)
  })
  
  
  
  
  
  output$emissionBarChart <- renderPlotly({
    emissionsCntry.dt <- emissions.dt[ ADM0_NAME == input$selectCountry, sum (value, na.rm = T), by = .(Animal,ProdSys )]
    if (input$inCheckDirectOnly == T){
      emissionsCntry.dt <- emissions.dt[ isDirect == T & ADM0_NAME == input$selectCountry, sum (value, na.rm = T), by = .(Animal,ProdSys )]
    }
    #
    emissionsCntry.dt[, label:= paste (Animal, ", ", ProdSys, ":", sprintf ("%.3f", V1/1e9), "Mt CO2eq")]
    
    data2plot <- emissionsCntry.dt[ !(is.na (V1 )) & V1>0,]
    # TODO: Do this in global.R
    #setnames (data2plot, "ProdSys", "ProdSys")
    anim <- subsetAnimalsFromInput (input$inAnimalSelect)
    data2plot <- data2plot[ Animal %in% anim]
    data2plot
    plotBarChartGeneric(data2plot, "CO<sub>2eq</sup>")
  })
  
  output$treeMapEmissions <- renderPlot({
    isDirect = input$inCheckDirectOnly
    input$emInGWPNO2
    input$emInGWPCH4
    em.dt <- allData.dt[ VarName == "Emissions" & ISO3 == input$selectCountry & Animal %in% input$em_SelAnimalsAll]
    em.dt[ Element == "CO2", GWP:= GWPSets[[ input$selectGWPSet ]]$CO2]
    em.dt[ Element == "N2O", GWP:=  GWPSets[[ input$selectGWPSet ]]$N2O]
    em.dt[ Element == "CH4", GWP:= GWPSets[[ input$selectGWPSet ]]$CH4]
    
    selNodes <- input$em_selectTreeMapNodes
    cat ("Teemap", selNodes, "\n")
    #selNodes <- c("Animal", "LPS", "Item")
    #selNodes <- c("Item")
    if (input$inCheckDirectOnly == T){
      data2plot <- em.dt[ isDirect ==T , sum (V1* GWP),by= selNodes]
      
    }else
    {
      data2plot <- em.dt[  , sum (V1* GWP),by= selNodes] 
      
    }

    fig <-  plotTreeMapEmissions_2(data2plot,  selNodes )
    
    return (fig)
    
  })
  
  output$SankeyEmissionAllSpecies <- renderSankeyNetwork({
    #Animal %in% input$inSelectRuminantsEmissions
    # nodes2show <- c("Animal", input$em_selectSankeyNodes)
    nodes2show <- input$em_selectSankeyNodesAll # <- c("Animal",  "HerdType", "ProdSys",  "Source","Gas") 
    selAnimals <-  input$em_SelAnimalsAll
    isDirect = input$inCheckDirectOnly
    cat (selAnimals,"DIREDCT", isDirect, "ISO3", input$selectCountry, "\n")
    nodes2show <- c("Animal", nodes2show)
    
    p <- makeSankeyDataEmissionDetails_2(nodes2show, selAnimals, input$selectCountry, isDirect,GWPSets[[ input$selectGWPSet ]]$N2O, GWPSets[[ input$selectGWPSet ]]$CH4 )
    p
    
  }) 
  
  output$selRuminantHerdType <- renderUI({
    list( checkboxGroupInput("axisx", "",choices = c("Beef","Dairy","All"), selected = "All", inline  = T) )
  })

  output$selRuminantProdSys <- renderUI({
    list( checkboxGroupInput("axisasd", "",choices = ruminants, selected= ruminants, inline = T) )
  })

  output$emissionPieChartAll <- renderPlotly({
    isDirect = input$inCheckDirectOnly
    fig <- plotyPieChart(input$selectCountry ,  input$em_SelAnimalsAll, 
                         isDirect,GWPSets [[ input$selectGWPSet]]$N2O , GWPSets [[ input$selectGWPSet]]$CH4  )
    return (fig)

  })
  

  observeEvent(input$selectCountry, {
    # print(input$selectCountry_comp)
    # print(input$selectAnimals_comp)
    # print(input$selectHerd_comp)
    # print(input$selectLPS_comp1)
    # print(input$selectLPS_comp2)
    # print(input$selectLPS_comp3)
    # print(input$em_products_comp)
    # print(input$em_ghg_comp)
    # print(input$em_sources_CO2_comp)
    # print(input$em_sources_CH4_comp)
    # print(input$em_sources_N2O_comp)
    # print(input$em_node_comp)
    # print(GWPSets[[input$selectGWPSet]]$N2O)
    # print(GWPSets[[input$selectGWPSet]]$CH4)
    print(input$selectCountry)
    print(input$prod_selAnimals)
    print(input$prod_selCommoditiesBar)
  })
  
  
}

