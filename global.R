rm (list = ls ())
library (rgdal) # needed at shinyapps.io !!!!!!
library (sp)
library(data.table)
library (pryr) 
library (scales)
library (tinytex) # for the pdf reports 
library (shiny)
library (plotly)
#library (gridBase) #???  
library (RColorBrewer)
library(rmarkdown)
library(openxlsx)
library(rhandsontable)
library (flexdashboard)
library (shinydashboard)
library (shinydashboardPlus)
#library (leaflet)
#library (leaflet.opacity)
#library (mapview)
#library(lobstr)
library(knitr)
library (pander)
library(shinyLP)
library(shinyBS)
#library(sankeywheel)
library(networkD3)
library (colorspace)
library (htmlwidgets)
#library(shinyAmBarCharts)
library(shinyjs)
library(esquisse)
library (treemapify) # to make treemaps with ggplot2
library (shinycssloaders) # for the spinning wheel. 
library (webshot) # to export the sankey diagram
library(dplyr)
options(warn=2) # turn warnings into errors
# reads all input data and loads libraries for the shiny application #############
mem_used()
# animalNumbers

# cut_short_scale <- function (space = FALSE) 
# {
#   out <- c(0, K = 1000, M = 1e+06, B = 1e+09, T = 1e+12)
#   if (space) {
#     names(out) <- paste0(" ", names(out))
#   }
#   out
# }


cut_short_scale2 <- function (space = TRUE)
{
  out <- c(t = 0, Kt = 1000, Mt = 1e+06, Gt = 1e+09, T = 1e+12)
  if (space) {
    names(out) <- paste0(" ", names(out))
  }
  out
}


labelFunct <- label_number(scale_cut = cut_short_scale(space = TRUE), accuracy =  0.01)
# labelFunctProd <- label_number(scale_cut = cut_short_scale(), accuracy =  0.01)
labelFunctProd <- label_number(scale_cut = cut_short_scale2(), accuracy =  0.01)
# labelFunctEm <- label_number(scale_cut = cut_short_scale(), accuracy =  0.01)
labelFunctEm <- label_number(scale_cut = cut_short_scale2(), accuracy =  0.1)


cat ("Starting to process global.R\n")

isPublicVersion  <- F


# rootDir <- "/Users/dwisser/DocumentsDW/FAO_GLEAM/GLEAMv3Dashboard"
# rootDir <- getwd()
# dataDir <- file.path (rootDir, "data")
dashboardData <- file.path ("dashboardData") # only the data that will be uploaded when deployed !

# read the table data on startup here ################################################# 
# gitHubImageDir <- "https://raw.githubusercontent.com/dommens/GleamPublicData/master/"
# gitHubImageDir <- "https://github.com/GleamDashboardTeam/GleamDashboard/blob/images/"

iframeEmissionSRC <- "https://data.apps.fao.org/gleam/?hideFeedback=1&hideStory=1&noBanner=1&share=f-ca73f1a3-6309-4e72-8042-77b546b9b7eb"
iframeProductionSRC <-"https://data.apps.fao.org/gleam/?hideFeedback=1&hideStory=1&noBanner=1&share=f-06260fa5-952a-44be-8e7b-bf834fac1d9b"
iframeHerdsSRC <- "https://data.apps.fao.org/gleam/?hideFeedback=1&hideStory=1&noBanner=1&share=f-e502b466-3f62-46d1-b38a-531fd7a5a697&hideStory=1"
iframeEmissionIntensitySRC <-"https://data.apps.fao.org/gleam/?hideFeedback=1&hideStory=1&noBanner=1&share=f-70bbc195-af31-479b-a9c1-40b5b4fffaa8"

GWPSets <-  list()
GWPSets [[ "AR6"]]  = list (CH4 = 27, CO2 = 1, N2O = 273)
GWPSets [[ "AR5 incl"]]  = list (CH4 = 34, CO2 = 1, N2O = 298)
GWPSets [[ "AR5 excl"]]  = list (CH4 = 28, CO2 = 1, N2O = 265)
GWPSets [[ "SAR"]]  = list (CH4 = 21, CO2 = 1, N2O = 310)
GWPSets [[ "AR4"]] = list (CH4 = 25, CO2 = 1, N2O = 298)
names (GWPSets)

# IPCCTotals <- list ( )
# IPCCTotals [["AR6"]] <- 55.5 * 1e9
# IPCCTotals [["AR5 incl"]] <- 54.68 * 1e9
# IPCCTotals [["AR5 excl"]] <- 57.23 * 1e9
# IPCCTotals [["AR4"]] <- 53.9 * 1e9
# IPCCTotals [["SAR"]] <- 52.55 * 1e9

IPCCTotals <- list ( )
IPCCTotals [["AR6"]] <- 51.01 * 1e9
IPCCTotals [["AR5 incl"]] <- 53.76 * 1e9
IPCCTotals [["AR5 excl"]] <- 51.28 * 1e9
IPCCTotals [["AR4"]] <- 50.56 * 1e9
IPCCTotals [["SAR"]] <- 49.27 * 1e9


GWPChoices <- c("AR6 (CH4=27, N2O=273)" = "AR6",
                "AR5 incl(CH4=34, N2O=298)" = "AR5 incl",
                "AR5 excl(CH4=28, N2O=265)" = "AR5 excl",
                "AR4 (CH4=25, N2O=298)" = "AR4",
                "SAR (CH4=21, N2O=310)" = "SAR")


gleam4AnimalNames <- list ( "CTL" = "Cattle", "BFL" = "Buffalo", "SHP" = "Sheep", "GTS" = "Goats", "PGS"= "Pigs", "CHK"= "Chickens")
gleam4LPS <- list ( "GRS" = "Grassland", "MXD" = "Mixed", "LYR" = "Layer", "BRL" = "Broiler", "BCK"= "Backyard", "MED"= "Intermediate","IND"= "Industrial")
gleam4Herds <- list ( "DRY" = "Dairy", "BEF" = "Beef",  "PGS"= "Pigs", "CHK"= "Chickens")



# makeDownloadTable <- function (varName, selAnimals  ){
#   # varName <- "EmissionsIntensity"
#   #selAnimals <- Monogastrics
#   #if (varName == "AnimalNumbers" ){ 
#   if (varName == "EmissionsIntensity"){
#     data2show <- getEmissionIntensityAll  (  "Production" , selAnimals,  unique (allData.dt[ VarName == "Production"]$HerdType),  unique (allData.dt[ VarName == "Production"]$LPS),  c("Meat", "Milk", "Eggs"),
#                                              unique (allData.dt[ VarName == "Emissions"]$Item),c("Weight", "CarcassWeight"), c("Animal","HerdType", "Commodity", "LPS"))
#     
#     data2show <- merge (gaulList.dt[, .(COUNTRY, ISO3) ], data2show,by= "COUNTRY", all.y = T )
#     tabUnits <- "Tonnes"
#     tabCaption <- paste("Aggregated values for regions and countries, ", tabUnits)
#     ret <- DT::datatable(data2show,  caption = tabCaption,
#                          extensions=c("Buttons",'Scroller'),
#                          options = list(dom = 'Bfrtip',
#                                         scrollY = 500,
#                                         scroller = TRUE,
#                                         scrollX=TRUE,
#                                         
#                                         buttons = c('copy', 'csv', 'excel','pdf')
#                          )
#     )%>% DT::formatRound(columns = c("Emission", "Production","Intensity" ), digits = 2, interval= 3, mark= ",")
#     return (ret)
#     
#     
#     
#     
#   }
#   
#   if (varName == "AnimalNumbers"){
#     
#     data2show <- allData.dt[VarName %in% varName & Animal %in% selAnimals  ,.( COUNTRY, ISO3,  Animal,HerdType, LPS,  V1 )]
#     setnames (data2show, "V1", "heads")
#     setkey(data2show, "COUNTRY", "Animal")
#     tabCaption <- paste("Aggregated values for regions and countries, ", "heads")
#     ret <- DT::datatable(data2show,  caption = tabCaption,
#                          extensions=c("Buttons",'Scroller'),
#                          options = list(dom = 'Bfrtip',
#                                         scrollY = 500,
#                                         scroller = TRUE,
#                                         scrollX=TRUE,
#                                         
#                                         buttons = c('copy', 'csv', 'excel','pdf')
#                          )
#     )%>% DT::formatRound(columns = c("heads" ), digits = 2, interval= 3, mark= ",")
#     
#     return (ret)
#   }
#   if (varName == "Production"){
#     
#     data2show <- allData.dt[VarName %in% varName & Animal %in% selAnimals  ,.( COUNTRY, ISO3,  Animal,HerdType, LPS, Item, Element, V1 )]
#     setnames (data2show, "V1", "tonnes")
#     #setnames (data2show, "V1", "heads")
#     setnames (data2show, "Element", "units")
#     
#     setkey(data2show, "COUNTRY", "Animal")
#     tabCaption <- paste("Aggregated values for regions and countries, ", "tonnes")
#     ret <- DT::datatable(data2show,  caption = tabCaption,
#                          extensions=c("Buttons",'Scroller'),
#                          options = list(dom = 'Bfrtip',
#                                         scrollY = 500,
#                                         scroller = TRUE,
#                                         scrollX=TRUE,
#                                         
#                                         buttons = c('copy', 'csv', 'excel','pdf')
#                          )
#     )%>% DT::formatRound(columns = c("tonnes" ), digits = 2, interval= 3, mark= ",")
#     
#     return (ret)
#   }
#   if (varName == "Emissions"){
#     
#     data2show <- allData.dt[VarName %in% varName & Animal %in% selAnimals  ,.( COUNTRY, ISO3,  Animal, HerdType,LPS, Item, Element, Element2,GWP,isDirect, V1 )]
#     data2show[, CO2eq:= V1 * GWP] 
#     setnames (data2show, "V1", "emissions")
#     setnames (data2show, "Element", "gas")
#     setnames (data2show, "Element2", "commodity")
#     setnames (data2show, "Item", "source")
#     #setnames (data2show, "V1", "heads")
#     setnames (data2show, "isDirect", "direct emissions")
#   
#   
#     
#     setkey(data2show, "COUNTRY", "Animal")
#     tabCaption <- paste("Aggregated values for regions and countries, ", "tonnes")
#     ret <- DT::datatable(data2show,  caption = tabCaption,
#                          extensions=c("Buttons",'Scroller'),
#                          options = list(dom = 'Bfrtip',
#                                         scrollY = 500,
#                                         scroller = TRUE,
#                                         scrollX=TRUE,
#                                         
#                                         buttons = c('copy', 'csv', 'excel','pdf')
#                          )
#     )%>% DT::formatRound(columns = c("CO2eq", "emissions"), digits = 2, interval= 3, mark= ",")
#     
#     return (ret)
#   }
  
  
  #  # data2show <- data2show[, head (.SD,1 ), by= c("COUNTRY","Animal", "LPS", "HerdType") ] # only first row per group
  # 
  #   #names (data2show) <- c ("Country/Region", "ISO3", "Animal", "Production System","Herd Type", "Value")
  #  if (varName == "AnimalNumbers"){tabUnits <-  "head"}
  #   if (varName == "Production"){tabUnits <-"tonnes of product"}
  #   if (varName == "Emissions"){tabUnits <-"tonnes of Gas"}
  #   
  #   tabCaption <- paste("Aggregated values for regions and countries, ", tabUnits)
  #      ret <- DT::datatable(data2show,  caption = tabCaption,
  #                 extensions=c("Buttons",'Scroller'),
  #                 options = list(dom = 'Bfrtip',
  #                                scrollY = 500,
  #                                scroller = TRUE,
  #                                scrollX=TRUE,
  #                                
  #                                buttons = c('copy', 'csv', 'excel','pdf')
  #                 )
  #   )%>% DT::formatRound(columns = c("V1" ), digits = 2, interval= 3, mark= ",")
  #   return (ret)
  #   
  # }
  

 



# makeValueBox <- function (boxValue, unitsStr){
#  
#   shinydashboard:: valueBox(sprintf ("%.1f ", 1223.4990),
#                           tags$div(
#                             HTML(paste("CO", tags$sub(2), sep = "")), 
#                             HTML(paste("eq per kg milk"))
#                           ), 
#                           
#                           icon = icon ("fa-wine-bottle", lib = "font-awesome"),
#                           color = "blue"
# )
# }


infoTextHiHMaps <- tags$div("Spatial data are shown using FAO's geospatial data platfrom ", tags$a(href = 'http://www.fao.org/hih-geospatial-platform/en/', "geospatial platform"), 
                            "that is part of the", tags$a(href = 'http://www.fao.org/hih-geospatial-platform/en/', "Hand-in-Hand Initiative."), "For detailed information on the features of the app, please refer to the documentation on the HiH website."
                            )




# colors  
gleamColorAndText.file <- file.path (dashboardData, "GLEAMColorsAndDashboardTexts.xlsx")
gleamColorAnimals.dt <- data.table (openxlsx::read.xlsx(gleamColorAndText.file, sheet = "Colors", cols= 1:3))
gleamColorAnimals.dt$Color = trimws (gleamColorAnimals.dt$Color)
setnames (gleamColorAnimals.dt, "ProdSystem", "LPS")
gleamColorCommodity.dt  <- data.table (openxlsx::read.xlsx
                                       (gleamColorAndText.file,sheet= "Colors", cols=6:7))

gleamColorEmissions.dt  <- data.table (openxlsx::read.xlsx
                                       (gleamColorAndText.file, sheet = "Colors", cols=9:11))

gleamColorHerdType.dt  <- data.table (openxlsx::read.xlsx
                                       (gleamColorAndText.file, sheet = "Colors", cols=13:15))

gleamColorGas.dt  <- data.table (openxlsx::read.xlsx
                                      (gleamColorAndText.file, sheet = "Colors", cols=10:11))


# 
# cat ("Colors\n")
#xxx <- data.table (Animal = "Pigs", ProdSys = "Industrial", Color = "#c0421b" )
#gleamColorAnimals.dt <- rbindlist (list (gleamColorAnimals.dt, xxx), use.names = T)

emissionSankeyNodes <- c("Animal", "HerdType", "LPS", "Commodity", "EmissionSource", "Gas")


makeSankeyColorsAllEmissionTypes  <- function (){
  #cat ("Finding color for " ,nodeN, "\n")
  #return ("red")
  animalColors <- gleamColorAnimals.dt[, tail(.SD,1), Animal][, .(Animal, Color)]
  #herdTypeColors <- gleamColorAnimals.dt[, tail(.SD,1), Animal]
  
  data2plot.dt <-  allData.dt[  COUNTRY == "World" & VarName == "Emissions"]
  data2plot.dt$Element
   
  allNodes <- names (data2plot.dt)
  allNodes <- allNodes[ allNodes %in% c("Animal", "HerdType", "LPS", "Item", "Element", "Element2") ]
  
  nodes2plot <- character ()  
  for (curNode in allNodes){
    # cat (unique (curData.dt[, get(curNode)]))
    nodes2plot <- c(nodes2plot, unique (data2plot.dt[, get(curNode)]) )
  }
  #sort (nodes2plot)
  nodeColors.dt <- data.table (nodeName  = nodes2plot)
  nodeColors.dt[ nodeName %in% animalColors$Animal, color:=animalColors [match (nodeName,animalColors$Animal)]$Color ]
  #nodeColors.dt[ nodeName %in% gasColors$Gas, color:=gasColors [match (nodeName,gasColors$Gas)]$Color ]
  nodeColors.dt[ nodeName %in% gleamColorCommodity.dt$Commodity, color:=gleamColorCommodity.dt [match (nodeName,gleamColorCommodity.dt$Commodity)]$Color ]
  nodeColors.dt[ nodeName %in% gleamColorAnimals.dt$LPS, color:=gleamColorAnimals.dt [match (nodeName,gleamColorAnimals.dt$LPS)]$Color ]
  nodeColors.dt[ nodeName %in% gleamColorHerdType.dt$HerdType, color:=gleamColorHerdType.dt [match (nodeName,gleamColorHerdType.dt$HerdType)]$Color ]
  
   emSourcesGas <- unique (data2plot.dt[, .(Item, Element)])
   #gg <- "CH4"
   sourcNodes <- data.table ()
   for (gg in unique (emSourcesGas$Element)){
     nodeColors.dt[ nodeName == gg, color:=gleamColorGas.dt[ Gas == gg]$Color] 
     selNodes <- emSourcesGas[ Element == gg]
     nodeColors.dt[ nodeName == selNodes[1]$Item , color:= gleamColorGas.dt[ Gas == gg]$Color ]
     for (i in 2:nrow (selNodes)){
       curC <-  lighten (col = gleamColorGas.dt[ Gas == gg]$Color, amount = (i -1)*.2 )
       nodeColors.dt[ nodeName == selNodes[i]$Item , color:= curC]
       #nodeColors.dt <- rbindlist (list (nodeColors.dt, curD),use.names = T)
     }
   }    
     
     
  
  nodeColors.dt[ ,nodeName := gsub (" ", "_", nodeName)]
  nodeColors.dt <- rbindlist (list (nodeColors.dt, data.table (nodeName = c("ChickensHerd", "PigsHerd"), color = c("#CCD8D2" , "#F9C4B0" ))),use.names = T)
  
 
  nodeColors.dt[, color:= gsub (" ", "", color, fixed = T)]
  
  nodeColors.dt[ order (nodeName)]
  nodeColors.dt<- unique (nodeColors.dt[, .(nodeName, color)])
  nodeColors.dt[ nodeName == "PastureExpansion", color:= "#bfbbbb"]
  return (nodeColors.dt)
}



# gleam Animal colors (take last = darkest color) for each combination
#gleamColorsAnimals.dt <- gleamColors.dt[,tail(Color,1),by= "Animal"]
#setnames (gleamColorsAnimals.dt, "V1", "Color")




#dataDir <- "/Users/dwisser/Documents/FAO_GLEAM/GLEAMViz/data/"
# tempDir <- file.path (rootDir, "temp") #where is this?? #help



# read GLEAM country data ####################################################################################
dataFile <- file.path (dashboardData, "GLEAMDataALL.csv")


if (file.exists(dataFile)){
  cat ("Data file found!\n")
  allData.dt <-  fread (dataFile) # ony the data that will be uploaded when deployed !
  
}else{
  cat ("Data file not found!\n")
}

Animal_colors <- gleamColorAnimals.dt[c(3, 6, 8, 11, 13, 15), c(1,3)]

LPS_colors <- gleamColorAnimals.dt[c(1:3, 5:9), c(2,3)]
names(LPS_colors)[1] <- "LPS"

HerdType_colors <- as.data.frame(data.table (openxlsx::read.xlsx(gleamColorAndText.file, sheet = "Colors", cols= 13:14)))
HerdType_colors$HerdType[3:4] <- c("Chickens", "Pigs")

Product_colors <- as.data.frame(data.table (openxlsx::read.xlsx(gleamColorAndText.file, sheet = "Colors", cols= 6:7)))
names(Product_colors)[1] <- "Item"

Gas_colors <- as.data.frame(data.table (openxlsx::read.xlsx(gleamColorAndText.file, sheet = "Colors", cols= 10:11)))

Source_colors <- as.data.frame(data.table (openxlsx::read.xlsx(gleamColorAndText.file, sheet = "Colors", cols= 16:17)))

 #gleamColorAnimals.dt

#gaulList.dt <- fread (file.path (dashboardData, "GAULListUsedByGLEAM_20230316.csv"))
#gaulList.dt$COUNTRY
paramListSuppl.dt <-data.table (openxlsx::read.xlsx(file.path (dashboardData, "paramListSupplement.xlsx")))
paramData.dt <- fread (file.path (dashboardData, "ParameterALLNoCohorts.csv"))
dmIntake.dt <- fread (file.path (dashboardData, "feedIntakeCat.csv"))

# use AR6 GWP throughout. 
unique (allData.dt$Element)

allData.dt[VarName == "Emissions" & Element == "CH4", GWP:= 27 ]
allData.dt[VarName == "Emissions" &  Element == "N2O", GWP:= 273 ]



allData.dt[VarName == "Emissions" & Item == "TotalGHGAlloc", sum (V1/1000)/1e6,by = .(Animal) ]

allData.dt[ VarName == "Production" & Item == "Meat" & Element == "CarcassWeight", sum (V1, na.rm = T)/1e9, by= "Animal"] # OK! 
allData.dt[ VarName == "Emissions" & Item == "TotalGHGAlloc"  , sum (V1, na.rm = T)/1e9, by= "Animal"] # OK! 

allData.dt[VarName == "Emissions" , sum (V1*GWP)/1e12, by= "Animal"]


allData.dt[VarName == "Emissions" , sum (V1*GWP)/1e12]

allData.dt[Animal == "Cattle" & VarName == "AnimalNumbers" , sum (V1)/1e9,by=  c("HerdType","LPS")]

if (isPublicVersion == T) {
  allData.dt <- allData.dt[ Animal  != "Camels"]  
}else{
  allData.dt <- allData.dt 
}

allData.dt[ Element == "CH4", sum (V1)/1e9]


# subset variables for dashboard #################
unique (allData.dt$VarName)
unique (allData.dt$COUNTRY)


allData.dt <- allData.dt[VarName %in% c("Emissions", "Production", "AnimalNumbers", "DryMatterIntake")]


#gaulList.dt$ID
#allData.dt[ is.na (ISO3_num)]
allData.dt <- allData.dt[Element2 != "CO2eq"]
allData.dt <- allData.dt[Item != "Intensity"]
allData.dt <- allData.dt[ Item != "TotalGHGAlloc"]
#allData.dt <- allData.dt[ !(Item %in% c("IntensityMeat", "IntensityEgg", "IntensityProtein", "IntensityEggProtein"))]
allData.dt[VarName == "Emissions" & COUNTRY == "WORLD", sum (V1 * GWP/1000)/1e6,by = .(Animal) ]
allData.dt[VarName == "Emissions" & COUNTRY == "WORLD", sum (V1 * GWP/1000)/1e6 ]
allData.dt[VarName == "Emissions" & RegionClass == "ISO3COUNTRY", sum (V1 * GWP/1000)/1e6 ]


 allData.dt[ COUNTRY == "WORLD", COUNTRY:= "World"]
 paramData.dt[ COUNTRY == "WORLD", COUNTRY:= "World"]
dmIntake.dt[ COUNTRY == "WORLD", COUNTRY:= "World"]
 

allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions" & Animal == "Cattle" & Element == "CO2", sum (V1 * GWP/1000)/1e6 ,by = c("Animal", "Element", "Item")]

tot <- allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9/tot,by= Element ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9,by= Element ]

allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9/tot,by= Element2 ]



allData.dt[VarName == "Emissions" & HerdType == "Dairy" & Animal == "Cattle" & Element == "CH4", sum (V1 * GWP/1000)/1e6 ,by = c("Animal", "Element", "Item")]
allData.dt[ COUNTRY == "World"]

# I hate this list. Should not be used anymore. 
#gaulList.dt[ ISO3== "SER", ISO3 := "SRB"]

# only to get the names back...
#merge (gaulList.dt,allData.dt, by= c("ISO3")  )

#regions2Aggr <- c ("FAO REGION", "WORLD" , "EU27", "OECD", "WORLD BANK REGION", "GLEAM REGION", "CONTINENT")

# turn lists to named list and use only the ISO3 code, not the country name
RegionNames <- unique (allData.dt[, .(RegionClass, COUNTRY, ISO3) ])

dd <- RegionNames [RegionClass ==  "WORLD"  ]
worldRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "CONTINENT"  ]
contRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "SIDS"  ]
sidsRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "LLDC"  ]
lldcRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "LDC"  ]
ldcRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "SDGs REGION"  ]
sdgRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "GLEAM REGION" ]
gleamRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "FAO REGION"  ]
faoRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "WORLD BANK REGION"  ]
wbRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "OECD"  ]
oecdRegions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "EU27" ]
eu27Regions <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))

dd <- RegionNames [RegionClass == "Countries" ]
countries <- unlist(split(as.character(dd$ISO3), dd$COUNTRY))


# if (isPublicVersion == T) {
#   area_choices <- list(              World = worldRegions,
#                                          Continents = contRegions,
#                                          'FAO Regions' =  faoRegions,
#                                          'GLEAM Regions' = gleamRegions,
#                                          'SDGs Regions'  = sdgRegions,
#                                          'World Bank Regions' = wbRegions,
#                                          'EU27 Countries'  = eu27Regions,
#                                          'LDC Countries' = ldcRegions,
#                                          'LLDC Countries' = lldcRegions,
#                                          'OECD Countries' =oecdRegions,
#                                          'SIDS Countries' = sidsRegions)
# 
# 
# }else{
#   area_choices <- list(              World = worldRegions,
#                                           Continents = contRegions,
#                                           'FAO Regions' =  faoRegions,
#                                           'GLEAM Regions' = gleamRegions,
#                                           'SDGs Regions'  = sdgRegions,
#                                           'World Bank Regions' = wbRegions,
#                                           'EU27 Countries'  = eu27Regions,
#                                           'LDC Countries' = ldcRegions,
#                                           'LLDC Countries' = lldcRegions,
#                                           'OECD Countries' =oecdRegions,
#                                           'SIDS Countries' = sidsRegions,
#                                            Country= countries)
# }


if (isPublicVersion == T) {
  #countries <- NA
  allRegionCountries <- c (contRegions, gleamRegions, faoRegions, wbRegions, oecdRegions, eu27Regions, worldRegions, sidsRegions, lldcRegions, ldcRegions, sdgRegions)
  
}else{
  allRegionCountries <- c (contRegions, gleamRegions, faoRegions, wbRegions, oecdRegions, eu27Regions, worldRegions, countries, sidsRegions, lldcRegions, ldcRegions, sdgRegions)
  
}



# very ugly
# emSources2Choose <- c (
#   "DirectOnFarmEnergy"  ,
#   "EmbeddedOnFarmEnergy",
#   "EntericFermentation" ,
#   "Feed-CH4",            
#   "Feed-CO2"  ,          
#   "Feed-N2O"   ,         
#   "LandUseChange"  ,   
#   "Manure-CH4"  ,        
#   "Manure-N2O"   ,
#   "PastureExpansion"  ,
#   "Postfarm"   
# )
# emSources2ChooseNames <-c (
#   "Direct on-farm energy use"  ,
#   "Embedded on-farm energy"  ,
#   "Enteric fermentation"  ,
#   "Feed (CH4)" ,
#   "Feed (CO2)" ,    
#   "Feed (N2O)"   ,       
#   "Land use change" ,  
#   "Manure (CH4)" ,
#   "Manure (N2O)",  
#   "Pasture expansion" ,
#   "Post-farm emissions")


emSources2Choose <- c (
  "EntericFermentation",
  "Manure-CH4",        
  "Manure-N2O",
  "Feed-CH4",            
  "Feed-N2O",         
  "Feed-CO2",          
  "LandUseChange",   
  "DirectOnFarmEnergy",
  "PastureExpansion",
  "EmbeddedOnFarmEnergy",
  "Postfarm"   
)

emSources2ChooseNames <-c (
  "Enteric fermentation (CH4)"  ,
  "Manure (CH4)" ,
  "Manure (N2O)", 
  "Feed (CH4)" ,
  "Feed (N2O)"   ,       
  "Feed (CO2)" ,    
  "LUC: soy and palm (CO2)" ,  
  "Direct on-farm energy (CO2)"  ,
  "LUC: pasture expansion (CO2)" ,
  "Embedded on-farm energy (CO2)"  ,
  "Post-farm (CO2)")



#curCom <- unique (allData.dt[ VarName== "Emissions"& Animal %in% c("Cattle") ]$Item)
#names (emSources2Choose) [ emSources2Choose ==curCom ]

# to look up names in 


# find_offending_character <- function(x, maxStringLength=256){  
#   print(x)
#   for (c in 1:maxStringLength){
#     offendingChar <- substr(x,c,c)
#     #print(offendingChar) #uncomment if you want the indiv characters printed
#     #the next character is the offending multibyte Character
#   }    
# }
# #Sys.setlocale("LC_ALL", "C") 
# string_vector <- c("test", "Se\x96ora", "works fine")
# 
# lapply(string_vector, find_offending_character)
# 
# allData.dt$COUNTRY <- iconv (allData.dt$COUNTRY , from = "latin1",to = 'UTF-8//IGNORE' ) # fix the usual FAOSTAT problem with Cote d'Ivoire
unique (allData.dt$COUNTRY)



allData.dt[ VarName == "Emissions" , isDirect:= F]

allData.dt[ VarName == "Emissions" & Item %in% c("Manure-CH4", "Manure-N2O", "EntericFermentation"), isDirect:= T]

# convert production to tonnes
allData.dt[ VarName == "Production" , V1:= V1 / 1000]
# convert emissions to tonnes
allData.dt[ VarName == "Emissions" , V1:= V1 / 1000]


#allData.dt[ VarName == "Production" & Item == "Eggs" & COUNTRY== "World" & Element == "Weight", sum (V1)]/1e6

#allData.dt[ VarName == "Production" & ISO3 == "PAK" & Animal == "Buffalo" & Element == "Protein" & Item == "Milk", sum (V1)/1e6]
#allData.dt[ VarName == "Production" & ISO3 == "PAK" & Animal == "Buffalo" & Element == "Weight" & Item == "Milk", sum (V1)/1e6]





#SpecialRegions <- unique (allData.dt[ ISO3_num == -1]$COUNTRY)
#Commodities <-  unique (allData.dt[ VarName == "Production"]$Item)
# Ruminants <- c("Cattle", "Buffalo", "Goats", "Sheep", "Camels")
if (isPublicVersion == T) {
  Ruminants <- c("Cattle", "Buffalo", "Goats", "Sheep")
  
}else{
  Ruminants <- c("Cattle", "Buffalo", "Goats", "Sheep", "Camels")
}

Monogastrics <-  c("Pigs", "Chickens")
Animals <- unique (allData.dt$Animal)
LPS <- unique (allData.dt$LPS)
EmissionSources <- unique (allData.dt[ VarName == "Emissions"]$Item)



 
 makeSankeyDataEmissionDetails <- function (data.dt , nodes){
   ret <- data.table()
   
   data2plot2 <-  data.dt[, sum (value, na.rm = T),by=  nodes ]
   for (i in 1:(length (nodes)-1 )) {
     keep_cols <- c(nodes[i], nodes[i+1],"V1")
     DT <- data2plot2[,..keep_cols]
     print ( keep_cols)
     names (DT) <- c("from", "to", "weight")
     ret <- rbindlist (list (ret, DT), use.names = T )
   }
   return (ret)
 }
 
 
 
SankeyEmissionColors <- makeSankeyColorsAllEmissionTypes()

getEmissionNodeColor <- function (nodeN){
  return (SankeyEmissionColors[ nodeName == nodeN]$color)
}



# this makes the captions for the the different tabs
makeTabCaption <- function (mainCaption, regionName){
  baseYear <- 2015
  
  ret <- paste("","<h2>", mainCaption, "for ", names (allRegionCountries) [ allRegionCountries ==    regionName], " in ", baseYear, "</h2>")  
  
  #if (regionName %in% c(names (SpecialRegions))){
  #  ret <- paste("","<h2>", mainCaption, "for the  ",regionName, "region in ", baseYear, "</h2")  
  #}
  if (names (allRegionCountries [ allRegionCountries ==  regionName])  == "World") {
    ret <- paste("","<h2>", "Global", tolower (mainCaption), "in ", baseYear, "</h2>")  
    
  }
  ret
  
}



smallruminants <- c( "Goats", "Sheep")


infoTextProductionStatistis <- "Output of raw animal products (meat, milk, and eggs) as well as expressed in total protein, as calculated by GLEAM and harmonized with FAOSTAT. Further information available
at "
infoTextEmissions <- "Total greenhouse gas emissions. Direct emissions refer to emissions from manure and enteric fermentation, indirect emissions include emissions to feed and other inputs as well as Post-farm emissions. For further details see the "
infoTextProduction <- "Total production of animal products, expressed in tonnes of meat/milk/egg, protein of meat/milk/eggs/ and carcass weight. Data are calculated by GLEAM and harmonized with "
infoTextProductionSystem <- "Total herd size by species and production system. Based on Gridded Livestock of the World (GLW) 3, edition 2015. Further details available at "

infoTextEmissionIntensity <- "Emission intensity, emisssion per unit of output. Diagrams show the distribution of emission intentities for selected production systems in all countries (
weighted by production), the global average (blue line), and the value for a selected country (red line). Further information available at "

animalChoices <-  c (
  "Pigs" = "Pigs",
  "Chickens" = "Chickens",
  "Goats" = "Goats",
  "Sheep" = "Sheep",
  "Camels" = "Camels",
  "Cattle" = "Cattle",
  "Buffalo" = "Buffalo",
  "Monogastrics"= "Monogastrics",
  "Ruminants" = "Ruminants",
  "All" = "All"
)



animalChoicesSingleAnimals <- animalChoices[!animalChoices %in% c("All", "Ruminants", "Monogastrics")]

gasChoices <- c(
  "CH4" = "CH4" , 
  "N2O" = "N2O", 
  "CO2" = "CO2"
  
)


curVarName <- "Production"
curCountries <-  "World"
curAnimals <- c("Chickens")
curLPS <- c("Backyard", "Industrial", "Intermediate", "Layer")
curLPS <-unique (allData.dt[ VarName == "Production"]$LPS)

#curLPS <- "ALL"
selCommodity <- c("Meat")
selSources <-  unique (allData.dt[ VarName == "Emissions"]$Item)
curHerds <- "Dairy"
#allData.dt[ VarName == "Emissions" & COUNTRY == "World", sum (V1* GWP )/1e9,by= "Animal"]

#allData.dt[  VarName == "Emissions" & COUNTRY == "World", sum (V1* GWP )/1e9,by= "Animal"]
#allData.dt[ COUNTRY == "World" &   VarName == "Emissions" & Element2 == "Milk", sum (V1* GWP )/1e9 ]

# emissions dairy
#dairyEM.dt <- allData.dt[   VarName == "Emissions" & Element2 == "Milk", sum (V1* GWP ),by= .(COUNTRY) ]
# allData.dt[   VarName == "AnimalNumbers" ]

#dairyEM.dt <- allData.dt[ Animal %in% c("Cattle", "Buffalo") &    VarName == "Emissions" & Element2 == "Milk", sum (V1* GWP ),by= .(COUNTRY) ]
# methane only 
#dairyEM.dt <- allData.dt[ Animal %in% c("Cattle", "Buffalo") &    VarName == "Emissions" & Element2 == "Milk", sum (V1 ),by= .(COUNTRY) ]

#dairyMethane <- allData.dt[ Item== "EntericFermentation" &  Element == "CH4" & Element2== "Milk" & 
#              Animal %in% c("Cattle", "Buffalo") &    VarName == "Emissions", sum (GWP * V1, na.rm = T),by= COUNTRY]

#production 
#allData.dt[ Element == "Protein" & Animal %in% c("Cattle", "Buffalo") & VarName == "Production" , sum (V1,na.rm = T),by= COUNTRY]

#dairyProd.dt <- allData.dt[ Item == "Milk" & Element == "Protein" & Animal %in% c("Cattle", "Buffalo") & VarName == "Production", sum (V1),by= COUNTRY ]


#dairyData.dt <- merge (dairyEM.dt, dairyMethane, by= "COUNTRY", all =T)

#setnames (dairyData.dt, "V1.x", "Emissions")
#setnames (dairyData.dt, "V1.y", "Enteric")

#dairyData.dt <- merge (dairyData.dt, dairyProd.dt, by= "COUNTRY", all = T)
# #setnames (dairyData.dt, "V1", "MilkProduction")
# 
# 
# # herd size 
# dairyAnimals.dt <- allData.dt[ Animal %in% c("Cattle", "Buffalo")  & VarName == "AnimalNumbers", sum (V1),by= c( "COUNTRY")] 
# dairyData.dt <- merge (dairyData.dt, dairyAnimals.dt, by= "COUNTRY", all = T)
# setnames (dairyData.dt, "V1", "NumAnimals")
# dairyData.dt [ , entFrac:= Enteric / Emissions]
# dairyData.dt [ , emIntensity:= Emissions / MilkProduction]
# dairyData.dt [ , yield:= MilkProduction *1000 / NumAnimals]
# options (warn =1)
# # Chekc data
# dairyData.dt <- dairyData.dt[ !(COUNTRY %in% names (specialRegions)) ]
# dairyData.dt[, sum (Emissions)/1e9]
# 
# gp <- ggplot (dairyData.dt) + theme_bw()
# gp <- gp + geom_point(aes (x = yield, y= emIntensity, color = entFrac, size= Emissions/1e6), alpha = 0.8)
# gp  <- gp + scale_size(range = c(1, 30))
# gp <- gp + scale_color_gradient(low="blue", high="red4")
# gp <- gp + xlim (0,150)
# gp <- gp + ylab ("Emission intensity [ kg CO2eq / kg milk protein ]" )
# gp <- gp + xlab ("Productivity per animal [ kg milk / animal /year] ")
# gp
# 
# gp


  
getEmissionIntensityAll_2 <- function (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, GWPCH4, GWPN2O, selUnits ){
  cat ("Sel commodity", selCommodity, "\n")
  cat ("SelUnits" , selUnits, "\n")
  # harmonize tables 
  #if (setequal (curAnimals,  "Total")){
  #  selAnimals <- unique (allData.dt$Animal)
  #}
  prod.dt <- allData.dt[ VarName == "Production"]
  setnames (prod.dt,"Item", "Commodity")
  setnames (prod.dt,"Element", "Unit")
  prod.dt[ ,Element2 := NULL]
  
  em.dt <- allData.dt[ VarName == "Emissions"]
  
  em.dt[ Element== "CO2", GWP:= 1.0]
  em.dt[ Element== "CH4", GWP:= GWPCH4]
  em.dt[ Element== "N2O", GWP:= GWPN2O]
  
  
  setnames (em.dt,"Element2", "Commodity")
  setnames (em.dt,"Element", "Gas")
  setnames (em.dt,"Item", "EmissionSource")
  
  herd.dt <- allData.dt[ VarName == "AnimalNumbers"]
  emNodes <- selNodes
  prodNodes <- selNodes 
  herdNodes <- selNodes
  herdNodes <- herdNodes[ herdNodes != "Commodity"]
  emNodes <- selNodes
  if  (bySource== T){
    emNodes <-  c (selNodes, "EmissionSource")
  }
  if (setequal(selNodes, "per head")){
    perHead= T
  }
  perHead = F
  if (setequal(selNodes, "per head")){
    perHead= T
  }
  perHead= F
  if (setequal (selCommodity, "per animal")){
    perHead <- T
  }
  # if (setequal (selCommodity, "Total protein")){
  #   curUnits <- c("Protein")
  #   selCommodity <- c("Meat", "Milk", "Eggs")
  #   
  #   
  # }else{
  #   curUnits <-selUnits
  # }
  
  # if (length (selUnit) ==0){
     curUnits <- selUnits
  # }else{
  #   if (selUnit == "Protein" ){
  #     curUnits <- c("Protein")
  #}
  # }
 
  if (perHead ==  T){
    curProd.dt <- allData.dt[  Animal %in% curAnimals & VarName == "AnimalNumbers"  & HerdType %in% curHerds &   LPS %in% curLPS, sum (V1, na.rm = T),by= c("COUNTRY", "ISO3", herdNodes)]
    
  }else{
    curProd.dt <- prod.dt [ Animal %in% curAnimals &  VarName == "Production" &  HerdType %in% curHerds & LPS %in% curLPS & Commodity %in% 
                              selCommodity & Unit %in% curUnits, sum (V1, na.rm = T), 
                            by= c("COUNTRY", "ISO3", selNodes)]
    
  }
  
  curEm.dt <- em.dt [ Animal %in% curAnimals &  VarName == "Emissions" & HerdType %in% curHerds & LPS %in% curLPS & Commodity %in% selCommodity & 
                        EmissionSource %in% selSources ,  
                      sum (V1* GWP , na.rm = T), by= c("COUNTRY", "ISO3", emNodes)]
  
  
  if (perHead == T){
    herdNodes <- herdNodes[ herdNodes != "Commodity"]
    curEm.dt <- em.dt [ Animal %in% curAnimals &  VarName == "Emissions"  & HerdType %in% curHerds &  LPS %in% curLPS  &
                          EmissionSource %in% selSources , 
                        sum (V1* GWP , na.rm = T), by= c("COUNTRY", "ISO3",  emNodes)]
    
  }
  mergeNodes <- intersect (names (curEm.dt), names (curProd.dt))
  mergeNodes <- mergeNodes[ mergeNodes != "V1" ]
  emIntensity.dt <- merge (curProd.dt, curEm.dt, by=  mergeNodes)
  setnames (emIntensity.dt, "V1.x", "Production")
  setnames (emIntensity.dt, "V1.y", "Emissions")
  
  emIntensity.dt[, Intensity:= Emissions/ Production]
  if (perHead ==T){
    emIntensity.dt[, Intensity:= Intensity * 1000]
  }
  emIntensity.dt[, Emissions := Emissions]
  emIntensity.dt[, Production := Production]
  if (is.null (selNodes)==T ){
    emIntensity.dt[ ,Animal:= "ALL"]
  }
  if  (bySource== F){
    emIntensity.dt[, EmissionSource:= "Total"]
    #setcolorder(emIntensity.dt, c("ISO3","COUNTRY",	"Animal",	"Production",	"EmissionSource",	"Emissions",	"Intensity"))
  }  
  return (emIntensity.dt)
}

# 
 selNodes <- "Animal"
  curAnimals <- "Cattle"
  curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
  curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
  selNodes <- NULL
  selCommodity <- c("Milk", "Meat")
  selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
  bySource <- F
  selUnit <- "Protein"
  GWPCH4 <- GWPSets[[ "AR6"]]$CH4
  GWPN2O <- GWPSets[[ "AR6"]]$N2O
  
  xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, c("Milk"), selSources, bySource= F, GWPCH4, GWPN2O,selUnit = NULL )
    xx[ COUNTRY== 'World']
    xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, c("Meat", "Milk"), selSources, bySource= F, GWPCH4, GWPN2O,selUnit = NULL )
    xx[ COUNTRY== 'World']
    
    



subset_data <- function (data2plot.dt, sourceCol, targetCol, allNodes ){
  keep_cols <- c(sourceCol, targetCol, "Value")
  d1 <- data2plot.dt[, keep_cols , with = FALSE]
  setnames (d1, sourceCol, "From")
  setnames (d1, targetCol, "To")
  d1[, indexFrom:= match (From , allNodes)-1] #index in plotly is zero-based!
  d1[, indexTo:=   match (To , allNodes)-1]
  return (d1)
}

make_sankey_data <- function (data2plot.dt , col_names, allNodes){
  ret <- data.table()
  for (i in 1: (length (col_names)-1)  ) {
    print (i)
    curD <- subset_data (data2plot.dt,col_names[i], col_names[i+1], allNodes)
    #print (curD)
    ret <- rbindlist (list (ret, curD), use.names = T)
  }
  return (ret)  
}

getAllNodes <- function (data.dt, col_names){
  allNodes <- character()
  for (selCol in col_names){
    allNodes <- c (allNodes, unique (data.dt[, get(selCol)]))
  }
  return (allNodes)
}


makeSankeyPlot <- function (data.dt, colNames,plotTitle ){
  allNodes <- getAllNodes (data.dt, colNames )
  
  sankeyData <- make_sankey_data(data.dt, colNames, allNodes)
  p <- plot_ly(
    type = "sankey",
    orientation = "h",
    text = "fuck",
    hoverinfo = 'text',
    node = list(
      label = allNodes,
      #   color = sankeyData$nodeColor,
      pad = 15,
      thickness = 20,
      line = list(
        #color = data2plot.dt$Animal,
        width = 0.5
      )
    ),
    
    link = list(
      source =sankeyData$indexFrom,
      target = sankeyData$indexTo,
      value = sankeyData$Value,
      # color = sankeyData$colorCode
      label = sankeyData$Value
    )
  ) %>% 
    layout(
      title = plotTitle,
      font = list(
        size = 10
      )
    )
  
  

  
  
  p
  
}
options(warn=1) # turn warnings into errors off 
cat ("Done with processing global.R\n")


