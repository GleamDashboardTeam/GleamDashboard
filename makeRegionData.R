rm (list = ls ())
source ("global.R")
library(data.table)
# animalNumbers


dashboardData <- file.path ("dashboardData") # ony the data that will be uploaded when deployed !
# rootDir <- "/Users/dwisser/Source/GLEAMv3Dashboard/dashboardData"
# does not use cell table calc tables but aggrated tables by country !
# add paramters ####################### 
calcT <- list.files ("/Users/dwisser/Data/GLEAM4/CalcTables/ADM0", pattern = ".csv", full.names = T)

# load all country tables into one file 
gleamSucks.dt <- fread ( calcT[7])

calcT <- calcT[ grep ("Global20221019IPCC2006_2006", calcT)]
calcT <- calcT[ grep ("ADM0AndCohort", calcT)]

params.list <-  lapply(calcT, fread, sep=",")
paramData.dt  <- rbindlist( params.list , fill = T, use.names = T)
# hate hate hate 
paramData.dt <- paramData.dt[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

paramData.dt[,  junk1:= MMSSOLIDOT + MMSSOLID ]
paramData.dt[, junk2:= MMSPASTURE  + MMSPASTPADD ]
paramData.dt[, junk3:= MMSDEEPLITTER+ MMSDEEPLITT ]

sort (names (paramData.dt))

paramData.dt[, c("MMSSOLID", "MMSPASTPADD", "MMSDEEPLITT", "MMSDEEPLITTER", "MMSSOLIDOT", "MMSPASTURE"):= NULL]
setnames (paramData.dt, "junk1", "MMSSOLID")
setnames (paramData.dt, "junk2", "MMSPASTPADD")
setnames (paramData.dt, "junk3", "MMSDEEPLITT")





#paramData.dt[ is.na (paramData.dt)]

#paramData.dt[is.na(paramData.dt)] <- 0

numCols <-  unlist(lapply(paramData.dt, is.numeric), use.names = FALSE)  
selVars <- colnames (paramData.dt)[numCols]
selVars <- setdiff (selVars, c("ADM0_CODE"))



GWPSets <-  list()
GWPSets [[ "AR6"]]  = list (CH4 = 27, CO2 = 1, N2O = 273)
GWPSets [[ "AR5 incl"]]  = list (CH4 = 34, CO2 = 1, N2O = 298)
GWPSets [[ "AR5 excl"]]  = list (CH4 = 28, CO2 = 1, N2O = 265)
GWPSets [[ "SAR"]]  = list (CH4 = 21, CO2 = 1, N2O = 310)
GWPSets [[ "AR4"]] = list (CH4 = 25, CO2 = 1, N2O = 298)
names (GWPSets)


# read GLEAM country data ####################################################################################
dataFile <- file.path (dashboardData, "GLEAMv3Data.csv")
gleamData.dt <- fread (dataFile)
gleamData.dt[, RegionClass:= "Countries"]

# I hate this list. Should not be used anymore. 
#gaulList1.dt <- fread (file.path (dashboardData, "GAULListUsedByGLEAM_GIUSY.csv"))
#library(xlsx)
#library (sringi)
Sys.setlocale(category = "LC_ALL")
gaulList.dt <- fread (file.path (dashboardData, "GAULListUsedByGLEAM_20230508.csv") )
library(stringi)
#stri_trans_general(gaulList.dt$COUNTR, "Latin-ASCII")
#stri_trans_general(gaulList.dt$COUNTR, "UTF-8"
#stri_enc_toutf8(gaulList.dt$COUNTRY)

f <- function(x) {
  
  x <- gsub(">", "", gsub("<U\\+", "\\\\u", x))
  stringi::stri_unescape_unicode(x)
}
gaulList.dt$COUNTRY <- f(gaulList.dt$COUNTRY)

propCNames <- gaulList.dt$COUNTRY
names (propCNames) <- gaulList.dt$ISO3
str (propCNames)
# TODO: make sure to use the exact same country names after the umlaut issue was fixed.!




#gaulList.dt$COUNTRY <- iconv (gaulList.dt$COUNTRY , to = "latin1",from= 'UTF-8//IGNORE' ) # fix the usual FAOSTAT problem with Cote d'Ivoire

unique (gaulList.dt$COUNTRY)

#GAULListUsedByGLEAM_20230316.csv
#gaulList.dt[ ISO3== "SER", ISO3 := "SRB"]
#gaulList.dt[ COUNTRY== "Turkey", COUNTRY:= "Republic of Türkiye"]
#gaulList.dt[ COUNTRY== "Netherlands", COUNTRY:= "Netherlands (Kingdom of the)"]
#gaulList1.dt [ COUNTRY == "Germany"]
#gaulList2.dt [ COUNTRY == "Germany"]

# horrible data
gaulList.dt[ , EU27:= as.character( EU27)]
gaulList.dt[ , OECD:= as.character( OECD)]

gaulList.dt[ EU27== "1", EU27:= "EU27"]
gaulList.dt[ OECD== "1", OECD:= "OECD"]
gaulList.dt[ EU27== "0", EU27:= "NonEU27"]
gaulList.dt[ OECD== "0", OECD:= "NonOECD"]


# to sum up with the same function
gaulList.dt[, WORLD:= "WORLD"]
gaulList.dt[, ISO3COUNTRY:= ISO3]

#gaulList.dt <- gaulList.dt[ ISO3 != ""]
#gaulList.dt <- gaulList.dt[ ISO3 != " "]

gleamData.dt <- merge (gleamData.dt, gaulList.dt [,.(ADM0_CODE, COUNTRY, ISO3)], by = "ADM0_CODE")
paramData.dt[ ADM0_NAME == "Germany"]
paramData.dt <- merge (paramData.dt, gaulList.dt [,.(ISO3, ADM0_CODE)], by= "ADM0_CODE")
setnames (paramData.dt, "ADM0_NAME", "COUNTRY")
paramData.dt[, RegionClass:= "Countries"]
paramData.dt[ ISO3 == "DEU"]
curRegionType <- "FAO REGION"


makeRegionDataGLEAM <- function ( selADM0Codes, curRegionName, fakeISO3, curRegionType){
  curAgg.dt <- gleamData.dt[ADM0_CODE %in% selADM0Codes, sum (V1,na.rm= T), by = .(VarName, Animal, HerdType, LPS,Item, Element, Element2, GWP )]
  curAgg.dt[, ISO3_num:= -1]
  curAgg.dt[, ISO3:= fakeISO3]
  curAgg.dt[, RegionClass:=curRegionType ]
  curAgg.dt[, COUNTRY:= curRegionName]
  
  return (curAgg.dt)
}


# horrible horrible horrrilbe only because some GLEAM input no longer availablel in vector format. 
# very slow because it treats each country as a region ..





#CT.dt <- fread (file.path ("/Users/dwisser/Data", "CellTable" , "ADM0_CODE_Fraction.csv"))
curRegionType <- "FAO REGION"
# load all country tables into one file 



#makeParamValues ( selISOCodes, curReg, fakeISO3, curRegionType)))


makeParamValues <- function (selADM0Codes,  RegionName, fakeISO3, curRegionType){
  
  paramData.temp <- data.table ()
  curVar <- "AFKG"
  numCols <-  unlist(lapply(paramData.dt, is.numeric), use.names = FALSE)  
  selVars <- colnames (paramData.dt)[numCols]
  selVars <- setdiff (selVars, c("ADM0_CODE"))
  #varData.dt <- data.table ()
  for (curVar in selVars){
    
     cat ("\t", curVar, "\n")
    x1 <- paramData.dt[ ADM0_CODE %in% selADM0Codes, sum (get (curVar) * POPULATION , na.rm = T) / sum (POPULATION,na.rm = T), 
                     by=.(cohort, animal, herd, lps)]  
   # x1[ is.na (V1), V1:= 0.0]
    x1[, varName:= curVar]
    paramData.temp <- rbindlist (list (paramData.temp, x1), use.names = T)
    
  }
  paramData.temp[, ISO3_num:= -1]
  paramData.temp[, ISO3:= fakeISO3]
  paramData.temp[, RegionClass:=curRegionType ]
  paramData.temp[, COUNTRY:= RegionName]
  
  return (paramData.temp)
}



regions2Aggr <- c ("FAO REGION", "WORLD" , "EU27", "OECD", "WORLD BANK REGION", "GLEAM REGION", "CONTINENT")

curReg <- "WORLD"

gleamDataAll.dt <- data.table ()
paramDataAll.dt <- data.table()

curRegionType <- "WORLD BANK REGION"

for (curRegionType in regions2Aggr){
  regionIDOPrefix <- substr(curRegionType, 1,7)
  regionIDOPrefix <- gsub (" ", "", regionIDOPrefix)
  curISO3Region.dt <- gaulList.dt[ , .(ADM0_CODE, get(curRegionType))]
  setnames (curISO3Region.dt, "V2", "Region")
  curISO3Region.dt <- curISO3Region.dt[ Region != "Antarctica" ]
  #curISO3Region.dt <- curISO3Region.dt[ !(is.na (ISO3))]
  #curISO3Region.dt <- curISO3Region.dt[  ISO3 != ""]
  selRegions <- unique (curISO3Region.dt$Region)
  regionID <- 1
  for (curReg in (selRegions)) {
    ISO3DW <-  paste0(regionIDOPrefix, regionID) 
    #if (curRegionType == "ISO3COUNTRY"){
    #  RegName <- curReg
    #}
    cat (curRegionType, ":", curReg, ":", ISO3DW, ":", curReg, "\n")
    
    
    
    
    fakeISO3 <- paste0(regionIDOPrefix, regionID)
    
    selADM0Codes <- curISO3Region.dt[ Region == curReg ]$ADM0_CODE
    gleamDataAll.dt <- rbindlist (list (gleamDataAll.dt, makeRegionDataGLEAM ( selADM0Codes, curReg, fakeISO3, curRegionType)))
    paramDataAll.dt <- rbindlist (list (paramDataAll.dt, makeParamValues     ( selADM0Codes, curReg, fakeISO3, curRegionType)))
    
    regionID <- regionID +1
  }
}

# TODO Add natinoal data from origainal data 


gleamDataAll.dt <- rbindlist (list (gleamDataAll.dt, gleamData.dt), use.names = T, fill = T)


gleamDataAll.dt[ RegionClass == "WORLD" & VarName == "Emissions" & Element == "CH4", sum (V1)/1e9]
gleamDataAll.dt[ RegionClass == "WORLD" & VarName == "Emissions" & Element == "CH4", sum (V1*27)/1e9]


paramCountry.m <- melt (paramData.dt,id.vars = c( "animal", "herd", "lps" ,"cohort" , "COUNTRY", "ISO3", "RegionClass", "ADM0_CODE"))
setnames (paramCountry.m, "variable","varName")
setnames (paramCountry.m, "value","V1")

summary (gleamDataAll.dt)
gleamDataAll.dt 
# this is still be cohort. 
paramDataAll.dt <- rbindlist (list (paramDataAll.dt, paramCountry.m), use.names = T, fill = T)

paramDataAll.dt[ COUNTRY == "Germany"]
paramDataAll.dt[ ISO3 == "TZA"]


# rename to be aligned with GLEAm names
paramDataAll.dt[, Animal:= gleam4AnimalNames[ animal] [[1]] , by = seq_along(1:nrow (paramDataAll.dt)) ]
paramDataAll.dt[, HerdType:= gleam4Herds[herd][[1]], by = seq_along(1:nrow (paramDataAll.dt)) ]
paramDataAll.dt[, LPS:= gleam4LPS [lps][[1]], by = seq_along(1:nrow (paramDataAll.dt)) ]

cols2delete <- c("animal", "herd", "lps")
paramDataAll.dt [, c(cols2delete):=NULL ]

str (paramDataAll.dt)
sort (unique (paramDataAll.dt$varName))


summary (paramDataAll.dt[ Animal == "Chicken" & varName == "AFKG"])

# high quality GLEAM data . HORRIBLE quick fix that takes away the weighting but will work to remove the duplicates. MMS data disaster. 
#paramDataAll.dt[varName == "MMSSOLIDOT", varName:= "MMSSOLID" ]
#paramDataAll.dt[varName == "MMSPASTURE", varName:= "MMSPASTPADD" ]
#paramDataAll.dt[varName == "MMSDEEPLITTER", varName:= "MMSDEEPLITT" ]

#paramDataAll.dt <- paramDataAll.dt[, mean (V1, na.rm  = T), by = .(varName, ISO3_num, ISO3, RegionClass, COUNTRY, ADM0_CODE, Animal, HerdType, LPS, cohort) ]


paramDataAll.dt [ duplicated(paramDataAll.dt)]

unique (gleamDataAll.dt$RegionClass)
unique (gleamDataAll.dt$COUNTRY)
gleamDataAll.dt[ COUNTRY ==0]



unique (paramDataAll.dt$c)
paramDataAll.dt[ is.na (RegionClass)]
unique (paramDataAll.dt$varName)
paramDataAll.dt[ is.na (varName)]
paramDataAll.dt[ COUNTRY == "Germany"]
unique (paramDataAll.dt$COUNTRY)
paramDataAll.dt[ COUNTRY ==0]


nrow (paramDataAll.dt)/1e6
paramDataAll.dt <- paramDataAll.dt[ !(is.na (V1))]
nrow (paramDataAll.dt)/1e6

str (paramDataAll.dt)
paramDataAll.dt[ V1 ==0]

paramDataAll.dt[ varName == "AFC"]

#ParameterALLNoCohorts.csv
fwrite (paramDataAll.dt, "/Users/dwisser/Data/GLEAM4/ParamAll.csv", sep = "\t")

# ACOSTA
paramAA.dt <- paramDataAll.dt[ RegionClass == "Countries"]
summary (paramAA.dt)
fwrite(paramAA.dt,  "/Users/dwisser/Data/GLEAM4/ParamAA.csv", sep = "\t")

paramDataAll.dt[ RegionClass == "GLEAM REGION" & COUNTRY == "Sub-Saharan Africa"]


paramDataAll.dt[ ISO3 == "APR"]
paramDataAll.dt [ Animal == "Chicken" & varName == "LW"]

# get rid of cohorts. 
Pop.dt <- paramDataAll.dt[ varName == "POPULATION"]
Pop.dt[, sum (V1), by  = "Animal"]

ParameterALLNoCohorts.dt  <- merge (paramDataAll.dt, Pop.dt, by= c("ISO3_num", "ISO3" ,"RegionClass","COUNTRY", "ADM0_CODE"  ,"Animal", "HerdType",      "LPS" ,"cohort"  ) )
ParameterALLNoCohorts1.dt<- ParameterALLNoCohorts.dt[, sum (V1.x * V1.y, na.rm = T)/ sum (V1.y, na.rm = T), 
                                                    by= .(ISO3_num,ISO3,   RegionClass,   COUNTRY, ADM0_CODE,  Animal ,HerdType,  LPS,  varName.x) ]

ParameterALLNoCohorts1.dt[ varName.x == "LW" & RegionClass == "FAO REGION"]
#setnames (ParameterALLNoCohorts.dt, "varName.x", "varName")
bySpec.dt <- ParameterALLNoCohorts.dt[, sum (V1.x * V1.y, na.rm = T)/ sum (V1.y, na.rm = T), 
                                                          by= .(ISO3_num,ISO3,   RegionClass,   COUNTRY, ADM0_CODE,  Animal,   varName.x) ]


lw.dt <- bySpec.dt[ varName.x == "LW" & RegionClass == "FAO REGION"]

xxx <- dcast (lw.dt[, .( COUNTRY, Animal, V1)],  COUNTRY~Animal, value.var = "V1")

junk <- ParameterALLNoCohorts.dt[ Animal == "Cattle" & HerdType == "Dairy" & LPS == "Grassland" & COUNTRY== "WORLD" ]


# feed data conversion ########### what a bad data structure. 
library(openxlsx)
feedItems.dt <- data.table(openxlsx:: read.xlsx("/Users/dwisser/Data/GLEAM4/Feed_items_categories_feed-paper.xlsx")) #help
dmRaw.dt <- gleamDataAll.dt[ VarName == "DryMatterIntake"]
dmRaw.dt[COUNTRY== "WORLD", sum (V1)]
dmRaw.dt[, AnimalGroup:= "Ruminants"]
dmRaw.dt[Animal %in% c("Pigs", "Chicken"), AnimalGroup:= "Monogastrics"]
dmRaw.dt[, Element:= as.numeric(Element)]
dm.dt <- merge (dmRaw.dt, 
                feedItems.dt[ , .(AnimalGroup, Name, OrigGLEAMID,MonoBackyard, Others)], 
                by.x= c ("AnimalGroup","Item", "Element"), 
                by.y= c("AnimalGroup", "Name", "OrigGLEAMID"), call.x  = T)

dm.dt[, feedCat:= Others]
dm.dt[LPS == "Backyard", feedCat:= MonoBackyard]
dm.dt <- dm.dt[, sum (V1,na.rm = T), by= .(ISO3_num,ISO3,   RegionClass,   COUNTRY,   Animal ,HerdType,  LPS, feedCat ) ] 
dm.dt[COUNTRY == "WORLD", sum (V1)]
# horrible data managment 


gleamDataAll.dt[ ISO3 %in% gaulList.dt$ISO3, COUNTRY := propCNames[ ISO3] ] 
ParameterALLNoCohorts.dt[ ISO3 %in% gaulList.dt$ISO3, COUNTRY := propCNames[ ISO3] ] 
dm.dt[ ISO3 %in% gaulList.dt$ISO3, COUNTRY := propCNames[ ISO3] ] 
unique (dm.dt$COUNTRY)

unique (gleamDataAll.dt$Country)

fwrite (ParameterALLNoCohorts.dt, (file.path (dashboardData, "ParameterALLNoCohorts.csv")))
fwrite (gleamDataAll.dt, (file.path (dashboardData, "GLEAMDataALL.csv")))
fwrite (dm.dt, (file.path (dashboardData, "feedIntakeCat.csv")))

mcKinsey.dt <- gleamDataAll.dt[ RegionClass == "GLEAM REGION" & COUNTRY == "Sub-Saharan Africa" & VarName %in% c("Production", "AnimalNumbers", "Emissions")]

fwrite (mcKinsey.dt, "/Users/dwisser/Downloads/gleamSSA.csv") #help
mcKinsey.dt[VarName == "Emissions", sum (GWP * V1)/1e9]
mcKinsey.dt[VarName == "Emissions", sum (GWP * V1)/1e9, by = Animal]


write.csv(gleamDataAll.dt, "test2.csv", fileEncoding = "latin1")
xx <- fread ("test2.csv")
unique (xx$COUNTRY)

gleamDataAll.dt[ VarName == "Emissions" & RegionClass == "Countries" & Element == "CH4"]
gleamDataAll.dt[ ADM0_CODE ==2]
methaneTot.dt <- gleamDataAll.dt[ !(is.na (COUNTRY)) & VarName == "Emissions" & RegionClass == "Countries" & Element == "CH4", sum (V1),
                                  by = .(ADM0_CODE, COUNTRY)]

#fwrite (methaneTot.dt, "/Users/dwisser/Downloads/TotalLivestockMethaneByCountry.csv")
hist.dt <- fread ("/Users/dwisser/Downloads/historical_emissions.csv", header = T) #help
meth2015.dt <- hist.dt[ Gas == "CH4", c("Country", "Gas", "Unit", "2015")]
setnames (meth2015.dt, "2015", "MethaneMtCo2eq")
meth2015.dt[ Country== "Iran", Country:= "Iran, Islamic Republic of"]
meth2015.dt[ Country== "United States", Country:= "United States of America"]
meth2015.dt[ Country== "Russia", Country:= "Russian Federation"]
meth2015.dt[ Country== "Vietnam", Country:= "Viet Nam"]
meth2015.dt[ Country== "United Kingdom", Country:= "United Kingdom of Great Britain and Northern Ireland"]
meth2015.dt[ Country== "Tanzania", Country:= "United Republic of Tanzania"]
meth2015.dt[ Country== "Turkey", Country:= "Türkiye"]
meth2015.dt[ Country== "Netherlands", Country:= "Netherlands (Kingdom of)"]
meth2015.dt[ Country== "Venezuela", Country:= "Venezuela (Bolivarian Republic of)"]
meth2015.dt[ Country== "Bolivia", Country:= "Bolivia (Plurinational State of)"]




meth2015.dt[, MethaneTot_kg:= MethaneMtCo2eq /34 * 1e9]
methane.dt <- merge (meth2015.dt, methaneTot.dt, by.y = "COUNTRY", by.x= "Country", all = T)
methane.dt[, percentCh4Livestock:= V1/ MethaneTot_kg*100]
fwrite (methane.dt, "/Users/dwisser/Data/MethaneOverview.csv") #help

stop()

#fwrite (paramDataAll.dt[ RegionClass != "Countries"], (file.path ("/Users/dwisser/Source/GLEAMv3Dashboard/dashboardData", "ParameterRegions.csv")))
#fwrite (gleamDataAll.dt [ RegionClass != "Countries"], (file.path ("/Users/dwisser/Source/GLEAMv3Dashboard/dashboardData", "GLEAMDataRegions.csv")))


# some tests
library(rbenchmark)

benchmark(fread ((file.path (dashboardData, "GLEAMDataALL.csv"))), replications = 3 )
benchmark(fread ((file.path (dashboardData, "ParameterALL.csv"))), replications = 3 ) #missing



#benchmark(fread ((file.path ("/Users/dwisser/Source/GLEAMv3Dashboard/dashboardData", "ParameterALL_ISO3COUNTRY.csv"))), replications = 3 )
# too slow. 
#split by regionType
for (curRT in regions2Aggr){
  xx <- paramDataAll.dt[ RegionClass == curRT]
  outN <- paste0 ("ParameterALL_", curRT, ".csv")
  fwrite (xx, (file.path (dashboardData, outN))) #missing
}



library (fst)
fst_file <- "/Users/dwisser/Downloads/FSTTest.fst" #help

write_fst(paramDataAll.dt, fst_file)  # filesize: 17 KB
benchmark( y <- read_fst(fst_file), replications = 3) # read fst file



library(googleCloudStorageR)
gar_auth(email = "dominik.wisser@fao.org")
Sys.setenv("GCS_DEFAULT_BUCKET" = "fao-gleami-review")
gcs_setup()





paramListSuppl.dt <-data.table (openxlsx::read.xlsx(file.path (dashboardData, "paramListSupplement.xlsx")))


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

allData.dt <- allData.dt[ Animal  != "Camels"]

allData.dt[ Element == "CH4", sum (V1)/1e9]





#gaulList.dt$ID
#allData.dt[ is.na (ISO3_num)]
allData.dt <- allData.dt[Element2 != "CO2eq"]
allData.dt <- allData.dt[Item != "Intensity"]
allData.dt <- allData.dt[ Item != "TotalGHGAlloc"]
allData.dt <- allData.dt[ !(Item %in% c("IntensityMeat", "IntensityEgg", "IntensityProtein", "IntensityEggProtein"))]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e6,by = .(Animal) ]

allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions" & Animal == "Cattle" & Element == "CO2", sum (V1 * GWP/1000)/1e6 ,by = c("Animal", "Element", "Item")]

tot <- allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9 ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9/tot,by= Element ]
allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9,by= Element ]

allData.dt[VarName == "Emissions", sum (V1 * GWP/1000)/1e9/tot,by= Element2 ]



allData.dt[VarName == "Emissions" & HerdType == "Dairy" & Animal == "Cattle" & Element == "CH4", sum (V1 * GWP/1000)/1e6 ,by = c("Animal", "Element", "Item")]



allData.dt <- merge (allData.dt [!(is.na (ISO3))], gaulList.dt[  ISO3 != "" ,.(ISO3, COUNTRY)],by= "ISO3")
allData.dt[COUNTRY== "Serbia"]
allData.dt[COUNTRY %in% c("Djibouti","Eritrea","Ethiopia",  "Kenya", "Somalia","South Sudan", "Sudan", "Uganda") 
           & Animal == "Cattle" & VarName == "AnimalNumbers" & LPS == "Grassland", sum (V1)/1e6, by = c("COUNTRY")]

allData.dt[COUNTRY %in% c("Djibouti","Eritrea","Ethiopia",  "Kenya", "Somalia","South Sudan", "Sudan", "Uganda") 
           & Animal == "Cattle" & VarName == "AnimalNumbers" & LPS == "Mixed", sum (V1)/1e6]


# tonnes
dairyEmissions.dt <- allData.dt[ VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & 
                                   HerdType== "Dairy" , sum (V1 * GWP/1000),by=ISO3 ]



#dairyEmissions.dt[, sum (V1)/1e9]
#allData.dt[ isDirect == T & VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & 
#              HerdType== "Dairy" , sum (V1 * GWP/1000),by=ISO3 ]


allData.dt[ VarName == "Emissions" & Item %in% c("Manure-CH4", "Manure-N2O", "EntericFermentation"), isDirect:= T]


totDairyDirect <- allData.dt[ Item %in% c("Manure-CH4", "Manure-N2O", "EntericFermentation") & VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & 
                                HerdType== "Dairy" , sum (V1 * GWP/1000) ]/1e9

# regional parameter data ###############
regAvgParamA5.file  <- file.path (dashboardData, "ParamsAnnex5.csv")
regAvgParamReg.file  <-file.path (dashboardData, "ParamsFAORegions.csv")

dmAnnex5.file <- file.path (dashboardData, "dmIntakeAnnex5.csv")
dmFAORegion.file <-  file.path (dashboardData, "dmIntakeFAOReg.csv")

#x1 <- fread (regAvgParamA5.file)

#allData.dt[ VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & 
#              HerdType== "Dairy" , sum (V1 * GWP/1000)/totDairy,by=Element ]

#allData.dt[ VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & 
#              HerdType== "Dairy" , sum (V1 * GWP/1000/1e6),by=Animal ]
# fwrite (dairyEmissions.dt, "/Users/dwisser/Data/Dairy/GLEAMDairyEmissions2015_tonnesCO2eq.csv")

#allData.dt[ VarName == "Emissions" & Animal %in% c("Cattle", "Buffalo") & HerdType== "Dairy" & Element2 == "Meat", sum (V1 * GWP)/1e9]


#junk <- allData.dt[ Element == "CH4" & VarName == "Emissions" & Animal %in% c("Pigs", "Chicken")  , sum (V1 * GWP/1000) /1e6,by= .(Animal, Item, LPS)]

#allData.dt[ Element == "CH4" & VarName == "Emissions" & Animal %in% c("Pigs", "Chicken")  , sum (V1 * GWP/1000) /1e6, by= "Item"]
#allData.dt[ Element == "N2O" & VarName == "Emissions" & Animal %in% c("Pigs", "Chicken")  , sum (V1 * GWP/1000) /1e6,by= .(Animal, LPS)]


#allData.dt[ VarName =="Emissions"  , sum (V1) /1e6,by= .(Element)]


# PAKISTAN #######

#pakEM.dt <- allData.dt[ isDirect == T & VarName %in% c("Emissions"), sum (V1) /1e6, by= c("COUNTRY", "Animal", "HerdType", "LPS", "Item")]

#fwrite (pakEM.dt, "/Users/dwisser/Downloads/GLEAM3EmissionsCountry.csv")


#pakNum.dt <- allData.dt[  VarName %in% c("AnimalNumbers"),sum (V1) /1e6, by= c("COUNTRY", "Animal", "HerdType", "LPS") ]

#fwrite (pakNum.dt, "/Users/dwisser/Downloads/GLEAM3PopulationCountry.csv")


#allData.dt[ COUNTRY == "United States of America" & LPS == "Feedlots"]

#allData.dt[ VarName == "Emissions" & ISO3 == "TZA", sum (V1*GWP )/1e9]

#allData.dt[ VarName == "Emissions" & ISO3 == "TZA", sum (V1*GWP )/1e9,by= "Animal"]

#allData.dt[ VarName == "Emissions" & ISO3 == "TZA", sum (V1*GWP )/1e9,by= "Item"]

#allData.dt[ VarName == "Emissions" & ISO3 == "PAK", sum (V1*GWP )/1e9,by= .(Animal, HerdType, Item)]

allData.dt[ COUNTRY== "Turkey" , COUNTRY:= "Republic of Türkiye"]

allData.dt

# add total emissions from CAIT data base 
# 
# caitFile <- file.path (dashboardData, "CAIT_TotalGHGEmissionsHistorical.xlsx")
# cait.dt <- data.table (openxlsx::read.xlsx(caitFile, sheet= "historical_emissions"), check.names = T)
# 
# cait.dt <- cait.dt[ !(is.na (ISO3)), .(ISO3, X2015)]
# setnames (cait.dt, "X2015", "V1")
# cait.dt[, sum (V1)]
# cait.dt <- merge (cait.dt [!(is.na (ISO3))], gaulList.dt[  ISO3 != "" ,.(ISO3, COUNTRY)],by= "ISO3")
# cait.dt[, VarName:= "Historial Emissions"]
# cait.dt[, ("Animal"):= "Total"]
# cait.dt[, ("HerdType"):= "Total"]
# cait.dt[, ("LPS"):= "Total"]
# cait.dt[, ("Element"):= "Total"]
# cait.dt[, ("Element2"):= "Total"]
# cait.dt[, ("Item"):= "Total"]
# cait.dt[, ("GWP"):= 1.0]
# cait.dt[, V1:= V1 * 1e8]

#str(cait.dt)
#allData.dt <- rbindlist (list (allData.dt, cait.dt), use.names = T, fill = T)

#allData.dt[ VarName == "Historial Emissions", sum (V1),by= .(Animal, HerdType, LPS, ISO3, Element, Element2, Item, GWP)]
#allData.dt[ VarName == "Historial Emissions"]$ISO3

#          !!! Putting expression in a string is a sign of bad design, it is fine to use it, but one should not put such code in production !!!


#allData.dt <- allData.dt [ VarName != "EmissionsAllocated"]
# Add special regions maybe this should be moved to global.R 
names (gaulList.dt) <- make.names (names (gaulList.dt))

#strip leading and trailing whitspaces so data.table does not produce an errorr
gaulList.dt$FAO.REGION <- gsub("`", '', gaulList.dt$FAO.REGION)
gaulList.dt$CONTINENT <- gsub("`", '', gaulList.dt$CONTINENT)
gaulList.dt$GLEAM.REGION <- gsub("`", '', gaulList.dt$GLEAM.REGION)
gaulList.dt$WORLD.BANK.REGION <- gsub("`", '', gaulList.dt$WORLD.BANK.REGION)


# remove empty ISO3
gaulList.dt <- gaulList.dt[ ISO3 != ""]
specialRegions <- list ()
specialRegions [["World"]] <- unique (gaulList.dt$ISO3)
specialRegions [["OECD"]] <- unique (gaulList.dt[ OECD ==1]$ ISO3)
specialRegions [["EU27"]] <- unique (gaulList.dt[ EU27 ==1]$ ISO3)

specialRegions [["EU27"]] <- unique (gaulList.dt[ EU27 ==1]$ ISO3)
specialRegions <- specialRegions [ specialRegions != "Antarctica"]

continents <-  unique (gaulList.dt$CONTINENT)
continents <- continents [ continents != "Antarctica"]

for (cCont in continents){
  specialRegions[[ cCont ]] <- unique (gaulList.dt[ CONTINENT == cCont]$ ISO3)
}

A5Regions <- unique (gaulList.dt$GLEAM.REGION)
A5Regions <- A5Regions [ A5Regions != "Antarctica"]
for (cCont in A5Regions){
  specialRegions[[ paste0("", cCont) ]] <- unique (gaulList.dt[ GLEAM.REGION == cCont]$ ISO3)
}


WBRegions <- unique (gaulList.dt$WORLD.BANK.REGION)
WBRegions <- WBRegions [ WBRegions != "Antarctica"]
for (cCont in WBRegions){
  specialRegions[[ paste0( cCont) ]] <- unique (gaulList.dt[ WORLD.BANK.REGION == cCont]$ ISO3)
}

FAORegions <- unique (gaulList.dt$FAO.REGION)
FAORegions <- FAORegions [ FAORegions != "Antarctica"]
for (cCont in FAORegions){
  specialRegions[[ paste0( cCont) ]] <- unique (gaulList.dt[ FAO.REGION == cCont]$ ISO3)
}

#names (specialRegions) <- make.names (names (specialRegions))

#library (stringr  )

#str_replace_all(string, “[^[:alnum:]]”, “”))



#for (curR in unique (gaulList.dt$REG_ANNEX5)){
# specialRegions[ ]
#}
#REG_ANNEX5 
#gaulList.dt [ A]
#allData.dt[ ISO3 == "PAK" & VarName == "Production" & Animal == "Buffalo"]

#allData.dt[ ISO3 == "PAK" & VarName == "Production" & Animal == "Cattle"]

#TODO add country names using the ISO3 Code
curReg <- "EU27"
#str (cait.dt)
#str (allData.dt)
#allData.dt[ VarName == "Historial Emissions"]$ISO3

#allData.dt[VarName == "Historial Emissions" & ISO3 %in% specialRegions[ curReg][[1]]]
curReg <- "Russian Federation"
regionID <- 1000
for (curReg in names (specialRegions)){
  curAgg.dt <- allData.dt[ISO3 %in% specialRegions[ curReg][[1]] , sum (V1,na.rm= T), by = .(VarName, Animal, HerdType, LPS,Item, Element, Element2, GWP )]
  curAgg.dt[, ISO3_num:= -1]
  curAgg.dt[, ISO3:= as.character(regionID )]
  curAgg.dt[, COUNTRY:= curReg]
  #curAgg.dt[ VarName== "Historical Emissions"]
  unique (curAgg.dt$VarName)
  regionID <- regionID + 1
  allData.dt <- rbindlist (list (allData.dt, curAgg.dt), use.names = T, fill = T)
  
}

unique (allData.dt$COUNTRY)

allData.dt[ COUNTRY == "Russian Federation"]

allData.dt[ COUNTRY == "Low Income Economies" & VarName == "Emissions"]
allData.dt[ COUNTRY == "World" & VarName == "Emissions"]

allData.dt[ COUNTRY == "Asia" & VarName == "Emissions"]

allData.dt[ COUNTRY == "Northern Africa" & VarName == "Emissions"]

str (allData.dt[ COUNTRY == "Asia" & VarName == "Emissions"])

str (allData.dt[ COUNTRY == "Northern Africa" & VarName == "Emissions"])

# make named list for UI so that aggregation is done by ISO3, not by region name.
contChoices <- unique (allData.dt[ COUNTRY %in% gaulList.dt$CONTINENT] [, ISO3, COUNTRY])$ISO3
names (contChoices) <- unique (allData.dt[ COUNTRY %in% gaulList.dt$CONTINENT] [, ISO3, COUNTRY])$COUNTRY

gleamChoices <- unique (allData.dt[ COUNTRY %in% gaulList.dt$GLEAM.REGION] [, ISO3, COUNTRY])$ISO3
names (gleamChoices) <- unique (allData.dt[ COUNTRY %in% gaulList.dt$GLEAM.REGION] [, ISO3, COUNTRY])$COUNTRY

faoChoices <- unique (allData.dt[ COUNTRY %in% gaulList.dt$FAO.REGION] [, ISO3, COUNTRY])$ISO3
names (faoChoices) <- unique (allData.dt[ COUNTRY %in% gaulList.dt$FAO.REGION] [, ISO3, COUNTRY])$COUNTRY

wbChoices <- unique (allData.dt[ COUNTRY %in% gaulList.dt$WORLD.BANK.REGION] [, ISO3, COUNTRY])$ISO3
names (wbChoices) <- unique (allData.dt[ COUNTRY %in% gaulList.dt$WORLD.BANK.REGION] [, ISO3, COUNTRY])$COUNTRY

otherChoices <- unique (allData.dt[ COUNTRY %in% c("World", "EU27", "OECD")] [, ISO3, COUNTRY])$ISO3
names (otherChoices) <- unique (allData.dt[ COUNTRY %in%  c("World", "EU27", "OECD")] [, ISO3, COUNTRY])$COUNTRY

regionNamesISO <- c (contChoices, gleamChoices, wbChoices, otherChoices, faoChoices)
names(regionNamesISO) [ regionNamesISO == "1000"]
#otherChoices <- unique (allData.dt[ COUNTRY %in% ("World", "OECD", "EU27", [, ISO3, COUNTRY])$ISO3
#names (wbChoices) <- unique (allData.dt[ COUNTRY %in% gaulList.dt$WORLD.BANK.REGION] [, ISO3, COUNTRY])$COUNTRY




# allData.dt[  VarName== "Historial Emissions"]
# 
# 
# unique (allData.dt$COUNTRY)
# unique (allData.dt[ !(is.na (ISO3_num))]$COUNTRY)
# 
# unique (allData.dt[ VarName == "Emissions"]$Item)
allData.dt[ VarName == "Emissions" , isDirect:= F]

allData.dt[ VarName == "Emissions" & Item %in% c("Manure-CH4", "Manure-N2O", "EntericFermentation"), isDirect:= T]

# convert production to tons
allData.dt[ VarName == "Production" , V1:= V1 / 1000]
# convert emissions to tons
allData.dt[ VarName == "Emissions" , V1:= V1 / 1000]


#allData.dt[ VarName == "Production" & Item == "Eggs" & COUNTRY== "World" & Element == "Weight", sum (V1)]/1e6

#allData.dt[ VarName == "Production" & ISO3 == "PAK" & Animal == "Buffalo" & Element == "Protein" & Item == "Milk", sum (V1)/1e6]
#allData.dt[ VarName == "Production" & ISO3 == "PAK" & Animal == "Buffalo" & Element == "Weight" & Item == "Milk", sum (V1)/1e6]


SpecialRegions <- unique (allData.dt[ ISO3_num == -1]$COUNTRY)
#Commodities <-  unique (allData.dt[ VarName == "Production"]$Item)
Ruminants <- c("Cattle", "Buffalo", "Goats", "Sheep")
Monogastrics <-  c("Pigs", "Chicken")
Animals <- unique (allData.dt$Animal)
LPS <- unique (allData.dt$LPS)
EmissionSources <- unique (allData.dt[ VarName == "Emissions"]$Item)

#allData.dt[COUNTRY== "World" & VarName == "Emissions", sum (V1 * GWP)/1e9,by= Animal ]  # OK!
#allData.dt[COUNTRY== "World" & VarName == "Emissions", sum (V1 * GWP)/1e9 ]  # OK!

#allData.dt[COUNTRY== "World" & VarName == "Emissions", sum (V1 * GWP)/1e9,by= .( Element, Animal, LPS) ] [order (-Element)]

#allData.dt[COUNTRY== "World" & VarName == "Historial Emissions"] 




if (isPublicVersion ==T) {
  allData.dt <- allData.dt [ COUNTRY %in% SpecialRegions]
} 
Countries <- unique  (allData.dt$COUNTRY [ !(allData.dt$COUNTRY %in% SpecialRegions) ])



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
  
  ret <- paste("","<h2>", mainCaption, "for ", regionName, " in ", baseYear, "</h2>")  
  
  if (regionName %in% c(names (SpecialRegions))){
    ret <- paste("","<h2>", mainCaption, "for the  ",regionName, "region in ", baseYear, "</h2")  
  }
  if (regionName == "World") {
    ret <- paste("","<h2>", "Global", tolower (mainCaption), "in ", baseYear, "</h2>")  
    
  }
  ret
  
}

#Animal <- c("Cattle", "Buffalo", "Goats", "Sheep", "Pigs", "Chicken", "Chicken","Cattle", "Buffalo", "Goats", "Sheep" )
#Commodity <- c("Milk", "Milk", "Milk", "Milk", "Meat", "Meat", "Eggs", "Meat", "Meat", "Meat", "Meat")
#proteinContent <- c(0.034, 0.039, 0.039, 0.058, 0.2, .19, .124, .2113, .2113, .1929, .2013)
#proteinContent.dt <- data.table (Animal = Animal, Commodity = Commodity, proteinContent = proteinContent)




smallruminants <- c( "Goats", "Sheep")






infoTextProductionStatistis <- "Output of raw animal products (meat, milk, and eggs) as well as expressed in total protein, as calculated by GLEAM and harmonized with FAOSTAT. Further information available
at "
infoTextEmissions <- "Total greenhouse gas emissions. Direct emissions refer to emissions from manure and enteric fermentation, indirect emissions include emissions to feed and other inputs as well as postfarm emissions. For further details see the "
infoTextProduction <- "Total production of animal products, expressed in tonnes of meat/milk/egg, protein of meat/milk/eggs/ and carcass weight. Data are calculated by GLEAM and harmonized with "
infoTextProductionSystem <- "Total herd size by species and production system. Based on Gridded Livestock of the World (GLW) 3, edition 2015. Further details available at "

infoTextEmissionIntensity <- "Emission intensity, emisssion per unit of output. Diagrams show the distribution of emission intentities for selected production systems in all countries (
weighted by production), the global average (blue line), and the value for a selected country (red line). Further information available at "

animalChoices <-  c (
  "Pigs" = "Pigs",
  "Chicken" = "Chicken",
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
curAnimals <- c("Chicken")
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
    curProd.dt <- allData.dt[  Animal %in% curAnimals & VarName == "AnimalNumbers"  & HerdType %in% curHerds &   LPS %in% curLPS, sum (V1, na.rm = T),by= c("COUNTRY", herdNodes)]
    
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


#
# curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
# curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
# curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
# selNodes <- NULL
# selCommodity <- "Meat"
# selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
# bySource <- F
# selUnit <- "Protein"
# 
# is.null(selNodes)
# 
# xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, selUnit = NULL )
#  xx[ COUNTRY== 'World']
# 
#  selNodes <- "Animal"
#  curAnimals <-unique (allData.dt[VarName == "Emissions"]$Animal)
#  curHerds <- unique (allData.dt[VarName == "Emissions"]$HerdType)
#  curLPS <- unique (allData.dt[VarName == "Emissions"]$LPS)
#  selNodes <- NULL
#  selCommodity <- "Milk"
#  selSources <- unique (allData.dt[VarName == "Emissions"]$Item)
#  bySource <- F
#  selUnit <- "Protein"
#  
#  is.null(selNodes)
#  
#  xx <- getEmissionIntensityAll_2 (curAnimals, curHerds, curLPS, selNodes, selCommodity, selSources, bySource= F, selUnit = NULL )
#  xx[ COUNTRY== 'World']
#  
# 

# getEmissionIntensityAll <- function (  curVarName , curAnimals,curHerds, curLPS, curCommodities,curEmSources,curUnits, emNodes){
#   
#   #emNodes <-c ("Animal", "LPS", "Item", "Element2")
#   #emNodes <-c ("Animal", "HerdType", "Commodity")
#   cat ("Intensity for ",curHerds, "\n" )
#    
#  # if (curUnits == "Weight"){
# #    curUnits <- c("CarcassWeight", "Weight")
# #  }
#   
#   if (curVarName == "AnimalNumbers"){
#     curProd.dt <- allData.dt[  Animal %in% curAnimals & VarName == "AnimalNumbers"  & LPS %in% curLPS]
#     
#   }else{
#     curProd.dt <- allData.dt [ Animal %in% curAnimals &  VarName == curVarName & HerdType %in% curHerds & LPS %in% curLPS & Item %in% 
#                                  curCommodities & Element %in% curUnits, sum (V1, na.rm = T), 
#                                by= .(Animal, HerdType, LPS, COUNTRY, Item, Element)]
#     
#   }
#   setnames (curProd.dt, "Element", "Unit")
#   setnames (curProd.dt, "Item", "Commodity")
#    
#   emNodes <- emNodes[ emNodes != ""]
#   curProd.dt <- curProd.dt[, sum (V1,na.rm = T),by= c("COUNTRY", emNodes)]
#   
#   curEm.dt <- allData.dt [ Animal %in% curAnimals &  VarName == "Emissions"  & HerdType %in% curHerds & LPS %in% curLPS & Element2 %in% curCommodities & Item %in% curEmSources, 
#                            sum (V1* GWP , na.rm = T), by= .(Animal, HerdType,LPS, COUNTRY, Item, Element, Element2)]
#   
#    
#   #curEm.dt[ COUNTRY== "World", sum (V1 ),by= .(Commodity, Animal)]
#   
#   
#   setnames (curEm.dt, "Element2", "Commodity")
#   setnames (curEm.dt, "Element", "Gas")
#   setnames (curEm.dt, "Item", "EmissionSource")
#   
#   mergeNames <- intersect(names (curEm.dt), names (curProd.dt))
#   mergeNames <- mergeNames[  mergeNames != "V1"]
#   
#   
#   curEm.dt <- curEm.dt[, sum (V1, na.rm = T),by= c("COUNTRY", emNodes)]
# 
#   
#   curIntt.dt <- merge (curProd.dt, curEm.dt, by=mergeNames)
#   
#   
#   setnames (curIntt.dt, "V1.x", "Weight")
#   setnames (curIntt.dt, "V1.y", "Emissions")
#   
#   #ret.dt <- curIntt.dt[,  sum (Emissions, na.rm = T) / sum(Weight, na.rm = T),by = c("COUNTRY", emNodes)]
#   #ret.dt[ COUNTRY== "World"]
#   
#   ret.dt <- curIntt.dt[,{ Emission = sum (Emissions, na.rm = T) ; Production = sum (Weight, na.rm= T); Intensity= sum (Emissions,na.rm = T) / sum(Weight, na.rm = T);
#   list(Emission = Emission, Production = Production, Intensity= Intensity)}, by=c("COUNTRY", emNodes)]
#     return (ret.dt)
# }
# 
# 
# 
# 
# 





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

