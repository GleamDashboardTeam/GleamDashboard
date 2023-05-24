
source ("plottingFunctions.R")
source ("landingPageText.R")
# gitHubImageDir <- "https://raw.githubusercontent.com/dommens/GleamPublicData/master/"
# gitHubImageDir <- "https://raw.githubusercontent.com/GleamDashboardTeam/GleamDashboard/images/"
gitHubImageDir <- "https://github.com/GleamDashboardTeam/GleamDashboard/images/"


tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
#source ("infoBoxes.R")

#tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/dommens/GleamPublicData/master/favicon-32x32.ico"))

#https://github.com/dommens/GleamPublicData/raw/master/favicon-32x32.png



dbTitle=div(img(src= paste0 (gitHubImageDir, "logo_fao.png"), height= 70), "GLEAM V3.0 dashboard v 1.0")
dbTitle= "GLEAM V3.0 dashboard v 1.0"
lp_Box <- function (titleStr, subTitleStr, imageScr,mainText, footerText, buttonID, buttonStr ){
  
  ret <- userBox(
    title = userDescription(
      title = titleStr,
      image = imageScr,
      subtitle = subTitleStr,
      type = 2),
    footer = mainText,
    collapsed= T,
    height= 250,
    actionButton(buttonID, buttonStr, class="btn-primary" )
  )
  
  # ret <- userBox(
  #   title = titleStr,
  #   subtitle = subTitleStr,
  #   #type = 2,
  #   src = imageScr,
  #   color = "primary",
  #   height = 250,
  #   mainText,
  #   footer = actionButton(buttonID, buttonStr, class="btn-primary" ),
  #   collapsible = F
  #   #br(),
  #   #actionButton(buttonID, buttonStr, class="btn-primary" ),
  # )
  # return (ret)
}

#https://github.com/dommens/GleamPublicData/blob/master/logo_fao.png

#dbHeader <- dashboardHeader(title = "GLEAM V3.0 dashboard", titleWidth = 600,
#                            tags$li(a(href = 'http://fao.org',
#                                      tags$img(src =paste0 (gitHubImageDir, "logo_fao.png"), height= 70    ,
#                                          title = "FAO", height = "50px"),
#                                      style = "padding-top:0px; padding-bottom:0px"),
#                                    class = "dropdown"))


header <- dashboardHeader(titleWidth = 850)
anchor <- tags$a(href='http://www.fao.org',
                 tags$img(src=paste0 (gitHubImageDir, "logo_fao.png"), height='73'), style = "color: white; font-size: 32px;font-weight: normal;
                 font-family: Open Sans,sans-serif",
                 '   GLEAM v3.0 dashboard')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color:  #5792c9l; color: #FFFFFF }"))),
  anchor,
  class = 'name')


shinyUI <- shinyUI (
  
  dashboardPage( 
    
    
    header,
    dashboardSidebar(collapsed = F,
                     includeCSS("www/gleam.css"),
                     tags$head(includeHTML(("googleAnalytics.html"))),
                     sidebarMenu( id  = 'sidebar',
                                  
                                  
                                  menuItem("Home", icon = icon("home", lib = "font-awesome"), tabName ="tab_emissions",
                                           badgeLabel = "", badgeColor = "black"),
                                  
                                  
                                  # ,style="text-align:center;color:#FFA319;font-size:100%")      
                                  # div (id = 'd_selCountry',
                                  
                                  
                                  selectInput("selectCountry", tags$span(style="color: white;","Area"),
                                              multiple = F, selected = "World", 
                                              # list (choices = list (Region = list (c(names (SpecialRegions), "World")) ,
                                              choices = list(
                                                World = worldRegions,
                                                Continents = contRegions,
                                                'GLEAM Regions' = gleamRegions,
                                                'FAO Regions' =  faoRegions, 
                                                'World Bank Regions' = wbRegions,
                                                'OECD Regions' =oecdRegions, 
                                                EU27  =eu27Regions, 
                                                Country= countries
                                                ),
                                                
                                                
                                                
                                                #Country =  (sort (Countries))), 
                                              selectize = T)
                                  
                                  #,  
                                  #tags$style(type="text/css", "d_selCountry {color: red}"))
                                  ,
                                  
                                  
                                  menuItem("Animal Population",   tabName ="tab_population",
                                           badgeLabel = "", badgeColor = "black"),
                                  
                                  menuItem("Animal Products",  tabName ="tab_production",
                                           badgeLabel = "", badgeColor = "black"),
                                  
                                  # menuItem("Greenhouse Gas Emissions",icon =  icon("cloud", lib = "font-awesome"),
                                  menuItem("Emissions",   tabName = "tab_emissions"),
                                  menuItem("Emission Intensities",   tabName =  "tab_emissionIntensity"),
                                  
                                  
                                  
                                  
                                  
                                  
                                  selectInput("selectGWPSet", tags$span(style="color: white;", "GWP100 set"),
                                              multiple = F, selected = "AR6", GWPChoices),
                                  #actionButton("help_emissions_map", label = icon("info-circle")),
                                  #tags$hr(style="border-color: white;"),
                                  #menuItem("Data Access",  tabName = "tab_data",
                                  #         badgeLabel = "", badgeColor = "red")
                                  #hl(),
                                  
                                  menuItem("Input data",   tabName =  "tab_inputData"),
                                  
                                  tags$hr(style="border-color: grey;"),
                                  
                                  menuItem("About GLEAM v3 data",  tabName = "tab_about",
                                           badgeLabel = "", badgeColor = "green"),
                                  
                                  menuItem("Release notes",  tabName = "tab_readme",
                                           badgeLabel = "", badgeColor = "green"),
                                  
                                  
                                  menuItem("Change log",  tabName = "tab_changes",
                                           badgeLabel = "", badgeColor = "green"),
                                  
                                  menuItem("Regions", tabName =   "tab_Regions",
                                           badgeLabel = "", badgeColor = "green")
                                  
                                  # menuItem("Release notes",  tabName = "tab_readme",
                                  #           badgeLabel = "", badgeColor = "green")
                                  
                                  
                                  
                     )
    ), 
    #   sidePanel (textOutput("selCountry")),
    
    dashboardBody(
      
      tags$head(tags$link(rel = "shortcut icon", href ="favicon-16x16.ico")),
      title = dbTitle,
      # create popover using shinyBS package - on hover the infobox
      #There must be at least one shinyBS component in the UI of your app in order for the n
      bsPopover(id="infoHerdSizeAll", title = "Median", 
                content = "Median price of diamonds", 
                trigger = "hover", 
                placement = "top",
                options = list(container="body")),
      tabItems(
        tabItem(
          tabName ="tab_inputData",
          h1("Input data for GLEAM v3"),
          uiOutput ("inputDatSelAnimal"),
          h3 ("Herd parameters"),
          DT::dataTableOutput('inputDataHerdParameters') %>% withSpinner(color="#0A97D9"), 
          h3 ("Manure management systems"),
          DT::dataTableOutput('inputDataMMS') %>% withSpinner(color="#0A97D9"),
          h3 ("Live weights"),
          
          DT::dataTableOutput('inputDataLiveWeight') %>% withSpinner(color="#0A97D9"), 
          h3 ("Dry matter intake"),
          
          DT::dataTableOutput ('inputDryMatterIntakeSummary') %>% withSpinner(color="#0A97D9"),
          #textOutput("txtSelCountries")
        ),
        tabItem(
          tabName = "tab_emissions",
          
          
          tabPanel("Emissions",
                   fluidRow(
                     column(12,
                            div(style = "display: inline-block;", h1("Emissions"),
                                h3(htmlOutput("emission_caption"))
                            ),
                            
                            # div(style = "display: inline-block;margin-left: 100px; margin-right: 10px; ", 
                            #      numericInput("emInGWPNO2",label = "N2O GWP", value = 298, min = 265, max = 298,step = 1,width = "70px")
                            # ),
                            #  div(style = "display: inline-block;", 
                            #      numericInput("emInGWPCH4",label = "CH4 GWP", value = 34, min = 20, max = 86,step = 1, width = "70px")
                            #  )
                            
                            
                     )
                   ),
                   
                   
                   
                   fluidRow (
                     # column (4, h3(htmlOutput("emission_caption"))),
                     #     column (8, checkboxInput("inCheckDirectOnly",label = "Direct emissions only (Manure and enteric fermantation)", value = F ))
                   ),
                   
                   
                   valueBoxOutput("infoEmissionsTotal", width = 3),
                   valueBoxOutput("infoEmissionsTotFrac", width = 3),
                   
                   valueBoxOutput("infoEmissionsRank", width = 3),
                   # valueBoxOutput("infoEmissionsTotFracDirect"),
                   
                   
                   # myPopovers,
                   # tags$hr(style="border-color: lightblue;"),
                   
                   
                   fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions by species"))),
                                   actionButton("help_emissions", label = icon("info-circle"),
                                                style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
                   
                   checkboxInput("inCheckDirectOnly", label = "Direct emissions only",value = F, width = "100%"),
                   #fluidRow(
                   column (12,
                           checkboxGroupInput ("em_SelAnimalsAll", "Animal", inline = T, choices =sort ( c(Monogastrics, Ruminants )), selected =c(Monogastrics, Ruminants )) ,
                           #checkboxGroupInput("em_selectHerd", )
                           
                           tabBox(
                             title = "Emission sources",
                             # The id lets us use input$tabset1 on the server to find the current tab
                             side = "right",
                             selected = "Sankey",
                             width = NULL,
                             id = "tabset1",#, height = "250px",
                             # tabPanel("About",
                             #          h4('details on the data.. ')),
                             
                             #tabPanel("Table",
                             #          DT::dataTableOutput('tableEmissionsRumin') %>% withSpinner(color="#0A97D9")
                             # ),
                             
                             
                             tabPanel("Treemap",
                                      
                                      checkboxGroupInput ("em_selectTreeMapNodes", "Nodes", inline = T, 
                                                          choiceNames = c( "Animal", "Production system", "Emission source" ), 
                                                          choiceValues = c( "Animal", "LPS", "Item"),
                                                          selected = c("Animal")),
                                      
                                      #checkboxInput ("em_TreeMapPercent", label = "Percentage", value = T),
                                      
                                      
                                      #   downloadButton ("downloadSankeyEmissions", "Download graph"),
                                      #checkboxGroupInput ("em_selectSankeyNodesAll", "Select nodes", inline = T, choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), 
                                      #                    choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                      #        selected = c(   "Gas")),
                                      
                                      #choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                      
                                      #   checkboxGroupInput("em_selectSankeyNodesRumin", "Select nodes", inline= T,choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                      #                       choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), selected = c( "Herd", "Gas")),
                                      plotOutput(outputId = "treeMapEmissions" )
                                      # sankeyNetworkOutput ("SankeyEmissionAllSpecies",width = "100%"),
                                      #   sankeywheelOutput("SankeyWheelEmissionRumin", width = "100%", height = "400px"),
                                      
                             ),
                             tabPanel("Sankey",
                                      #   downloadButton ("downloadSankeyEmissions", "Download graph"),
                                      checkboxGroupInput ("em_selectSankeyNodesAll", "Nodes", inline = T, choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), 
                                                          choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                                          selected = c(   "Gas")),
                                      
                                      #choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                      
                                      #   checkboxGroupInput("em_selectSankeyNodesRumin", "Select nodes", inline= T,choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                                      #                       choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), selected = c( "Herd", "Gas")),
                                      
                                      sankeyNetworkOutput ("SankeyEmissionAllSpecies",width = "100%"),
                                      #   sankeywheelOutput("SankeyWheelEmissionRumin", width = "100%", height = "400px"),
                                      
                             ),
                             tabPanel("Pie",
                                      
                                      plotlyOutput("emissionPieChartAll"),
                                      
                             )
                             
                           )
                   ),
                   
                   fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions Map"))),
                                   actionButton("help_emissions_map", label = icon("info-circle"),
                                                style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
                   
                   
                   htmlOutput(  "HiHFrameEmissions") %>% withSpinner(color="#0A97D9"),
                   #checkboxGroupInput ("em_SelAnimalsAll", "Select Species", inline = T, choices = c(Monogastrics, Ruminants), selected = Ruminants) ,
                   #checkboxGroupInput ("em_selectSankeyNodesAll", "Select nodes", inline = T, choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), 
                   #                    choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"),
                   #                    selected = c(   "Gas")),
                   #sankeyNetworkOutput ("SankeyEmissionAllSpecies",width = "100%")
                   
                   
          ), # tabPanel emission
          
          
          
        ),
        
        
        
        tabItem(
          tabName = "tab_home",
          h2("Welcome to the GLEAM dashboard"),
          h3("Livestock emission data at a glance"),
          lpTextWelcome,
          tags$br(),
          tags$br(),
          
          fluidRow (
            
            lp_Box("Livestock Population", "Herd size of different animals",
                   paste0 (gitHubImageDir,"Livest_population.png"),
                   lpTextPopulation,  "fooT", "switch_tab_population", "Explore"),
            
            
            lp_Box("Production of Animal Products", "Production of milk, meat, and eggs",paste0 (gitHubImageDir,"Livest_prod.png"), 
                   lpTextProduction, "fooT", "switch_tab_production", "Explore"),
            
            
          ),
          
          fluidRow(
            
            lp_Box("GHG Emissions", "Direct and indirect emissions from the sector",paste0 (gitHubImageDir,"3_total.png"), lpTextEmissions, "fooT", "switch_tab_emissions", "Explore"),
            
            lp_Box("Emissions Intensity", "Emissions related to production",paste0 (gitHubImageDir,"4_gas_meat.png"), lpTextEmIntensity, "fooT", "switch_tab_emIntensity", "Explore"),
            
            
          ),
          
          
        ),
        ########################################################################################################################################################################\
        ########################################################### POPULATION ###############################################################################################
        
        tabItem(
          tabName = "tab_population", h1("Animal Population"),
          h3(htmlOutput("productionsystem_caption")),
          valueBoxOutput("infoHerdSizeBUF", width =2),
          valueBoxOutput("infoHerdSizeCTL", width=2),
          valueBoxOutput("infoHerdSizeSHP", width=2),
          valueBoxOutput("infoHerdSizeGTS", width = 2),
          valueBoxOutput("infoHerdSizePGS", width = 2),
          valueBoxOutput("infoHerdSizeCHK", width = 2),
          
          checkboxGroupInput("pop_SelectAnimals", "Animals",
                             choices = sort ( c(Ruminants, Monogastrics)), inline = T, selected =c(Monogastrics, Ruminants)),
          
          checkboxGroupInput("pop_SelectHerdType", "Herds",
                             choices = c("Beef", "Dairy"), inline = T, selected = c("Beef", "Dairy")),
          
          
          
          plotlyOutput("animalNumberAll"),
          
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Animal Population Map"))),
                          actionButton("help_population_map", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          
          
          
          htmlOutput(  "HiHFramePopulation") %>% withSpinner(color="#0A97D9"),
          
        ),
        
        #######################################################################################################################################################################
        ################################################ EMISSION INTENSITY ###################################################################################################
        
        tabItem(
          tabName = "tab_emissionIntensity",
          # h3("emission intensity")
          h1("Emission Intensities"),
          h3(htmlOutput("emissionintensity_caption")),
          # column (6, checkboxInput("inCheckDirectOnly",label = "Direct emissions only", value = F ))
          
          #h3(htmlOutput("emissionintensity_caption")) ,
          valueBoxOutput("infoIntensityMeat", width = 3),
          valueBoxOutput("infoIntensityMilk",width = 3),
          valueBoxOutput("infoIntensityEggs", width =3 ),
          valueBoxOutput ("infoIntensityAll", width = 3),
          #  valueBoxOutput("two"),
          # myPopovers,
          # tags$hr(style="border-color: blue;"),
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions intensity of animal products"))),
                          actionButton("help_emissionintensity", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          
          checkboxGroupInput("int_selAnimals", "Animals",
                             choices = sort ( c(Monogastrics, Ruminants)), inline = T, selected =c(Monogastrics, Ruminants)),
          
          #fluidRow(column(6,
          uiOutput ("EmIntensityHerdType"),
          #br(),
          #br(),
          #column(6, 
          uiOutput("EmIntensityLPS"),
          radioButtons("emIntensityCommodityOrAnimal", "Reference", choiceName = c("Animal", "Commodity"), choiceValues =   c("Animal", "Commodities" ), inline = T,selected = "Commodities"),
          #fluidRow(column (6,))
          fluidRow(
            column(width = 4,
                   # uiOutput("EmIntensityCommodity")
            ),
            column(width = 4, 
                   #       uiOutput ("EmIntensityCommodityAsProtein")
            )
            
          ),
          
          fluidRow(
            column(12,
                   div(style = "display: inline-block;", 
                       uiOutput("EmIntensityCommodity")
                   ),
                   div(style = "display: inline-block; margin-left: 20px; margin-right: 20px; vertical-align: -3px;", 
                       uiOutput ("EmIntensityCommodityAsProtein")
                   )
                   #div(style = "display: inline-block;", 
                   #     sliderInput("periods", "Number of periods", min = 1, max = 10,  value = 1)
                   # )
            )
          ),
          #checkboxInput("emIntensityAccess_UnitProtein", label= "as protein", value = T),
          
          uiOutput ("EmIntensity_emSource"),
          checkboxGroupInput("EMIntensity_Nodes","Nodes", choiceValues =   c("Animal", "HerdType", "LPS" ), 
                             choiceNames = c("Species", "Herd", "Production system" ), selected = "Animal", inline = T ),
          checkboxInput("emIntensityBySource", "By sources", value= F ),
          
          
          
          
          
          #),
          #column (6, 
          #checkboxInput("inSyncScales",label = "sync x scales",  value = F))
          #),
          #fluidRow(
          tabBox(
            title = "Emission intensity",
            # The id lets us use input$tabset1 on the server to find the current tab
            side = "right",
            selected = "Bar chart",
            id = "tabset1", width= "100%",
            #tabPanel("About",
            #          h4('details on the data.. ')),
            # tabPanel("Table",
            #          DT::dataTableOutput('tableIntensityRumin') %>% withSpinner(color="#0A97D9"),
            # ),
            # 
            
            tabPanel("Bar chart",
                     # fluidRow(
                     #   checkboxGroupInput("inSelectRuminants", "",
                     #                      choices = Ruminants, inline = T, selected =Ruminants),
                     
                     # ),
                     #uiOutput("inSelRuminantsLPS"),
                     #uiOutput("inSelRuminantsHerdType"),
                     # downloadButton('emIntensity_downloadPlot','Download Plot'),
                     plotlyOutput ("emIntensityBar")%>% withSpinner(color="#0A97D9"),
                     
            ),
            tabPanel("Data",
                     DT::dataTableOutput('DatatableIntensityAll_2') %>% withSpinner(color="#0A97D9")
                     
                     
            ),
            
            
            
            
          ),
          
          
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emission Intensity Map"))),
                          actionButton("help_emissionIntensity_map", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          
          
          
          htmlOutput(  "HiHFrameEmissionIntensity") %>% withSpinner(color="#0A97D9"),
          
          
          
          #leafletOutput( "rasterMapEmissionIntensity"),
          
        ),
        ################################################### EMISSIONS ##################################################################################################
        ############################################################################################################################################################
        ############################################################# PRODUCTION #########################################################################################
        #################################################################################################################################################################
        tabItem(
          tabName = "tab_production",
          h1("Animal Products"),
          
          h3(htmlOutput("production_caption")),
          #),
          #column (6,
          # )
          #),
          # h4 ("Total producion of animal products"),
          valueBoxOutput("infoProdMeatTotal", width = 3),
          valueBoxOutput("infoProdMilkTotal", width=3),
          valueBoxOutput("infoProdEggsTotal", width = 3),
          valueBoxOutput ("infoProdProteinTotal", width = 3),
          #  myPopovers_Prod,
          # tags$hr(style="border-color: lightblue;"),
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Production of animal products"))),
                          actionButton("help_production", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          checkboxGroupInput("prod_selAnimals", "Animal",
                             choices = sort (c(Ruminants, Monogastrics)), inline = T, selected = c(Ruminants, Monogastrics)),
          
          
          
          
          tabBox(
            title = "Production of animal products",
            # The id lets us use input$tabset1 on the server to find the current tab
            side = "right",
            selected = "Sankey",
            width = NULL,
            id = "tabset1",#, height = "250px",
            
            
            tabPanel("Sankey",
                     
                     fluidRow(column(6, 
                                     checkboxGroupInput("prod_selCommodities", "Product",
                                                        choices = c("Meat", "Milk", "Eggs"), inline = T, selected = c("Meat","Milk", "Eggs"))),
                              column (6,
                                      radioButtons ("prod_selCommodityUnit", "Unit", inline = T, choices = c( "Protein", "Weight") ),
                                      
                              )),
                     
                     
                     checkboxGroupInput("prod_selSankeyNodes", "Nodes", inline= T,
                                        choiceValues = c("Animal", "HerdType", "LPS", "Item"),  
                                        choiceNames = c("Animal", "Herd", "Production system", "Commodity"),
                                        selected = c("Animal", "Item")), 
                     
                     
                     sankeyNetworkOutput ("SankeyNetwork3D_ProductionAll",width = "100%")
                     
            ),
            tabPanel("Bar",
                     fluidRow(column(6, 
                                     radioButtons("prod_selCommoditiesBar", "Product",
                                                  choices = c("Meat", "Milk", "Eggs", "Total protein"), inline = T, selected = c("Total protein"))),
                              #column (6,
                              #        checkboxGroupInput ("prod_selCommodityBarHerd", "Select herd", inline = T, choices = c( "Dairy", "Beef"), selected = c("Dairy", "Beef") ),
                              
                              #)
                     ),
                     
                     # fluidRow (column (6,
                     #                   radioButtons("selectProdOutputProductRumin", "Select product",
                     #                                choices = c("Meat", "Milk")
                     #                                , selected = "Meat",
                     #                                inline = T, width = NULL,
                     #                   )),
                     #           column (6,
                     #                   checkboxGroupInput("inProdSelectAnimalsRuminantsHerdType", "",
                     #                                      choices = c("Beef", "Dairy"), inline = T, selected = c("Beef", "Dairy")),
                     #                   
                     #           )),
                     plotlyOutput("ProductionBarChartAll") %>% withSpinner(color="#0A97D9")
                     
            )
            
          ),
          
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Animal Products Map"))),
                          actionButton("help_production_map", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          htmlOutput(  "HiHFrameProduction") %>% withSpinner(color="#0A97D9"),
          
        ),
        
        tabItem(
          tabName = "tab_report",
          h3(htmlOutput ("report_caption")),
          "The GLEAM dashboard creates downloadable reports with figures, tables, and analysis of key data sets that where produced by GLEAM or that serverd as input to the model. 
                                  The country report report summarizes input data, as well as key figures and tables from the dashboard in one document. Reports are created on-the-fly, depending on the selected country or region 
                                  , this can take a minute or two.",
          br(),
          #  
          br(),
          
          
          
          radioButtons('country_report_format', 'Choose format', c('PDF', 'HTML', 'Word'), selected= "HTML", inline = TRUE),
          #   ),
          #  column (4,
          downloadButton("downloadCountryReport", label = "Create report",class = "download_this"),
          #         )
          #     ),
          #    h4("Country report"),
          " ",
          
          br(),
          
          
          
          br()
          
          
          
        ),
        
        # ################################################################# DATA Section ##########################################################################                              
        
        tabItem(
          h1("Data Access"),
          tabName = "tab_data", h3("GLEAM data for the 2015 base year"),
          "Here, you find the aggregated data, aggregated to countries and regions for the year 2015, available for browsing, filtering, sorting, and download. Depending on the size of the table 
                      and your computer, the download might take a minute or two.",
          br(),
          hr(),
          #checkboxGroupInput("worksnow", 
          #            label = tags$span("This works now too", bsButton("thisworks", label = "", icon = icon("info"), style = "info", size = "extra-small")),
          #            choices = c("a","b")
          #),
          checkboxGroupInput(inputId = "Data_selAnimals",label = "Animals" ,choices= sort (unique (allData.dt[VarName == "Emissions"]$Animal)), selected = "Cattle", inline = T),
          tags$hr(style="border-color: grey;"),
          uiOutput ("Data_EmIntensityHerdType"),
          uiOutput("DataEmIntensityLPS"),
          
          uiOutput("DataEmIntensityCommodity"), checkboxInput("dataAccess_UnitProtein", label= "as protein", value = T),
          uiOutput ("DataEmIntensity_emSource"),
          checkboxGroupInput("Data_selNodes","Nodes", choiceValues =   c("Animal", "HerdType", "LPS" ), inline=T,
                             choiceNames = c("Species", "Herd", "Production system" ), selected = "Animal"),
          checkboxInput("Data_emIntensityBySource", "By sources", value= T ),
          
          
          br(), 
          br(),
          #tabBox(
          #  title = "GLEAM data",
          # The id lets us use input$tabset1 on the server to find the current tab
          #  side = "right",
          #  selected = "Emission intensity",
          #  id = "tabset1", width= "100%",
          #tabPanel("About",
          #          h4('details on the data.. ')),
          # tabPanel("Table",
          #          DT::dataTableOutput('tableIntensityRumin') %>% withSpinner(color="#0A97D9"),
          # ),
          # 
          # tabPanel("Animal population",
          #           DT::dataTableOutput('DatatableIntensityAll_ByAnimal') %>% withSpinner(color="#0A97D9")
          
          # ),
          
          
          #tabPanel("Emissions",
          #         DT::dataTableOutput('tableEmissionsAll') %>% withSpinner(color="#0A97D9")
          
          #),
          
          #tabPanel("Emission intensity",
          #DT::dataTableOutput('tableIntensityAll') %>% withSpinner(color="#0A97D9")
          #tableIntensityAll
          # fluidRow(
          #   checkboxGroupInput("inSelectRuminants", "",
          #                      choices = Ruminants, inline = T, selected =Ruminants),
          
          # ),
          #uiOutput("inSelRuminantsLPS"),
          #uiOutput("inSelRuminantsHerdType"),
          #plotOutput("IntensityFrequencyAll") %>% withSpinner(color="#0A97D9"),
          
          
          #)
          
          
          #  ),
          #      downloadButton("downloadData", "Download")
        ),
        # tab_data
        
        # tabItem(
        #   tabName = "tab_playground", h2("GLEAM playground"),
        #   selectInput("ply_selCountry", "Select countries/regions", selected = "World", multiple =  T,
        #               choices =  list("Regions" = SpecialRegions, "Countries"= Countries )),
        #   
        #   
        #   
        #   checkboxGroupInput("ply_selVarName", "Select Variable", selected = "Production", 
        #                      choices =    c("Animal Numbers", "Production", "Emissions", "Emission Intensity")),
        #   checkboxGroupInput("ply_selAnimal", "Select animal", selected = "Pigs",
        #                      choices =  unique (allData.dt$Animal)),
        #   
        #   
        #   
        #   esquisse_ui(
        #     id = "esquisse", 
        #     header = FALSE, # dont display gadget title
        #     container = esquisseContainer(height = "700px")
        #   )
        #   
        # )
        
        tabItem(
          tabName = "tab_about", 
        #  h3("About"),
          includeHTML("html/aboutGLEAM.html")
        ),
        tabItem(
          tabName = "tab_readme" , 
         # h3("README")
        
          includeHTML("html/readme.html")
        ),
        
        tabItem(
          tabName = "tab_changes", 
        #  h3("ChangeLog")
          includeHTML("html/changeLog.html")
        ),
        tabItem(
          tabName = "tab_Regions", 
          #  h3("ChangeLog")
          includeHTML("html/regions.htm")
        )
        
        
      ),
      
      div (class = "footer", 
           includeHTML("www/footer.html")),
      #tags$footer("footer"  ),
      useShinyjs() 
      
    ), # dashboard body
    
    
  )
)

