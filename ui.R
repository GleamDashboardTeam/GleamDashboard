source ("plottingFunctions.R")
source ("landingPageText.R")

gitHubImageDir <- "https://raw.githubusercontent.com/GleamDashboardTeam/GleamDashboard/main/images/"

tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")

dbTitle=div(img(src= file.path (gitHubImageDir, "logo_fao.png"), height= 70), "GLEAM 3 dashboard")
dbTitle= "GLEAM 3 dashboard"
lp_Box <- function (titleStr, subTitleStr, imageScr,mainText, footerText, buttonID, buttonStr ){
  
  ret <- userBox(
    title = userDescription(
      title = titleStr,
      image = imageScr,
      subtitle = subTitleStr,
      type = 2),
    footer = mainText,
    collapsed= F,
    height= 250,
    actionButton(buttonID, buttonStr, class="btn-primary" )
  )
  
}

tags$body(includeHTML(("html/googleAnalytics_body.html")))

header <- dashboardHeader(titleWidth = 850)
anchor <- tags$a(href='http://www.fao.org',
                 tags$img(src=file.path (gitHubImageDir, "logo_fao.png"), height='73'), style = "color: white; font-size: 32px;font-weight: normal;
                 font-family: Open Sans,sans-serif",
                 '   GLEAM 3 dashboard')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color:  #5792c9l; color: #FFFFFF }"))),
  anchor,
  class = 'name')


shinyUI <- shinyUI (
  
  dashboardPage( 

    header,

        
#############################################################################
################################## SIDEBAR ##################################
#############################################################################

dashboardSidebar(collapsed = F,

  includeCSS("www/gleam.css"),

  tags$head(includeHTML(("html/googleAnalytics_header.html"))),
                     
  sidebarMenu( id  = 'sidebar',
                                  
    tags$hr(style="border-color: grey;"),
                     
    menuItem("Home", icon = icon("home", lib = "font-awesome"), tabName ="tab_home2",
      badgeLabel = "", badgeColor = "black"),

    if (isPublicVersion == T){
      selectInput("selectCountry", tags$span(style="color: white;","Area"),
                  multiple = F, selected = "World", 
                  choices = list(World = worldRegions, Continents = contRegions, 'FAO Regions'   = faoRegions, 
                                 'GLEAM Regions' = gleamRegions, 'SDGs Regions'  = sdgRegions, 'World Bank Regions' = wbRegions,
                                 'EU27 Countries' = eu27Regions, 'LDC Countries'  = ldcRegions, 'LLDC Countries' = lldcRegions, 
                                 'OECD Countries' = oecdRegions, 'SIDS Countries' = sidsRegions
                  ),
      )
    } else {
      selectInput("selectCountry", tags$span(style="color: white;","Area"),
                  multiple = F, selected = "World", 
                  choices = list(World = worldRegions, Continents = contRegions, 'FAO Regions'   = faoRegions, 
                                 'GLEAM Regions' = gleamRegions, 'SDGs Regions'  = sdgRegions, 'World Bank Regions' = wbRegions,
                                 'EU27 Countries' = eu27Regions, 'LDC Countries'  = ldcRegions, 'LLDC Countries' = lldcRegions, 
                                 'OECD Countries' = oecdRegions, 'SIDS Countries' = sidsRegions,
                                 'Countries' = countries
                  ),
      )
    },

      

                                  
    selectInput("selectGWPSet", tags$span(style="color: white;", "GWP100 set"),
      multiple = F, selected = "AR6", GWPChoices
    ),

    tags$hr(style="border-color: grey;"),
                                  
    menuItem( text = HTML("<img src='", paste0(gitHubImageDir, "Livest_population.png"), "' alt='Logo' style='height: 30px;'> Animal Population"), tabName = "tab_population"),
    menuItem( text = HTML("<img src='", paste0(gitHubImageDir, "Livest_prod.png"), "' alt='Logo' style='height: 30px;'> Animal Products"), tabName = "tab_production" ),
    menuItem( text = HTML("<img src='", paste0(gitHubImageDir, "3_total.png"), "' alt='Logo' style='height: 30px;'> Emissions"), tabName = "tab_emissions" ),
    menuItem(text = HTML("<img src='", paste0(gitHubImageDir, "4_gas_meat.png"), "' alt='Logo' style='height: 30px; margin-right: 0px'> Emission Intensities"), tabName = "tab_emissionIntensity" ),
                                  
    tags$hr(style="border-color: grey;"),
                                  
    menuItem(HTML("&nbsp;&nbsp;<i class='fas fa-chart-simple   chart-icon' style='font-size: 24px;'></i>&nbsp;&nbsp; Compare Regions"), tabName = "tab_comparison"),
    menuItem(HTML("&nbsp;&nbsp;<i class='fas fa-database       database-icon' style='font-size: 24px;'></i>&nbsp;&nbsp; Input Data"), tabName = "tab_inputData"),
    menuItem(HTML("&nbsp;&nbsp;<i class='fas fa-globe-americas globe-icon' style='font-size: 24px;'></i>&nbsp;&nbsp; Regions"), tabName = "tab_Regions"),
    
    tags$hr(style="border-color: grey;"),
    
    menuItem(HTML("&nbsp;&nbsp;<i class='fas fa-circle-info    circle-icon' style='font-size: 24px;'></i>&nbsp;&nbsp; About GLEAM"), tabName = "tab_about"),
    menuItem(HTML("&nbsp;&nbsp;<i class='fas fa-cash-register  cash-register-icon' style='font-size: 24px;'></i>&nbsp;&nbsp; Change Log"), tabName = "tab_changes")

  )
), 
    

#######################################################################
############################### BODY ##################################
#######################################################################

    dashboardBody(
      tags$body(includeHTML(("html/googleAnalytics_body.html"))),
      tags$head(
        tags$style(HTML("
        
          .main-header .logo:hover { background-color: #0073B3 !important; }
          
          .main-header .sidebar-toggle { display: none !important; }
        
          .custom-action-button { background-color: transparent; border: none; padding: 0; }
     
          .box { border-color: #0073B3 !important; }
        
          #compbox1 .box-header, #compbox2 .box-header, #compbox3 .box-header { background-color: #0073B3 !important; }
        
          #compbox2 .checkbox { margin-left: 200px; }

          .custom-action-button:hover { outline: none; background-color: transparent; }
        
          .custom-box { margin-bottom: 20px; text-align: center; }

          .upper-part { background-color: #fff; padding: 10px; }
        
          .lower-part { background-color: #0073B3; color: white; padding: 10px; }
        
       ")),
        
      tags$link(rel = "shortcut icon", href ="favicon-16x16.ico")),
      
      # tags$style(type="text/css",
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: hidden; }"
      # ),
      
      title = dbTitle,
      # create popover using shinyBS package - on hover the infobox
      #There must be at least one shinyBS component in the UI of your app in order for the n
      
      bsPopover(id="infoHerdSizeAll", title = "Median", 
                content = "Median price of diamonds", 
                trigger = "hover", 
                placement = "top",
                options = list(container="body")),
      
      
########################################################################################################################################################################\
########################################################### INPUT DATA ###############################################################################################     
      
      tabItems(
        tabItem(
          tabName ="tab_inputData",
          
          h1("Input Data For GLEAM 3"),
          uiOutput ("inputDatSelAnimal"),
          
          h3 ("Herd Parameters"),
          DT::dataTableOutput('inputDataHerdParameters') %>% withSpinner(color="#0A97D9"), 
          
          h3 ("Manure Management Systems"),
          DT::dataTableOutput('inputDataMMS') %>% withSpinner(color="#0A97D9"),
          
          h3 ("Live Weights"),
          DT::dataTableOutput('inputDataLiveWeight') %>% withSpinner(color="#0A97D9"), 
          
          h3 ("Dry Matter Intake"),
          DT::dataTableOutput ('inputDryMatterIntakeSummary') %>% withSpinner(color="#0A97D9"),
          
        ),
        
        
########################################################################################################################################################################\
########################################################### EMISSIONS ###############################################################################################     
        
        tabItem(
          tabName = "tab_emissions",
          
          tabPanel("Emissions",
                   fluidRow(
                     column(12,
                            div(style = "display: inline-block;", h1("Emissions"),
                                h3(htmlOutput("emission_caption"))
                            ),
                     )
                   ),

                   valueBoxOutput("infoEmissionsTotal", width = 3),
                   valueBoxOutput("infoEmissionsTotFrac", width = 3),
                   valueBoxOutput("infoEmissionsRank", width = 3),

                   fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions by species"))),
                                   actionButton("help_emissions", label = icon("info-circle"),
                                                style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
                   
                   checkboxInput("inCheckDirectOnly", label = "Direct emissions only",value = F, width = "100%"),

                   column (12,
                           checkboxGroupInput ("em_SelAnimalsAll", "Species", inline = T, choices =sort ( c(Monogastrics, Ruminants )), selected =c(Monogastrics, Ruminants )) ,
                           
                           tabBox(
                             title = "Emission sources",
                             # The id lets us use input$tabset1 on the server to find the current tab
                             side = "right",
                             selected = "Sankey",
                             width = NULL,
                             id = "tabset1",#, height = "250px",

                             tabPanel("Treemap",
                                      
                                      checkboxGroupInput ("em_selectTreeMapNodes", "Nodes", inline = T, choiceNames = c( "Animal", "Production system", "Emission source" ), 
                                                          choiceValues = c( "Animal", "LPS", "Item"), selected = c("Animal")),
                                      
                                      plotOutput(outputId = "treeMapEmissions" )
                                      
                             ),
                             tabPanel("Sankey",
                                      
                                      checkboxGroupInput ("em_selectSankeyNodesAll", "Nodes", inline = T, choiceNames = c( "Herd", "Production system", "Commodity", "Source", "Gas"), 
                                                          choiceValues = c( "HerdType", "LPS", "Commodity", "EmissionSource", "Gas"), selected = c("Gas")),
                                      
                                      sankeyNetworkOutput ("SankeyEmissionAllSpecies",width = "100%"),
                                      
                             ),
                             tabPanel("Pie",
                                      
                                      plotlyOutput("emissionPieChartAll"),
                                      
                             )
                           )
                   ),
                   
                   fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions Map"))),
                                   actionButton("help_emissions_map", label = icon("info-circle"),
                                                style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
                   
                   htmlOutput("HiHFrameEmissions") %>% withSpinner(color="#0A97D9"),

          ), # tabPanel emission
        ),
        
#########################################################################################################################################
########################################################### LANDING PAGE ################################################################   
#########################################################################################################################################

tabItem(
  tabName = "tab_home2",
  tags$h3("Welcome to the GLEAM dashboard", style = "font-size: 32px;  margin-top: 10px; margin-left: 10px;"),
  tags$br(),
  
  
  box(
    style = "margin-top: 10px;", 
    width = NULL, status = "primary", solidHeader = FALSE, 
    title = tags$h3("Livestock emission data at a glance", style = "font-size: 28px; margin-top: 0px; margin-left: 20px;"),
    tags$div(style = "font-size: 16px;  margin-left: 20px; margin-right: 20px;", 
             
             HTML(paste0("The GLEAM dashboard is an interactive web-application presenting data 
             produced by the Global Livestock Environmental Assessment Model ", 
             tags$a(href="http://www.fao.org/gleam/en/","(GLEAM)"), 
             " a project within the Animal Production and Health (NSA) division 
             of the Food and Agriculture Organisation of the United Nations (FAO). 
             The data is organised and can be accessed through four main pages: 
             Animal Population, Animal Products, Emissions, and Emission Intensities. 
             Additional pages can be found to provide supplementary information 
             regarding the use of this dashboard. This platform intends to support FAO 
             Members when assessing environmental impacts of livestock, such as Greenhouse 
             Gas (GHG) emissions, and to explore additional data sets related 
             to livestock supply chains. Selected input and output data from the GLEAM 
             can be downloaded within the Input Data and Emission Intensities pages, 
             with the intent to support further processing and analysis related to 
             research contributing to a more sustainable livestock sector. 
             All data displayed is based on GLEAM 3, with reference year of 2015.", br(),
             "To start, please select one of the pages below."))
             ),

    tags$br(),
    tags$br(),

    fluidRow(style = "margin-left: 10px; margin-right: 10px;",
    
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Animal Population", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_population_new", 
            label = HTML("<img src='", paste0(gitHubImageDir, "Livest_population.png"), "' alt='Icon' style='width: 150px; height: 110px;'>"),
            class = "custom-action-button"
          ),
          
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Data on the population of different livestock species, 
               herd, and production system. Animal population estimates are based on the Gridded Livestock of the World
               <a href=https://www.fao.org/livestock-systems/global-distributions/en/> GLW4</a>.</p>")
          
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Animal Products", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_production_new", 
            label = HTML("<img src='", paste0(gitHubImageDir, "Livest_prod.png"), "' alt='Icon' style='width: 150px; height: 110px;'>"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Production of milk, meat, and eggs</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Data for livestock products (meat, milk and eggs) by species, 
               herd, production system and commodity, per unit of protein, and weight. </p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Emissions", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_emissions_new", 
            label = HTML("<img src='", paste0(gitHubImageDir, "3_total.png"), "' alt='Icon' style='width: 150px; height: 110px;'>"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Direct and indirect emissions</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Estimated GHG emissions from the entire livestock supply chain by species, 
               herd, production system, commodity, source, and gas.  </p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Emission Intensities", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_emIntensity_new", 
            label = HTML("<img src='", paste0(gitHubImageDir, "4_gas_meat.png"), "' alt='Icon' style='width: 150px; height: 110px;'>"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Emissions related to production</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Estimated GHG emissions per unit of livestock product and stock by species, herd, 
               and production system. Data is downloadable at a chosen geographic level.</p>")
        )
      )
    )
  ),

  
  box(
    width = NULL, status = "primary", solidHeader = FALSE,
    title = tags$h3("Explore with more detail", style = "font-size: 28px;  margin-top: 0px; margin-left: 20px;"),
    tags$br(),
      
    fluidRow(style = "margin-left: 10px; margin-right: 10px;",

      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Compare Regions", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_comparison_new",
            label = icon("chart-simple", class = "fa-7x"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Select several regions and compare them</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>A tool to compare animal populations, animal products,
               emissions and emission intensities between multiple regions. </p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Input Data", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_inputdata_new", 
            label = icon("database", class = "fa-7x"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Browse the input data of GLEAM</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Selected input data for the model, by species, 
               herd and production systems. Data is downloadable at a chosen geographic level. </p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Regions", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_regions_new", 
                       label = icon("globe-americas", class = "fa-7x"),
                       class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Maps of the different world regions</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Regions for which the GLEAM data can be aggregated, 
               based on geographic, economic or administrative criteria. </p>")
        )
      )
      
    )
  ),
  
  
  box(
    width = NULL, status = "primary", solidHeader = FALSE,
    title = tags$h3("Supplementary information", style = "font-size: 28px;  margin-top: 0px; margin-left: 20px;"),
    tags$br(),
    
    fluidRow(style = "margin-left: 10px; margin-right: 10px;",
             
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("About GLEAM", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_aboutgleam_new", 
            label = icon("circle-info", class = "fa-7x"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Learn more about GLEAM</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Background information about GLEAM, the underlying data, 
               methods and parameters, and the differences in methodology between versions of the model. </p>")
        )
      ),
     
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Change Log", style = "font-size: 26px; margin-top: 0px;"),
          actionButton("switch_tab_changes_new", 
            label = icon("cash-register", class = "fa-7x"),
            class = "custom-action-button"
          ),
          #HTML("<h4 style='margin-top: 10px; font-size: 18px;'>Review all current changes</h4>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Reference list describing all recent changes made to the dashboard.</p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          h3("Pathways Report", style = "font-size: 26px; margin-top: 0px;"),
          tags$a(href = "https://doi.org/10.4060/cc9029en", target = "_blank",
                 img(src = paste0(gitHubImageDir, "PathwaysCoverSmaller.png"),
                     width = 425*0.33, height = 602*0.33,
                     style = "border: 1px solid black; margin-left: 0px; margin-right: 0px; font-size: 10px"),
          ),
          # HTML("<p style='font-size: 14px; margin-top: 20px;'>Pathways Towards Lower Emissions - A global assessment of the 
          #      greenhouse gas emissions and mitigation options from livestock agrifood systems. </p>")
        )
      ),
      
      column(
        width = 3,
        div(
          class = "custom-box",
          br(), 
          br(),
          br(),
          #h3("Pathways Report", style = "font-size: 26px; margin-top: 0px;"),
          HTML("<p style='font-size: 14px; margin-top: 20px; margin-left: 0px; text-align: left;font-style: italic;'>Pathways Towards Lower Emissions - A global assessment of the 
         greenhouse gas emissions and mitigation options from livestock agrifood systems. </p>")
        )
      )
      

      
    )
  )
),

#########################################################################################################################################
########################################################### COMPARISON TOOL #############################################################   
#########################################################################################################################################      
        
tabItem(

  tabName = "tab_comparison",
  tags$h3("Compare regions against each other", style = "font-size: 32px;  margin-top: 10px; margin-left: 10px;"),
  tags$br(),
  
  if (isPublicVersion == T){
  box(
    collapsible = T,
    title = "Select regions", id = "compbox1", status = "primary", solidHeader = TRUE, width = NULL,
    tags$br(),
    column(
      width =6,
      
      selectizeInput("selectCountry_comp",  label = NULL,
                       choices = list(
                         World = worldRegions, Continents = contRegions, 'FAO Regions' =  faoRegions, 'GLEAM Regions' = gleamRegions,
                         'SDGs Regions'  = sdgRegions, 'World Bank Regions' = wbRegions, 'EU27 Countries'  = eu27Regions,
                         'LDC Countries' = ldcRegions, 'LLDC Countries' = lldcRegions, 'OECD Countries' = oecdRegions, 'SIDS Countries' = sidsRegions
                       ),
                     
                     selected = c("WORLD1", "CONTINE4"), multiple = TRUE,
                     options = list( maxItems = 4, 'plugins' = list('remove_button'), 'create' = TRUE, 'persist' = FALSE),
      )
      )
    )} else {
    box(
      collapsible = T,
      title = "Select regions", id = "compbox1", status = "primary", solidHeader = TRUE, width = NULL,
      tags$br(),
      column(
        width =6,
        selectizeInput("selectCountry_comp",  label = NULL,
                         choices = list(
                           World = worldRegions, Continents = contRegions, 'FAO Regions' =  faoRegions, 'GLEAM Regions' = gleamRegions,
                           'SDGs Regions'  = sdgRegions, 'World Bank Regions' = wbRegions, 'EU27 Countries'  = eu27Regions,
                           'LDC Countries' = ldcRegions, 'LLDC Countries' = lldcRegions, 'OECD Countries' = oecdRegions, 'SIDS Countries' = sidsRegions, 
                           'Countries' = countries
                           ),
                       
                       selected = c("WORLD1", "CONTINE4"), multiple = TRUE,
                       options = list( maxItems = 4, 'plugins' = list('remove_button'), 'create' = TRUE, 'persist' = FALSE),
        ),
                       selected = c("WORLD1", "CONTINE4"), multiple = TRUE,
                       options = list( maxItems = 4, 'plugins' = list('remove_button'), 'create' = TRUE, 'persist' = FALSE)
        )
      )
  },

  box(
    tags$br(),
    collapsible = T,
    title = "Select species, herd type and production system", id = "compbox2", status = "primary", solidHeader = TRUE, width = NULL,
    div(
      style = "margin-left: 20px;",
      checkboxGroupInput("selectAnimals_comp", label = "Species",
        choices = sort(c(Ruminants, Monogastrics)), inline = T,
        selected = c(Monogastrics, Ruminants)
      )
    ),

    div(
      style = "margin-left: 40px;",
      conditionalPanel("(input.selectAnimals_comp.length >= 1) &&
                       ((input.selectAnimals_comp.includes('Cattle')) ||
                        (input.selectAnimals_comp.includes('Goats')) ||
                        (input.selectAnimals_comp.includes('Sheep')) ||
                        (input.selectAnimals_comp.includes('Buffalo'))) ",
                       #uiOutput("HerdType_comp"))
                       checkboxGroupInput("selectHerd_comp", "Herd type of ruminants", choices = c("Beef", "Dairy"), selected = c("Beef", "Dairy"), inline = T)
      )
    ),

    div(
      style = "margin-left: 40px;",

      conditionalPanel("(input.selectAnimals_comp.length >= 1) &&
                       ((input.selectAnimals_comp.includes('Cattle')) ||
                        (input.selectAnimals_comp.includes('Goats')) ||
                        (input.selectAnimals_comp.includes('Sheep')) ||
                        (input.selectAnimals_comp.includes('Buffalo'))) ",
                       #uiOutput("LPS_comp"))
                       checkboxGroupInput("selectLPS_comp1", "Production system of ruminants", choices =  c("Feedlots", "Grassland", "Mixed"),
                                          selected = c("Feedlots", "Grassland", "Mixed"), inline = T)
      ),

      conditionalPanel("(input.selectAnimals_comp.length >= 1) &&
                        (input.selectAnimals_comp.includes('Chickens'))",
                       checkboxGroupInput("selectLPS_comp2", "Production system of chickens", 
                                          #choices =  c("Backyard", "Broiler", "Layer"),
                                          choices = c("Backyard" = "Backyard_chickens", "Broiler" = "Broiler", "Layer" = "Layer"),
                                          selected = c("Backyard_chickens", "Broiler", "Layer"), inline = T)
      ),
      
      conditionalPanel("(input.selectAnimals_comp.length >= 1) &&
                        (input.selectAnimals_comp.includes('Pigs'))",
                       checkboxGroupInput("selectLPS_comp3", "Production system of pigs", 
                                          # choices =  c("Backyard", "Industrial", "Intermediate"), 
                                          choices = c("Backyard" = "Backyard_pigs", "Industrial" = "Industrial", "Intermediate" = "Intermediate"),
                                          selected = c("Backyard_pigs", "Industrial", "Intermediate"), inline = T)
      )
    )
  ),

  box(
    title = "Results", id = "compbox3", status = "primary", solidHeader = TRUE, width = NULL,
    tags$br(),
    tabBox(
      width = 12,
      title = NULL,
      id = "tabset1",


      tabPanel("Animal Population",

        plotlyOutput("AnimalPopulations_Bars_comp")

      ),


      tabPanel("Animal Products",

        tags$br(),

        checkboxGroupInput("prod_products_comp", label = "Product",
          choices = c("Meat", "Milk", "Eggs"), inline = T,
          selected = c("Meat", "Milk", "Eggs")),

        radioButtons("prod_unit_comp", "Unit", choices = c("Product weight", "Protein weight"),
          inline = T, selected = c("Product weight")),

        radioButtons("prod_node_comp", "Group results by", choices = c("Species", "Herd type", "Production system", "Product"),
          inline = T, selected = c("Species")),

        plotlyOutput("AnimalProducts_Bars_comp")

      ),


      tabPanel("Emissions",

        tags$br(),

        checkboxGroupInput("em_products_comp", label = "Product",
          choices = c("Meat", "Milk", "Eggs"), inline = T,
          selected = c("Meat", "Milk", "Eggs")),

        checkboxGroupInput("em_ghg_comp", label = "Greenhouse Gas",
          choices = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)"), inline = T,
          selected = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)")),

        div(
          style = "margin-left: 20px;",

          conditionalPanel("(input.em_ghg_comp.includes('Carbon dioxide (CO2)'))",
            checkboxGroupInput("em_sources_CO2_comp", label = "Emission source for Carbon dioxide (CO2)",
              choices  = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm"), inline = T,
              selected = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm"))
          ),

          conditionalPanel("(input.em_ghg_comp.includes('Methane (CH4)'))",
            checkboxGroupInput("em_sources_CH4_comp", label = "Emission source for Methane (CH4)",
              choices  = c("Feed", "Manure", "Enteric fermentation"), inline = T,
              selected = c("Feed", "Manure", "Enteric fermentation"))
          ),

          conditionalPanel("(input.em_ghg_comp.includes('Nitrous oxide (N2O)'))",
            checkboxGroupInput("em_sources_N2O_comp", label = "Emission source for Nitrous oxide (N2O)",
              choices  = c("Feed", "Manure"), inline = T,
              selected = c("Feed", "Manure"))
          )
        ),

        radioButtons("em_node_comp", "Group results by", choices = c("Species", "Herd type", "Production system", "Product", "Greenhouse gas", "Emission source"),
          inline = T, selected = c("Species")),

        plotlyOutput("Emissions_Bars_comp")

      ),


      tabPanel("Emission Intensities",

        tags$br(),

        checkboxGroupInput("emint_products_comp", label = "Product",
          choices = c("Meat", "Milk", "Eggs"), inline = T,
          selected = c("Meat", "Milk", "Eggs")),

        checkboxGroupInput("emint_ghg_comp", label = "Greenhouse Gas",
          choices = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)"), inline = T,
          selected = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)")),

        div(
          style = "margin-left: 20px;",

          conditionalPanel("(input.emint_ghg_comp.includes('Carbon dioxide (CO2)'))",
            checkboxGroupInput("emint_sources_CO2_comp", label = "Emission source for Carbon dioxide (CO2)",
              choices  = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm"), inline = T,
                selected = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm")
            )
          ),

          conditionalPanel("(input.emint_ghg_comp.includes('Methane (CH4)'))",
            checkboxGroupInput("emint_sources_CH4_comp", label = "Emission source for Methane (CH4)",
              choices  = c("Feed", "Manure", "Enteric fermentation"), inline = T,
              selected = c("Feed", "Manure", "Enteric fermentation")
            )
          ),

          conditionalPanel("(input.emint_ghg_comp.includes('Nitrous oxide (N2O)'))",
            checkboxGroupInput("emint_sources_N2O_comp", label = "Emission source for Nitrous oxide (N2O)",
              choices  = c("Feed", "Manure"), inline = T,
              selected = c("Feed", "Manure")
            )
          )
        ),

        radioButtons("emint_unit_comp", "Unit", choices = c("Emissions per animal", "Emissions per kg of product", "Emissions per kg of protein"),
                     inline = T, selected = c("Emissions per animal")),

        radioButtons("emint_node_comp", "Group results by", choices = c("Species", "Herd type", "Production system", "Product", "Greenhouse gas", "Emission source"),
                            inline = T, selected = c("Species")),

        plotlyOutput("EmissionIntensities_Bars_comp")

      ),

    )
  )
),

########################################################################################################################################################################\
########################################################### POPULATION ###############################################################################################
        
if (isPublicVersion == T) {
  tabItem( 
            tabName = "tab_population", h1("Animal Population"),
            h3(htmlOutput("productionsystem_caption")),
            valueBoxOutput("infoHerdSizeBUF", width =2),
            valueBoxOutput("infoHerdSizeCTL", width =2),
            valueBoxOutput("infoHerdSizeSHP", width =2),
            valueBoxOutput("infoHerdSizeGTS", width = 2),
            valueBoxOutput("infoHerdSizePGS", width = 2),
            valueBoxOutput("infoHerdSizeCHK", width = 2),

            checkboxGroupInput("pop_SelectAnimals", "Species",
                               choices = sort ( c(Ruminants, Monogastrics)), inline = T, selected =c(Monogastrics, Ruminants)),
            
            checkboxGroupInput("pop_SelectHerdType", "Herds",
                               choices = c("Beef", "Dairy"), inline = T, selected = c("Beef", "Dairy")),

            plotlyOutput("animalNumberAll"),
            
            fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Animal Population Map"))),
                            actionButton("help_population_map", label = icon("info-circle"),
                                         style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
            
            htmlOutput(  "HiHFramePopulation") %>% withSpinner(color="#0A97D9"),)
        }else{
          tabItem( 
            tabName = "tab_population", h1("Animal Population"),
            h3(htmlOutput("productionsystem_caption")),
            valueBoxOutput("infoHerdSizeBUF", width = 2),
            valueBoxOutput("infoHerdSizeCTL", width = 2),
            valueBoxOutput("infoHerdSizeSHP", width = 2),
            valueBoxOutput("infoHerdSizeGTS", width = 2),
            valueBoxOutput("infoHerdSizePGS", width = 2),
            valueBoxOutput("infoHerdSizeCHK", width = 2),
            valueBoxOutput("infoHerdSizeCML", width = 2),
                       
            checkboxGroupInput("pop_SelectAnimals", "Species",
                               choices = sort ( c(Ruminants, Monogastrics)), inline = T, selected =c(Monogastrics, Ruminants)),
            
            checkboxGroupInput("pop_SelectHerdType", "Herds",
                               choices = c("Beef", "Dairy"), inline = T, selected = c("Beef", "Dairy")),
     
            plotlyOutput("animalNumberAll"),
            
            fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Animal Population Map"))),
                            actionButton("help_population_map", label = icon("info-circle"),
                                         style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
    
            htmlOutput(  "HiHFramePopulation") %>% withSpinner(color="#0A97D9"),)
        },

        
#######################################################################################################################################################################
################################################ EMISSION INTENSITY ###################################################################################################
        
        tabItem(
          tabName = "tab_emissionIntensity",
          h1("Emission Intensities"),
          h3(htmlOutput("emissionintensity_caption")),
          valueBoxOutput("infoIntensityMeat", width = 3),
          valueBoxOutput("infoIntensityMilk",width = 3),
          valueBoxOutput("infoIntensityEggs", width =3 ),
          valueBoxOutput ("infoIntensityAll", width = 3),

          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Emissions intensities of animal products"))),
                          actionButton("help_emissionintensity", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          
          checkboxGroupInput("int_selAnimals", "Species",
                             choices = sort ( c(Monogastrics, Ruminants)), inline = T, selected =c(Monogastrics, Ruminants)),
          
          
          uiOutput ("EmIntensityHerdType"),

          uiOutput("EmIntensityLPS"),
          radioButtons("emIntensityCommodityOrAnimal", "Reference", choiceName = c("Animal", "Commodity"), choiceValues =   c("Animal", "Commodities" ), inline = T,selected = "Commodities"),
          
          fluidRow(
            column(12,
                   div(style = "display: inline-block;", 
                       uiOutput("EmIntensityCommodity")
                   ),
                   div(style = "display: inline-block; margin-left: 20px; margin-right: 20px; vertical-align: -3px;", 
                       uiOutput ("EmIntensityCommodityAsProtein")
                   )
            )
          ),
         
          uiOutput ("EmIntensity_emSource"),
          checkboxGroupInput("EMIntensity_Nodes","Nodes", choiceValues =   c("Animal", "HerdType", "LPS" ), 
                             choiceNames = c("Species", "Herd", "Production system" ), selected = "Animal", inline = T ),
          checkboxInput("emIntensityBySource", "By sources", value= F ),
          

          tabBox(
            title = "Emission intensities",
            # The id lets us use input$tabset1 on the server to find the current tab
            side = "right",
            selected = "Bar chart",
            id = "tabset1", width= "100%",

            
            tabPanel("Bar chart",
                    
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
          
        ),
        
#################################################################################################################################################################
############################################################# PRODUCTION #########################################################################################

        tabItem(
          tabName = "tab_production",
          h1("Animal Products"),
          
          h3(htmlOutput("production_caption")),

          valueBoxOutput("infoProdMeatTotal", width = 3),
          valueBoxOutput("infoProdMilkTotal", width=3),
          valueBoxOutput("infoProdEggsTotal", width = 3),
          valueBoxOutput ("infoProdProteinTotal", width = 3),

          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Production of animal products"))),
                          actionButton("help_production", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          checkboxGroupInput("prod_selAnimals", "Species",
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
                                        choiceNames = c("Species", "Herd", "Production system", "Commodity"),
                                        selected = c("Animal", "Item")), 
                     
                     sankeyNetworkOutput ("SankeyNetwork3D_ProductionAll",width = "100%")
            ),
            tabPanel("Bar",
                     fluidRow(column(6, 
                                     radioButtons("prod_selCommoditiesBar", "Product",
                                                  choices = c("Meat", "Milk", "Eggs", "Total protein"), inline = T, selected = c("Total protein"))),
                     ),
            
                     plotlyOutput("ProductionBarChartAll") %>% withSpinner(color="#0A97D9")
            )
          ),
          
          fluidRow(column(12, div(style="display: inline-block;",tags$h3(("Animal Products Map"))),
                          actionButton("help_production_map", label = icon("info-circle"),
                                       style = "padding:10px; color: #000000; background-color: #ffffff00; border-color: #ffffff00"))),
          
          htmlOutput(  "HiHFrameProduction") %>% withSpinner(color="#0A97D9"),
        ),

        
########################################################################################################################################################################\
########################################################### REPORT ###############################################################################################     

        tabItem(
          tabName = "tab_report",
          h3(htmlOutput ("report_caption")),
          "The GLEAM dashboard creates downloadable reports with figures, tables, and analysis of key data sets that where produced by GLEAM or that serverd as input to the model. 
                                  The country report report summarizes input data, as well as key figures and tables from the dashboard in one document. Reports are created on-the-fly, depending on the selected country or region 
                                  , this can take a minute or two.",
          br(),
          br(),

          radioButtons('country_report_format', 'Choose format', c('PDF', 'HTML', 'Word'), selected= "HTML", inline = TRUE),
          downloadButton("downloadCountryReport", label = "Create report",class = "download_this"),
          " ",
          br(),
          br()
        ),
        

################################################################# DATA Section ##########################################################################                              
        
        # tabItem(
        #   h1("Data Access"),
        #   tabName = "tab_data", h3("GLEAM data for the 2015 base year"),
        #   "Here, you find the aggregated data, aggregated to countries and regions for the year 2015, available for browsing, filtering, sorting, and download. Depending on the size of the table 
        #               and your computer, the download might take a minute or two.",
        #   br(),
        #   hr(),
        # 
        #   checkboxGroupInput(inputId = "Data_selAnimals",label = "Animals" ,choices= sort (unique (allData.dt[VarName == "Emissions"]$Animal)), selected = "Cattle", inline = T),
        #   tags$hr(style="border-color: grey;"),
        #   uiOutput ("Data_EmIntensityHerdType"),
        #   uiOutput("DataEmIntensityLPS"),
        #   
        #   uiOutput("DataEmIntensityCommodity"), checkboxInput("dataAccess_UnitProtein", label= "as protein", value = T),
        #   uiOutput ("DataEmIntensity_emSource"),
        #   checkboxGroupInput("Data_selNodes","Nodes", choiceValues =   c("Animal", "HerdType", "LPS" ), inline=T,
        #                      choiceNames = c("Species", "Herd", "Production system" ), selected = "Animal"),
        #   checkboxInput("Data_emIntensityBySource", "By sources", value= T ),
        # 
        #   br(), 
        #   br(),
        # ),

        tabItem(
          tabName = "tab_about", 
          box(width = NULL, status = "primary", solidHeader = FALSE,
              title = tags$h2("About GLEAM", style = "font-size: 28px;  margin-top: 0px; margin-left: 20px"),
              tags$div(includeHTML("html/aboutGLEAM.html"), style = "margin-top: 5px; margin-left: 20px;")
          )
        ),
        
        tabItem(
          tabName = "tab_changes", 
          box(width = NULL, status = "primary", solidHeader = FALSE,
              title = tags$h3("Change log", style = "font-size: 28px;  margin-top: 0px; margin-left: 20px;"),
              tags$div(includeHTML("html/changeLog.html"), style = "margin-top: 5px; margin-left: 20px;")
          )
        ),

        tabItem(
          tabName = "tab_Regions", 
          includeHTML("html/regions.htm")
        )
        
      ),
      
      div(class = "footer", 
          includeHTML("www/footer.html")
      ),
      
      useShinyjs() 
    
    ), # dashboard body
    
  )
)

