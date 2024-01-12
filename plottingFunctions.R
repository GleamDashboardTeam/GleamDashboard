

plot_emissionIntensityBoxplotPlotly_2 <- function (
    selCountry,
    selAnimals , 
    selHerds  ,
    selLPS  ,
    selCommodity,  
    selNodes  ,
    bySource  ,
    selSources, 
    GWPCH4,
    GWPN2O,
    unitProtein)
{
  
  legendTitle <- ""
  if (bySource  == T ) {
    legendTitle <- "Emission source"
  }
  
  # # this is for the order and to set the colors of the bar ################
  # sourceOrder <- rev(c("Enteric fermentation", "Manure-CH4" ,"Feed-CH4", "Manure-N2O","Feed-N2O" ,"Feed-CO2" ,   "Land use change", 
  #                      "Direct on-farm energy",  "Pasture expansion", "Embedded on-farm energy", "Post-farm", "Total"))
  
  # this is for the order and to set the colors of the bar ################
  sourceOrder <- rev(c(emSources2Choose, "Total"))
  
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
  
  
  colorG <-data.frame (nodeName = "Total", color = "grey")
  
  if (bySource ==T){
    colorG <- makeSankeyColorsAllEmissionTypes()
    colorG <- colorG[ colorG$nodeName %in% selSources]
  } 
  colorG$nodeName <-factor (colorG$nodeName, levels = sourceOrder)
  dd.col <- colorG$color
  
  names (dd.col) <- colorG$nodeName
  f1 <- colorG %>% 
    arrange(factor(nodeName, levels = sourceOrder))
  f1
  dd.col <- f1$color
  
  names (dd.col) <- f1$nodeName
  # #########################
  yLabel <- "Emission Intensity [ kg CO₂eq / kg product]"
  if (setequal (unitProtein, "Protein")){
    yLabel <-  "Emission Intensity [ kg CO₂eq / kg protein]"
  }
  if (setequal (selCommodity , "per animal")){
    yLabel <-  "Emission Intensity [ kg CO₂eq / head]"
  }
  
  
  
  
  
  ret.dt <- getEmissionIntensityAll_2(selAnimals, selHerds,  selLPS, selNodes, selCommodity, 
                                      selSources, bySource,  GWPCH4,
                                      GWPN2O,unitProtein)
  ret.dt <- ret.dt[ Intensity >0]
  if (nrow (ret.dt) ==0){
    return (empty_plotGG("No plot available. \nPlease select a different combination"))
  }
  
  
  
  animals <- gleamColorAnimals.dt[, .SD[.N], Animal]$Animal
  animal.col <- gleamColorAnimals.dt[, .SD[1:1], Animal]$Color
  names(animal.col)  <- animals
  
  #herds <- gleamColorAnimals.dt[, .SD[.N], Animal]$
  #herds.col <- gleamColorAnimals.dt[, .SD[1:1], Animal]$Color
  #names(herds.col)  <- animals
  
  
  lps <- gleamColorAnimals.dt[, .SD[.N], LPS]$LPS
  lps.col <- gleamColorAnimals.dt[, .SD[1:1], LPS]$Color
  names(lps.col)  <- lps
  
  
  
  
  if (setequal(selNodes, c("Animal")) == T ) {
    cat ("Animals only\n")
    cat ("Details,", bySource, "\n")
    
    xx <- ret.dt[ ISO3 == selCountry & !(is.na (Intensity))]
    
    cOrder <- xx[, sum (Intensity), by= "COUNTRY"][ order (V1)]$COUNTRY
    
    xx$EMSource <- factor (xx$EmissionSource, levels = sourceOrder)
    xx <- data.frame(xx)
    gp <- ggplot (xx) + theme_bw()
    
    if (bySource == T){
      gp <- gp + geom_col (aes 
                           ( x = Animal ,y= Intensity, fill = EMSource,  
                             text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),    position = "stack", show.legend = F)
      gp <- gp + scale_fill_manual (values = dd.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      gp <- gp + scale_color_manual (values = dd.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
      
    }else{
      gp <- gp + geom_col (aes 
                           ( x = Animal ,y= Intensity, fill = Animal,  
                             text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),    position = "stack", show.legend = F)
      gp <- gp + scale_fill_manual (values = animal.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      gp <- gp + scale_color_manual (values = animal.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
      
    }
    gp
    gp <- gp + ylab (yLabel) + xlab ("")
    gp <- gp + theme(legend.title=element_blank())
    
    
    gp <- gp +  labs(fill=legendTitle) 
    gp <- gp + theme(legend.position = "none")
    # a hack to surpress the legend. GGPLOTLY ignore the  poistion = none option
    #for (i in 1:nrow(xx)){
    # gp$x$data[[i]]$text <- c(gp$x$data[[i]]$text, "") 
    # gp$x$data[[i]]$showlegend <- FALSE
    #}
    
    
    gp <- ggplotly (gp,tooltip = "text", )
    gp <-  hide_legend(gp)
    # gp
    return (gp)
  }
  
  
  
  if (setequal(selNodes, c("Animal", "HerdType")) == T ) {
    
    cat ("Animals  and Herds \n")
    cat ("Details,", bySource, "\n")
    
    xx <- ret.dt[ ISO3 == selCountry & !(is.na (Intensity))]
    
    cOrder <- xx[, sum (Intensity), by= "COUNTRY"][ order (V1)]$COUNTRY
    
    xx$EMSource <- factor (xx$EmissionSource, levels = sourceOrder)
    xx <- data.frame(xx)
    gp <- ggplot (xx) + theme_bw()
    if (bySource ==T){
      gp <- gp + geom_col (aes ( x = HerdType ,y= Intensity, fill = EMSource, color = EMSource, text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),  color = "white", position = "stack")
      gp <- gp + scale_fill_manual (values = dd.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      gp <- gp + scale_color_manual (values = dd.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
    }else{
      gp <- gp + geom_col (aes ( x = HerdType ,y= Intensity, fill = HerdType, color = HerdType, text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),  color = "white", position = "stack")
      #gp <- gp + scale_fill_manual (values = dd.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      #gp <- gp + scale_color_manual (values = dd.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
      
    }
    gp
    gp <- gp + ylab (yLabel) + xlab ("")
    gp <- gp + theme(legend.title=element_blank())
    
    gp <- gp +  labs(fill=legendTitle) 
    gp <- gp + facet_wrap(~Animal,scales= "free", strip.position = "right")
    #gp <- gp + theme(strip.clip = "off")
    gp <- gp + theme(panel.spacing = unit(1, "lines"))
    
    gp <- gp + theme(legend.position = "none")
    gp <- ggplotly (gp,tooltip = "text" )
    
    gp
    return (gp)
    
  }
  
  
  if (setequal(selNodes, c("Animal", "LPS")) == T  ) {
    cat ("Animals  and LPS \n")
    cat ("Details,", bySource, "\n")
    
    xx <- ret.dt[ ISO3 == selCountry & !(is.na (Intensity))]
    
    cOrder <- xx[, sum (Intensity), by= "COUNTRY"][ order (V1)]$COUNTRY
    
    xx$EMSource <- factor (xx$EmissionSource, levels = sourceOrder)
    xx <- data.frame(xx)
    gp <- ggplot (xx) + theme_bw()
    if (bySource == T){
      gp <- gp + geom_col (aes ( x = LPS,y= Intensity, fill = EMSource, color = EMSource, text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),  position = "stack")
      gp <- gp + scale_fill_manual (values = dd.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      gp <- gp + scale_color_manual (values = dd.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
    }else{
      gp <- gp + geom_col (aes ( x = LPS,y= Intensity, fill = LPS, color = LPS, text = paste (EMSource, ":", round (Intensity,2),"kg/kg")),  position = "stack")
      gp <- gp + scale_fill_manual (values = lps.col) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      gp <- gp + scale_color_manual (values =lps.col, name= NULL) #+ facet_wrap(~Animal,scales= "free", strip.position = "right")
      
    }
    gp
    gp <- gp + ylab (yLabel) + xlab ("")
    gp <- gp + theme(legend.title=element_blank())
    
    gp <- gp +  labs(fill=legendTitle) 
    gp <- gp + facet_wrap(~Animal,scales= "free", strip.position = "right")
    gp <- gp + theme(legend.position = "none")
    gp <- gp + theme(panel.spacing = unit(1, "lines"))
    
    
    gp <- ggplotly (gp,tooltip = "text" )
    gp
    return (gp)
  }
  
  return  (ggplotly (empty_plotGG("No plot available! \nPlease select a different combination.")))
  
  
  
  
}




plotTreeMapEmissions <- function (selCountry, selAnimals, isDirect, GWPN2O, GWPCH4){
  cat ("Treemap\n ")
  selNodes <-c( "Animal", "HerdType", "Item")
  
  data2plot <- allData.dt[VarName == "Emissions" & ISO3 ==  selCountry & Animal %in% selAnimals]
  
  if (isDirect){
    data2plot <- data2plot [isDirect == T]
  }
  
  data2plot[ Element == "CH4", GWP:= GWPCH4]
  data2plot[ Element == "N2O", GWP:= GWPN2O]
  
  data2plot <- data2plot [,sum (V1 * GWP), by= selNodes ]
  
  
  dd <- gleamColorAnimals.dt[, .SD[.N], Animal]$Animal
  dd.col <- gleamColorAnimals.dt[, .SD[1:1], Animal]$Color
  names(dd.col)  <- dd
  gp <- ggplot(data2plot, aes(area = V1/1e9, label = format(as.numeric(..area..), scientific = F,nsmall=2, digits= 1, big.mark=","), subgroup = Animal,
                              subgroup2 = HerdType, subgroup3 = Item, fill = Animal), alpha= 1) +
    geom_treemap() +
    geom_treemap_subgroup3_border(colour = "white", size = 1) +
    geom_treemap_subgroup2_border(colour = "grey10", size = 3) +
    geom_treemap_subgroup_border(colour = "grey10", size = 5) +
    geom_treemap_subgroup_text(place = "middle", colour = "black", alpha = 0.2, grow = T) +
    geom_treemap_subgroup2_text(colour = "blue", alpha = 0.5, fontface = "italic") +
    geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 1) +
    geom_treemap_text(colour = "white", place = "middle", reflow = T)
  
  gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
  gp 
}

plotTreeMapEmissions_2 <- function (data2plot, selNodes ){
  gleamColorAnimals.dt[, .SD[.N], Animal]
  gleamColorGas.dt
  dd <- gleamColorAnimals.dt[, .SD[.N], Animal]$Animal
  dd.col <- gleamColorAnimals.dt[, .SD[1:1], Animal]$Color
  names(dd.col)  <- dd
  if (length (unique (data2plot$animal) == 0)){
    gp <- ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = "No data for this combination.\nPlease select at least one species.") + 
      theme_void()
    
  }
  
  if ( setequal (selNodes, c("Animal", "LPS", "Item"))){
    
    data2plot[Item == "Feed-CO2", Item:= "Feed (CO2)"]
    data2plot[Item == "LandUseChange", Item:= "LUC: soy and palm (CO2)"]
    data2plot[Item == "DirectOnFarmEnergy", Item:= "Direct on-farm energy (CO2)"]
    data2plot[Item == "PastureExpansion", Item:= "LUC: pasture expansion (CO2)"]
    data2plot[Item == "EmbeddedOnFarmEnergy", Item:= "Embedded on-farm energy (CO2)"]
    data2plot[Item == "Postfarm", Item:= "Post-farm (CO2)"]
    data2plot[Item == "Feed-CH4", Item:= "Feed (CH4)"]
    data2plot[Item == "Manure-CH4", Item:= "Manure (CH4)"]
    data2plot[Item == "EntericFermentation", Item:= "Enteric fermentation (CH4)"]
    data2plot[Item == "Feed-N2O", Item:= "Feed (N2O)"]
    data2plot[Item == "Manure-N2O", Item:= "Manure (N2O)"]
    
    gp <- ggplot(data2plot, aes(area = V1/1e6, label = paste (format(as.numeric(..area..), scientific = F,nsmall=2, digits= 1, big.mark=","), "Mt"), subgroup = Animal,
                                subgroup2 = LPS, subgroup3 = Item, fill = Animal), alpha= 1) +
      geom_treemap() +
      geom_treemap_subgroup3_border(colour = "grey10", size = 1) +
      geom_treemap_subgroup2_border(colour = "black", size = 3) +
      geom_treemap_subgroup_border(colour = "grey10", size = 5) +
      geom_treemap_subgroup_text(place = "top", colour = "black", alpha = 0.2, grow = T) +
      geom_treemap_subgroup2_text(colour = "black", alpha = 0.5, fontface = "italic") +
      geom_treemap_subgroup3_text(place = "top", colour = "grey10", alpha = 1) +
      geom_treemap_text(colour = "grey10", place = "middle", reflow = T)
    
    gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
    gp  <-gp +  theme(legend.position="none")
    return (gp)
    
  }  
  if (setequal (selNodes, c("Animal",  "Item"))){
    
    data2plot[Item == "Feed-CO2", Item:= "Feed (CO2)"]
    data2plot[Item == "LandUseChange", Item:= "LUC: soy and palm (CO2)"]
    data2plot[Item == "DirectOnFarmEnergy", Item:= "Direct on-farm energy (CO2)"]
    data2plot[Item == "PastureExpansion", Item:= "LUC: pasture expansion (CO2)"]
    data2plot[Item == "EmbeddedOnFarmEnergy", Item:= "Embedded on-farm energy (CO2)"]
    data2plot[Item == "Postfarm", Item:= "Post-farm (CO2)"]
    data2plot[Item == "Feed-CH4", Item:= "Feed (CH4)"]
    data2plot[Item == "Manure-CH4", Item:= "Manure (CH4)"]
    data2plot[Item == "EntericFermentation", Item:= "Enteric fermentation (CH4)"]
    data2plot[Item == "Feed-N2O", Item:= "Feed (N2O)"]
    data2plot[Item == "Manure-N2O", Item:= "Manure (N2O)"]
    
    gp <- ggplot(data2plot, aes(area = V1/1e6, label = paste (format(as.numeric(..area..), scientific = F,nsmall=2, digits= 1, big.mark=","), "Mt"), subgroup = Animal,
                                subgroup2 = Item, fill = Animal), alpha= 1) +
      geom_treemap() +
      #geom_treemap_subgroup3_border(colour = "white", size = 1) +
      geom_treemap_subgroup2_border(colour = "black", size = 3) +
      geom_treemap_subgroup_border(colour = "grey10", size = 5) +
      geom_treemap_subgroup_text(place = "top", colour = "black", alpha = 0.2, grow = T) +
      geom_treemap_subgroup2_text(colour = "black", alpha = 0.5, fontface = "italic") +
      #geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 1) +
      geom_treemap_text(colour = "grey10", place = "middle", reflow = T)
    
    gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
    gp  <-gp +  theme(legend.position="none")
    return (gp)
    
  }  
  if (setequal (selNodes, c("Animal",  "LPS"))){
    gp <- ggplot(data2plot, aes(area = V1/1e6, label = paste (format(as.numeric(..area..), scientific = F,nsmall=2, digits=1, big.mark=","), "Mt"), subgroup = Animal,
                                subgroup2 = LPS, fill = Animal), alpha= 1) +
      geom_treemap() +
      #geom_treemap_subgroup3_border(colour = "white", size = 1) +
      geom_treemap_subgroup2_border(colour = "black", size = 3) +
      geom_treemap_subgroup_border(colour = "grey10", size = 5) +
      geom_treemap_subgroup_text(place = "top", colour = "black", alpha = 0.2, grow = T) +
      geom_treemap_subgroup2_text(colour = "black", alpha = 0.5, fontface = "italic") +
      #geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 1) +
      geom_treemap_text(colour = "grey10", place = "middle", reflow = T)
    
    gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
    gp  <-gp +  theme(legend.position="none")
    return (gp)
    
  }  
  
  if (setequal (selNodes, c("Animal"))){
    cat ("Animal onlye \n") 
    
    gp <- ggplot(data2plot, aes(area = V1/1e6, label = paste (format(as.numeric(..area..), scientific = F,nsmall=2, digits= 1, big.mark=","), "Mt"), subgroup = Animal,
                                fill = Animal), alpha= 1) +
      geom_treemap() +
      #geom_treemap_subgroup3_border(colour = "white", size = 1) +
      #geom_treemap_subgroup2_border(colour = "grey10", size = 3) +
      geom_treemap_subgroup_border(colour = "grey10", size = 5) +
      geom_treemap_subgroup_text(place = "top", colour = "black", alpha = 0.2, grow = T) +
      #geom_treemap_subgroup2_text(colour = "blue", alpha = 0.5, fontface = "italic") +
      #geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 1) +
      geom_treemap_text(colour = "grey10", place = "middle", reflow = T)
    
    gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
    gp  <-gp +  theme(legend.position="none")
    return (gp)
  } 
  if (setequal (selNodes, c("Item"))){
    cat ("Item onlye \n") 
    
    data2plot[Item == "Feed-CO2", Item:= "Feed (CO2)"]
    data2plot[Item == "LandUseChange", Item:= "LUC: soy and palm (CO2)"]
    data2plot[Item == "DirectOnFarmEnergy", Item:= "Direct on-farm energy (CO2)"]
    data2plot[Item == "PastureExpansion", Item:= "LUC: pasture expansion (CO2)"]
    data2plot[Item == "EmbeddedOnFarmEnergy", Item:= "Embedded on-farm energy (CO2)"]
    data2plot[Item == "Postfarm", Item:= "Post-farm (CO2)"]
    data2plot[Item == "Feed-CH4", Item:= "Feed (CH4)"]
    data2plot[Item == "Manure-CH4", Item:= "Manure (CH4)"]
    data2plot[Item == "EntericFermentation", Item:= "Enteric fermentation (CH4)"]
    data2plot[Item == "Feed-N2O", Item:= "Feed (N2O)"]
    data2plot[Item == "Manure-N2O", Item:= "Manure (N2O)"]
    
    gp <- ggplot(data2plot, aes(area = V1/1e6, label = paste (format(as.numeric(..area..), scientific = F,nsmall=2, digits= 1, big.mark=","), "Mt"), subgroup = Item,
                                fill = Item), alpha= 1) +
      geom_treemap() +
      #geom_treemap_subgroup3_border(colour = "white", size = 1) +
      #geom_treemap_subgroup2_border(colour = "grey10", size = 3) +
      geom_treemap_subgroup_border(colour = "grey10", size = 5) +
      geom_treemap_subgroup_text(place = "top", colour = "black", alpha = 0.2, grow = T) +
      #geom_treemap_subgroup2_text(colour = "blue", alpha = 0.5, fontface = "italic") +
      #geom_treemap_subgroup3_text(place = "top", colour = "white", alpha = 1) +
      geom_treemap_text(colour = "grey10", place = "middle", reflow = T)
    
    #gp <- gp + scale_fill_manual("Legend", values = dd.col)#gp <- gp + scale_fill_manual(values = data2plot$Color)
    gp  <-gp +  theme(legend.position="none")
    return (gp)
  } 
  
  
  
  # all other cases
  gp <- ggplot() + 
    annotate("text", x = 4, y = 25, size=8, label = "No data for this combination.\nPlease make another choice.") + 
    theme_void()
  return(gp)
  
} 


plotBarChartProduction <- function (curCountry, curAnimals, curProducts,curUnits ){
  
  if (curUnits == "Weight"){
    curUnits <- c("CarcassWeight", "Weight")
    varUnits <- paste ("tonnes of products")
  }else{
    varUnits <- paste ("tonnes of protein")
  }
  
  data2plot <- allData.dt[ VarName == "Production" & ISO3 == curCountry &  Animal %in% curAnimals &  
                             Item %in% curProducts &  Element %in% curUnits, sum (V1),by = .(Animal, LPS, Item) ]
  
  cat ("prod Bar", str (data2plot), "\n")
  
  data2plot <- merge (data2plot, gleamColorAnimals.dt,by= c("Animal", "LPS"))
  data2plot[ ,Label:= paste (Animal, "\n", LPS)]
  data2plot[ ,HoverText:= paste (Animal, "-", LPS)]
  data2plot$Color = trimws (data2plot$Color)
  # data2plot[ Animal == "Pigs"]
  
  
  
  if (nrow (data2plot) == 0){
    return (empty_plot("no data for selection"))
  }
  data2plot$Animal <- reorder(x = data2plot$Animal, X = data2plot$V1, FUN = sum) 
  # data2plot$V2 <- data2plot$V1
  #data2plot$V2[ data2plot$Animal == "Chickens"] <- data2plot$V2 / 1000 
  data2plot %>% 
    plot_ly(
      y = ~Animal, 
      x = ~V1, 
      color= ~Label,
      colors = ~Color,
      hoverinfo = "text",
      text = ~paste('</br>', HoverText,
                    
                    '</br> Output: ', formatC(V1, big.mark = ",", format = "f",digits =0), varUnits ),
      #hovertemplate = paste('%{y}', '%{color}', '<br>V1: %{text:.2s}<br>'),
      #texttemplate = '%{y:.2s}', textposition = 'outside',
      
      orientation = "h",
      type = 'bar') %>% 
    layout(
      yaxis = list(title = ''), 
      xaxis = list(title = varUnits), 
      barmode = 'stack', 
      orientation = "h") %>% config(displayModeBar = F) 
  
}

curCountry <- "World"
curAnimals <- c("Cattle", "Buffalo")
curProducts <- "Total protein"

plotBarChartProduction_2 <- function (curCountry, curAnimals, curProducts ){
  nodes <- c("Animal", "LPS")
  cat ("production", curProducts, "\n")
  allData.dt[ VarName == "Production" & ISO3 == curCountry &  Animal %in% curAnimals]
  if (curProducts == "Total protein"){
    curElement <- "Protein"
    curProducts <- c("Meat", "Milk", "Eggs")
    varUnits <- paste ("tonnes of protein")
    
  }else{
    curElement <- c("Weight", "CarcassWeight")
    
    varUnits <- paste ("tonnes of product")
  }
  
  gleamColorAnimals.dt
  allData.dt[ VarName == "Production" & ISO3 == curCountry &  Animal %in% curAnimals & 
                Item %in% curProducts &  Element %in% curElement]
  data2plot <- allData.dt[ VarName == "Production" & ISO3 == curCountry &  Animal %in% curAnimals &   
                             Item %in% curProducts &  Element %in% curElement, sum (V1),by = nodes ]
  
  data2plot <- merge (data2plot, gleamColorAnimals.dt,by.x= c("Animal", "LPS"), by.y = c("Animal", "LPS"))
  
  gleamColorAnimals.dt
  
  if (nrow (data2plot) == 0){
    return (empty_plot("no data for selection"))
  }
  colV <- c("brown", "green","red", "green","red", "black")
  dd.col <- data2plot$Color
  names (dd.col) <- paste (data2plot$Animal, "-", data2plot$LPS)
  # f1 <- colorG %>% 
  #    arrange(factor(nodeName, levels = sourceOrder))
  #  f1
  # dd.col <- f1$color
  #names (dd.col) <- f1$nodeName
  data2plot[, LPSPlot:= paste (Animal, "-", LPS)]
  
  # gp <- ggplot (data2plot) + theme_bw()
  # gp <- gp + geom_col (aes (x = Animal, y=  V1/1e6, fill= LPSPlot, text = paste (LPSPlot ," :", round (V1/1e6,3) , "Mt" )),  position = "stack")
  # gp <- gp + ylab (varUnits) + xlab ("")
  # gp <- gp + coord_flip()
  # gp <- gp + scale_fill_manual (values = dd.col)
  # gp <- gp + theme(legend.position="bottom") + labs(fill="Production system")
  # ggplotly (gp,  tooltip = "text")
  
  data2plot <- merge (data2plot, gleamColorAnimals.dt,by= c("Animal", "LPS"))
  data2plot[ ,Label:= paste (Animal, "\n", LPS)]
  data2plot[ ,HoverText:= paste (Animal, "-", LPS)]
  #data2plot$Color = trimws (data2plot$Color)
  data2plot$Animal <- reorder(x = data2plot$Animal, X = data2plot$V1, FUN = sum) 
  data2plot$V2 <- data2plot$V1
  
  data2plot %>% 
    plot_ly(
      y = ~Animal, 
      x = ~V2, 
      color= ~Label,
      colors = ~Color.x,
      hoverinfo = "text",
      text = ~paste('</br>', HoverText,
                    
                    '</br>', 'Tons: ', formatC(V1, big.mark = ",", format = "f", digits =3)),
      #hovertemplate = paste('%{y}', '%{color}', '<br>V1: %{text:.2s}<br>'),
      #texttemplate = '%{y:.2s}', textposition = 'outside',
      
      orientation = "h",
      

      type = 'bar') %>% 
    layout(
      #marker = list(textposition = 'none'),  ### trying to get rid of the end label but this line does not help
      #annotation = list(),### trying to get rid of the end label but this line does not help
      yaxis = list(title = ''), 
      xaxis = list(title = varUnits), 
      barmode = 'stack', 
      orientation = "h") %>% config(displayModeBar = F) 
}



############################################################################
######################## PLOTS IN COMPARISON TOOL ##########################
############################################################################


plot_AnimalPopulations_comp <- function (curCountry, curAnimals, curHerdType, curLPS1, curLPS2, curLPS3){
  
  allData.dt2 <- allData.dt
  allData.dt2[Animal == "Chickens" & LPS == "Backyard", LPS:= "Backyard_chickens"]
  allData.dt2[Animal == "Pigs" & LPS == "Backyard", LPS:= "Backyard_pigs"]
  
  curLPS <- unique(c(curLPS1, curLPS2, curLPS3))
  
  varUnits <- "million animals"
  
  curHerdType <- c("Chickens", "Pigs", curHerdType)
  
  data2plot <- allData.dt2[  VarName == "AnimalNumbers" & 
                               ISO3 %in% curCountry & 
                               LPS %in% curLPS &
                               Animal %in% curAnimals & 
                               HerdType %in% curHerdType,
                             sum(V1), by = .(Animal, ISO3, COUNTRY) ]
  
  if (nrow (data2plot)  == 0){ return (empty_plot("no data for selection")) }
  
  data2plot <- merge (data2plot, Animal_colors, by= c("Animal"))
  
  data2plot$Color = trimws (data2plot$Color)
  data2plot$Animal <- reorder(x = data2plot$Animal, X = data2plot$V1, FUN = sum) 
  
  data2plot <- as.data.frame(data2plot)
  
  data2plot$V1 <- data2plot$V1/1000000
  
  regcomp <- unique(data2plot[, c(2:3)])
  regcomp2 <- regcomp[match(curCountry, regcomp$ISO3),]
  
  region_table <- table(data2plot$ISO3)
  region_sel <- names(region_table)[which(region_table == max(region_table))]
  if (curCountry[1] %in% region_sel == T) { region_sel2 <- curCountry[1] 
  } else { region_sel2 <- region_sel[1] }
  region1 <- data2plot[data2plot$ISO3 == region_sel2,]
  
  if (length(unique(data2plot$ISO3)) == 1) { alpha_scale <- 1
  } else if (length(unique(data2plot$ISO3)) == 2) { alpha_scale <- c(0.4, 1)
  } else if (length(unique(data2plot$ISO3)) == 3) { alpha_scale <- c(0.4, 0.7, 1)
  } else if (length(unique(data2plot$ISO3)) == 4) { alpha_scale <- c(0.4, 0.6, 0.8, 1) }
  
  # Determine color order
  colors_sel <- region1[rev(order(region1$V1)),]$Color
  
  # Reorder the levels of Animals
  facet_order <- region1[rev(order(region1$V1)),]$Animal
  data2plot$Animal <- factor(data2plot$Animal, levels = facet_order)
  
  # Reorder the levels of COUNTRY
  data2plot$COUNTRY <- as.factor(data2plot$COUNTRY)
  data2plot$COUNTRY <- factor(data2plot$COUNTRY, levels = rev(regcomp2$COUNTRY))
  
  ggplotly(ggplot(data2plot, aes(y = COUNTRY, x = V1, fill = Animal, alpha = COUNTRY,
                                 text=paste0("<b>", COUNTRY, "<br></b>",
                                             Animal, ": ", format(round(as.numeric(V1), digits=0), nsmall=0, big.mark=","), " ", varUnits, "<br>"))) +
             geom_bar(stat = 'identity', position = 'dodge') +
             scale_alpha_manual(values = alpha_scale) +
             #scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
             scale_fill_manual(values = colors_sel) +
             theme_minimal() +
             facet_wrap(~Animal, ncol=1, strip.position = "left") +
             theme(
               legend.position = "none",  
               strip.background = element_blank(), 
               panel.grid.major.y = element_blank(), 
               panel.grid.minor.y = element_blank(), 
               panel.grid.minor.x = element_blank(),
               panel.background = element_blank(), 
               strip.placement = "outside",
               strip.text = element_text(angle = 0, vjust = 0.5, hjust = 0, size = 10, face = "bold")) +
             ylab("") + xlab(varUnits),
           tooltip =c("text"))
}


plot_AnimalProducts_comp <- function (curCountry, curAnimals, curHerdType, curLPS1, curLPS2, curLPS3, curProducts, curUnit, curNode ){
  
  allData.dt2 <- allData.dt
  allData.dt2[Animal == "Chickens" & LPS == "Backyard", LPS:= "Backyard_chickens"]
  allData.dt2[Animal == "Pigs" & LPS == "Backyard", LPS:= "Backyard_pigs"]
  
  curLPS <- unique(c(curLPS1, curLPS2, curLPS3))
  
  curHerdType <- c("Chickens", "Pigs", curHerdType)
  
  if (curUnit == "Protein weight") {
    curElement <- "Protein"
    varUnits <- paste ("Mt of protein")
  } else if (curUnit == "Product weight") {
    curElement <- c("Weight", "CarcassWeight")
    varUnits <- paste ("Mt of product")
  }
  
  if (curNode == "Species") { 
    
    data2plot <- allData.dt[ VarName == "Production" &  ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                               LPS %in% curLPS & Item %in% curProducts & Element %in% curElement, sum (V1), by = .(Animal, ISO3, COUNTRY) ]
    
    node_colors <- Animal_colors
    
    
  } else if (curNode == "Herd type") { 
    
    data2plot <- allData.dt[ VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                               LPS %in% curLPS & Item %in% curProducts & Element %in% curElement, sum (V1), by = .(HerdType, ISO3, COUNTRY) ]
    
    node_colors <- HerdType_colors
    
    
  } else if (curNode == "Production system") {  
    
    data2plot <- allData.dt[ VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                               LPS %in% curLPS & Item %in% curProducts & Element %in% curElement, sum (V1), by = .(LPS, ISO3, COUNTRY) ]
    
    node_colors <- LPS_colors
    
    
  } else if (curNode == "Product") {  
    
    data2plot <- allData.dt[ VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                               LPS %in% curLPS & Item %in% curProducts & Element %in% curElement, sum (V1), by = .(Item, ISO3, COUNTRY) ]
    
    node_colors <- Product_colors
    
  }
  
  names(node_colors)[1] <- "node"
  names(data2plot)[1]   <- "node"
  
  data2plot <- merge (data2plot, node_colors, by= c("node"))
  data2plot$Color = trimws (data2plot$Color)
  data2plot$node <- reorder(x = data2plot$node, X = data2plot$V1, FUN = sum)
  
  if (nrow (data2plot) == 0){ return (empty_plot("no data for selection")) }
  
  data2plot <- as.data.frame(data2plot)
  
  data2plot$V1 <- data2plot$V1/1000000
  
  regcomp <- unique(data2plot[, c(2:3)])
  regcomp2 <- regcomp[match(curCountry, regcomp$ISO3),]
  
  region_table <- table(data2plot$ISO3)
  region_sel <- names(region_table)[which(region_table == max(region_table))]
  if (curCountry[1] %in% region_sel == T) { region_sel2 <- curCountry[1] 
  } else { region_sel2 <- region_sel[1] }
  region1 <- data2plot[data2plot$ISO3 == region_sel2,]
  
  if (length(unique(data2plot$ISO3)) == 1) { alpha_scale <- 1
  } else if (length(unique(data2plot$ISO3)) == 2) { alpha_scale <- c(0.4, 1)
  } else if (length(unique(data2plot$ISO3)) == 3) { alpha_scale <- c(0.4, 0.7, 1)
  } else if (length(unique(data2plot$ISO3)) == 4) { alpha_scale <- c(0.4, 0.6, 0.8, 1) }
  
  # Determine color order
  colors_sel <- region1[rev(order(region1$V1)),]$Color
  
  # Reorder the levels of COUNTRY
  data2plot$COUNTRY <- as.factor(data2plot$COUNTRY)
  data2plot$COUNTRY <- factor(data2plot$COUNTRY, levels = rev(regcomp2$COUNTRY))
  
  # Reorder the Facet levels
  facet_order <- region1[rev(order(region1$V1)),]$node
  data2plot$node <- factor(data2plot$node, levels = facet_order)
  
  ggplotly(ggplot(  data2plot, 
                    aes(y = COUNTRY, x = V1, fill = node, alpha = COUNTRY,
                        text=paste0("<b>", COUNTRY, "<br></b>",
                                    node, ": ", format(round(as.numeric(V1), digits=3), nsmall=0, big.mark=","), " ", varUnits, "<br>")
                    )
  ) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_alpha_manual(values = alpha_scale) +
    scale_fill_manual(values = colors_sel) +
    theme_minimal() +
    facet_wrap(~node, ncol=1, strip.position = "left") +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(angle = 0, vjust = 0.5, hjust = 0, size = 10, face = "bold")
    ) +
    ylab("") + 
    xlab(varUnits),
  tooltip =c("text")
  )
  
}  


plot_Emissions_comp <- function(curCountry, curAnimals, curHerdType, curLPS1, curLPS2, curLPS3, curProducts, curGHG, curCO2sources, curCH4sources, curN2Osources, curNode, GWP_N2O, GWP_CH4) {
  
  allData.dt2 <- allData.dt
  allData.dt2[Animal == "Chickens" & LPS == "Backyard", LPS:= "Backyard_chickens"]
  allData.dt2[Animal == "Pigs" & LPS == "Backyard", LPS:= "Backyard_pigs"]
  
  curLPS <- unique(c(curLPS1, curLPS2, curLPS3))
  
  varUnits <- "Mt CO2eq."
  
  GHG_lookup <- data.frame(ui  = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)"),
                           dat = c("CO2", "CH4", "N2O"))
  
  CO2_lookup <- data.frame(ui  = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm"),
                           dat = c("Feed-CO2", "LandUseChange", "DirectOnFarmEnergy", "PastureExpansion", "EmbeddedOnFarmEnergy", "Postfarm"))             
  
  CH4_lookup <- data.frame(ui  = c("Feed", "Manure", "Enteric fermentation"),
                           dat = c("Feed-CH4", "Manure-CH4", "EntericFermentation"))
  
  N2O_lookup <- data.frame(ui  = c("Feed", "Manure"),
                           dat = c("Feed-N2O", "Manure-N2O"))
  
  GHG_ident  <- GHG_lookup$dat[GHG_lookup$ui %in% curGHG]
  curGHG     <- GHG_ident 
  
  CO2_ident  <- CO2_lookup$dat[CO2_lookup$ui %in% curCO2sources]
  CH4_ident  <- CH4_lookup$dat[CH4_lookup$ui %in% curCH4sources]
  N2O_ident  <- N2O_lookup$dat[N2O_lookup$ui %in% curN2Osources]
  curSources <- c(CO2_ident, CH4_ident, N2O_ident)  
  
  curHerdType <- c("Chickens", "Pigs", curHerdType)
  
  data2plotpre <- allData.dt
  
  ## change emission factors of CH4 and N2O according to selected GWP
  data2plotpre[Element == "CH4", GWP:= GWP_CH4]
  data2plotpre[Element == "N2O", GWP:= GWP_N2O]
  
  
  if (curNode == "Species") { 
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(Animal, ISO3, COUNTRY) ]
    
    node_colors <- Animal_colors
    
    
  } else if (curNode == "Herd type") { 
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(HerdType, ISO3, COUNTRY) ]
    
    node_colors <- HerdType_colors
    
    
  } else if (curNode == "Production system") {  
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(LPS, ISO3, COUNTRY) ]
    
    node_colors <- LPS_colors
    
    
  } else if (curNode == "Product") {  
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(Element2, ISO3, COUNTRY) ]
    
    node_colors <- Product_colors
    
    
  } else if (curNode == "Greenhouse gas") { 
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(Element, ISO3, COUNTRY) ]
    
    node_colors <- Gas_colors
    
    
  } else if (curNode == "Emission source") { 
    
    data2plot <- data2plotpre[ VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                 LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                               sum (V1 * GWP, na.rm = T), by = .(Item, ISO3, COUNTRY) ]
    
    node_colors <- Source_colors
  }
  
  names(node_colors)[1] <- "node"
  names(data2plot)[1]   <- "node"
  
  data2plot <- merge (data2plot, node_colors, by= c("node"))
  data2plot$Color = trimws (data2plot$Color)
  data2plot$node <- reorder(x = data2plot$node, X = data2plot$V1, FUN = sum)
  
  if (curNode == "Emission source") {
    ## rename sources in data2plot 
    data2plot[node == "Feed-CO2", node:= "Feed (CO2)"]
    data2plot[node == "LandUseChange", node:= "LUC: soy and palm (CO2)"]
    data2plot[node == "DirectOnFarmEnergy", node:= "Direct on-farm energy (CO2)"]
    data2plot[node == "PastureExpansion", node:= "LUC: pasture expansion (CO2)"]
    data2plot[node == "EmbeddedOnFarmEnergy", node:= "Embedded on-farm energy (CO2)"]
    data2plot[node == "Postfarm", node:= "Post-farm (CO2)"]
    data2plot[node == "Feed-CH4", node:= "Feed (CH4)"]
    data2plot[node == "Manure-CH4", node:= "Manure (CH4)"]
    data2plot[node == "EntericFermentation", node:= "Enteric fermentation (CH4)"]
    data2plot[node == "Feed-N2O", node:= "Feed (N2O)"]
    data2plot[node == "Manure-N2O", node:= "Manure (N2O)"]
  }
  
  if (nrow (data2plot) == 0){ return (empty_plot("no data for selection")) }
  
  data2plot <- as.data.frame(data2plot)
  
  data2plot$V1 <- data2plot$V1/1000000
  
  regcomp <- unique(data2plot[, c(2:3)])
  regcomp2 <- regcomp[match(curCountry, regcomp$ISO3),]
  
  region_table <- table(data2plot$ISO3)
  region_sel <- names(region_table)[which(region_table == max(region_table))]
  if (curCountry[1] %in% region_sel == T) { region_sel2 <- curCountry[1] 
  } else { region_sel2 <- region_sel[1] }
  region1 <- data2plot[data2plot$ISO3 == region_sel2,]
  
  if (length(unique(data2plot$ISO3)) == 1) { alpha_scale <- 1
  } else if (length(unique(data2plot$ISO3)) == 2) { alpha_scale <- c(0.4, 1)
  } else if (length(unique(data2plot$ISO3)) == 3) { alpha_scale <- c(0.4, 0.7, 1)
  } else if (length(unique(data2plot$ISO3)) == 4) { alpha_scale <- c(0.4, 0.6, 0.8, 1) }
  
  # Determine color order
  colors_sel <- region1[rev(order(region1$V1)),]$Color
  
  # Reorder the levels of COUNTRY
  data2plot$COUNTRY <- as.factor(data2plot$COUNTRY)
  data2plot$COUNTRY <- factor(data2plot$COUNTRY, levels = rev(regcomp2$COUNTRY))
  
  # Reorder the Facet levels
  facet_order <- region1[rev(order(region1$V1)),]$node
  data2plot$node <- factor(data2plot$node, levels = facet_order)
  
  ggplotly(ggplot(  data2plot, 
                    aes(y = COUNTRY, x = V1, fill = node, alpha = COUNTRY,
                        text=paste0("<b>", COUNTRY, "<br></b>",
                                    node, ": ", format(round(as.numeric(V1), digits=3), nsmall=0, big.mark=","), " ", varUnits, "<br>")
                    )
  ) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_alpha_manual(values = alpha_scale) +
    scale_fill_manual(values = colors_sel) +
    theme_minimal() +
    facet_wrap(~node, ncol=1, strip.position = "left") +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(angle = 0, vjust = 0.5, hjust = 0, size = 10, face = "bold")
    ) +
    ylab("") + 
    xlab(varUnits),
  tooltip =c("text")
  )
  
}

plot_EmissionIntensities_comp <- function(curCountry, curAnimals, curHerdType, curLPS1, curLPS2, curLPS3, curProducts, curGHG, curCO2sources, curCH4sources, curN2Osources, curUnit, curNode, GWP_N2O, GWP_CH4) {
  
  allData.dt2 <- allData.dt
  allData.dt2[Animal == "Chickens" & LPS == "Backyard", LPS:= "Backyard_chickens"]
  allData.dt2[Animal == "Pigs" & LPS == "Backyard", LPS:= "Backyard_pigs"]
  
  curLPS <- unique(c(curLPS1, curLPS2, curLPS3))
  
  GHG_lookup <- data.frame(ui  = c("Carbon dioxide (CO2)", "Methane (CH4)", "Nitrous oxide (N2O)"),
                           dat = c("CO2", "CH4", "N2O"))
  
  CO2_lookup <- data.frame(ui  = c("Feed", "LUC: soy and palm", "Direct on-farm energy", "LUC: pasture expansion", "Embedded on-farm energy", "Post-farm"),
                           dat = c("Feed-CO2", "LandUseChange", "DirectOnFarmEnergy", "PastureExpansion", "EmbeddedOnFarmEnergy", "Postfarm"))             
  
  CH4_lookup <- data.frame(ui  = c("Feed", "Manure", "Enteric fermentation"),
                           dat = c("Feed-CH4", "Manure-CH4", "EntericFermentation"))
  
  N2O_lookup <- data.frame(ui  = c("Feed", "Manure"),
                           dat = c("Feed-N2O", "Manure-N2O"))
  
  GHG_ident  <- GHG_lookup$dat[GHG_lookup$ui %in% curGHG]
  curGHG     <- GHG_ident 
  
  CO2_ident  <- CO2_lookup$dat[CO2_lookup$ui %in% curCO2sources]
  CH4_ident  <- CH4_lookup$dat[CH4_lookup$ui %in% curCH4sources]
  N2O_ident  <- N2O_lookup$dat[N2O_lookup$ui %in% curN2Osources]
  curSources <- c(CO2_ident, CH4_ident, N2O_ident)  
  
  curHerdType <- c("Chickens", "Pigs", curHerdType)
  
  data2plotpre <- allData.dt
  
  ## change emission factors of CH4 and N2O according to selected GWP
  data2plotpre[Element == "CH4", GWP:= GWP_CH4]
  data2plotpre[Element == "N2O", GWP:= GWP_N2O]
  
  
  if (curNode == "Species") { 
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(Animal, ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(Animal, ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(Animal, ISO3, COUNTRY) ]
    
    data2plot_em   <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                   & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                                   sum(V1 * GWP, na.rm = T), by = .(Animal, ISO3, COUNTRY) ]
    
    node_colors    <- Animal_colors
    
    names(data2plot_pop)[4]  <- "animals"
    names(data2plot_product)[4] <- "product"
    names(data2plot_protein)[4] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("Animal", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("Animal", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("Animal", "ISO3", "COUNTRY"))
    
    names(data2plot)[1] <- "node"
    
    
  } else if (curNode == "Herd type") { 
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(HerdType, ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(HerdType, ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(HerdType, ISO3, COUNTRY) ]
    
    data2plot_em   <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                     LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                                   sum(V1 * GWP, na.rm = T), by = .(HerdType, ISO3, COUNTRY) ]
    
    node_colors    <- HerdType_colors
    
    names(data2plot_pop)[4]  <- "animals"
    names(data2plot_product)[4] <- "product"
    names(data2plot_protein)[4] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("HerdType", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("HerdType", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("HerdType", "ISO3", "COUNTRY"))
    
    names(data2plot)[1] <- "node"
    
    
  } else if (curNode == "Production system") {  
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(LPS, ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(LPS, ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(LPS, ISO3, COUNTRY) ]
    
    data2plot_em   <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                     LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                                   sum(V1 * GWP, na.rm = T), by = .(LPS, ISO3, COUNTRY) ]
    
    node_colors    <- LPS_colors
    
    names(data2plot_pop)[4]  <- "animals"
    names(data2plot_product)[4] <- "product"
    names(data2plot_protein)[4] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("LPS", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("LPS", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("LPS", "ISO3", "COUNTRY"))
    
    names(data2plot)[1] <- "node"
    
    
  } else if (curNode == "Product") {  
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(Item, ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(Item, ISO3, COUNTRY) ]
    
    data2plot_em <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & 
                                   LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources, 
                                 sum(V1 * GWP, na.rm = T), by = .(Element2, ISO3, COUNTRY) ]
    
    node_colors <- Product_colors
    
    names(data2plot_em)[1] <- "Item"
    names(data2plot_pop)[3]  <- "animals"
    names(data2plot_product)[4] <- "product"
    names(data2plot_protein)[4] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("Item", "ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("Item", "ISO3", "COUNTRY"))
    
    names(data2plot)[1] <- "node"
    
    
  } else if (curNode == "Greenhouse gas") {
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_em <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType &
                                   LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources,
                                 sum(V1 * GWP, na.rm = T), by = .(Element, ISO3, COUNTRY) ]
    
    node_colors <- Gas_colors
    
    names(data2plot_pop)[3]  <- "animals"
    names(data2plot_product)[3] <- "product"
    names(data2plot_protein)[3] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("ISO3", "COUNTRY"))
    
    names(data2plot)[6] <- "node"
    
    
  } else if (curNode == "Emission source") {
    
    data2plot_pop  <- data2plotpre[VarName == "AnimalNumbers" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS, 
                                   sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_product <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element %in% c("Weight", "CarcassWeight"), 
                                      sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_protein <- data2plotpre[VarName == "Production" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType & LPS %in% curLPS 
                                      & Item %in% curProducts & Element == "Protein", 
                                      sum(V1), by = .(ISO3, COUNTRY) ]
    
    data2plot_em <- data2plotpre[VarName == "Emissions" & ISO3 %in% curCountry & Animal %in% curAnimals & HerdType %in% curHerdType &
                                   LPS %in% curLPS & Element2 %in% curProducts & Element %in% curGHG & Item %in% curSources,
                                 sum(V1 * GWP, na.rm = T), by = .(Item, ISO3, COUNTRY) ]
    
    node_colors <- Source_colors
    
    names(data2plot_pop)[3]  <- "animals"
    names(data2plot_product)[3] <- "product"
    names(data2plot_protein)[3] <- "protein"
    names(data2plot_em)[4]   <- "emissions"
    
    data2plot <- merge(data2plot_pop, data2plot_product, by = c("ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_protein, by = c("ISO3", "COUNTRY"))
    data2plot <- merge(data2plot, data2plot_em, by = c("ISO3", "COUNTRY"))
    
    names(data2plot)[6] <- "node"
    
  }
  
  names(node_colors)[1]   <- "node"
  
  ## calculate emission intensity
  data2plot$em_head    <- data2plot$emissions / data2plot$animals
  data2plot$em_product <- data2plot$emissions / data2plot$product
  data2plot$em_protein <- data2plot$emissions / data2plot$protein
  
  
  if (curUnit == "Emissions per animal") {
    
    data2plot <- dplyr::select(data2plot, node, ISO3, COUNTRY, em_head)
    varUnits <- "tonnes CO2eq./animal"
    
  } else if (curUnit == "Emissions per kg of product") {
    
    data2plot <- dplyr::select(data2plot, node, ISO3, COUNTRY, em_product)
    varUnits <- "kg CO2eq./kg of product"
    
  } else if (curUnit == "Emissions per kg of protein") {
    
    data2plot <- dplyr::select(data2plot, node, ISO3, COUNTRY, em_protein)
    varUnits <- "kg CO2eq./kg of protein"
    
  }
  
  names(data2plot)[4]  <- "V1"
  
  data2plot <- merge (data2plot, node_colors, by= c("node"))
  data2plot$Color = trimws (data2plot$Color)
  data2plot$node <- reorder(x = data2plot$node, X = data2plot$V1, FUN = sum)
  
  if (curNode == "Emission source") {
    ## rename sources in data2plot 
    data2plot[node == "Feed-CO2", node:= "Feed (CO2)"]
    data2plot[node == "LandUseChange", node:= "LUC: soy and palm (CO2)"]
    data2plot[node == "DirectOnFarmEnergy", node:= "Direct on-farm energy (CO2)"]
    data2plot[node == "PastureExpansion", node:= "LUC: pasture expansion (CO2)"]
    data2plot[node == "EmbeddedOnFarmEnergy", node:= "Embedded on-farm energy (CO2)"]
    data2plot[node == "Postfarm", node:= "Post-farm (CO2)"]
    data2plot[node == "Feed-CH4", node:= "Feed (CH4)"]
    data2plot[node == "Manure-CH4", node:= "Manure (CH4)"]
    data2plot[node == "EntericFermentation", node:= "Enteric fermentation (CH4)"]
    data2plot[node == "Feed-N2O", node:= "Feed (N2O)"]
    data2plot[node == "Manure-N2O", node:= "Manure (N2O)"]
  }
  
  if (nrow (data2plot) == 0){ return (empty_plot("no data for selection")) }
  
  data2plot <- as.data.frame(data2plot)
  
  regcomp <- unique(data2plot[, c(2:3)])
  regcomp2 <- regcomp[match(curCountry, regcomp$ISO3),]
  
  region_table <- table(data2plot$ISO3)
  region_sel <- names(region_table)[which(region_table == max(region_table))]
  if (curCountry[1] %in% region_sel == T) { region_sel2 <- curCountry[1] 
  } else { region_sel2 <- region_sel[1] }
  region1 <- data2plot[data2plot$ISO3 == region_sel2,]
  
  if (length(unique(data2plot$ISO3)) == 1) { alpha_scale <- 1
  } else if (length(unique(data2plot$ISO3)) == 2) { alpha_scale <- c(0.4, 1)
  } else if (length(unique(data2plot$ISO3)) == 3) { alpha_scale <- c(0.4, 0.7, 1)
  } else if (length(unique(data2plot$ISO3)) == 4) { alpha_scale <- c(0.4, 0.6, 0.8, 1) }
  
  # Determine color order
  colors_sel <- region1[rev(order(region1$V1)),]$Color
  
  # Reorder the levels of COUNTRY
  data2plot$COUNTRY <- as.factor(data2plot$COUNTRY)
  data2plot$COUNTRY <- factor(data2plot$COUNTRY, levels = rev(regcomp2$COUNTRY))
  
  # Reorder the Facet levels
  facet_order <- region1[rev(order(region1$V1)),]$node
  data2plot$node <- factor(data2plot$node, levels = facet_order)
  
  ggplotly(ggplot(  data2plot, 
                    aes(y = COUNTRY, x = V1, fill = node, alpha = COUNTRY,
                        text=paste0("<b>", COUNTRY, "<br></b>",
                                    node, ": ", format(round(as.numeric(V1), digits=2), nsmall=0, big.mark=","), " ", varUnits, "<br>")
                    )
  ) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_alpha_manual(values = alpha_scale) +
    scale_fill_manual(values = colors_sel) +
    theme_minimal() +
    facet_wrap(~node, ncol=1, strip.position = "left") +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(angle = 0, vjust = 0.5, hjust = 0, size = 10, face = "bold")
    ) +
    ylab("") + 
    xlab(varUnits),
  tooltip =c("text")
  )
}

####################################################################################
####################################################################################
####################################################################################

plotBarChartAnimalNumbersGGPLOT <- function (curCountry, animals, herdType){
  #curCountry <- "IGAD"
  #animals <- c("Cattle", "Goats")
  #herdType <- "Beef"
  data2plot <- herds.dt[ ISO3 == curCountry &  Animal %in% animals]
  #data2plot [ Animal]
  if (length (herdType)  == 0){
    return (empty_plot("no data for selection"))
  }
  if (herdType == "All"){
    herdType <- unique (data2plot$HerdType)
  }
  
  varUnits <- "animals"
  
  
  data2plot <- data2plot[  HerdType %in% herdType ]
  
  data2plot <- merge (data2plot, gleamColorAnimals.dt,by= c("Animal", "ProdSys"))
  pal <- data2plot$Color
  gp <- ggplot(data = data2plot ,aes (x= Animal, fill = ProdSys, y = V1))
  gp <- gp + geom_bar (position = "stack", stat = "identity")
  gp <- gp + ylab ("Number of animals")
  gp <- gp +  coord_flip ()
  gp <- gp + guides(fill=guide_legend(title="Production System"))
  #gp <- gp + scale_fill_manual(name = "Production System")
  gp
  
}


plotBarChartAnimalNumbers <- function (curCountry, selAnimals, herdType){
  #curCountry <- "World"
  # selAnimals <- c("Chickens")
  #herdType <- "All"
  #if (selAnimals %in% Monogastrics){HerdType <- selAnimals}
  herdType <- c("Chickens", "Pigs", herdType)
  data2plot <- allData.dt[ VarName == "AnimalNumbers" & ISO3 ==curCountry  &  Animal %in%  selAnimals  & HerdType %in% herdType, sum (V1), by=  .(Animal,  LPS)]
  #data2plot <- data2plot [, head (.SD,1 ), by= c("Animal", "LPS", "HerdType") ]
  
  #data2plot [ Animal]
  if (nrow (data2plot)  == 0){
    return (empty_plot("no data for selection"))
  }
  #if (herdType == "All"){
  #  herdType <- unique (data2plot$HerdType)
  #}
  
  varUnits <- "heads"
  #if (is.null (animals)){
  #  return (empty_plot("no data for selection"))
  # }
  #if (nrow (data2plot) == 0){
  #  return (empty_plot("no data for selection"))
  #}
  #if (animals[1] %in% monogastrics){
  
  #data2plot <- data2plot[  HerdType %in% herdType ]
  data2plot <- merge (data2plot, gleamColorAnimals.dt,by= c("Animal", "LPS"))
  data2plot[ ,Label:= paste (Animal, "\n", LPS)]
  data2plot[ ,HoverText:= paste (Animal, "-", LPS)]
  data2plot$Color = trimws (data2plot$Color)
  data2plot$Animal <- reorder(x = data2plot$Animal, X = data2plot$V1, FUN = sum) 
  data2plot$V2 <- data2plot$V1
  #data2plot$V2[ data2plot$Animal == "Chickens"] <- data2plot$V2 / 100000 
  #data2plot$Color <- "#FF0000"
  data2plot %>% 
    plot_ly(
      y = ~Animal, 
      x = ~V2, 
      color= ~Label,
      colors = ~Color,
      hoverinfo = "text",
      text = ~paste('</br>', HoverText,
                    
                    '</br> Heads: ', formatC(V1, big.mark = ",", format = "f",digits =0)  ),
      #hovertemplate = paste('%{y}', '%{color}', '<br>V1: %{text:.2s}<br>'),
      #texttemplate = '%{y:.2s}', textposition = 'outside',
      
      orientation = "h",
      type = 'bar') %>% 
    layout(
      yaxis = list(title = ''), 
      xaxis = list(title = varUnits), 
      barmode = 'stack', 
      orientation = "h") %>% config(displayModeBar = F) 
  
}


empty_plotGG <- function(text){
  #text = paste("\n   The following is text that'll appear in a plot window.\n",
  #             "       As you can see, it's in the plot window\n",
  #             "       One might imagine useful information here")
  gp <- ggplot() + 
    annotate("text", x = 4, y = 25, size=8, label = text) + 
    theme_void()
  return(gp)
} 

empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
  return(p)
} 

# plotyPieChart <- function (  curCountry, curAnimal, isDirect=FALSE, GWP_N2O,GWP_CH4){
#   
#   #GWP_CH4 <- 34
#   #GWP_N2O <- 298
#   #  curCountry <- "World"
#   #  
#   #isDirect <- F
#   # curAnimal <- c("Chickens")
#   data2plot <- allData.dt[VarName == "Emissions" & ISO3 ==  curCountry & Animal %in% curAnimal]
#   
#   setnames (data2plot, "Element", "Gas")
#   setnames (data2plot, "Item", "Source")
#   #junk <- emissions.dt[ADM0_NAME ==  "Kenya" & Animal %in% c("Chickens", "Pigs")]
#   
#   #data2plot <- emissions.dt[ADM0_NAME ==  "Kenya" & Animal %in% c("Chickens", "Pigs"), 
#   #                         sum (value,na.rm = T), by= .(Source, Gas)]
#   
#   data2plot[ Gas == "CH4", GWP:= GWP_CH4]
#   data2plot[ Gas == "N2O", GWP:= GWP_N2O]
#   
#   # data2plot[, V1:= V1 / 1e6 ] # Mt - new
#   # data2plot <- data2plot[, 
#   #                        sum (V1 * GWP,na.rm = T), by= .(Source, Gas, isDirect)]
#   
#   data2plot[, V1:= V1 / 1e6 ] # Mt - new
#   data2plot <- data2plot[, sum (V1 * GWP,na.rm = T), by= .(Source, Gas, isDirect)]
#   
#   
#   if (isDirect == TRUE){
#     data2plot <- data2plot[ isDirect == "TRUE"]
#   }
#   
#   
#   
#   if (nrow (data2plot) ==0){
#     return (empty_plot("no data for selection"))
#   }
#   cat ("Pie chart" , isDirect, "STR", str(data2plot), "\n")
#   
#   data2plot <- merge (data2plot, SankeyEmissionColors,by.x = c("Source"),by.y = "nodeName",  all.x = T)
#   #data2plot
#   
#   # keep similar colors together! 
#   gases <- unique (data2plot$Gas)
#   data2plot <- data2plot[ order (Gas,V1)]
#   #data2plot[, pct:= 100 * V1/sum(V1)]
#   #data2plot[ , hoverText := paste0(Source , "(", Gas, "):", formatC ( V1/1e6, digits= 2, format = "f", big.mark = ",", decimal.mark = "."), ' Mt')]
#   
#   fig <- plot_ly(data2plot, 
#                  labels = ~Source, values = ~V1, 
#                  type = 'pie', sort = FALSE,
#                  text = ~paste0(Source , "(", Gas, "):", formatC ( V1, digits= 2, format = "f", big.mark = ",", decimal.mark = "."), ' Mt') ,    
#                  textinfo = 'label+percent',
#                  colors = ~color,
#                  hoverinfo = 'text',
#                  textposition= "outside",
#                  #hovertemplate = "%{Source}: <br>Popularity: %{V1} </br> Mt",
#                  marker = list(colors = ~color,
#                                line = list(color = '#FFFFFF', width = 1)))  ##~paste( sprintf ("%.2f", V1), ' million t'))
#   
#   fig <- fig %>% layout(showlegend = F,title = "",
#                         margin = list (b = 100, l = 50, r= 50, t= 100, pad= 0),
#                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F) 
#   
#   fig
#   
#   return (fig)
#   
# }

plotyPieChart <- function (  curCountry, curAnimal, isDirect=FALSE, GWP_N2O,GWP_CH4){
  
  #GWP_CH4 <- 34
  #GWP_N2O <- 298
  #  curCountry <- "World"
  #
  #isDirect <- F
  # curAnimal <- c("Chickens")
  data2plot <- allData.dt[VarName == "Emissions" & ISO3 ==  curCountry & Animal %in% curAnimal]
  
  setnames (data2plot, "Element", "Gas")
  setnames (data2plot, "Item", "Source")
  #junk <- emissions.dt[ADM0_NAME ==  "Kenya" & Animal %in% c("Chickens", "Pigs")]
  
  #data2plot <- emissions.dt[ADM0_NAME ==  "Kenya" & Animal %in% c("Chickens", "Pigs"),
  #                         sum (value,na.rm = T), by= .(Source, Gas)]
  
  data2plot[ Gas == "CH4", GWP:= GWP_CH4]
  data2plot[ Gas == "N2O", GWP:= GWP_N2O]
  
  
  data2plot[, V1:= V1 / 1e6 ] # Mt - new
  data2plot <- data2plot[, sum (V1 * GWP,na.rm = T), by= .(Source, Gas, isDirect)]
  
  
  if (isDirect == TRUE){
    data2plot <- data2plot[ isDirect == "TRUE"]
  }
  
  
  if (nrow (data2plot) ==0){
    return (empty_plot("no data for selection"))
  }
  cat ("Pie chart" , isDirect, "STR", str(data2plot), "\n")
  
  data2plot <- merge (data2plot, SankeyEmissionColors,by.x = c("Source"),by.y = "nodeName",  all.x = T)
  #data2plot
  
  # keep similar colors together!
  gases <- unique (data2plot$Gas)
  data2plot <- data2plot[ order (Gas,V1)]
  #data2plot[, pct:= 100 * V1/sum(V1)]
  #data2plot[ , hoverText := paste0(Source , "(", Gas, "):", formatC ( V1/1e6, digits= 2, format = "f", big.mark = ",", decimal.mark = "."), ' Mt')]
  
  data2plot[Source == "Feed-CO2", Source := "Feed (CO2)"]
  data2plot[Source == "LandUseChange", Source:= "LUC: soy and palm (CO2)"]
  data2plot[Source == "DirectOnFarmEnergy", Source:= "Direct on-farm energy (CO2)"]
  data2plot[Source == "PastureExpansion", Source:= "LUC: pasture expansion (CO2)"]
  data2plot[Source == "EmbeddedOnFarmEnergy", Source:= "Embedded on-farm energy (CO2)"]
  data2plot[Source == "Postfarm", Source:= "Post-farm (CO2)"]
  data2plot[Source == "Feed-CH4", Source:= "Feed (CH4)"]
  data2plot[Source == "Manure-CH4", Source:= "Manure (CH4)"]
  data2plot[Source == "EntericFermentation", Source:= "Enteric fermentation (CH4)"]
  data2plot[Source == "Feed-N2O", Source:= "Feed (N2O)"]
  data2plot[Source == "Manure-N2O", Source:= "Manure (N2O)"]
  
  fig <- plot_ly(data2plot,
                 labels = ~Source, values = ~V1,
                 type = 'pie', sort = FALSE,
                 # text = ~paste0(Source , "(", Gas, "):", formatC ( V1, digits= 2, format = "f", big.mark = ",", decimal.mark = "."), ' Mt') ,
                 text = ~paste0(Source , ": ", formatC ( V1, digits= 2, format = "f", big.mark = ",", decimal.mark = "."), ' Mt') ,
                 
                 textinfo = 'label+percent',
                 colors = ~color,
                 hoverinfo = 'text',
                 textposition= "outside",
                 #hovertemplate = "%{Source}: <br>Popularity: %{V1} </br> Mt",
                 marker = list(colors = ~color,
                               line = list(color = '#FFFFFF', width = 1)))  ##~paste( sprintf ("%.2f", V1), ' million t'))
  
  fig <- fig %>% layout(showlegend = F,title = "",
                        margin = list (b = 100, l = 50, r= 50, t= 100, pad= 0),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
  
  fig
  
  return (fig)
  
}

makeSankeyDataProduction <- function (nodes, selAnimals, selCountry , selCom = c("Meat", "Milk", "Eggs"), comodityUnit ){
  #selAnimals <- c("Cattle", "Goats")
  #selCountry <- "World"
  #nodes <- c("Animal", "LPS", "Item", "Element")
  #selCom <- c("Meat", "Milk")
  cat ("selcom in plotting", selCom, "\n")
  cat ("selUnitin plotting", comodityUnit, "\n")
  
  #rumEm.dt <- allData.dt[ ssddAnimal %in% selAnimals & COUNTRY == selCountry & Element2 %in% selCom]
  #rumEm.dt <- allData.dt[ VarName == "Production" & Animal %in% c("Pigs", "Chickens")  & COUNTRY == "World"]
  if (comodityUnit != "Protein"){
    rumEm.dt <- allData.dt[VarName == "Production"  & Animal %in%  selAnimals & ISO3  %in% selCountry & Item %in% selCom
                           & Element %in% c("Weight", "CarcassWeight"),.(Animal, HerdType, LPS,Element, Item, V1)]
    rumEm.dt <- rumEm.dt [, sum (V1, na.rm = T), by =  .(Animal, HerdType, LPS, Item, V1)]
    rumEm.dt[, Element:= "Weight"]
    
    
  }else{
    rumEm.dt <- allData.dt[VarName == "Production"  & Animal %in%  selAnimals & ISO3  %in% selCountry & Item %in% selCom
                           & Element %in% c("Protein"),.(Animal, HerdType, LPS,Element, Item, V1)]
    
  }
  
  
  #rumEm.dt <- rumEm.dt[ Element == "Protein"]
  # rumEm.dt <- rumEm.dt[ Element %in% c("Weight", "CarcassWeight")]
  
  #protein.dt <- allData.dt[VarName == "Production"  & Animal %in%  selAnimals & COUNTRY  %in% selCountry & Item %in% selCom
  #                         & Element %in% c("Protein"),.(Animal, HerdType, LPS, Item, V1)]
  #protein.dt[, Unit:= "Protein"]
  
  rumEm.dt[, sum (V1), by= "Animal"]
  rumEm.dt[ Animal %in% c("Chickens", "Pigs"), HerdType:= paste0 (Animal, "Herd")]
  
  #rumEm.dt <- rumEm.dt[VarName == "Production"  & Animal %in% selAnimals & COUNTRY %in% selCountry,.(Animal, HerdType, LPS, Item, V1)]
  
  if (nrow (rumEm.dt )==0){
    return (make_emptySankeyPlot())
  }
  curData.dt <- rumEm.dt[, sum (V1),by = nodes]
  curData.dt<- curData.dt[ V1 >0]
  curData.dt[, V1:= V1 / 1e6 ] # Mt
  nodes2plot <- character()
  for (curNode in nodes){
    # cat (unique (curData.dt[, get(curNode)]))
    nodes2plot <- c(nodes2plot, unique (curData.dt[, get(curNode)]) )
  }
  #nodes2plot <- c("Animal", nodes2plot)
  
  if (nrow (curData.dt )==0){
    return (make_emptySankeyPlot())
  }
  if (length (nodes )<= 1){
    return (make_emptySankeyPlot())
  }
  links <- data.table ()
  for (i in 1: (length (nodes) -1)){
    #print (i)
    curNodes <- c(nodes[i], nodes [i+1])
    xx <- curData.dt[ , sum (V1), by= curNodes]
    names (xx) <- c("from", "to", "value")
    #nodes2plot <- c(unique (xx$from), unique (xx$to))
    xx[ , fromID:= nodes2plot %>% match (x =from)-1]
    xx[ , toID:= nodes2plot %>% match (x =to)-1]
    # print (xx)
    # xx <- xx[ complete.cases(xx)]
    links <- rbindlist (list (links, xx), use.names = T)
  }
  # this is a hack becasue the 3js color scheme does not accept blank
  links[ ,from:= gsub (" ", "_", from)]
  links[ ,to:= gsub (" ", "_", to)]
  nodes2plot <-  gsub (" ", "_" , nodes2plot)
  
  
  colorDF <- data.table (nodeName = nodes2plot)
  #TODO: fix colors
  # colorDF[, color:= rainbow ( nrow (colorDF))]
  colorDF[ ,color:= getEmissionNodeColor (nodeName), by= seq_along(1:nrow (colorDF))]
  
  my_colorX <- paste ('d3.scaleOrdinal() .domain([',  toString(shQuote(colorDF$nodeName)), ' ]) .range([',
                      toString((shQuote(colorDF$color))), '])'
  )
  
  
  
  data2plot <- list (nodes = data.frame (name= nodes2plot), links = links)
  
  p <- sankeyNetwork(Links = data2plot$links, Nodes = data2plot$nodes, Source = "fromID", iterations = 128,
                     Target = "toID", Value = "value", NodeID = "name",colourScale = my_colorX,
                     units = "millon tonnes of product", fontSize = 12, nodeWidth = 40)
  
  #p
  #cat (getwd(), "\n")
  
  
  #saveNetwork(p, "ProductinSankey.html")
  #webshot("ProductinSankey.html", "ProductinSankey.png", vwidth = 2000, vheight = 900)
  
  p$x$links$name <- paste (data2plot$links$from, "->",  
                           data2plot$links$to,format(round(as.numeric(data2plot$links$value), 3), nsmall=1, big.mark=",")      , "Mt")
  
  p$x$node$labStr <- paste (p$x$node$name, "Total")
  
  p <- htmlwidgets::onRender(
    p,
    '
  function(el, x) {
  d3.selectAll(".link").select("title foreignObject body pre")
  .text(function(d) { return d.name; });
  }
  '
  )
  p <- htmlwidgets::onRender(
    p,
    '
  function(el, x) {
  d3.selectAll(".node").select("title foreignObject body pre")
  .text(function(d) { return d.labStr; });
  }
  '
  )
  
  p
  
  
  
}

makeSankeyDataEmissionDetails_2 <- function (nodes, selAnimals, selCountryISO3,isDirect, GWP_N2O, GWP_CH4 ){
  #GWP_N2O <- 298
  #GWP_CH4 <- 34
  #selAnimals <- c( "Cattle", "Buffalo")
  #selCountry <- "World"
  #selCommodity <- "Meat"
  #nodes <- emissionSankeyNodes
  #nodes <-  c("Animal", "EmissionSource","Gas")
  #emissionSankeyNodes <- c("Animal", "HerdType", "LPS", "Commodity", "EmissionSource", "Gas")
  # some renaming for older code..
  #isDirect = F
  #allData.dt[ COUNTRY== "World"]
  rumEm.dt <- allData.dt[Animal %in% selAnimals & ISO3 == selCountryISO3 & VarName == "Emissions"]
  
  setnames (rumEm.dt, "Item", "EmissionSource")
  setnames (rumEm.dt, "Element", "Gas")
  setnames (rumEm.dt, "Element2", "Commodity")
  
  rumEm.dt[ Animal %in% c("Chickens", "Pigs"), HerdType:= paste0 (Animal, "Herd")]
  if (length (nodes )== 1){
    cat ("nodes zero \n")
    return (make_emptySankeyPlot())
  }
  if (nrow (rumEm.dt )== 0){
    cat ("nodes zero \n")
    return (make_emptySankeyPlot())
  }
  #rumEm.dt <- rumEm.dt[Commodity %in% selCommodity]
  #names (rumEm.dt) <- c("Animal", "HerdType", "LPS", "EmissionSouce", "Gas", "GWP")
  rumEm.dt[ Gas == "CO2", GWP:= 1.0]
  rumEm.dt[ Gas == "CH4", GWP:=  GWP_CH4]
  rumEm.dt[ Gas == "N2O", GWP:= GWP_N2O]
  cat ("Sankey", GWP_N2O, GWP_CH4, "\n")
  if (isDirect == TRUE){
    rumEm.dt <- rumEm.dt[ isDirect == TRUE]
  }
  # rumEm.dt <- emissionsDetail.dt[Animal %in% c("Cattle", "Sheep", "Goats") &  ADM0_NAME == "Kenya"]
  rumEm.dt[, V1:= V1* GWP / 1e6 ] # Mt
  #nodes <- emissionSankeyNodes
  # nodes <- c( "Animal","emissionSource", "Gas")
  #nodes <- nodes [ !(nodes %in% c("Animal"))]
  curData.dt <- rumEm.dt[, sum (V1),by = nodes]
  curData.dt<- curData.dt[ V1 >0]
  
  # cat ("curD", length (curData.dt),"\n" )
  nodes2plot <- character()
  for (curNode in nodes){
    # cat (unique (curData.dt[, get(curNode)]))
    nodes2plot <- c(nodes2plot, unique (curData.dt[, get(curNode)]) )
  }
  # nodes2plot <- c("Animal", nodes2plot)
  
  if (length (nodes2plot )== 0){
    return (make_emptySankeyPlot())
  }
  links <- data.table ()
  for (i in 1: (length (nodes) -1)){
    #print (i)
    curNodes <- c(nodes[i], nodes [i+1])
    xx <- curData.dt[ , sum (V1), by= curNodes]
    names (xx) <- c("from", "to", "value")
    #nodes2plot <- c(unique (xx$from), unique (xx$to))
    xx[ , fromID:= nodes2plot %>% match (x =from)-1]
    xx[ , toID:= nodes2plot %>% match (x =to)-1]
    # print (xx)
    # xx <- xx[ complete.cases(xx)]
    links <- rbindlist (list (links, xx), use.names = T)
  }
  # this is a hack becasue the 3js color scheme does not accept blank
  links[ ,from:= gsub (" ", "_", from)]
  links[ ,to:= gsub (" ", "_", to)]
  nodes2plot <-  gsub (" ", "_" , nodes2plot)
  
  
  colorDF <- data.table (nodeName = nodes2plot)
  for (xx in colorDF$nodeName){
    #print (xx)
    #print (SankeyEmissionColors[ nodeName == xx]$color )
    colorDF[ nodeName ==xx, color:=SankeyEmissionColors[ nodeName == xx]$color ]
  }
  
  colorDF[nodeName == "Feed-CO2", nodeName := "Feed_(CO2)"]
  colorDF[nodeName == "LandUseChange", nodeName:= "LUC:_soy_and_palm_(CO2)"]
  colorDF[nodeName == "DirectOnFarmEnergy", nodeName:= "Direct_on-farm_energy_(CO2)"]
  colorDF[nodeName == "PastureExpansion", nodeName:= "LUC:_pasture_expansion_(CO2)"]
  colorDF[nodeName == "EmbeddedOnFarmEnergy", nodeName:= "Embedded_on-farm_energy_(CO2)"]
  colorDF[nodeName == "Postfarm", nodeName:= "Post-farm_(CO2)"]
  colorDF[nodeName == "Feed-CH4", nodeName:= "Feed_(CH4)"]
  colorDF[nodeName == "Manure-CH4", nodeName:= "Manure_(CH4)"]
  colorDF[nodeName == "EntericFermentation", nodeName:= "Enteric_fermentation_(CH4)"]
  colorDF[nodeName == "Feed-N2O", nodeName:= "Feed_(N2O)"]
  colorDF[nodeName == "Manure-N2O", nodeName:= "Manure_(N2O)"]
  
  #colorDF[ ,color:=SankeyEmissionColors[ nodeName == node]$color, by= row.names(colorDF) ]
  #colorDF[, color:= rainbow ( nrow (colorDF))]
  my_colorX <- paste ('d3.scaleOrdinal() .domain([',  toString(shQuote(colorDF$nodeName)), ' ]) .range([',
                      toString((shQuote(colorDF$color))), '])'
  )
  
  
  
  data2plot <- list (nodes = data.frame (name= nodes2plot, noName = ""), links = links)
  # data2plot[, labelTxt:= as.character(value)]
  data2plot$noText
  
  data2plot$nodes$name[data2plot$nodes$name == "Feed-CO2"] <- "Feed_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "LandUseChange"] <- "LUC:_soy_and_palm_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "DirectOnFarmEnergy"] <- "Direct_on-farm_energy_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "PastureExpansion"] <- "LUC:_pasture_expansion_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "EmbeddedOnFarmEnergy"] <- "Embedded_on-farm_energy_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "Postfarm"] <- "Post-farm_(CO2)"
  data2plot$nodes$name[data2plot$nodes$name == "Feed-CH4"] <- "Feed_(CH4)"
  data2plot$nodes$name[data2plot$nodes$name == "Manure-CH4"] <- "Manure_(CH4)"
  data2plot$nodes$name[data2plot$nodes$name == "EntericFermentation"] <- "Enteric_fermentation_(CH4)"
  data2plot$nodes$name[data2plot$nodes$name == "Feed-N2O"] <- "Feed_(N2O)"
  data2plot$nodes$name[data2plot$nodes$name == "Manure-N2O"] <- "Manure_(N2O)"
  
  data2plot$links$from[data2plot$links$from == "Feed-CO2"] <- "Feed_(CO2)"
  data2plot$links$from[data2plot$links$from == "LandUseChange"] <- "LUC:_soy_and_palm_(CO2)"
  data2plot$links$from[data2plot$links$from == "DirectOnFarmEnergy"] <- "Direct_on-farm_energy_(CO2)"
  data2plot$links$from[data2plot$links$from == "PastureExpansion"] <- "LUC:_pasture_expansion_(CO2)"
  data2plot$links$from[data2plot$links$from == "EmbeddedOnFarmEnergy"] <- "Embedded_on-farm_energy_(CO2)"
  data2plot$links$from[data2plot$links$from == "Postfarm"] <- "Post-farm_(CO2)"
  data2plot$links$from[data2plot$links$from == "Feed-CH4"] <- "Feed_(CH4)"
  data2plot$links$from[data2plot$links$from == "Manure-CH4"] <- "Manure_(CH4)"
  data2plot$links$from[data2plot$links$from == "EntericFermentation"] <- "Enteric_fermentation_(CH4)"
  data2plot$links$from[data2plot$links$from == "Feed-N2O"] <- "Feed_(N2O)"
  data2plot$links$from[data2plot$links$from == "Manure-N2O"] <- "Manure_(N2O)"
  
  data2plot$links$to[data2plot$links$to == "Feed-CO2"] <- "Feed_(CO2)"
  data2plot$links$to[data2plot$links$to == "LandUseChange"] <- "LUC:_soy_and_palm_(CO2)"
  data2plot$links$to[data2plot$links$to == "DirectOnFarmEnergy"] <- "Direct_on-farm_energy_(CO2)"
  data2plot$links$to[data2plot$links$to == "PastureExpansion"] <- "LUC:_pasture_expansion_(CO2)"
  data2plot$links$to[data2plot$links$to == "EmbeddedOnFarmEnergy"] <- "Embedded_on-farm_energy_(CO2)"
  data2plot$links$to[data2plot$links$to == "Postfarm"] <- "Post-farm_(CO2)"
  data2plot$links$to[data2plot$links$to == "Feed-CH4"] <- "Feed_(CH4)"
  data2plot$links$to[data2plot$links$to == "Manure-CH4"] <- "Manure_(CH4)"
  data2plot$links$to[data2plot$links$to == "EntericFermentation"] <- "Enteric_fermentation_(CH4)"
  data2plot$links$to[data2plot$links$to == "Feed-N2O"] <- "Feed_(N2O)"
  data2plot$links$to[data2plot$links$to == "Manure-N2O"] <- "Manure_(N2O)"
  
  
  p <- sankeyNetwork(Links = data2plot$links, Nodes = data2plot$nodes, Source = "fromID", iterations = 128,
                     Target = "toID", Value = "value", NodeID = "name",colourScale = my_colorX,
                     units = " millon tonnes CO2eq", fontSize = 12, nodeWidth = 40)
  # add onRender JavaScript to set the title to the value of 'State' for each node
  p$x$links$name <- paste (data2plot$links$from, "->",  
                           data2plot$links$to,format(round(as.numeric(data2plot$links$value), 3), nsmall=1, big.mark=",")      , "Mt of CO2eq")
  
  p$x$node$labStr <- paste (p$x$node$name, "Total")
  
  p <- htmlwidgets::onRender(
    p,
    '
  function(el, x) {
  d3.selectAll(".link").select("title foreignObject body pre")
  .text(function(d) { return d.name; });
  }
  '
  )
  p <- htmlwidgets::onRender(
    p,
    '
  function(el, x) {
  d3.selectAll(".node").select("title foreignObject body pre")
  .text(function(d) { return d.labStr; });
  }
  '
  )
  p <- htmlwidgets::onRender(
    p,
    '
 function(el, x) {
      d3.selectAll(".link")
        .append("node")
        .text(function(d) { return d.labStr; });
  }
  '
  )
  
  
  
  p
  #require(htmlwidgets)
  #saveWidget(p, file="name_of_your_file.html")
  #p
  #saveNetwork(p, "sn.html")
  
  # you convert it as png
  #webshot("sn.html","sn.png", vwidth = 1000, vheight = 900)
  #p
  
}


make_emptySankeyPlot <- function(){
  links <- data.frame (from = 0, to= 1, value = 1)
  nn <- c("No data for selection", "Select different combination!")
  nodes <- data.frame  (nn)
  data2plot <- list (links= links, nodes = nodes)
  p <- sankeyNetwork(Links = data2plot$links, Nodes = data2plot$nodes, Source = "from",
                     Target = "to", Value = "value", NodeID = "nn",
                     units = "CO2eq", fontSize = 16, nodeWidth = 40)
  
  
  
}


selProduct <- "Meat"
selAnimals <- "Chickens"
selCountry <- "World"
selLPS <-  c("Backyard", "Broiler")
if (selProduct == "Meat"){
  curUnits <- "CarcassWeight"
}else{
  curUnits <- "Weight"
}
#plot_emissionIntensityBoxplot (input$selectCountry,input$inOuputSelect, input$inSelectMonogastrics, input$intSelectLPSMono, input$inSyncScales )

plot_emissionIntensityBoxplot <-function (selCountry, selProduct, selUnit,  selAnimals, selLPS){
  #  selCountry <- "World"
  #   selProduct <- c("Meat")
  #  selAnimals <- c("Cattle", "Pigs")
  # selLPS <- c("Mixed", "Industrial")
  #  selUnit <- "Weight"
  # selLPS <- c("Mixed", "Broiler")
  #selHerds <- ("ALL")
  cat ("SelCountry", selCountry, "\n")
  cat ("SelAnimal", selAnimals, "\n")
  cat ("SelComn", selProduct, "\n")
  cat ("SelComn", selLPS, "\n")
  #syncXScale <- F
  # if (selProduct %in% c("Eggs", "Milk")){
  #   curUnits <- "Weight"
  # }else{
  #   curUnits <- "CarcassWeight"
  # }
  # if (length (selProduct) >1){
  #  
  # }
  # if (selProduct == "Protein" ){
  #   ylabel <- "Protein"
  #   cat ("You selected protein\n")
  #   selProduct <- c("Meat", "Milk", "Eggs")
  #   curUnits <- "Protein"
  # }
  makeFacets <- F
  # build the nodes ###################################
  
  if (length (selLPS) == 0){
    return (empty_plotGG("No data for selection."))
  }
  
  
  if (length (selProduct) == 3){
    selNodes <- "LPS"
  }
  
  
  if (selLPS != "ALL"){
    makeFacets = T
    selNodes <- c("Commodity", "LPS")
    if (length (selProduct) == 3) { # Total protein
      selNodes <- "LPS"
    }
  }else{
    selLPS <- unique (allData.dt[ VarName == "Production"]$LPS)
    selNodes <- "Commodity"
    if (length (selProduct) == 3) { # Total protein
      selNodes <- ""
    }
    
  } 
  if (selUnit == "Weight") {
    selUnit<- c("CarcassWeight", "Weight")
  }
  
  selNodes <- c("Commodity", "LPS")
  allHerds <- unique   (  allData.dt[ VarName == "Production" ]$LPS)
  countryData.dt <- getEmissionIntensityAll  (  "Production" , selAnimals, c("Dairy", "Beef", "Pigs", "Chickens"),
                                                selLPS,selProduct ,unique (allData.dt[ VarName == "Emissions"]$Item),selUnit,selNodes)
  
  #countryData.dt <- getEmissionIntensityAll  (  "Production" , c("Chickens", "Pigs") , c("Dairy", "Beef", "Pigs", "Chickens"),
  #                                              c("Backyard", "Industrial", "Layers"),c("Meat", "Eggs") ,unique (allData.dt[ VarName == "Emissions"]$Item),c("Protein"),selNodes)
  
  
  
  #countryData.dt <- getEmissionIntensityAll  (  "Production" , c("Chickens", "Pigs") , c("Dairy", "Beef", "Pigs", "Chickens"),
  #                                              c("Backyard", "Industrial", "Layers"),c("Meat", "Eggs") ,unique (allData.dt[ VarName == "Emissions"]$Item),c("Weight"),selNodes)
  
  
  
  
  unique (countryData.dt$LPS)
  # global max for all animals 
  #if (syncXScale == T){
  
  allData.temp <- getEmissionIntensityAll  (  "Production" , c(Monogastrics, Ruminants),c("Dairy", "Beef", "Pigs", "Chickens"),
                                              unique (allData.dt$LPS),selProduct ,unique (allData.dt[ VarName == "Emissions"]$Item),selUnit,selNodes)
  maxXValue <- quantile(allData.temp$Intensity,0.8,na.rm = T) 
  numBins <-30 
  cat ("Sync scales requested, maxX = ", maxXValue, "\n")
  brks <- seq (from = 0, to = maxXValue,length.out = numBins )
  
  
  #  }else{
  #cat ("Free scales requested\n")
  maxXValue <- quantile(countryData.dt$Intensity,0.9,na.rm = T) 
  numBins <-30 
  brks <- seq (from = 0, to = maxXValue,length.out = numBins )
  cat ("Free scales requested, maxX = ", maxXValue, "\n")
  # }
  
  
  
  globalAvg.dt <-   countryData.dt[COUNTRY== "World",]
  globalAvg.dt
  globalAvg.dt$LegLabel <- "Global average"
  #if (selCountry %in% names (specialRegions)){
  #  cCountries <- gaulList.dt[ ISO3 != ""  & ISO3 %in% specialRegions  [[  selCountry]]  ]$COUNTRY
  #  countryData.dt <- countryData.dt[ COUNTRY %in% cCountries ]
  #}else{
  #  countryData.dt <- countryData.dt[ COUNTRY %in% selCountry]
  #}
  
  
  if (nrow (countryData.dt) == 0){
    return (empty_plotGG("No data for selection"))
  }
  
  
  
  binwidth <- brks[2]
  
  #n_bins <- length(ggplot2:::bin_breaks_width(range(countryData.dt$Intensity), width = binwidth)$breaks) - 1L
  # ggplot() + geom_histogram(aes(x = myData), binwidth = binwidth, fill = rainbow(numBins)) 
  
  gp <- ggplot(countryData.dt, aes(Intensity  , weight = Production,after_stat(density)), fill= cut (Intensity,1)) + geom_histogram(breaks = brks, fill = "grey80" )
  gp
  #gp <- gp +   geom_freqpoly( )
  gp <- gp + geom_vline (data = globalAvg.dt, aes (xintercept =  Intensity), linetype = "dashed", color = "blue", size=.7)
  
  gp <- gp + geom_text(data = globalAvg.dt, aes(x=Intensity, label="\nGlobal average", y=Inf), vjust = 0.6, hjust =1.2, colour="blue", angle=90, text=element_text(size=11)) 
  
  # gp <- gp + annotate ("text", data = globalAvg.dt, aes(x=Intensity, label="\nGlobal average", y=.5), colour="blue", angle=0, text=element_text(size=11)) 
  # gp <- gp + scale_color_manual(values = LegLabel )
  gp
  
  if (!(selCountry %in% names (specialRegions))){
    curCountry.dt <- countryData.dt[COUNTRY == selCountry]
    gp <- gp + geom_vline (data = curCountry.dt, aes (xintercept =  Intensity), linetype = "solid", color = "red", size = .7)
    gp <- gp + geom_text(data = curCountry.dt, aes(x=Intensity, label=paste0("\n", selCountry), y=Inf), vjust = 0.5, hjust =1.2, colour="red", angle=90, text=element_text(size=11)) 
    
  }
  
  gp <- gp + xlim (0,maxXValue)
  exx <- expression(Anthropogenic~COSO[2]^~(ngm^-3))
  if (selProduct == "Meat"){
    gp <- gp +   labs(x=expression(paste('kg ',CO[2]^{},'eq per kg carcass weight')))
  }
  if (length (selProduct) ==3){
    gp <- gp +   labs(x=expression(paste('kg ',CO[2]^{},'eq per kg protein')))
  }
  if (selProduct %in% c("Milk", "Eggs")){
    gp <- gp +   labs(x=expression(paste('kg ',CO[2]^{},'eq per kg product')))
  }
  
  # gp <- gp +   labs(x=expression(paste('kg ',CO[2]^{},'eq per kg Carcass weight')))
  
  
  gp <- gp+ theme_bw()
  if (makeFacets == T){
    gp <- gp + facet_wrap(~ LPS, ncol=1)
  }
  gp <- gp + theme(strip.text.x = element_text(size = 20, color = "grey10"))
  gp
  gp <- gp + theme (strip.background = element_rect(fill = "lightblue", colour = "grey90", size = 1))
  # p <- ggplotly(gp)
  gp
  #p
  
  
  
  
  
  return (gp)
}

plot_emissionIntensityBoxplotGGPLOT <-function (selCountry, selProduct, animals, freeScale = F){
  #return (empty_plot())
  # selProduct <- "CarcassWeight"
  animals = ruminants
  maxY <- quantile (prodEm.dt[ !(ADM0_NAME %in% (names (SpecialRegions)))  & Commodity == selProduct]$EmissionIntensity,0.9, na.rm = T)
  maxY <- quantile (prodEm.dt[ !(ADM0_NAME %in% (names (SpecialRegions)))  & Commodity == selProduct]$EmissionIntensity,0.9, na.rm = T)
  
  if (length (animals) ==0){
    return (empty_plot ("no data for selection"))
  }
  
  if (animals[1] %in% monogastrics){
    data2plot <- prodEm.dt[Commodity == selProduct &  !(is.na (EmissionIntensity)) & Animal %in% animals ]
    
    data2plot[ ,Label:= paste (Animal, "\n", ProdSys)]
    
  }else{
    data2plot <- prodEm.dt[Commodity == selProduct &  !(is.na (EmissionIntensity)) & Animal %in% animals ]
    
    data2plot[ ,Label:= paste (Animal, "\n", HerdType, "-", ProdSys)]
    
  }
  if (nrow (data2plot) ==0){
    return (empty_plot ("no data for selection"))
  }
  maxY <- max (maxY, max (data2plot[ADM0_NAME == selCountry ]$EmissionIntensity)*1.1, na.rm = T)
  if (freeScale ==T){
    maxY <- quantile (prodEm.dt[Commodity == selProduct &  Animal %in% monogastrics]$EmissionIntensity,0.9, na.rm = T)
    max (maxY, max (data2plot[ADM0_NAME == selCountry & Animal %in% monogastrics ]$EmissionIntensity)*1.1, na.rm = T)
    
  }
  AnimalProdSysHerdType2Show <- unique (data2plot[ ADM0_NAME == selCountry &!(is.na (EmissionIntensity))]$Label)                     
  data2plot <- data2plot[Label %in% AnimalProdSysHerdType2Show]
  setnames (data2plot, "EmissionIntensity", "value")
  gp <- ggplot (data2plot) + theme_bw()
  gp <- gp + geom_boxplot(aes (y = value, x= Label), outlier.shape = NA, fill = "grey90") 
  
  gp <- gp + ylim (0,maxY)
  
  
  
  gp
  
  gp <- gp + geom_point(data = data2plot[ ADM0_NAME == selCountry],
                        aes (x =Label , y = value,
                             text=sprintf("<b>%s:</b><br> %s", Label, sprintf("%.0f kg", value)),
                        ),shape = 21, fill= "dodgerblue3", color = "blue",size= 8)
  
  
  gp <- gp + geom_text(data = data2plot[ ADM0_NAME == selCountry], 
                       aes (x =Label , y = value, label = sprintf ("%.0f", value) ), color= "white")
  gp <- gp + coord_flip ()
  gp
  
}

#fig <- plot_emissionIntensityBoxplot ("Italy", "Meat protein", c("Chickens", "Pigs"))
#fig 



#bins <- c(1, 50,100,  150, 200,250,300,350,20000)

#plot_rasterMap2 ("Italy", emIntensity.ras.file, "fuck",bins)
#mem_used()




makeSankeyPlot <- function (data.dt, colNames,plotTitle ){
  # debug 
  #return (empty_plot("Coming soon"))
  colNames <- c( "Animal" ,  "ProdSystem", "Commodity", "Source", "Gas")
  #colNames <- c( "Animal" ,  "ProdSystem", "Output")
  cat (str (data))
  data.dt <- emissions.dt [ ADM0_NAME == "Italy" & Animal %in% c("Cattle") ]
  #data.dt <- prod.dt[ ADM0_NAME == "Italy" & Animal %in% c("Chickens","Pigs") &
  #           Output == "Meat total",
  #         sum (value, na.rm = T), by = .(Animal,ProdSystem,HerdType ,Output)]
  
  plotTitle <- "FUck"
  setnames (data.dt, "value", "Value")
  
  numNodes <- numeric ()
  for (cCol in colNames){
    numNodes <- c(numNodes,  length (unique (data.dt[, get(cCol)])))
  }
  
  
  allNodes <- getAllNodes (data.dt, colNames )
  
  
  if (nrow (data.dt) ==0){
    return (empty_plot ("no data for selection"))
  }
  
  #data.dt <- merge (data.dt)
  sankeyData <- make_sankey_data(data.dt, colNames, allNodes)
  #sankeyData <- merge (sankeyData, gleamColors.dt,by.x = c("From", "To"), by.y = c("Animal", "ProdSystem"), all.x = T)
  
  #sankeyData[ is.na (Color), Color:= "red" ]
  #colNames
  
  # first node
  
  #sankeyData[ is.na (Color), Color:= "red"]
  # fwrite (data.dt, "testData.csv")
  #  fwrite (allNodes, "testDataNodes.csv")
  
  #sankeyData[ indexFrom <= length (unique (data.dt$Animal ]
  # sankeyData <- fread ('testData.csv')
  #  allNodes <- fread ('testDataNodes.csv')
  #allNodes
  #sankeyData[ is.na (Color), Color:= gleamColorCommodity.dt[ gleamColorCommodity.dt$Commodity == sankeyData$To]$Color ]
  
  #sColors <- unique (sankeyData$Color)
  #sankeyData[order (indexFrom, indexTo)]
  #sankeyData$Color <- "blue"
  #sColors <-  c("red","blue", "green", "orange", "darkgreen", "darkblue","olivgreen", "lightblue")
  #findColors <- function (allN){
  
  aa <- allNodes [allNodes %in% gleamColorAnimals.dt$Animal]
  
  #sankeyData[indexFrom < length (aa)]
  
  
  
  outColors <- character (length (allNodes))
  #outColors  <- "#D3D3D3"
  aa <- allNodes [allNodes %in% gleamColorAnimals.dt$Animal]
  
  i <- 1
  for (curNode in allNodes){
    outColors[i] <- "#D3D3D3"
      if (curNode %in% gleamColorAnimals.dt$Animal){
        outColors[i] <- tail (gleamColorAnimals.dt[ Animal == curNode]$Color,1)
      }
    if (curNode %in% gleamColorAnimals.dt$ProdSystem){
      outColors[i] <- tail (gleamColorAnimals.dt[ ProdSystem == curNode]$Color,1)
    }
    if (curNode %in% gleamColorCommodity.dt$Commodity){
      outColors[i] <- tail (gleamColorCommodity.dt[ Commodity == curNode]$Color,1)
    }
    if (curNode %in% gleamColorEmissions.dt$Source){
      outColors[i] <- tail (gleamColorEmissions.dt[ Source == curNode]$Color,1)
    }
    
    if (curNode %in% gleamColorEmissions.dt$Gas){
      outColors[i] <- tail (gleamColorEmissions.dt[ Gas == curNode]$Color,1)
    }
    
    #gleamColorEmissions.dt
    
    
    i <- i + 1
  }
  
  outColors <- trimws (outColors)  
  #merge (sankeyData, gleamColorAnimals.dt, by.x = c("From",))
  
  sankeyData %>%  plot_ly(
    type = "sankey",
    orientation = "h",
    # text = "fuck",
    #hoverinfo = paste (sankeyData$From, "----", sankeyData$To),
    #hoverlabel = paste (sankeyData$From, "----", sankeyData$To),
    
    node = list(
      label = allNodes,
      color = outColors,
      pad = 15,
      thickness = 20,
      hoverinfo = 'text',
      hovertext = paste ("MyTest", sankeyData$From, "----", sankeyData$To),
      # hoverinfo = paste ("MyTest", sankeyData$From, "----", sankeyData$To),
      hoverinfo = "none",
      
      
      colors = sankeyData$Color,
      line = list(
        #color = data2plot.dt$Animal,
        width = 0.0
      )
    ),
    
    link = list(
      source =sankeyData$indexFrom,
      target = sankeyData$indexTo,
      value = sankeyData$Value,
      #color = outColors
      #text = "fucksss",
      label = 'text'
    )
  )
  
  
}


