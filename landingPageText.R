 



lpTextWelcome  <- tags$div ( "This dashboard is a conveniently accessible interface to data produced by the Global Livestock Environmental Assessment Model",
                             tags$a(href="http://www.fao.org/gleam/en/", 
                                    "(GLEAM)."),"It is intended to support FAO member countries when compiling data on greenhouse gas emissions from livestock and to explore geospatial and other data sets related to livestock production. 
Data are organized in four different domains. Reports can be produced on the fly, summarizing input and output data for selected countries or regions. Data can be downloaded for further processing and analysis. To start, please select one of the domains below, or click on the toggle button above. ", br(), br())


lpTextProduction  <- tags$div ( "Here you find production data for livestock products (meat, milk and eggs). Data are for the reference year 2015 and based on GLEAM V 3.0. ", br(), br ())

lpTextPopulation  <- tags$div ( "This section provides data on the populations of different livestock species and production systems at the pixel level. Data are
                                                  based on the Gridded Livestock of the World ",
                                tags$a(href="http://www.fao.org/livestock-systems/global-distributions/en/", 
                                       "(GLW)." ))

lpTextEmissions  <- tags$div ( "This section provides greenhouse gas emissions from livestock systems, estimated using GLEAM for the reference year 2015."
                               )

lpTextEmIntensity  <- tags$div ("Greenhouse gas emission per unit of livestock product.")




lpTextMRV  <- tags$div ( "Addidional resources related to Measuring, Reporting and Verificatio (MRV) of greenhouse gas emissions.")
                         


lpTextReports  <- tags$div ( "Create automated country reports for emissions in the livestock sector.")
