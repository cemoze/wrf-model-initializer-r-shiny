# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinyFiles)
library(shinyTime)
library(stringr)
library(leaflet)
library(lubridate)
library(readr)

#FNL and ERA5 Download Functions

#FNL Downloader
#Please create a rda.ucar.edu account and type the email and password below that you've registered.
email = ""
pass = ""

fnl_downloader = function(email, pass, path, from, to) {
  
  email = email
  pass = pass
  
  setwd(path)
  
  Sys.setenv(TZ="GMT")
  data_i = from 
  data_f = to 
  
  dir.create(paste(path,"/fnl_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
  setwd(paste(path,"/fnl_",data_i,"_to_",data_f, sep = ""))
  
  seq_dates = seq(as.POSIXct(data_i), as.POSIXct(data_f), by = "6 hour")
  list_dates = format(seq_dates, "%Y%m%d_%H_00")
  file_name = paste0("fnl_", list_dates, ".grib2") #as.list(paste(my_url, file_name, sep = ""))
  
  if (data_i <= "2007-12-06" & data_f <= "2007-12-06") {
    
    down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib1/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib1")
    
  } else if (data_i >= "2007-12-06" & data_f >= "2007-12-06") {
    
    down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib2/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib2")
    
  }
  
  system(paste0("wget -O Authentication.log --save-cookies auth.rda_ucar_edu  --post-data 'email=", email, "&passwd=", pass, "&action=login' https://rda.ucar.edu/cgi-bin/login"))
  
  
  for (i in 1:length(file_name)){
    
    if (file.exists(file_name[i])) {
      
      cat(paste0(file_name[i], " - already downloaded. \n"))
      
    } else {
      
      system(paste0("wget -N --load-cookies auth.rda_ucar_edu ", down_link[i]))
      
    }
  }
  
  system(paste0("rm -rf Authentication.log auth.rda_ucar_edu"))
  
}

#ERA5 Downloader
#Please follow the instructions found at the link below before using it!
#https://cds.climate.copernicus.eu/api-how-to

era5_surf_downloader = function(path, from, to, area) {
  
  # time sequence
  data_i = from
  data_f = to
  
  setwd(path)
  
  dir.create(paste(path,"/ERA5_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
  setwd(paste(path,"/ERA5_",data_i,"_to_",data_f, sep = ""))
  
  list_dates = seq.Date(as.Date(data_i), as.Date(data_f), by = "day")
  list_dates = format(list_dates, "%Y%m%d")
  
  for (i in 1:length(list_dates)) {
    
    if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".grib"))) {
      
      cat(paste0("ERA5_sfc_", list_dates[i], ".grib"), " - already downloaded. \n")
      
    } else if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".nc"))) {
      
      cat(paste0("ERA5_sfc_", list_dates[i], ".nc"), " - already downloaded. \n")
      
    } else {
      
      txt = c("#!/usr/bin/env python", 
              "import cdsapi",
              "",
              "c = cdsapi.Client()",
              "",
              "c.retrieve(",
              "'reanalysis-era5-single-levels',",
              "{",
              "'product_type':'reanalysis',",
              "'format':'grib',",
              "'variable':[",
              "'10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',",
              "'2m_temperature','land_sea_mask','mean_sea_level_pressure',",
              "'sea_ice_cover','sea_surface_temperature','skin_temperature',",
              "'snow_depth','soil_temperature_level_1','soil_temperature_level_2',",
              "'soil_temperature_level_3','soil_temperature_level_4','surface_pressure',",
              "'volumetric_soil_water_layer_1','volumetric_soil_water_layer_2','volumetric_soil_water_layer_3',",
              "'volumetric_soil_water_layer_4'",
              "],",
              paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
              paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
              paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
              "'time': [",
              "'00:00', '01:00', '02:00',",
              "'03:00', '04:00', '05:00',",
              "'06:00', '07:00', '08:00',",
              "'09:00', '10:00', '11:00',",
              "'12:00', '13:00', '14:00',",
              "'15:00', '16:00', '17:00',",
              "'18:00', '19:00', '20:00',",
              "'21:00', '22:00', '23:00',",
              "],",
              "'area': [",
              paste0(area[1],",",area[2],",",area[3],",",area[4]),
              "],",
              "},",
              paste0("'ERA5_sfc_", list_dates[i], ".grib')")
      )
      
      writeLines(txt, paste0("ECMWF_ERA5_sfc_", list_dates[i], ".py"))
      
      cat(paste0("Downloading ECMWF ERA5 WRF sfc variables, day: ", list_dates[i]), "\n")
      system(command = paste0("python ECMWF_ERA5_sfc_", list_dates[i], ".py"), ignore.stdout = F, ignore.stderr = F)
      system(command = paste0("rm -rf ECMWF_ERA5_sfc_", list_dates[i], ".py"), ignore.stdout = F, ignore.stderr = F)
    }
  }
}

era5_pl_downloader = function(path, from, to, area) {
  
  # time sequence
  data_i = from
  data_f = to
  
  setwd(path)
  
  dir.create(paste(path,"/ERA5_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
  setwd(paste(path,"/ERA5_",data_i,"_to_",data_f, sep = ""))
  
  list_dates = seq.Date(as.Date(data_i), as.Date(data_f), by = "day")
  list_dates = format(list_dates, "%Y%m%d")
  
  for (i in 1:length(list_dates)) {
    
    if (file.exists(paste0("ERA5_pl_", list_dates[i], ".grib"))) {
      
      cat(paste0("ERA5_pl_", list_dates[i], ".grib"), " - already downloaded. \n")
      
    } else if (file.exists(paste0("ERA5_pl_", list_dates[i], ".nc"))) {
      
      cat(paste0("ERA5_pl_", list_dates[i], ".nc"), " - already downloaded. \n")
      
    } else {
      
      txt = c("#!/usr/bin/env python", 
              "import cdsapi",
              "",
              "c = cdsapi.Client()",
              "",
              "c.retrieve(",
              "'reanalysis-era5-pressure-levels',",
              "{",
              "'product_type':'reanalysis',",
              "'format':'grib',",
              "'variable':[",
              "'geopotential', 'relative_humidity', 'specific_humidity',",
              "'temperature', 'u_component_of_wind', 'v_component_of_wind',",
              "],",
              "'pressure_level': [",
              "'1', '2', '3',",
              "'5', '7', '10',",
              "'20', '30', '50',",
              "'70', '100', '125',",
              "'150', '175', '200',",
              "'225', '250', '300',",
              "'350', '400', '450',",
              "'500', '550', '600',",
              "'650', '700', '750',",
              "'775', '800', '825',",
              "'850', '875', '900',",
              "'925', '950', '975',",
              "'1000',",
              "],",
              paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
              paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
              paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
              "'time': [",
              "'00:00', '01:00', '02:00',",
              "'03:00', '04:00', '05:00',",
              "'06:00', '07:00', '08:00',",
              "'09:00', '10:00', '11:00',",
              "'12:00', '13:00', '14:00',",
              "'15:00', '16:00', '17:00',",
              "'18:00', '19:00', '20:00',",
              "'21:00', '22:00', '23:00',",
              "],",
              "'area': [",
              paste0(area[1],",",area[2],",",area[3],",",area[4]),
              "],",
              "},",
              paste0("'ERA5_pl_", list_dates[i], ".grib')")
      )
      
      writeLines(txt, paste0("ECMWF_ERA5_pl_", list_dates[i], ".py"))
      
      cat(paste0("Downloading ECMWF ERA5 WRF pl variables, day: ", list_dates[i]), "\n")
      system(command = paste0("python ECMWF_ERA5_pl_", list_dates[i], ".py"), ignore.stdout = F, ignore.stderr = F)
      system(command = paste0("rm -rf ECMWF_ERA5_pl_", list_dates[i], ".py"), ignore.stdout = F, ignore.stderr = F)
      
    }
    
  }
  
}

ui = navbarPage(inverse = TRUE, "WRF Model Initializer",
                
                # First Tab - Intro        
                tabPanel("Summary About the App",
                         fluidPage(h1("WRF (Weather Research and Forecasting) Model Initializer"),
                                   br(),
                                   p(strong(em("This app provides to run WRF model on predefined domains."))),
                                   br(),
                                   p("The Weather Research and Forecasting (WRF) Model is a next-generation mesoscale numerical weather prediction system designed for both atmospheric research and operational forecasting applications."),
                                   br(),
                                   p("Even though this app can run the model from the data download to the WRF model initializing; it is strongly recommended to use/execute the final prepared bash script in Linux OS due to the background process problems of R Shiny platform!"),
                                   br(),
                                   p("This app has 5 different tabs;"),
                                   br(),
                                   p("Summary About the App: This tab!"),
                                   p("1. Data: Download the data required for WPS (FNL and ERA5)"),
                                   p("2. WPS: WRF Preprocessing configurations and initializing"),
                                   p("3. WRF: WRF model configurations and initializing"),
                                   p("4. Bash: Bash scripts that can be run in background!"),
                                   br(),
                                   p("Play with this interactive tool and find out!"),
                                   br(),
                                   br(),
                                   br(),
                                   br()
                         )
                ),
                #Second Tab - Data Downloader!
                tabPanel("Data",
                         fluidPage(titlePanel("Download the Initial Condition Data"),
                                   p("In this section, the initial condition data that is required for initializing the WRF will be downloaded.
                                      The worldwide known two datasets can be downloaded within this app."),
                                   p(strong("1. FNL:"),"These NCEP FNL (Final) Operational Global Analysis data are on 1-degree 
                                      by 1-degree grids prepared operationally every six hours. For the detailed explanation,
                                      click the",a("link", href = "https://rda.ucar.edu/datasets/ds083.2/#!description"),"."),
                                   p(strong("2. ERA5:"),"ERA5 is the fifth generation ECMWF reanalysis for the global climate and weather for the past 
                                      4 to 7 decades. Data has been regridded on 0.25-degree by 0.25-degree grids. ERA5 provides hourly estimates 
                                      for a large number of atmospheric, ocean-wave and land-surface quantities. For the detailed 
                                      explanation, click the",a("link", href = "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview"),"."),
                                   fluidRow(column(12, align = "center",
                                                   wellPanel(style = "background: white",
                                                             p(strong("Choose the path for data that will be downloaded")),
                                                             shinyDirButton("datadir", "Data directory", "Choose the path for the data")))),
                                   fluidRow(column(12, align = "center",
                                                   h4(strong("Choose the data and date range")),
                                                   fluidRow(column(6, align = "center",
                                                                   wellPanel(style = "backgroud: white",
                                                                             selectInput("datatype","Choose the data type",
                                                                                         choices = c("FNL","ERA5"),
                                                                                         selected = "FNL",
                                                                                         multiple = F))),
                                                            column(6, align = "center",
                                                                   wellPanel(style = "background: white",
                                                                             dateRangeInput("dateRange_data",
                                                                                            label = "Date range for downloading the data.",
                                                                                            start = Sys.Date() - 30, end = Sys.Date()-15)
                                                                   )
                                                            )
                                                   )
                                   )
                                   ),
                                   fluidRow(column(12, align = "center",
                                                   h4("Choose the area (if the data is ERA5)"),
                                                   fluidRow(column(3,
                                                                   wellPanel(style = "background: white",
                                                                             numericInput("north",
                                                                                          label = "North (Latitude)",value = 45,
                                                                                          min = -90, max = 90)
                                                                   )
                                                   ),
                                                   column(3,
                                                          wellPanel(style = "background: white",
                                                                    numericInput("west",
                                                                                 label = "West (Longitude)",value = 20,
                                                                                 min = -180, max = 180)
                                                          )
                                                   ),
                                                   column(3,
                                                          wellPanel(style = "background: white",
                                                                    numericInput("south",
                                                                                 label = "South (Latitude)",value = 33,
                                                                                 min = -90, max = 90)
                                                          )
                                                   ),
                                                   column(3,
                                                          wellPanel(style = "background: white",
                                                                    numericInput("east",
                                                                                 label = "East (Longitude)",value = 47,
                                                                                 min = -180, max = 180)
                                                          )
                                                   )
                                                   )
                                   )
                                   ),
                                   fluidRow(align = "center",
                                            actionButton("areadef", "Show the area")
                                   ),
                                   fluidRow(align = "center",
                                            br(),
                                            actionButton("download", "Download the data!")
                                   )
                         ),
                         mainPanel(
                           br(),
                           leafletOutput("mymap"),
                           textOutput("text_results"),
                           textOutput("text_area")
                         )
                ),
                
                # Third Tab  - WPS       
                tabPanel("WPS",
                         fluidPage(
                           fluidRow(column(6, align = "center",
                                           h4(strong("Configuring WPS")),
                                           fluidRow(column(12, align = "center",
                                                           h5("namelist.wps"),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     fileInput("wps_namelist", "Choose the predefined namelist.wps",
                                                                                               multiple = F)
                                                                           )
                                                           ),
                                                           column(6, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Update parameters on app!")),
                                                                            actionButton("update_wps","Update parameters"),
                                                                            p("Update parameters on app based on uploaded namelist.wps!")))
                                                           ))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("Choose the path for WPS directory")),
                                                                     shinyDirButton("wpsdir", "WPS directory", "Choose the path for WPS")))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Date & Time Manipulation"),
                                                           wellPanel(style = "background: white",
                                                                     dateRangeInput("dateRange_WPS",
                                                                                    label = "Date range input for WPS (Year - Month - Day)",
                                                                                    start = Sys.Date() - 7, end = Sys.Date())),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     timeInput("time_start_wps",
                                                                                               "Select the time for the first date:",
                                                                                               value = strptime("00:00:00","%T")))
                                                           ),
                                                           column(6,
                                                                  wellPanel(style = "background: white",
                                                                            timeInput("time_end_wps",
                                                                                      "Select the time for the first date:",
                                                                                      value = strptime("18:00:00","%T"))))))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Other Configurations"),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     numericInput("max_dom", "Define max domain size",
                                                                                                  value = 6,
                                                                                                  min = 1,
                                                                                                  max = 20))),
                                                                    column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     sliderInput("timeres", "Choose time resolution (hours) of input",
                                                                                                 value = 6,
                                                                                                 min = 1,
                                                                                                 max = 24,
                                                                                                 step = 1))
                                                                    )))
                                           ),
                                           fluidRow(column(6, align = "center",
                                                           wellPanel(style = "backgroud: white",
                                                                     selectInput("staticres", "Choose static data resolution",
                                                                                 choices = c("10m", "30s", "default"),
                                                                                 multiple = F,
                                                                                 selected = c("30s")))),
                                                    column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("Choose the path for the data (If the data is not downloaded on previous step!)")),
                                                                     shinyDirButton("datadir2", "Data directory", "Choose the path for the data")))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Control Buttons!"),
                                                           column(4, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Check the possible errors!")),
                                                                            actionButton("checkbutton_wps","Check errors!"),
                                                                            p("Click the button to check the problems!"))),
                                                           column(8, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Observe the changes")),
                                                                            actionButton("preprocess_WPS", "Make the changes!"),
                                                                            radioButtons("save_wps",label = "Overwrite namelist.wps", choices = c("Yes","No"), selected = "Yes", inline = T),
                                                                            p("Click the button to make the choices would be effective in namelist.wps! If No is selected, then you can download the configured namelist by using download button."))))
                                           ),
                                           fluidRow(column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("1. Geogrid")),
                                                                     actionButton("geogrid", "Execute Geogrid!"),
                                                                     p("Click the button to execute the geogrid of WPS"))),
                                                    column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("2. Ungrib")),
                                                                     actionButton("ungrib","Execute Ungrib!"),
                                                                     p("Click the button to execute ungrib of WPS")))
                                           ),
                                           fluidRow(column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("Metgrid")),
                                                                     actionButton("metgrid","Execute Metgrid!"),
                                                                     p("Click the button to execute metgrid of WPS"))),
                                                    column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("3 in 1")),
                                                                     actionButton("AllWPS", "WPS Initializer!"),
                                                                     p("Click the button to execute all WPS works")))
                                           )),
                                    column(6,
                                           titlePanel("WPS Configuration & Initializing"),
                                           p("In this section, you will;"),
                                           p("1. Choose predefined namelist for WPS (namelist.wps)."),
                                           p("2. Choose domain number for initializing."),
                                           p("3. Choose the WPS directory."),
                                           p("4. Select the date range that you want to initialize WRF."),
                                           p("5. Select the times for the dates you've selected."),
                                           p("6. Finally, you can run the WPS which includes three steps"),
                                           br(),
                                           br(),
                                           fluidRow(align = "center",
                                                    downloadButton("download_wps", "Download")
                                           ),
                                           br(),
                                           mainPanel(
                                             # p(strong(em("Selected WPS path is:"))),
                                             # br(),
                                             htmlOutput("update_warn_wps"),
                                             htmlOutput("checktext_wps"),
                                             # br(),
                                             # p(strong(em("Configured WPS"), "Observe the changes")),
                                             br(),
                                             htmlOutput("geogrid_msg"),
                                             br(),
                                             htmlOutput("ungrib_msg"),
                                             br(),
                                             htmlOutput("metgrid_msg"),
                                             br(),
                                             uiOutput("text_namelist_wps")))
                           )
                         )),
                
                
                # Fourth Tab  - WRF       
                tabPanel("WRF",
                         fluidPage(
                           fluidRow(column(6, align = "center",
                                           h4(strong("Configuring WRF")),
                                           fluidRow(column(12, align = "center",
                                                           h5("namelist.input"),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     fileInput("wrf_namelist", "Choose the predefined namelist.input",
                                                                                               multiple = F)
                                                                           )
                                                           ),
                                                           column(6, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Update parameters on app!")),
                                                                            actionButton("update_wrf","Update parameters"),
                                                                            p("Update parameters on app based on uploaded namelist.input!"))
                                                           )
                                                           ))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("Choose the path for WRF directory")),
                                                                     shinyDirButton("wrfdir", "WRF directory", "Choose the path for WRF")))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Date & Time Manipulation"),
                                                           wellPanel(style = "background: white",
                                                                     dateRangeInput("dateRange_WRF",
                                                                                    label = "Date range input for WRF (Year - Month - Day)",
                                                                                    start = Sys.Date() - 7, end = Sys.Date())),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     timeInput("time_start_wrf",
                                                                                               "Select the time for the first date:",
                                                                                               value = strptime("00:00:00","%T")))
                                                           ),
                                                           column(6,
                                                                  wellPanel(style = "background: white",
                                                                            timeInput("time_end_wrf",
                                                                                      "Select the time for the first date:",
                                                                                      value = strptime("18:00:00","%T"))))))
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Other Configurations"),
                                                           fluidRow(column(6,
                                                                           wellPanel(style = "background: white",
                                                                                     numericInput("max_dom_wrf", "Define max domain size",
                                                                                                  value = 6,
                                                                                                  min = 1,
                                                                                                  max = 20))),
                                                                    column(6,
                                                                           wellPanel(style = "backgroud: white",
                                                                                     selectInput("hist_dom", "Choose the domains that the outputs are desired!",
                                                                                                 choices = c("1","2","3","4","5","6","7","8"),
                                                                                                 multiple = T,
                                                                                                 selected = c("1","2")))
                                                                    )))
                                           ),
                                           fluidRow(column(6,align = "center",
                                                           wellPanel(style = "background: white",
                                                                     numericInput("hist_int", "Choose history interval (in minutes) of WRF output.",
                                                                                  value = 60))
                                           ),
                                           column(6,align = "center",
                                                  wellPanel(style = "backgroud: white",
                                                            numericInput("time_step", "Choose the time step of WRF model!",
                                                                         value = 180,
                                                                         min = 1))
                                           )
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           h5("Control Buttons!"),
                                                           column(6, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Check the possible errors!")),
                                                                            actionButton("checkbutton_wrf","Check errors!"),
                                                                            p("Click the button to check the problems!"))
                                                           ),
                                                           column(6, align = "center",
                                                                  wellPanel(style = "background: white",
                                                                            p(strong("Activate the configurations")),
                                                                            actionButton("preprocess_WRF", "Make the changes!"),
                                                                            radioButtons("save_wrf",label = "Overwrite namelist.input", choices = c("Yes","No"), selected = "Yes", inline = T),
                                                                            p("Click the button to make the choices would be effective in namelist.input! If No is selected, then you can download the configured namelist by using download button."))
                                                           )
                                           )
                                           ),
                                           fluidRow(
                                             column(6, align = "center",
                                                    wellPanel(style = "background: white",
                                                              p(strong("MPI")),
                                                              radioButtons("mpisupport",label = "Is WRF installed with MPI Support?",choices = c("Yes","No"),selected = "Yes"),
                                                              p("Whether WRF model is built with MPI support or not."))),
                                             column(6, align = "center",
                                                    wellPanel(style = "background: white",
                                                              p(strong("CPU")),
                                                              sliderInput("cpu_n","CPU number",
                                                                          value = 4,
                                                                          min = 1,
                                                                          max = 8,
                                                                          step = 1),
                                                              p("Checks and automatically returns optimal CPU number!"))
                                             )
                                           ),
                                           fluidRow(column(6, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("1. Real")),
                                                                     actionButton("real", "Execute real.exe!"),
                                                                     p("Click the button to execute the real.exe of WRF"))
                                           ),
                                           column(6, align = "center",
                                                  wellPanel(style = "background: white",
                                                            p(strong("2. WRF")),
                                                            actionButton("wrf","Execute WRF!"),
                                                            p("Click the button to execute the WRF"))
                                           )
                                           ),
                                           fluidRow(column(12, align = "center",
                                                           wellPanel(style = "background: white",
                                                                     p(strong("WRF Total")),
                                                                     actionButton("AllWRF","Execute WRF with real!"),
                                                                     p("Click the button to execute WRF with real")))
                                           )),
                                    column(6,
                                           titlePanel("WRF Configuration & Initializing"),
                                           p("In this section, you will;"),
                                           p("1. Choose predefined namelist for WRF (namelist.input)."),
                                           p("2. Choose domain number for initializing."),
                                           p("3. Choose the WRF directory."),
                                           p("4. Select the date range that you want to initialize WRF."),
                                           p("5. Select the times for the dates you've selected."),
                                           p("6. Finally, you can run the WRF which includes two steps"),
                                           br(),
                                           br(),
                                           fluidRow(align = "center",
                                                    downloadButton("download_wrf", "Download")
                                           ),
                                           br(),
                                           mainPanel(
                                             # p(strong(em("Selected WPS path is:"))),
                                             # br(),
                                             htmlOutput("update_warn_wrf"),
                                             htmlOutput("checktext_wrf"),
                                             br(),
                                             htmlOutput("real_msg"),
                                             br(),
                                             htmlOutput("wrf_msg"),
                                             br(),
                                             uiOutput("text_namelist_wrf"),
                                             # br(),
                                             # p(strong(em("Configured WPS"), "Observe the changes")),
                                             br()))
                           )
                         )),
                # Fifth Tab  - Bash       
                tabPanel("Bash Script",
                         sidebarLayout(
                           sidebarPanel(align = "center",
                                        p(strong("Create the bash script!")),
                                        p("This tab provides a bash script to run WRF model in Linux OS. 
                                Since executing system commands on the background is not supported on R Shiny; 
                                this bash script can be executed on background (nohup &, tmux, screen), Therefore; 
                                this app can also be used for creating bash scripts instead of executing WRF model.",
                                          align = "justify"),
                                        p("Click the button to create the bash script in order to be able to run WRF model background."),
                                        actionButton("bash_preparer", "Prepare the bash!"),
                                        br(),
                                        actionButton("bash_button","Bash script",align = "center"),
                                        br(),
                                        br(),
                                        p("Should this bash include data downloading part?"),
                                        radioButtons("datadownload",label = "Should data download be included?",inline = T, choices = c("Yes","No"), selected = "No"),
                                        downloadButton("download_bash", "Download"),
                                        br(),
                                        br(),
                                        p("This part is created for long WRF model runs with a speciefed sequence type in dates", align = "justify"),
                                        p("For instance; if you would like to run a WRF model between 2020-01-01 00:00:00 and 2021-01-01 00:00:00; you should follow;", align = "justify"),
                                        p("1. If you would like to download data; then, choose data type and data path in Data tab! Don't forget the push show area button if you've selected ERA5 data! (You don't need to specify date ranges in Data tab.)",align = "justify"),
                                        p("2. Upload namelist.wps, update the related parameters, choose a path for WPS directory and definetely press make changes button in WPS tab. You can change any configuration you want before pressing Make changes button! (You don't need to specify the date ranges in WPS tab)",align = "justify"),
                                        p("3. Upload namelist.input, update the related parameters, choose a path for WRF directory and definetely press make changes button in WRF tab. You can change any configurations you want beofre pressing Make changes button! (You don't need to specify the date range in WRF tab)",align = "justify"),
                                        p("4. If you also want to download dataset please choose Yes in this tab",align = "justify"),
                                        p("5. Please specify date and time ranges if you want to create a sequential bash scripts (ex. 2020-01-01, 2021-01-01)",align = "justify"),
                                        p("6. Choose your sequence type (Day, Week or Month)",align = "justify"),
                                        p("7. Choose your sequence numeric. For instance if you want to create a two-week time sequence, then Week and 2 should be chosen.",align = "justify"),
                                        p("8. Choose a spin-up time specified in hours (if you do not want any spin-up then you can choose 0)",align = "justify"),
                                        
                                        dateRangeInput("datebash",
                                                       label = "Date range",
                                                       start = Sys.Date() - 7, end = Sys.Date()),
                                        timeInput("timebash1", "Start Time"),
                                        timeInput("timebash2", "End Time"),
                                        selectInput("sequence_bash","Choose your sequence",choices = c("Day","Week","Month"), selected = "Day"),
                                        numericInput("seq_num_bash", "Choose your sequence number", min = 1, max = 31, value = 1),
                                        br(),
                                        numericInput("spinup_num","Choose a spin-up time in hours.", min = 0, max = 48, value = 3),
                                        br(),
                                        br(),
                                        actionButton("checkbutton_bash2","Check errors!"),
                                        br(),
                                        actionButton("bash2_preparer", "Prepare the bash with sequence!"),
                                        br(),
                                        actionButton("bash2_button","Bash script with sequence",align = "center"),
                                        br(),
                                        br(),
                                        downloadButton("download_bash2", "Download"),
                           ),
                           mainPanel(
                             htmlOutput("checktext_bash"),
                             textOutput("bash_text",container = pre),
                             textOutput("bash_text2",container = pre)
                             
                           )
                         ))
)


server = function(input, output,session) {
  
  volumes = getVolumes()
  shinyDirChoose(input, "datadir", roots = volumes(), session = session)
  
  dirpath_data = reactive({
    req(input$datadir)
    parseDirPath(volumes,input$datadir)
  })
  
  daterange_data = reactive({
    req(input$dateRange_data)
    date_range_data = as.character(input$dateRange_data)
    date_range_data
  })
  
  datatype = reactive({
    req(input$datatype)
    datatype = as.character(input$datatype)
  })
  
  north = reactive({
    req(input$north)
    north = input$north
    north
  })
  
  west = reactive({
    req(input$west)
    west = input$west
    west
  })
  
  south = reactive({
    req(input$south)
    south = input$south
    south
  })
  
  east = reactive({
    req(input$east)
    east = input$east
    east
  })
  
  area = eventReactive(input$areadef, {
    
    area = c(north(), west(), south(), east())
    
  })
  
  output$mymap <- renderLeaflet({
    
    area = area()
    
    leaflet() %>%
      addTiles() %>%
      addRectangles(
        lng1=area[2], lat1=area[1],
        lng2=area[4], lat2=area[3],
        fillColor = "transparent")
    
  })
  
  observeEvent(input$download,{
    
    datatype = datatype()
    dirpath_data = dirpath_data()
    daterange_data = daterange_data()
    
    
    if (datatype == "FNL") {
      
      fnl_downloader(email = email,
                     pass = pass,
                     path = dirpath_data,
                     from = daterange_data[1],
                     to = daterange_data[2])
      
    } else {
      
      area = area()
      
      era5_surf_downloader(path = dirpath_data,
                           from = daterange_data[1],
                           to = daterange_data[2],
                           area = area)
      era5_pl_downloader(path = dirpath_data,
                         from = daterange_data[1],
                         to = daterange_data[2],
                         area = area)
      
      output$text_results = renderText({
        list.files(path = path)
      })
      
    }
    
  })
  
  output$text_area = renderText({
    
    area = area()
    area
  })
  
  # WPS, Third Tab -----------------------------------------------------------
  
  wps_predefined = reactive({
    req(input$wps_namelist)
    
    file = input$wps_namelist
    ext = tools::file_ext(file$datapath)
    wps_predefined = readLines(file$datapath)
    wps_predefined
    
  })
  
  max_dom = reactive({
    req(input$max_dom)
    max_dom = input$max_dom
    max_dom
  })
  
  daterange_wps = reactive({
    
    req(input$dateRange_WPS)
    daterange_wps = as.character(input$dateRange_WPS)
    
  })
  
  date_time_start_wps = reactive({
    
    req(input$dateRange_WPS)
    req(input$time_start_wps)
    
    date_ranges_wps = daterange_wps()
    time_1_wps = as.character(strftime(input$time_start_wps, "%T"))
    date_time_start_wps = paste(as.character(date_ranges_wps[1]),time_1_wps, sep = "_")
    date_time_start_wps
    
  })
  
  date_time_end_wps = reactive({
    
    req(input$dateRange_WPS)
    req(input$time_end_wps)
    
    date_ranges_wps = daterange_wps()
    time_2_wps = as.character(strftime(input$time_end_wps, "%T"))
    date_time_end_wps = paste(as.character(date_ranges_wps[2]), time_2_wps, sep = "_")
    date_time_end_wps
    
  })
  
  volumes = getVolumes()
  shinyDirChoose(input, "wpsdir", roots = volumes(), session = session)
  
  dirpath_wps = reactive({
    req(input$wpsdir)
    parseDirPath(volumes,input$wpsdir)
  })
  
  timeres = reactive({
    
    req(input$timeres)
    timeres = as.numeric(input$timeres)*3600
    timeres
    
  })
  
  staticres = reactive({
    
    req(input$staticres)
    staticres = as.character(input$staticres)
    staticres = rep(staticres, max_dom())
    staticres =  paste("'",paste(staticres,collapse = "','"),"'",sep = "")
    staticres
    
  })
  
  volumes = getVolumes()
  shinyDirChoose(input, "datadir2", roots = volumes(), session = session)
  
  dirpath_data2 = reactive({
    req(input$datadir2)
    parseDirPath(volumes,input$datadir2)
  })
  
  observeEvent(input$update_wps, {
    
    output$update_warn_wps = renderUI({
      
      if(is.null(input$wps_namelist)) {
        #Negative warn!
        warn_wps = paste("<strong><span style=\"color:darkred\">Please upload the namelist.wps in order to be able to update the parameters!</span></strong>")
      } else {
        #Positive warn!
        warn_wps = paste("<strong><span style=\"color:darkgreen\">Related parameters updated!</span></strong>")
      }
      
      HTML(paste(warn_wps, sep="<br/>"))
      
    })
    
    wps_predefined = wps_predefined()
    
    #Update the namelist.wps based on the uploaded namelist.wps!
    #Start date
    Sys.setenv(TZ='GMT')
    wps_s_i = str_detect(wps_predefined,"start_date")
    s_i_1 = str_locate(wps_predefined[wps_s_i],"=")[1]
    s_i_2 = str_locate(wps_predefined[wps_s_i],",")[1]
    start_date_wps = ymd_hms(substr(wps_predefined[wps_s_i],(s_i_1+1), (s_i_2-1)))
    
    #End date
    wps_e_i = str_detect(wps_predefined,"end_date")
    e_i_1 = str_locate(wps_predefined[wps_e_i],"=")[1]
    e_i_2 = str_locate(wps_predefined[wps_e_i],",")[1]
    end_date_wps = ymd_hms(substr(wps_predefined[wps_e_i],(e_i_1+1), (e_i_2-1)))
    
    #Time
    time_s_wps = strftime(start_date_wps, format="%T")
    time_e_wps = strftime(end_date_wps, format="%T")
    
    #Max dom
    md_wps_i = str_detect(wps_predefined,"max_dom")
    md_wps_i_1 = str_locate(wps_predefined[md_wps_i],"=")[1]
    md_wps = parse_number(substr(wps_predefined[md_wps_i],md_wps_i_1,nchar(wps_predefined[md_wps_i])))
    
    #Timeres
    timeres_i = str_detect(wps_predefined,"interval_seconds")
    timeres_i_1 = str_locate(wps_predefined[timeres_i],"=")[1]
    timeres_u = parse_number(substr(wps_predefined[timeres_i],timeres_i_1,nchar(wps_predefined[timeres_i])))/3600
    
    #Staticres
    staticres_i = str_detect(wps_predefined,"geog_data_res")
    staticres_i = which(staticres_i)[!str_detect(wps_predefined[staticres_i],"!")]
    staticres_i_1 = str_locate(wps_predefined[staticres_i],"=")[1]
    staticres_i_2 = str_locate(wps_predefined[staticres_i],",")[1]
    staticres_u = substr(wps_predefined[staticres_i], (staticres_i_1+1), (staticres_i_2-1))
    staticres_u = str_replace_all(staticres_u,pattern = " ", replacement = "")
    staticres_u = str_replace_all(staticres_u,pattern = "'", replacement = "")
    
    updateDateRangeInput(session, inputId = "dateRange_WPS", start = start_date_wps, end = end_date_wps)
    updateTimeInput(session, "time_start_wps",value = strptime(time_s_wps,"%T"))
    updateTimeInput(session, "time_end_wps",value = strptime(time_e_wps,"%T"))
    updateNumericInput(session, "max_dom", value = md_wps)
    updateSliderInput(session, "timeres", value = timeres_u)
    updateSelectInput(session, "staticres", selected = staticres_u)
    
  })
  
  observeEvent(input$checkbutton_wps, {
    
    output$checktext_wps = renderUI({
      if (is.null(input$wps_namelist)) {
        first_message_wps = paste("<strong><span style=\"color:darkred\">Please upload the namelist.wps!</span></strong>")
      } else {
        first_message_wps = paste("<strong><span style=\"color:darkgreen\">You have uploaded the namelist.wps.</span></strong>", sep = "")
      }
      
      date_time_start_wps = date_time_start_wps()
      date_time_start_wps = ymd_hms(date_time_start_wps)
      date_time_end_wps = date_time_end_wps()
      date_time_end_wps = ymd_hms(date_time_end_wps)
      timeres = timeres()
      
      if (date_time_start_wps>=date_time_end_wps) {
        second_message_wps = paste("<strong><span style=\"color:darkred\">Please choose a start date which is smaller than the end date.</span></strong>")
      } else if(((as.numeric(date_time_end_wps)-as.numeric(date_time_start_wps)) %% timeres) == 0) { 
        
        second_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("WPS will be executed between ",
                                                                                     date_time_start_wps(),
                                                                                     " and ",
                                                                                     date_time_end_wps(),"!"),
                                   "</span></strong>", sep = "")
      } else {
        
        second_message_wps = paste("<strong><span style=\"color:darkred\">Please choose a valid date range that can be divisible by history interval (data time resolution) which is defined ",
                                   timeres/3600,
                                   " hours in WPS tab!</span></strong>", sep = "")
        
      }
      
      
      if (as.character(unlist(input$wpsdir)[1])==0) {
        third_message_wps = paste("<strong><span style=\"color:darkred\">Please choose a path for WPS directory!</span></strong>")
      } else {
        third_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("Path for the WPS is chosen as: ",
                                                                                    dirpath_wps()),
                                  "</span></strong>", sep = "")
      }
      
      if (is.null(input$staticres)) {
        fourth_message_wps = paste("<strong><span style=\"color:darkred\">Please choose the static data resolution for WPS!</span></strong>")
      } else {
        staticres = staticres()
        fourth_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("Static data resolution is chosen as ", staticres[1],"."),
                                   "</span></strong>", sep = "")
      }
      
      if (as.character(unlist(input$datadir)[1])==0 & as.character(unlist(input$datadir2)[1])==0 ) {
        fifth_message_wps = paste("<strong><span style=\"color:darkred\">You have not downloaded data on previous step or you have not specified a data path on this step. Please choose a path for data!</span></strong>")
      } else if (as.character(unlist(input$datadir)[1])!= 0){
        dirpath_data = dirpath_data()
        daterange_data = daterange_data()
        
        fifth_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("Data is downloaded on previous tab and the path for the data: ",
                                                                                    paste(dirpath_data,"/fnl_",daterange_data[1],"_to_",daterange_data[2],"")),
                                  "</span></strong>", sep = "")
      } else if (as.character(unlist(input$datadir2)[1])!= 0) {
        dirpath_data2 = dirpath_data2()
        fifth_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("The path for the data: ",
                                                                                    dirpath_data2),
                                  "</span></strong>", sep = "")
        
      } else {
        dirpath_data2 = dirpath_data2()
        
        fifth_message_wps = paste("<strong><span style=\"color:darkgreen\">",paste0("You both downloaded data on previous tab and set also a path for data. The path for the data: ",
                                                                                    dirpath_data2),
                                  "</span></strong>", sep = "")
        
      }
      
      HTML(paste(first_message_wps, second_message_wps, third_message_wps, fourth_message_wps,fifth_message_wps, sep="<br/>"))
      # validate(
      #   need(hist_dom(), "Your selected domains exceeds max domain number! Please check the desired domains or maximum domain numbers!")
      # )
      
    })
  })
  
  wps_predefined_updated = reactive({
    wps_predefined = wps_predefined()
    
    #First configure max_dom
    #Index
    wps_m_index = str_detect(wps_predefined, "max_dom")
    #max_dom remove and replace everything after first equal sign
    wps_m_fes = str_locate(wps_predefined[wps_m_index],"=")[1]
    max_dom = max_dom()
    wps_predefined[wps_m_index] = paste(substr(wps_predefined[wps_m_index],1,wps_m_fes), max_dom)
    
    #Interval seconds for input data.
    #Index
    wps_t_index = str_detect(wps_predefined, "interval_seconds")
    #interval_seconds remove and replace everything after first equal sign
    wps_t_fes = str_locate(wps_predefined[wps_t_index],"=")[1]
    timeres = timeres()
    wps_predefined[wps_t_index] = paste(substr(wps_predefined[wps_t_index],1,wps_t_fes), timeres)
    
    #Resolution of the static data
    #Index
    staticres_i = str_detect(wps_predefined,"geog_data_res")
    staticres_i = which(staticres_i)[!str_detect(wps_predefined[staticres_i],"!")]
    
    #geog_data_res remove and replace everything after equal sign
    staticres_i_fes = str_locate(wps_predefined[staticres_i],"=")[1]
    staticres = staticres()
    
    wps_predefined[staticres_i] = paste(substr(wps_predefined[staticres_i],1,staticres_i_fes), staticres)
    
    #WPS start_date indexes
    wps_s_index = str_detect(wps_predefined,"start_date")
    #WPS remove and replace everything after first equal sign
    wps_s_fes = str_locate(wps_predefined[wps_s_index],"=")[1]
    date_time_start_wps = date_time_start_wps()
    date_time_start_wps = rep(date_time_start_wps, max_dom())
    date_time_start_wps = paste("'",paste(date_time_start_wps,collapse = "','"),"'",sep = "")
    
    wps_predefined[wps_s_index] = paste(substr(wps_predefined[wps_s_index],1,wps_s_fes),date_time_start_wps)
    
    #WPS end_date indexes
    wps_e_index = str_detect(wps_predefined,"end_date")
    wps_e_fes = str_locate(wps_predefined[wps_e_index],"=")[1]
    date_time_end_wps = date_time_end_wps()
    date_time_end_wps = rep(date_time_end_wps, max_dom())
    date_time_end_wps =  paste("'",paste(date_time_end_wps,collapse = "','"),"'",sep = "")
    date_time_end_wps
    
    wps_predefined[wps_e_index] = paste(substr(wps_predefined[wps_e_index],1,wps_e_fes),date_time_end_wps)
    wps_predefined
  })
  
  observeEvent(input$preprocess_WPS, {
    wps_predefined_updated = wps_predefined_updated()
    dirpath_wps = dirpath_wps()
    
    #Subset only "namelist.wps" means [1]!!!
    prev_wps = list.files(dirpath_wps,pattern = "namelist.wps", all.files = T, full.names = T)[1]
    
    if (input$save_wps == "Yes") {
      
      Sys.setenv(TZ='GMT')
      file.copy(from = prev_wps, paste(dirpath_wps,"/namelist.wps.",str_replace_all(str_replace_all(Sys.time(),":","")," ", "_"),".previous", sep = ""))
      write(x = wps_predefined_updated, file = paste(dirpath_wps,"/namelist.wps",sep = ""))
      
    }
    
    # wps_predefined
    output$text_namelist_wps = renderUI({
      p(strong(em("Configured WPS"), "Observe the changes"))
      wps_splitText = stringi::stri_split(str = wps_predefined_updated, regex = '\\n')
      
      # Wrap a paragraph tag around each element in the list
      wps_replacedText = lapply(wps_splitText, p)
      wps_replacedText})
    
  })
  
  #Download button for namelist.wps
  observe({
    dirpath_wps = dirpath_wps()
    wps_predefined_updated = wps_predefined_updated()
    date_time_start_wps = substr(str_replace_all(date_time_start_wps(),":",""),1,15)
    date_time_end_wps = substr(str_replace_all(date_time_end_wps(),":",""),1,15)
    
    download_script = function(data) {
      downloadHandler(
        filename = function() {
          paste(dirpath_wps,"/namelist_",date_time_start_wps,"_",date_time_end_wps,".wps", sep = "")
        },
        content = function(file) {
          write(data, file, sep = "")
        }
      )
    }
    output$download_wps = download_script(wps_predefined_updated)
  })
  
  #Geogrid.exe!
  observeEvent(input$geogrid, {
    
    dirpath_wps = dirpath_wps()
    setwd(dirpath_wps)
    system("./geogrid.exe")
    
    output$geogrid_msg = renderUI({
      
      existed_geo = list.files(pattern = "geo_em*")
      #Geogrid control
      max_dom = max_dom()
      shouldbe_geo = paste("geo_em.d0",1:max_dom,".nc",sep = "")
      existed_count = shouldbe_geo%in%existed_geo
      unexisted_geo = shouldbe_geo[!shouldbe_geo%in%existed_geo]
      
      if (length(shouldbe_geo) == sum(existed_count)) {
        final_msg_geo = paste("<strong><span style=\"color:darkgreen\">",paste0("Geogrid.exe succesfully completed! ",paste(existed_geo,collapse = ", ")," created."),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe succesfully completed! ",paste(existed_geo,collapse = ", ")," created.")
      } else if (sum(existed_count)==0){
        final_msg_geo = paste("<strong><span style=\"color:darkred\">",paste0("Geogrid.exe is not completed! ",paste(unexisted_geo,collapse = ", "),"is not created! Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe is not completed! ",unexisted_geo,"is not created! Check namelist!")
      } else {
        final_msg_geo = paste("<strong><span style=\"color:darkred\">",paste0("Geogrid.exe partially completed! Only ",existed_geo," is created. ",paste(unexisted_geo,collapse = ", "),"is missing! Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe partially completed! Only ",existed_geo," is created. ",unexisted_geo,"is missing! Check namelist!")
      }
      
      # final_msg_geo
      HTML(paste(final_msg_geo, sep="<br/>"))
      
    })
    
  })
  
  #Ungrib.exe!
  observeEvent(input$ungrib, {
    
    daterange_wps = daterange_wps()
    dirpath_wps = dirpath_wps()
    datatype = datatype()
    
    
    setwd(dirpath_wps)
    
    if (as.character(unlist(input$datadir)[1])!= 0 & as.character(unlist(input$datadir)[1])!= 0){
      dirpath_data2 = dirpath_data2()
      system(paste("./link_grib.csh ", dirpath_data2,"/*", sep = ""))
      
    } else if (as.character(unlist(input$datadir2)[1])!= 0) {
      dirpath_data2 = dirpath_data2()
      system(paste("./link_grib.csh ", dirpath_data2,"/*", sep = ""))
      
    } else {
      dirpath_data = dirpath_data()
      daterange_data = daterange_data()
      system(paste("./link_grib.csh ", dirpath_data,"/",datatype,"_",daterange_data[1],"_to_",daterange_data[2],"/*", sep = ""))
      
    }
    
    #Vtable!
    if (datatype == "FNL") {
      
      system("ln -sf ungrib/Variable_Tables/Vtable.GFS Vtable")
      
    } else {
      
      system("ln -sf ungrib/Variable_Tables/Vtable.ERA-interim.pl Vtable")
      
    }
    
    system("./ungrib.exe")
    
    output$ungrib_msg = renderUI({
      
      existed_ung = list.files(pattern = "FILE:")
      #Ungrib.exe control!
      date_time_start_wps = date_time_start_wps()
      date_time_end_wps = date_time_end_wps()
      timeres = timeres()
      Sys.setenv(TZ='GMT')
      
      shouldbe_ung = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
      # shouldbe_ung = seq(ymd_hms("2021-01-01 00:00:00"),ymd_hms("2021-01-01 18:00:00"), by = 21600)
      shouldbe_ung = paste0("FILE:",strftime(shouldbe_ung, "%Y-%m-%d_%H"))
      
      existed_count = shouldbe_ung%in%existed_ung
      unexisted_ung = shouldbe_ung[!shouldbe_ung%in%existed_ung]
      
      if (length(shouldbe_ung) == sum(existed_count)) {
        final_msg_ung = paste("<strong><span style=\"color:darkgreen\">ungrib.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkgreen\">",paste0("ungrib.exe succesfully completed! ",paste(existed_ung,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_ung = paste("<strong><span style=\"color:darkred\">ungrib.exe is not completed! Check namelist or data links or data!</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe is not completed! ",paste(unexisted_ung,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe is partially completed! Only ",existed_ung," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe partially completed! Only ",existed_ung," is created. ",paste(unexisted_ung,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_ung
      HTML(paste(final_msg_ung, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
  })
  
  #metgrid.exe
  observeEvent(input$metgrid, {
    
    dirpath_wps = dirpath_wps()
    setwd(dirpath_wps)
    
    system("./metgrid.exe")
    
    output$metgrid_msg = renderUI({
      
      existed_met = list.files(pattern = "met_em")
      #Metgrid.exe control
      max_dom = as.numeric(max_dom())
      max_dom_g = 1:max_dom
      date_time_start_wps = date_time_start_wps()
      date_time_end_wps = date_time_end_wps()
      timeres = timeres()
      Sys.setenv(TZ='GMT')
      
      shouldbe_met = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
      mg_grid = expand.grid(max_dom_g,shouldbe_met)
      mg_grid$Var2 = strftime(mg_grid$Var2, "%Y-%m-%d_%H:%M:%S")
      shouldbe_met = paste0("met_em.d0",str_c(mg_grid$Var1,mg_grid$Var2,sep = "."),".nc")
      
      existed_count = shouldbe_met%in%existed_met
      unexisted_met = shouldbe_met[!shouldbe_met%in%existed_met]
      
      if (length(shouldbe_met) == sum(existed_count)) {
        final_msg_met = paste("<strong><span style=\"color:darkgreen\">metgrid.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkgreen\">",paste0("metrid.exe succesfully completed! ",paste(existed_met,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_met = paste("<strong><span style=\"color:darkred\">metrib.exe is not completed! Check namelist or data links or data!</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe is not completed! ",paste(unexisted_met,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe partially completed! Only ",paste(existed_met, collapse = ", ")," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe partially completed! Only ",existed_met," is created. ",paste(unexisted_met,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_met
      HTML(paste(final_msg_met, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
  })
  
  observeEvent(input$AllWPS, {
    
    dirpath_wps = dirpath_wps()
    setwd(dirpath_wps)
    system("./geogrid.exe")
    
    output$geogrid_msg = renderUI({
      
      existed_geo = list.files(pattern = "geo_em*")
      #Geogrid.exe control
      max_dom = max_dom()
      # max_dom = 2
      shouldbe_geo = paste("geo_em.d0",1:max_dom,".nc",sep = "")
      existed_count = shouldbe_geo%in%existed_geo
      unexisted_geo = shouldbe_geo[!shouldbe_geo%in%existed_geo]
      
      if (length(shouldbe_geo) == sum(existed_count)) {
        final_msg_geo = paste("<strong><span style=\"color:darkgreen\">",paste0("Geogrid.exe succesfully completed! ",paste(existed_geo,collapse = ", ")," created."),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe succesfully completed! ",paste(existed_geo,collapse = ", ")," created.")
      } else if (sum(existed_count)==0){
        final_msg_geo = paste("<strong><span style=\"color:darkred\">",paste0("Geogrid.exe is not completed! ",paste(unexisted_geo,collapse = ", "),"is not created! Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe is not completed! ",unexisted_geo,"is not created! Check namelist!")
      } else {
        final_msg_geo = paste("<strong><span style=\"color:darkred\">",paste0("Geogrid.exe partially completed! Only ",existed_geo," is created. ",paste(unexisted_geo,collapse = ", "),"is missing! Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_geo = paste0("Geogrid.exe partially completed! Only ",existed_geo," is created. ",unexisted_geo,"is missing! Check namelist!")
      }
      
      # final_msg_geo
      HTML(paste(final_msg_geo, sep="<br/>"))
      
    })
    
    #Ungrib starts!
    daterange_wps = daterange_wps()
    datatype = datatype()
    
    if (as.character(unlist(input$datadir)[1])!= 0 & as.character(unlist(input$datadir)[1])!= 0){
      dirpath_data2 = dirpath_data2()
      system(paste("./link_grib.csh ", dirpath_data2,"/*", sep = ""))
      
    } else if (as.character(unlist(input$datadir2)[1])!= 0) {
      dirpath_data2 = dirpath_data2()
      system(paste("./link_grib.csh ", dirpath_data2,"/*", sep = ""))
      
    } else {
      dirpath_data = dirpath_data()
      daterange_data = daterange_data()
      system(paste("./link_grib.csh ", dirpath_data,"/",datatype,"_",daterange_data[1],"_to_",daterange_data[2],"/*", sep = ""))
      
    }
    
    #Vtable ayarlamak!
    if (datatype == "FNL") {
      
      system("ln -sf ungrib/Variable_Tables/Vtable.GFS Vtable")
      
    } else {
      
      system("ln -sf ungrib/Variable_Tables/Vtable.ERA-interim.pl Vtable")
      
    }
    
    system("./ungrib.exe")
    
    output$ungrib_msg = renderUI({
      
      existed_ung = list.files(pattern = "FILE:")
      #Ungrib.exe control
      date_time_start_wps = date_time_start_wps()
      date_time_end_wps = date_time_end_wps()
      timeres = timeres()
      Sys.setenv(TZ='GMT')
      
      shouldbe_ung = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
      shouldbe_ung = paste0("FILE:",strftime(shouldbe_ung, "%Y-%m-%d_%H"))
      
      existed_count = shouldbe_ung%in%existed_ung
      unexisted_ung = shouldbe_ung[!shouldbe_ung%in%existed_ung]
      
      if (length(shouldbe_ung) == sum(existed_count)) {
        final_msg_ung = paste("<strong><span style=\"color:darkgreen\">ungrib.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkgreen\">",paste0("ungrib.exe succesfully completed! ",paste(existed_ung,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_ung = paste("<strong><span style=\"color:darkred\">ungrib.exe is not completed! Check namelist or data links or data!</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe is not completed! ",paste(unexisted_ung,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe is partially completed! Only ",existed_ung," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_ung = paste("<strong><span style=\"color:darkred\">",paste0("ungrib.exe partially completed! Only ",existed_ung," is created. ",paste(unexisted_ung,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_ung
      HTML(paste(final_msg_ung, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
    #Metgrid starts
    
    system("./metgrid.exe")
    
    output$metgrid_msg = renderUI({
      
      existed_met = list.files(pattern = "met_em")
      #Metgrid.exe control
      max_dom = as.numeric(max_dom())
      max_dom_g = 1:max_dom
      date_time_start_wps = date_time_start_wps()
      date_time_end_wps = date_time_end_wps()
      timeres = timeres()
      Sys.setenv(TZ='GMT')
      
      shouldbe_met = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
      mg_grid = expand.grid(max_dom_g,shouldbe_met)
      mg_grid$Var2 = strftime(mg_grid$Var2, "%Y-%m-%d_%H:%M:%S")
      shouldbe_met = paste0("met_em.d0",str_c(mg_grid$Var1,mg_grid$Var2,sep = "."),".nc")
      
      existed_count = shouldbe_met%in%existed_met
      unexisted_met = shouldbe_met[!shouldbe_met%in%existed_met]
      
      if (length(shouldbe_met) == sum(existed_count)) {
        final_msg_met = paste("<strong><span style=\"color:darkgreen\">metgrid.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkgreen\">",paste0("metrid.exe succesfully completed! ",paste(existed_met,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_met = paste("<strong><span style=\"color:darkred\">metrib.exe is not completed! Check namelist or data links or data!</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe is not completed! ",paste(unexisted_met,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe partially completed! Only ",paste(existed_met, collapse = ", ")," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_met = paste("<strong><span style=\"color:darkred\">",paste0("metrib.exe partially completed! Only ",existed_met," is created. ",paste(unexisted_met,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_met
      HTML(paste(final_msg_met, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
    
  })
  
  # WRF, Fourth Tab ---------------------------------------------------------
  
  wrf_predefined = reactive({
    req(input$wrf_namelist)
    
    file = input$wrf_namelist
    ext = tools::file_ext(file$datapath)
    wrf_predefined = readLines(file$datapath)
    wrf_predefined
    
  })
  
  max_dom_wrf = reactive({
    req(input$max_dom_wrf)
    max_dom_wrf = input$max_dom_wrf
    max_dom_wrf
  })
  
  daterange_wrf = reactive({
    
    req(input$dateRange_WRF)
    daterange_wrf = as.character(input$dateRange_WRF)
    
  })
  
  date_time_start_wrf = reactive({
    
    req(input$dateRange_WRF)
    req(input$time_start_wrf)
    
    date_ranges_wrf = as.character(input$dateRange_WRF)
    time_1_wrf = as.character(strftime(input$time_start_wrf, "%T"))
    date_time_start_wrf = paste(as.character(date_ranges_wrf[1]),time_1_wrf, sep = "_")
    date_time_start_wrf
    
  })
  
  date_time_end_wrf = reactive({
    
    req(input$dateRange_WRF)
    req(input$time_end_wrf)
    
    date_ranges_wrf = as.character(input$dateRange_WRF)
    time_2_wrf = as.character(strftime(input$time_end_wrf, "%T"))
    date_time_end_wrf = paste(as.character(date_ranges_wrf[2]), time_2_wrf, sep = "_")
    date_time_end_wrf
    
  })
  
  volumes = getVolumes()
  shinyDirChoose(input, "wrfdir", roots = volumes(), session = session)
  
  dirpath_wrf = reactive({
    req(input$wrfdir)
    parseDirPath(volumes,input$wrfdir)
  })
  
  hist_int = reactive({
    
    req(input$hist_int)
    hist_int = as.numeric(input$hist_int)
    hist_int
    
  })
  
  hist_dom = reactive({
    
    req(input$hist_dom)
    req(input$hist_int)
    req(input$max_dom_wrf)
    
    max_dom_wrf = max_dom_wrf()
    hist_dom = as.numeric(input$hist_dom)
    
    hist_dom_l = length(hist_dom)
    
    # First filter; if the domains which are desired to give outputs are bigger than max_dom; then set it to the maxdom with ascending sorting
    if (hist_dom_l > max_dom_wrf) {
      hist_dom = sort(hist_dom)[1:max_dom_wrf]
    }
    
    #Second filter; if you choose a domain to give outputs which is bigger than max_dom!
    hist_dom = hist_dom[!hist_dom>max_dom_wrf]
    hist_dom
    
  })
  
  time_step = reactive({
    
    req(input$time_step)
    time_step = as.numeric(input$time_step)
    time_step
    
  })
  
  mpisupport = reactive({
    
    req(input$mpisupport)
    mpisupport = as.character(input$mpisupport)
    mpisupport
    
  })
  
  observe({
    
    opt_cpu = round(as.numeric(system("nproc",intern = T))*3/4)
    max_cpu = as.numeric(system("nproc --all", intern = T))
    updateSliderInput(session, "cpu_n", value = opt_cpu,
                      min = 1, max = max_cpu, step = 1)
    # system("lscpu")
    # cpu = system("lscpu | grep 'CPU(s)'", intern = T)[1]
    # system("grep 'cpu cores' /proc/cpuinfo | uniq")
    
  })
  
  cpu_n = reactive({
    
    req(input$cpu_n)
    cpu_n = as.numeric(input$cpu_n)
    cpu_n
    
  })
  
  observeEvent(input$update_wrf, {
    
    output$update_warn_wrf = renderUI({
      
      if(is.null(input$wrf_namelist)) {
        #Negative warn!
        warn_wrf = paste("<strong><span style=\"color:darkred\">Please upload the namelist.input in order to be able to update the parameters!</span></strong>")
      } else {
        #Positive warn!
        warn_wrf = paste("<strong><span style=\"color:darkgreen\">Related parameters updated!</span></strong>")
      }
      
      HTML(paste(warn_wrf, sep="<br/>"))
      
    })
    
    wrf_predefined = wrf_predefined()
    
    #Lets update the namelist.input based on the uploaded one!
    #Start year
    Sys.setenv(TZ='GMT')
    wy_s_i = str_detect(wrf_predefined,"start_year")
    wy_s_i_1 = str_locate(wrf_predefined[wy_s_i],"=")[1]
    wy_s_i_2 = str_locate(wrf_predefined[wy_s_i],",")[1]
    wy_s_u = substr(wrf_predefined[wy_s_i], (wy_s_i_1+1), (wy_s_i_2-1))
    wy_s_u = str_replace(wy_s_u, pattern = " ", replacement = "")
    # wy_s_u = as.numeric(wy_s_u)
    
    #End year
    wy_e_i = str_detect(wrf_predefined,"end_year")
    wy_e_i_1 = str_locate(wrf_predefined[wy_e_i],"=")[1]
    wy_e_i_2 = str_locate(wrf_predefined[wy_e_i],",")[1]
    wy_e_u = substr(wrf_predefined[wy_e_i], (wy_e_i_1+1), (wy_e_i_2-1))
    wy_e_u = str_replace(wy_e_u, pattern = " ", replacement = "")
    # wy_e_u = as.numeric(wy_e_u)
    
    #Start month
    wm_s_i = str_detect(wrf_predefined,"start_month")
    wm_s_i_1 = str_locate(wrf_predefined[wm_s_i],"=")[1]
    wm_s_i_2 = str_locate(wrf_predefined[wm_s_i],",")[1] 
    wm_s_u = substr(wrf_predefined[wm_s_i], (wm_s_i_1+1), (wm_s_i_2-1))
    wm_s_u = str_replace(wm_s_u, pattern = " ", replacement = "")
    # wm_s_u = as.numeric(wm_s_u)
    
    #End month
    wm_e_i = str_detect(wrf_predefined,"end_month")
    wm_e_i_1 = str_locate(wrf_predefined[wm_e_i],"=")[1]
    wm_e_i_2 = str_locate(wrf_predefined[wm_e_i],",")[1] 
    wm_e_u = substr(wrf_predefined[wm_e_i], (wm_e_i_1+1), (wm_e_i_2-1))
    wm_e_u = str_replace(wm_e_u, pattern = " ", replacement = "")
    # wm_e_u = as.numeric(wm_e_u)
    
    #Start day
    wd_s_i = str_detect(wrf_predefined,"start_day")
    wd_s_i_1 = str_locate(wrf_predefined[wd_s_i],"=")[1]
    wd_s_i_2 = str_locate(wrf_predefined[wd_s_i],",")[1] 
    wd_s_u = substr(wrf_predefined[wd_s_i], (wd_s_i_1+1), (wd_s_i_2-1))
    wd_s_u = str_replace_all(wd_s_u,pattern = " ", replacement = "")
    # wd_s_u = as.numeric(wd_s_u)
    
    #End day
    wd_e_i = str_detect(wrf_predefined,"end_day")
    wd_e_i_1 = str_locate(wrf_predefined[wd_e_i],"=")[1]
    wd_e_i_2 = str_locate(wrf_predefined[wd_e_i],",")[1] 
    wd_e_u = substr(wrf_predefined[wd_e_i], (wd_e_i_1+1), (wd_e_i_2-1))
    wd_e_u = str_replace_all(wd_e_u,pattern = " ", replacement = "")
    # wd_e_u = as.numeric(wd_e_u)
    
    #Start hour
    wh_s_i = str_detect(wrf_predefined,"start_hour")
    wh_s_i_1 = str_locate(wrf_predefined[wh_s_i],"=")[1]
    wh_s_i_2 = str_locate(wrf_predefined[wh_s_i],",")[1] 
    wh_s_u = substr(wrf_predefined[wh_s_i], (wh_s_i_1+1), (wh_s_i_2-1))
    wh_s_u = str_replace_all(wh_s_u,pattern = " ", replacement = "")
    # wh_s_u = as.numeric(wh_s_u)
    
    #End hour
    wh_e_i = str_detect(wrf_predefined,"end_hour")
    wh_e_i_1 = str_locate(wrf_predefined[wh_e_i],"=")[1]
    wh_e_i_2 = str_locate(wrf_predefined[wh_e_i],",")[1] 
    wh_e_u = substr(wrf_predefined[wh_e_i], (wh_e_i_1+1), (wh_e_i_2-1))
    wh_e_u = str_replace_all(wh_e_u,pattern = " ", replacement = "")
    # wh_e_u = as.numeric(wh_e_u)
    
    #Start minutes
    wmi_s_i = str_detect(wrf_predefined,"start_minute")
    if (sum(wmi_s_i) == 0) {
      wmi_s_u = "00"
    } else {
      wmi_s_i_1 = str_locate(wrf_predefined[wmi_s_i],"=")[1]
      wmi_s_i_2 = str_locate(wrf_predefined[wmi_s_i],",")[1] 
      wmi_s_u = substr(wrf_predefined[wmi_s_i], (wmi_s_i_1+1), (wmi_s_i_2-1))
      wmi_s_u = str_replace_all(wmi_s_u,pattern = " ", replacement = "")
      wmi_s_u
      # wmi_s_u = as.numeric(wmi_s_u)
    }
    
    #End minutes
    wmi_e_i = str_detect(wrf_predefined,"end_minute")
    if (sum(wmi_e_i) == 0) {
      wmi_e_u = "00"
    } else {
      wmi_e_i_1 = str_locate(wrf_predefined[wmi_e_i],"=")[1]
      wmi_e_i_2 = str_locate(wrf_predefined[wmi_e_i],",")[1] 
      wmi_e_u = substr(wrf_predefined[wmi_e_i], (wmi_e_i_1+1), (wmi_e_i_2-1))
      wmi_e_u = str_replace_all(wmi_e_u,pattern = " ", replacement = "")
      # wmi_e_u = as.numeric(wmi_e_u)  
    }
    
    #Start seconds
    ws_s_i = str_detect(wrf_predefined,"start_second")
    if (sum(ws_s_i) == 0) {
      ws_s_u = "00"
    } else{
      ws_s_i_1 = str_locate(wrf_predefined[ws_s_i],"=")[1]
      ws_s_i_2 = str_locate(wrf_predefined[ws_s_i],",")[1] 
      ws_s_u = substr(wrf_predefined[ws_s_i], (ws_s_i_1+1), (ws_s_i_2-1))
      ws_s_u = str_replace_all(ws_s_u,pattern = " ", replacement = "")
      # ws_s_u = as.numeric(ws_s_u)
    }
    
    #End seconds
    ws_e_i = str_detect(wrf_predefined,"end_second")
    if (sum(ws_e_i) == 0) {
      ws_e_u = "00"
    } else {
      ws_e_i_1 = str_locate(wrf_predefined[ws_e_i],"=")[1]
      ws_e_i_2 = str_locate(wrf_predefined[ws_e_i],",")[1] 
      ws_e_u = substr(wrf_predefined[ws_e_i], (ws_e_i_1+1), (ws_e_i_2-1))
      ws_e_u = str_replace_all(ws_e_u,pattern = " ", replacement = "")
      # ws_e_u = as.numeric(ws_e_u)  
    }
    
    start_date_wrf = paste(paste(wy_s_u,wm_s_u,wd_s_u,sep = "-"),paste(wh_s_u, wmi_s_u, ws_s_u, sep = ":"), sep = " ")
    end_date_wrf = paste(paste(wy_e_u,wm_e_u,wd_e_u,sep = "-"),paste(wh_e_u, wmi_e_u, ws_e_u, sep = ":"), sep = " ")
    
    #Time
    time_s_wrf = strftime(start_date_wrf, format="%T")
    time_e_wrf = strftime(end_date_wrf, format="%T")
    
    #Max dom
    md_wrf_i = str_detect(wrf_predefined,"max_dom")
    md_wrf_i_1 = str_locate(wrf_predefined[md_wrf_i],"=")[1]
    md_wrf = parse_number(substr(wrf_predefined[md_wrf_i],md_wrf_i_1,nchar(wrf_predefined[md_wrf_i])))
    
    #History interval
    hist_int = str_detect(wrf_predefined, "history_interval")
    hist_i_wrf_i_1 = str_locate(wrf_predefined[hist_int],"=")[1]
    hist_i_wrf_i_2 = str_locate(wrf_predefined[hist_int],",")[1]
    hist_int_u = parse_number(substr(wrf_predefined[hist_int],hist_i_wrf_i_1,hist_i_wrf_i_2))
    
    #Timestep; unfortunately first time_step pattern is the desired one assumption is made.
    time_step_i = which(str_detect(wrf_predefined,"time_step"))[1]
    time_step_1 = str_locate(wrf_predefined[time_step_i],"=")[1]
    time_step_u = parse_number(substr(wrf_predefined[time_step_i],time_step_1,nchar(wrf_predefined[time_step_i])))
    
    updateDateRangeInput(session, inputId = "dateRange_WRF", start = start_date_wrf, end = end_date_wrf)
    updateTimeInput(session, "time_start_wrf",value = strptime(time_s_wrf,"%T"))
    updateTimeInput(session, "time_end_wrf",value = strptime(time_e_wrf,"%T"))
    updateNumericInput(session, "hist_int", value = hist_int_u)
    updateNumericInput(session, "max_dom_wrf", value = md_wrf)
    updateNumericInput(session, "time_step", value = time_step_u)
    
  })
  
  observeEvent(input$checkbutton_wrf, {
    
    output$checktext_wrf = renderUI({
      
      #WRF namelist upload message!
      if (is.null(input$wrf_namelist)) {
        first_message_wrf = paste("<strong><span style=\"color:darkred\">Please upload the namelist.input!</span></strong>")
      } else {
        first_message_wrf = paste("<strong><span style=\"color:darkgreen\">You have uploaded the namelist.input.</span></strong>", sep = "")
      }
      
      #Which domains will give outputs with how much time frequency?
      if (length(hist_dom())==0) {
        second_message_wrf = paste("<strong><span style=\"color:darkred\">Your selected domains exceeds max domain number! Please check the desired domains or maximum domain numbers!</span></strong>")
      } else {
        hist_dom = hist_dom()
        
        second_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("While maximum domain is selected ",
                                                                                     max_dom_wrf(),
                                                                                     ". Desired domains to give outputs is chosen as; ",
                                                                                     paste(hist_dom, collapse = ","),"!"),
                                   "</span></strong>", sep = "")
        
      }
      
      date_time_start_wrf = date_time_start_wrf()
      date_time_start_wrf = ymd_hms(date_time_start_wrf)
      date_time_end_wrf = date_time_end_wrf()
      date_time_end_wrf = ymd_hms(date_time_end_wrf)
      timeres = timeres()
      
      #WRF date range time message
      if (date_time_start_wrf>=date_time_end_wrf) {
        third_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose a start date which is smaller than the end date.</span></strong>")
      } else if(((as.numeric(date_time_end_wrf)-as.numeric(date_time_start_wrf)) %% timeres) == 0) { #bu else ifte startdate end'den kucuk oldugunda,
        
        third_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("Model will be run between ",
                                                                                    date_time_start_wrf(),
                                                                                    " and ",
                                                                                    date_time_end_wrf(),"!"),
                                  "</span></strong>", sep = "")
      } else {
        
        third_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose a valid date range that can be divisible by history interval (data time resolution) which is defined ",
                                  timeres/3600,
                                  " hours in WPS tab!</span></strong>", sep = "")
        
      }
      
      #WRF path message
      if (as.character(unlist(input$wrfdir)[1])==0) {
        fourth_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose a path for WRF directory!</span></strong>")
        # third_message = "Please choose a path for WRF directory!"
      } else {
        fourth_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("Path for the model is chosen as: ",
                                                                                     dirpath_wrf()),
                                   "</span></strong>", sep = "")
      }
      
      #Time step message
      if (is.null(time_step())) {
        fifth_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose time step for the model!</span></strong>")
        # fourth_message = "Please choose time step for the model!"
      } else {
        fifth_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("Time step is chosen ", time_step(),
                                                                                    " seconds. It is okay if dx for first domain is greater than or equal to ",
                                                                                    time_step()/6," km."),
                                  "</span></strong>", sep = "")
      }
      
      #MPI message
      if (mpisupport() == "Yes") {
        opt_cpu = round(as.numeric(system("nproc",intern = T))*3/4)
        max_cpu = as.numeric(system("nproc --all", intern = T))
        cpu_n = cpu_n()
        sixth_message_wrf = paste0("<strong><span style=\"color:darkgreen\">The machine has, ",max_cpu," CPU. And the optimum available CPU for model run is ",opt_cpu,", and you have chosen ",cpu_n," processors.</span></strong>")
      } else {
        sixth_message_wrf = paste("<strong><span style=\"color:darkgreen\">WRF is not installed with MPI support. WRF will be run with only one processor.</span></strong>", sep = "")
      }
      
      #WPS path message
      if (as.character(unlist(input$wpsdir)[1])==0) {
        seventh_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose the path for WPS directory. It is required for linking met_em files into WRF directory!</span></strong>")
      } else {
        seventh_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("WPS path is chosen as: ",
                                                                                      dirpath_wps()),
                                    "</span></strong>", sep = "")
      }
      
      #timeres message!
      if (is.null(timeres())) {
        eighth_message_wrf = paste("<strong><span style=\"color:darkred\">Please choose time resolution for met_em files! You should choose on previous tab, WPS!</span></strong>")
      } else {
        eighth_message_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("Time resolution is chosen as ", timeres(),
                                                                                     " seconds on previous tab, WPS."),
                                   "</span></strong>", sep = "")
      }
      
      
      
      HTML(paste(first_message_wrf, second_message_wrf, third_message_wrf, fourth_message_wrf, fifth_message_wrf, sixth_message_wrf, seventh_message_wrf, eighth_message_wrf, sep="<br/>"))
      # HTML(paste(first_message_wrf, second_message_wrf, third_message_wrf, fourth_message_wrf, fifth_message_wrf, sep="<br/>"))
      
      # validate(
      #   need(hist_dom(), "Your selected domains exceeds max domain number! Please check the desired domains or maximum domain numbers!")
      # )
      
    })
  })
  
  wrf_predefined_updated = reactive({
    
    wrf_predefined = wrf_predefined()
    
    #max dom
    #Index of max_dom
    wrf_m_index = str_detect(wrf_predefined, "max_dom")
    #max_dom remove and replace everything after equal sign
    wrf_m_fes = str_locate(wrf_predefined[wrf_m_index],"=")[1]
    max_dom_wrf = max_dom_wrf()
    wrf_predefined[wrf_m_index] = paste(substr(wrf_predefined[wrf_m_index],1,wrf_m_fes)," ", max_dom_wrf, ",", sep = "")
    
    #Index of iternval_seconds
    wrf_t_index = str_detect(wrf_predefined, "interval_seconds")
    #max_dom remove everything after equal sign
    wrf_t_fes = str_locate(wrf_predefined[wrf_t_index],"=")[1]
    timeres = timeres()
    wrf_predefined[wrf_t_index] = paste(substr(wrf_predefined[wrf_t_index],1,wrf_t_fes), timeres)
    
    # Configure the history interval
    #Index of it
    hist_int = hist_int()
    hist_dom = hist_dom()
    tumgrid = c(1:max_dom_wrf)
    hist_indeks = tumgrid %in% hist_dom
    hist_int_son = rep(hist_int, max_dom_wrf)
    hist_int_son[!hist_indeks] = 0
    hist_int_son = paste(paste(hist_int_son, collapse = ","),",", sep = "")
    
    wrf_h_index = str_detect(wrf_predefined, "history_interval")
    #geog_data_res equal sign
    wrf_h_fes = str_locate(wrf_predefined[wrf_h_index],"=")[1]
    wrf_predefined[wrf_h_index] = paste(substr(wrf_predefined[wrf_h_index],1,wrf_h_fes), hist_int_son)
    
    #WRF date indexes, start_year, month, day, hour, end_year, month, day, hour
    date_time_start_wrf = date_time_start_wrf()
    date_time_start_wrf = ymd_hms(date_time_start_wrf)
    
    start_year = paste(paste(rep(year(date_time_start_wrf),max_dom_wrf),collapse = ","),",",sep = "")
    
    start_month = month(date_time_start_wrf)
    start_month = ifelse(start_month<10, paste(0,start_month,sep = ""),start_month)
    start_month = paste(paste(rep(start_month,max_dom_wrf),collapse = ","),",",sep = "")
    
    start_day = day(date_time_start_wrf)
    start_day = ifelse(start_day<10, paste(0,start_day,sep = ""),start_day)
    start_day = paste(paste(rep(start_day,max_dom_wrf),collapse = ","),",",sep = "")
    
    start_hour = hour(date_time_start_wrf)
    start_hour = ifelse(start_hour<10, paste(0,start_hour,sep = ""),start_hour)
    start_hour = paste(paste(rep(start_hour,max_dom_wrf),collapse = ","),",",sep = "")
    
    wrf_predefined[str_detect(wrf_predefined,"start_year")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_year")],1,
                                                                           str_locate(wrf_predefined[str_detect(wrf_predefined,"start_year")],"=")[1]),
                                                                    start_year)
    
    wrf_predefined[str_detect(wrf_predefined,"start_month")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_month")],1,
                                                                            str_locate(wrf_predefined[str_detect(wrf_predefined,"start_month")],"=")[1]),
                                                                     start_month)
    
    wrf_predefined[str_detect(wrf_predefined,"start_day")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_day")],1,
                                                                          str_locate(wrf_predefined[str_detect(wrf_predefined,"start_day")],"=")[1]),
                                                                   start_day)
    
    wrf_predefined[str_detect(wrf_predefined,"start_hour")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_hour")],1,
                                                                           str_locate(wrf_predefined[str_detect(wrf_predefined,"start_hour")],"=")[1]),
                                                                    start_hour)
    
    #End date indexes!
    date_time_end_wrf = date_time_end_wrf()
    date_time_end_wrf = ymd_hms(date_time_end_wrf)
    
    end_year = paste(paste(rep(year(date_time_end_wrf),max_dom_wrf),collapse = ","),",",sep = "")
    
    end_month = month(date_time_end_wrf)
    end_month = ifelse(end_month<10, paste(0,end_month,sep = ""),end_month)
    end_month = paste(paste(rep(end_month,max_dom_wrf),collapse = ","),",",sep = "")
    
    end_day = day(date_time_end_wrf)
    end_day = ifelse(end_day<10, paste(0,end_day,sep = ""),end_day)
    end_day = paste(paste(rep(end_day,max_dom_wrf),collapse = ","),",",sep = "")
    
    end_hour = hour(date_time_end_wrf)
    end_hour = ifelse(end_hour<10, paste(0,end_hour,sep = ""),end_hour)
    end_hour = paste(paste(rep(end_hour,max_dom_wrf),collapse = ","),",",sep = "")
    
    wrf_predefined[str_detect(wrf_predefined,"end_year")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_year")],1,
                                                                         str_locate(wrf_predefined[str_detect(wrf_predefined,"end_year")],"=")[1]),
                                                                  end_year)
    
    wrf_predefined[str_detect(wrf_predefined,"end_month")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_month")],1,
                                                                          str_locate(wrf_predefined[str_detect(wrf_predefined,"end_month")],"=")[1]),
                                                                   end_month)
    
    wrf_predefined[str_detect(wrf_predefined,"end_day")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_day")],1,
                                                                        str_locate(wrf_predefined[str_detect(wrf_predefined,"end_day")],"=")[1]),
                                                                 end_day)
    
    wrf_predefined[str_detect(wrf_predefined,"end_hour")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_hour")],1,
                                                                         str_locate(wrf_predefined[str_detect(wrf_predefined,"end_hour")],"=")[1]),
                                                                  end_hour)
    
    #wrf time step
    time_step = time_step()
    #Take index of time_step. Unfortunately the assumption of first time_step pattern argument of all is the desired one is taken.
    time_step_i = which(str_detect(wrf_predefined, "time_step"))[1]
    #max_dom, remove the afterwards of first equal sign
    time_step_fes = str_locate(wrf_predefined[time_step_i],"=")[1]
    #Change time_step inside namelist.input
    wrf_predefined[time_step_i] = paste(substr(wrf_predefined[time_step_i],1,time_step_fes)," ", time_step, ",", sep = "")
    wrf_predefined
    
  })
  
  observeEvent(input$preprocess_WRF, {
    
    # Copying WRF path and the previous namelist
    wrf_predefined_updated = wrf_predefined_updated()
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run/")
    
    #Only take "namelist.wps"; means [1]!!!
    prev_wrf = list.files(dirpath_wrf,pattern = "namelist.input", all.files = T, full.names = T)[1]
    
    
    if (input$save_wrf == "Yes") {
      
      Sys.setenv(TZ='GMT')
      file.copy(from = prev_wrf, paste(dirpath_wrf,"/namelist.input.",str_replace_all(str_replace_all(Sys.time(),":","")," ", "_"),".previous", sep = ""))
      write(x = wrf_predefined_updated, file = paste(dirpath_wrf,"/namelist.input",sep = ""))
      
    }
    
    #WRF predefined updated for ui display
    output$text_namelist_wrf = renderUI({
      p(strong(em("Configured WRF namelist"), "Observe the changes"))
      wrf_splitText = stringi::stri_split(str = wrf_predefined_updated, regex = '\\n')
      
      # Wrap a paragraph tag around each element in the list
      wrf_replacedText = lapply(wrf_splitText, p)
      wrf_replacedText})
    
  })
  
  #Download button for namelist.input
  observe({
    
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run/")
    
    wrf_predefined_updated = wrf_predefined_updated()
    date_time_start_wrf = substr(str_replace_all(date_time_start_wrf(),":",""),1,15)
    date_time_end_wrf = substr(str_replace_all(date_time_end_wrf(),":",""),1,15)
    
    download_script = function(data) {
      downloadHandler(
        filename = function() {
          paste(dirpath_wrf,"/namelist_",date_time_start_wrf,"_",date_time_end_wrf,".input", sep = "")
        },
        content = function(file) {
          write(data, file, sep = "")
        }
      )
    }
    output$download_wrf = download_script(wrf_predefined_updated)
  })
  
  #Run Real.exe!
  observeEvent(input$real, {
    
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run")
    dirpath_wps = dirpath_wps()
    setwd(dirpath_wrf)
    
    #Link the met_em files
    system(paste0("ln -sf ",dirpath_wps,"/met_em* ."))
    system("./real.exe")
    
    output$real_msg = renderUI({
      
      existed_real = list.files(pattern = "wrfinput*")
      #Real.exe output control
      max_dom_wrf = max_dom_wrf()
      shouldbe_real = paste("wrfinput_d0",1:max_dom_wrf,sep = "")
      existed_count = shouldbe_real%in%existed_real
      unexisted_real = shouldbe_real[!shouldbe_real%in%existed_real]
      
      if (length(shouldbe_real) == sum(existed_count)) {
        final_msg_real = paste("<strong><span style=\"color:darkgreen\">",paste0("real.exe succesfully completed! ",paste(existed_real,collapse = ", ")," created."),
                               "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe succesfully completed! ",paste(existed_real,collapse = ", ")," created.")
      } else if (sum(existed_count)==0){
        final_msg_real = paste("<strong><span style=\"color:darkred\">real.exe is not completed! Check namelist or met_em file links!</span></strong>", sep = "")
        # final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("real.exe is not completed! ",paste(unexisted_real,collapse = ", "),"is not created! Check namelist or met_em file links!"),
        #                       "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe is not completed! ",unexisted_real,"is not created! Check namelist!")
      } else {
        final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("real.exe partially completed! Only ",paste(existed_real, collapse = ", ")," is created. Check namelist or met_em file links!"),
                               "</span></strong>", sep = "")
        # final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("realgrid.exe partially completed! Only ",existed_real," is created. ",paste(unexisted_real,collapse = ", "),"is missing! Check namelist or met_em file links!"),
        #                       "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe partially completed! Only ",existed_real," is created. ",unexisted_real,"is missing! Check namelist!")
      }
      
      # final_msg_real
      HTML(paste(final_msg_real, sep="<br/>"))
      
    })
    
  })
  
  #WRF.exe'yi calistir!
  observeEvent(input$wrf, {
    
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run/")
    setwd(dirpath_wrf) 
    
    cpu_n = cpu_n()
    system(paste0("mpirun -np ",cpu_n," ./wrf.exe "))
    
    output$wrf_msg = renderUI({
      
      existed_wrf = list.files(pattern = "wrfout")
      #WRF output control
      max_dom_wrf = as.numeric(max_dom_wrf())
      max_dom_wrf_g = 1:max_dom_wrf
      date_time_start_wrf = date_time_start_wrf()
      date_time_end_wrf = date_time_end_wrf()
      hist_int = hist_int()
      Sys.setenv(TZ='GMT')
      
      shouldbe_wrf = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(hist_int)*60)
      wrf_grid = expand.grid(max_dom_wrf_g, shouldbe_wrf)
      wrf_grid$Var2 = strftime(wrf_grid$Var2, "%Y-%m-%d_%H:%M:%S")
      shouldbe_wrf = paste0("wrfout_d0",str_c(wrf_grid$Var1,wrf_grid$Var2,sep = "_"))
      
      existed_count = shouldbe_wrf%in%existed_wrf
      unexisted_wrf = shouldbe_wrf[!shouldbe_wrf%in%existed_wrf]
      
      if (length(shouldbe_wrf) == sum(existed_count)) {
        final_msg_wrf = paste("<strong><span style=\"color:darkgreen\">wrf.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("wrfrid.exe succesfully completed! ",paste(existed_wrf,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_wrf = paste("<strong><span style=\"color:darkred\">wrf.exe is not completed! Check namelist!</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrfrib.exe is not completed! ",paste(unexisted_wrf,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrf.exe partially completed! Only ",paste(existed_wrf, collapse = ", ")," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrfrib.exe partially completed! Only ",existed_wrf," is created. ",paste(unexisted_wrf,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_wrf
      HTML(paste(final_msg_wrf, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
  })
  
  #All WRF
  observeEvent(input$AllWRF, {
    
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run")
    dirpath_wps = dirpath_wps()
    setwd(dirpath_wrf) 
    
    #Link the met_em files
    system(paste0("ln -sf ",dirpath_wps,"/met_em* ."))
    system("./real.exe")
    
    output$real_msg = renderUI({
      
      existed_real = list.files(pattern = "wrfinput*")
      #real.exe output control
      max_dom_wrf = max_dom_wrf()
      # max_dom_wrf = 2
      shouldbe_real = paste("wrfinput_d0",1:max_dom_wrf,sep = "")
      existed_count = shouldbe_real%in%existed_real
      unexisted_real = shouldbe_real[!shouldbe_real%in%existed_real]
      
      if (length(shouldbe_real) == sum(existed_count)) {
        final_msg_real = paste("<strong><span style=\"color:darkgreen\">",paste0("real.exe succesfully completed! ",paste(existed_real,collapse = ", ")," created."),
                               "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe succesfully completed! ",paste(existed_real,collapse = ", ")," created.")
      } else if (sum(existed_count)==0){
        final_msg_real = paste("<strong><span style=\"color:darkred\">real.exe is not completed! Check namelist or met_em file links!</span></strong>", sep = "")
        # final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("real.exe is not completed! ",paste(unexisted_real,collapse = ", "),"is not created! Check namelist or met_em file links!"),
        #                       "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe is not completed! ",unexisted_real,"is not created! Check namelist!")
      } else {
        final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("realgrid.exe partially completed! Only ",paste(existed_real, collapse = ", ")," is created. Check namelist or met_em file links!"),
                               "</span></strong>", sep = "")
        # final_msg_real = paste("<strong><span style=\"color:darkred\">",paste0("realgrid.exe partially completed! Only ",existed_real," is created. ",paste(unexisted_real,collapse = ", "),"is missing! Check namelist or met_em file links!"),
        #                       "</span></strong>", sep = "")
        # final_msg_real = paste0("realgrid.exe partially completed! Only ",existed_real," is created. ",unexisted_real,"is missing! Check namelist!")
      }
      
      # final_msg_real
      HTML(paste(final_msg_real, sep="<br/>"))
      
    })
    
    cpu_n = cpu_n()
    system(paste0("mpirun -np ",cpu_n," ./wrf.exe "))
    
    output$wrf_msg = renderUI({
      
      existed_wrf = list.files(pattern = "wrfout")
      # wrf output check!
      max_dom_wrf = as.numeric(max_dom_wrf())
      max_dom_wrf_g = 1:max_dom_wrf
      date_time_start_wrf = date_time_start_wrf()
      date_time_end_wrf = date_time_end_wrf()
      hist_int = hist_int()
      Sys.setenv(TZ='GMT')
      
      shouldbe_wrf = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(hist_int)*60)
      shouldbe_wrf = seq(ymd_hms("2021-01-01 00:00:00"),ymd_hms("2021-01-01 06:00:00"), by = 60*60)
      wrf_grid = expand.grid(max_dom_wrf_g, shouldbe_wrf)
      wrf_grid$Var2 = strftime(wrf_grid$Var2, "%Y-%m-%d_%H:%M:%S")
      shouldbe_wrf = paste0("wrfout_d0",str_c(wrf_grid$Var1,wrf_grid$Var2,sep = "_"))
      
      existed_count = shouldbe_wrf%in%existed_wrf
      unexisted_wrf = shouldbe_wrf[!shouldbe_wrf%in%existed_wrf]
      
      if (length(shouldbe_wrf) == sum(existed_count)) {
        final_msg_wrf = paste("<strong><span style=\"color:darkgreen\">wrf.exe succesfully completed!</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkgreen\">",paste0("wrfrid.exe succesfully completed! ",paste(existed_wrf,collapse = ", ")," created."),
        #                       "</span></strong>", sep = "")
      } else if (sum(existed_count)==0){
        final_msg_wrf = paste("<strong><span style=\"color:darkred\">wrf.exe is not completed! Check namelist!</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrfrib.exe is not completed! ",paste(unexisted_wrf,collapse = ", "),"is not created! Check namelist or data links or data!"),
        #                       "</span></strong>", sep = "")
      } else {
        final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrf.exe partially completed! Only ",paste(existed_wrf, collapse = ", ")," is created. Check namelist!"),
                              "</span></strong>", sep = "")
        # final_msg_wrf = paste("<strong><span style=\"color:darkred\">",paste0("wrfrib.exe partially completed! Only ",existed_wrf," is created. ",paste(unexisted_wrf,collapse = ", "),"is missing! Check namelist!"),
        #                       "</span></strong>", sep = "")
      }
      
      # final_msg_wrf
      HTML(paste(final_msg_wrf, sep="<br/>"))
      # file.remove("PFILE:")
      
    })
    
  })
  
  #Data download in bash?
  datadownload = reactive({
    datadownload = as.character(input$datadownload)
  })
  
  #Bash Script
  bash_reactive = eventReactive(input$bash_preparer, {
    
    datadownload = datadownload()
    
    if (datadownload == "Yes") {
      datatype = datatype()
      daterange_data = daterange_data()
      dirpath_data = dirpath_data()
      
      if (datatype == "FNL") {
        
        email = email
        pass = pass
        
        Sys.setenv(TZ="GMT")
        data_i = daterange_data[1]
        data_f = daterange_data[2]
        
        dir.create(paste(dirpath_data,"/FNL_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
        
        setwd(paste(dirpath_data,"/FNL_",data_i,"_to_",data_f, sep = ""))
        fnl_dirpath2 = paste0("cd ",dirpath_data,"/FNL_",data_i,"_to_",data_f)
        
        fnl_authentication = paste0("wget -O Authentication.log --save-cookies auth.rda_ucar_edu  --post-data 'email=", email, "&passwd=", pass, "&action=login' https://rda.ucar.edu/cgi-bin/login")
        
        seq_dates = seq(as.POSIXct(data_i), as.POSIXct(data_f), by = "6 hour")
        list_dates = format(seq_dates, "%Y%m%d_%H_00")
        file_name = paste0("fnl_", list_dates, ".grib2") #as.list(paste(my_url, file_name, sep = ""))
        
        if (data_i <= "2007-12-06" & data_f <= "2007-12-06") {
          
          down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib1/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib1")
          
        } else if (data_i >= "2007-12-06" & data_f >= "2007-12-06") {
          
          down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib2/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib2")
          
        }
        
        fnl_link = c()
        
        for (i in 1:length(file_name)){
          
          if (file.exists(file_name[i])) {
            
            cat(paste0(file_name[i], " - already downloaded. \n"))
            
          } else {
            
            # sink("fnl.log")
            a = paste0("wget -N --load-cookies auth.rda_ucar_edu ", down_link[i])
            fnl_link = append(fnl_link,a)
            # sink()
            
          }
        }
        
        fnl_link = paste(fnl_link,collapse = "\n")
        fnl_remove_auth = paste0("rm -rf Authentication.log auth.rda_ucar_edu")
        shouldbe_fnl = paste0("fnl_",list_dates[length(list_dates)], ".grib2")
        fnl_kontrol = paste0("if test -f '",shouldbe_fnl,"'; then echo 'FNL data sucessfuly downloaded!'; else echo 'FNL data is not downloaded!'; exit; fi")
        
        fnl_download_final = paste(fnl_dirpath2,fnl_authentication,fnl_link,fnl_remove_auth,fnl_kontrol, sep = "\n")
        
      } else 
      {
        
        data_i = daterange_data[1]
        data_f = daterange_data[2]
        area = area()
        
        dir.create(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
        setwd(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""))
        era5_dirpath2 = paste0("cd ",dirpath_data,"/ERA5_",data_i,"_to_",data_f)
        
        list_dates = seq.Date(as.Date(data_i), as.Date(data_f), by = "day")
        list_dates = format(list_dates, "%Y%m%d")
        
        era5_surf_commands = c()
        for (i in 1:length(list_dates)) {
          
          if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".grib"))) {
            
            cat(paste0("ERA5_sfc_", list_dates[i], ".grib"), " - already downloaded. \n")
            
          } else if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".nc"))) {
            
            cat(paste0("ERA5_sfc_", list_dates[i], ".nc"), " - already downloaded. \n")
            
          } else {
            
            txt = c("#!/usr/bin/env python", 
                    "import cdsapi",
                    "",
                    "c = cdsapi.Client()",
                    "",
                    "c.retrieve(",
                    "'reanalysis-era5-single-levels',",
                    "{",
                    "'product_type':'reanalysis',",
                    "'format':'grib',",
                    "'variable':[",
                    "'10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',",
                    "'2m_temperature','land_sea_mask','mean_sea_level_pressure',",
                    "'sea_ice_cover','sea_surface_temperature','skin_temperature',",
                    "'snow_depth','soil_temperature_level_1','soil_temperature_level_2',",
                    "'soil_temperature_level_3','soil_temperature_level_4','surface_pressure',",
                    "'volumetric_soil_water_layer_1','volumetric_soil_water_layer_2','volumetric_soil_water_layer_3',",
                    "'volumetric_soil_water_layer_4'",
                    "],",
                    paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
                    paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
                    paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
                    "'time': [",
                    "'00:00', '01:00', '02:00',",
                    "'03:00', '04:00', '05:00',",
                    "'06:00', '07:00', '08:00',",
                    "'09:00', '10:00', '11:00',",
                    "'12:00', '13:00', '14:00',",
                    "'15:00', '16:00', '17:00',",
                    "'18:00', '19:00', '20:00',",
                    "'21:00', '22:00', '23:00',",
                    "],",
                    "'area': [",
                    paste0(area[1],",",area[2],",",area[3],",",area[4]),
                    "],",
                    "},",
                    paste0("'ERA5_sfc_", list_dates[i], ".grib')")
            )
            
            writeLines(txt, paste0("ECMWF_ERA5_sfc_", list_dates[i], ".py"))
            
          }
          
          files_python = list.files(pattern = "*.py")
          files_python = files_python[length(files_python)]
          a = paste0("python ", files_python)
          era5_surf_commands = append(era5_surf_commands,a)
          
        }
        
        era5_surf_commands = paste(era5_surf_commands,collapse = "\n")
        
        shouldbe_era5_surf = paste0("ERA5_sfc_", list_dates[length(list_dates)],".grib")
        era5_surf_kontrol = paste0("if test -f '",shouldbe_era5_surf,"'; then echo 'ERA5 surface data sucessfuly downloaded!'; else echo 'ERA5 surface data is not downloaded!'; exit; fi")
        era5_surf_download_final = paste(era5_dirpath2,era5_surf_commands,era5_surf_kontrol, sep = "\n")
        
        #Pressure level for ERA5
        # time sequence
        setwd(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""))
        era5_pl_commands = c()
        
        for (i in 1:length(list_dates)) {
          
          if (file.exists(paste0("ERA5_pl_", list_dates[i], ".grib"))) {
            
            cat(paste0("ERA5_pl_", list_dates[i], ".grib"), " - already downloaded. \n")
            
          } else if (file.exists(paste0("ERA5_pl_", list_dates[i], ".nc"))) {
            
            cat(paste0("ERA5_pl_", list_dates[i], ".nc"), " - already downloaded. \n")
            
          } else {
            
            txt = c("#!/usr/bin/env python", 
                    "import cdsapi",
                    "",
                    "c = cdsapi.Client()",
                    "",
                    "c.retrieve(",
                    "'reanalysis-era5-pressure-levels',",
                    "{",
                    "'product_type':'reanalysis',",
                    "'format':'grib',",
                    "'variable':[",
                    "'geopotential', 'relative_humidity', 'specific_humidity',",
                    "'temperature', 'u_component_of_wind', 'v_component_of_wind',",
                    "],",
                    "'pressure_level': [",
                    "'1', '2', '3',",
                    "'5', '7', '10',",
                    "'20', '30', '50',",
                    "'70', '100', '125',",
                    "'150', '175', '200',",
                    "'225', '250', '300',",
                    "'350', '400', '450',",
                    "'500', '550', '600',",
                    "'650', '700', '750',",
                    "'775', '800', '825',",
                    "'850', '875', '900',",
                    "'925', '950', '975',",
                    "'1000',",
                    "],",
                    paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
                    paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
                    paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
                    "'time': [",
                    "'00:00', '01:00', '02:00',",
                    "'03:00', '04:00', '05:00',",
                    "'06:00', '07:00', '08:00',",
                    "'09:00', '10:00', '11:00',",
                    "'12:00', '13:00', '14:00',",
                    "'15:00', '16:00', '17:00',",
                    "'18:00', '19:00', '20:00',",
                    "'21:00', '22:00', '23:00',",
                    "],",
                    "'area': [",
                    paste0(area[1],",",area[2],",",area[3],",",area[4]),
                    "],",
                    "},",
                    paste0("'ERA5_pl_", list_dates[i], ".grib')")
            )
            
            writeLines(txt, paste0("ECMWF_ERA5_pl_",list_dates[i],".py"))
            
            
          }
          
          files_python = list.files(pattern = "ECMWF_ERA5_pl*")
          files_python = files_python[length(files_python)]
          a = paste0("python ", files_python)
          era5_pl_commands = append(era5_pl_commands,a)
          
          
        }
        
        era5_pl_commands = paste(era5_pl_commands,collapse = "\n")
        era5_remove_python = paste0("rm -rf *.py")
        
        shouldbe_era5_pl = paste0("ERA5_pl_", list_dates[length(list_dates)],".grib")
        era5_pl_kontrol = paste0("if test -f '",shouldbe_era5_pl,"'; then echo 'ERA5 pressure level data sucessfuly downloaded!'; else echo 'ERA5 pressure level data is not downloaded!'; exit; fi")
        era5_pl_download_final = paste(era5_dirpath2,era5_pl_commands,era5_remove_python,era5_pl_kontrol, sep = "\n")
        era5_download_final = paste(era5_surf_download_final,era5_pl_download_final, sep = "\n")
        
      }
    } else {
      
      download_no = paste0("echo 'This script does not download any data. Data path should be provided on Data or WPS tab'")  
      
    }
    
    #WPS path
    dirpath_wps = dirpath_wps()
    
    #Geogrid control
    max_dom = max_dom()
    shouldbe_geo = paste("geo_em.d0",1:max_dom,".nc",sep = "")
    shouldbe_geo = shouldbe_geo[length(shouldbe_geo)]
    geogrid_kontrol = paste0("if test -f '",shouldbe_geo,"'; then echo 'Geogrid.exe completed!'; else echo 'Geogrid.exe is not completed!'; exit; fi")
    
    datatype = datatype()
    #Data link with linkgrib inside WPS directory
    if (as.character(unlist(input$datadir)[1])!= 0 & as.character(unlist(input$datadir2)[1])!= 0){
      dirpath_data2 = dirpath_data2()
      datalink = paste("./link_grib.csh ", dirpath_data2,"/*", sep = "")
    } else if (as.character(unlist(input$datadir2)[1])!= 0) {
      dirpath_data2 = dirpath_data2()
      datalink = paste("./link_grib.csh ", dirpath_data2,"/*", sep = "")
    } else {
      dirpath_data = dirpath_data()
      daterange_data = daterange_data()
      datalink = paste("./link_grib.csh ", dirpath_data,"/",datatype,"_",daterange_data[1],"_to_",daterange_data[2],"/*", sep = "")
    }
    
    #Vtable!
    if (datatype == "FNL") {
      vtable = "ln -sf ungrib/Variable_Tables/Vtable.GFS Vtable"
    } else {
      vtable = "ln -sf ungrib/Variable_Tables/Vtable.ERA-interim.pl Vtable"
    }
    
    #Ungrib control!
    date_time_start_wps = date_time_start_wps()
    date_time_end_wps = date_time_end_wps()
    timeres = timeres()
    Sys.setenv(TZ='GMT')
    shouldbe_ung = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
    shouldbe_ung = paste0("FILE:",strftime(shouldbe_ung, "%Y-%m-%d_%H"))
    shouldbe_ung = shouldbe_ung[length(shouldbe_ung)]
    ungrib_kontrol = paste0("if test -f '",shouldbe_ung,"'; then echo 'Ungrib.exe completed!'; else echo 'Ungrib.exe is not completed!'; exit; fi")
    
    #Metgrid control!
    max_dom = as.numeric(max_dom())
    max_dom_g = 1:max_dom
    date_time_start_wps = date_time_start_wps()
    date_time_end_wps = date_time_end_wps()
    timeres = timeres()
    Sys.setenv(TZ='GMT')
    shouldbe_met = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(timeres))
    mg_grid = expand.grid(max_dom_g,shouldbe_met)
    mg_grid$Var2 = strftime(mg_grid$Var2, "%Y-%m-%d_%H:%M:%S")
    shouldbe_met = paste0("met_em.d0",str_c(mg_grid$Var1,mg_grid$Var2,sep = "."),".nc")
    shouldbe_met = shouldbe_met[length(shouldbe_met)]
    metgrid_kontrol = paste0("if test -f '",shouldbe_met,"'; then echo 'Metgrid.exe completed!'; else echo 'Metgrid.exe is not completed!'; exit; fi")
    
    #Real.exe
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run")
    
    #Real.exe control!
    max_dom_wrf = max_dom_wrf()
    shouldbe_real = paste("wrfinput_d0",1:max_dom_wrf,sep = "")
    shouldbe_real = shouldbe_real[length(shouldbe_real)]
    real_kontrol = paste0("if test -f '",shouldbe_real,"'; then echo 'Real.exe completed!'; else echo 'Real.exe is not completed!'; exit; fi")
    
    #Wrf.exe
    cpu_n = cpu_n()
    
    #Wrf.exe control!
    max_dom_wrf = as.numeric(max_dom_wrf())
    max_dom_wrf_g = 1:max_dom_wrf
    date_time_start_wrf = date_time_start_wrf()
    date_time_end_wrf = date_time_end_wrf()
    hist_int = hist_int()
    Sys.setenv(TZ='GMT')
    shouldbe_wrf = seq(ymd_hms(date_time_start_wps),ymd_hms(date_time_end_wps), by = as.numeric(hist_int)*60)
    wrf_grid = expand.grid(max_dom_wrf_g, shouldbe_wrf)
    wrf_grid$Var2 = strftime(wrf_grid$Var2, "%Y-%m-%d_%H:%M:%S")
    shouldbe_wrf = paste0("wrfout_d0",str_c(wrf_grid$Var1,wrf_grid$Var2,sep = "_"))
    shouldbe_wrf = shouldbe_wrf[length(shouldbe_wrf)]
    wrf_kontrol = paste0("if test -f '",shouldbe_wrf,"'; then echo 'Wrf.exe completed!'; else echo 'Wrf.exe is not completed!'; exit; fi")
    
    #Make directory for WRF outputs 
    wrf_out_dir = paste0("outputs_wrf_",substr(str_replace_all(date_time_start_wrf, ":",""),1,15),"_",substr(str_replace_all(date_time_end_wrf, ":", ""),1,15))
    
    bash = paste("#!/bin/bash",
                 ifelse(datadownload == "Yes", ifelse(datatype == "FNL",fnl_download_final,era5_download_final),download_no),
                 "#Change directory to the WPS",
                 paste0("cd ",dirpath_wps),
                 "#Clean the directory",
                 paste0("rm geo_em*"),
                 paste0("rm FILE:*"),
                 paste0("rm met_em*"),
                 "#Geogrid.exe",
                 paste0("./geogrid.exe"),
                 "#Geogrid check!",
                 geogrid_kontrol,
                 "#Link the data!",
                 datalink,
                 "#Link the related Vtable",
                 vtable,
                 "#Ungrib.exe",
                 paste0("./ungrib.exe"),
                 "#Ungrib check!",
                 ungrib_kontrol,
                 "#Metgrid.exe",
                 paste0("./metgrid.exe"),
                 "#Metgrid control",
                 metgrid_kontrol,
                 "#WRF",
                 paste0("cd ",dirpath_wrf),
                 "#Met_em files are being linked!",
                 paste0("ln -sf ",dirpath_wps,"/met_em* ."),
                 "#Real.exe",
                 paste0("./real.exe"),
                 "#Real.exe check!",
                 real_kontrol,
                 "#Wrf.exe",
                 paste0("mpirun -np ",cpu_n," ./wrf.exe "),
                 "#Wrf.exe check!",
                 wrf_kontrol,
                 "#Make a directory and move the wrf outputs inside it",
                 paste0("mkdir ",wrf_out_dir),
                 paste0("mv wrfout* ",wrf_out_dir),
                 sep = "\n")

  })
  
  observeEvent(input$bash_button, {
    bash = bash_reactive()
    bash
    
    output$bash_text = renderText(bash)
  })
  
  #Bash script download
  observe({
    
    bash = bash_reactive()
    date_time_start_wps = str_replace_all(date_time_start_wps(),":","")
    date_time_end_wps = str_replace_all(date_time_end_wps(),":","")
    
    download_script = function(data) {
      downloadHandler(
        filename = function() {
          paste("bash_wrf_",date_time_start_wps,"_",date_time_end_wps,".bash", sep = "")
        },
        content = function(file) {
          write(data, file, sep = "")
        }
      )
    }
    output$download_bash = download_script(bash)
  })
  
  datebash = reactive({
    req(date$datebash)
    datebash = input$datebash
  })
  
  date_time_start_bash = reactive({
    
    req(input$datebash)
    req(input$timebash1)
    
    date_ranges_bash = as.character(input$datebash)
    time_1_bash = as.character(strftime(input$timebash1, "%T"))
    date_time_start_bash = paste(as.character(date_ranges_bash[1]),time_1_bash, sep = "_")
    date_time_start_bash
    
  })
  
  date_time_end_bash = reactive({
    
    req(input$datebash)
    req(input$timebash1)
    
    date_ranges_bash = as.character(input$datebash)
    time_2_bash = as.character(strftime(input$timebash2, "%T"))
    date_time_end_bash = paste(as.character(date_ranges_bash[2]),time_2_bash, sep = "_")
    date_time_end_bash
    
  })
  
  sequence_bash = reactive({
    req(input$sequence_bash)
    sequence_bash = input$sequence_bash
  })
  
  seq_num_bash = reactive({
    req(input$seq_num_bash)
    seq_num_bash = input$seq_num_bash
  })  
  
  spinup_num = reactive({
    req(input$spinup_num)
    spinup_num = input$spinup_num
  })
  
  #Check the bash script requirements 
  observeEvent(input$checkbutton_bash2, {
    
    output$checktext_bash = renderUI({
      
      daterange1 = ymd_hms(date_time_start_bash())
      daterange2 = ymd_hms(date_time_end_bash())
      sekans = as.character(sequence_bash())
      sayi_sekans = as.numeric(seq_num_bash())
      
      ilerle = ifelse(sekans == "Day", sayi_sekans*24*3600, 
                      ifelse(sekans == "Week", paste(sayi_sekans, "week", sep = " "),
                             ifelse(sekans == "Month", paste(sayi_sekans,"month", sep = " "))))
      
      datatype = datatype()
      timeres = timeres()
      
      spinup_sayi = as.numeric(spinup_num())
      spinup = spinup_sayi*3600
      
      sekans_times = seq(from = daterange1, to = daterange2, by = ilerle)
      
      #If the last values of the sequence is equal with daterange2; then the last value of the sequence does not need to be added!
      if (sekans_times[length(sekans_times)] == daterange2) {
        sekans_times = sekans_times  
      } else {
        sekans_times = append(sekans_times, daterange2)
      }
      
      sekans_times_spinup = sekans_times-spinup
      
      #Namelist dates
      if (datatype == "FNL") {
        sekans_times_spinup = floor_date(sekans_times_spinup,unit = "6 hours")
      } else {
        sekans_times_spinup = sekans_times_spinup
      }
      
      #Date for the data!
      sekans_times_spinup_data = floor_date(sekans_times_spinup,unit = "1 day")
      sekans_times_data = ceiling_date(sekans_times,unit = "1 day")
      #Date for the data!
      datadates = c(sekans_times_spinup_data[1],sekans_times_data[length(sekans_times_data)])
      
      sekans_times_spinup_final=list()
      
      #Date for namelist
      for (i in 1:(length(sekans_times)-1)) {
        sekans_times_spinup_final[[i]] = c(sekans_times_spinup[i],sekans_times[i+1])
      }
      
      sekans_dates_1 = strftime(sekans_times_spinup_final[[1]][1],"%Y%m%d_%H%M")
      sekans_dates_2 = strftime(sekans_times_spinup_final[[length(sekans_times_spinup_final)]][2], "%Y%m%d_%H%M")
      
      #Bash date with sequence message!
      if (daterange1>=daterange2) {
        first_message_bash = paste("<strong><span style=\"color:darkred\">Please choose a start date which is smaller than the end date.</span></strong>")
      } else {
        
        first_message_bash = paste("<strong><span style=\"color:darkgreen\">",paste0("Model will be run between ",
                                                                                     sekans_times_spinup_final[[1]][1],
                                                                                     " and ",
                                                                                     sekans_times_spinup_final[[length(sekans_times_spinup_final)]][2],"! ",
                                                                                     "Total run will be in ",length(sekans_times_spinup_data), " parts."),
                                   "</span></strong>", sep = "")
      }
      
      #Data message!
      datadownload = datadownload()
      datatype = datatype()
      
      if (datadownload == "Yes" & as.character(unlist(input$datadir)[1])!=0) {
        dirpath_data = dirpath_data()
        directpath = paste(dirpath_data,"/",datatype,"_",datadates[1],"_to_",datadates[2], sep = "")
        second_message_bash = paste("<strong><span style=\"color:darkgreen\">",paste0(datatype," data will be downloaded between ",
                                                                                      datadates[1],
                                                                                      " and ",
                                                                                      datadates[2]," in ",directpath,".",
                                                                                      "Total run will be in ",length(sekans_times_spinup_data), " part."),
                                    "</span></strong>", sep = "")
        
      } else if (datadownload == "No" & as.character(unlist(input$datadir2)[1])==0) {
        second_message_bash = paste("<strong><span style=\"color:darkred\">Data will not be downloaded in bash script! However you have to specify a directory for data in WPS part!</span></strong>")
      } else if (datadownload == "No" & as.character(unlist(input$datadir2)[1])!=0) {
        dirpath_data2 = dirpath_data2()
        second_message_bash = paste("<strong><span style=\"color:darkgreen\">Data will not be downloaded in bash script! You have specified the path for data ",dirpath_data2,"</span></strong>")
      } else if (datadownload == "Yes" & as.character(unlist(input$datadir)[1])==0) {
        second_message_bash = paste("<strong><span style=\"color:darkred\">You have to specify the data path for the data that will be downloaded in Data tab!</span></strong>")
      } else if (as.character(unlist(input$datadir)[1])==0 & as.character(unlist(input$datadir2)[1])==0) {
        second_message_bash = paste("<strong><span style=\"color:darkred\">You didn'y specify any path for the data that will be downloaded in Data or WPS tab!</span></strong>")
      }
      
      #WPS path message
      if (as.character(unlist(input$wpsdir)[1])==0) {
        third_message_bash = paste("<strong><span style=\"color:darkred\">Please choose a path for WPS directory!</span></strong>")
      } else {
        dirpath_wps = dirpath_wps()
        namelistwps_dir = paste0(dirpath_wps,"/namelists_wps_seqs_",sekans_dates_1,"_",sekans_dates_2)
        third_message_bash = paste("<strong><span style=\"color:darkgreen\">",paste0("Path for the WPS is chosen as: ",
                                                                                     dirpath_wps(),". ",
                                                                                     namelistwps_dir,
                                                                                     " directory will be created in WPS and all the namelist.wps files for sequence run will be created inside it."),
                                   "</span></strong>", sep = "")
      }
      
      #WRF path message
      if (as.character(unlist(input$wrfdir)[1])==0) {
        fourth_message_bash = paste("<strong><span style=\"color:darkred\">Please choose a path for WRF directory!</span></strong>")
        # third_message = "Please choose a path for WRF directory!"
      } else {
        dirpath_wrf = dirpath_wrf()
        namelistinput_dir = paste0(dirpath_wrf,"/namelists_input_seqs_",sekans_dates_1,"_",sekans_dates_2)
        fourth_message_bash = paste("<strong><span style=\"color:darkgreen\">",paste0("Path for the WRF model is chosen as: ",
                                                                                      dirpath_wrf(),". ",
                                                                                      namelistinput_dir,
                                                                                      " directory will be created in WRF/run and all the namelist.input files for sequence run will be created inside it."),
                                    "</span></strong>", sep = "")
      }
      
      #MPI message
      if (mpisupport() == "Yes") {
        opt_cpu = round(as.numeric(system("nproc",intern = T))*3/4)
        max_cpu = as.numeric(system("nproc --all", intern = T))
        cpu_n = cpu_n()
        fifth_message_bash = paste0("<strong><span style=\"color:darkgreen\">The machine has, ",max_cpu," CPU. And the optimum available CPU for model run is ",opt_cpu,", and you have chosen ",cpu_n," processors.</span></strong>")
      } else {
        fifth_message_bash = paste("<strong><span style=\"color:darkgreen\">WRF is not installed with MPI support. WRF will be run with only one processor.</span></strong>", sep = "")
      }
      
      if (is.null(input$wrf_namelist)) {
        sixth_message_bash = paste0("<strong><span style=\"color:darkred\">Please upload namelist.wps and be sure to press Make changes button!</span></strong>")
      } else {
        sixth_message_bash = paste0("<strong><span style=\"color:darkgreen\">You have uploaded to namelist.wps, be sure to press Make changes button!</span></strong>")
      }
      
      if (is.null(input$wrf_namelist)) {
        seventh_message_bash = paste0("<strong><span style=\"color:darkred\">Please upload namelist.input and be sure to press Make changes button!</span></strong>")
      } else {
        seventh_message_bash = paste0("<strong><span style=\"color:darkgreen\">You have uploaded to namelist.input, be sure to press Make changes button!</span></strong>")
      }
      
      HTML(paste(first_message_bash, second_message_bash, third_message_bash, fourth_message_bash, fifth_message_bash, sixth_message_bash, seventh_message_bash, sep="<br/>"))
      # HTML(paste(first_message_bash, sep="<br/>"))
      
      # validate(
      #   need(hist_dom(), "Your selected domains exceeds max domain number! Please check the desired domains or maximum domain numbers!")
      # )
      
    })
  })
  
  #Bash2 for the bash script with date and time sequence with spin-up
  bash2 = eventReactive(input$bash2_preparer, {
    
    daterange1 = ymd_hms(date_time_start_bash())
    daterange2 = ymd_hms(date_time_end_bash())
    
    sekans = as.character(sequence_bash())
    sayi_sekans = as.numeric(seq_num_bash())
    
    ilerle = ifelse(sekans == "Day", sayi_sekans*24*3600, 
                    ifelse(sekans == "Week", paste(sayi_sekans, "week", sep = " "),
                           ifelse(sekans == "Month", paste(sayi_sekans,"month", sep = " "))))

    datatype = datatype()
    timeres = timeres()
    
    spinup_sayi = as.numeric(spinup_num())
    spinup = spinup_sayi*3600
    
    sekans_times = seq(from = daterange1, to = daterange2, by = ilerle)
    
    #If the last values of the sequence is equal with daterange2; then the last value of the sequence does not need to be added!
    if (sekans_times[length(sekans_times)] == daterange2) {
      sekans_times = sekans_times  
    } else {
      sekans_times = append(sekans_times, daterange2)
    }
    
    sekans_times_spinup = sekans_times-spinup
    
    #Namelist dates
    if (datatype == "FNL") {
      sekans_times_spinup = floor_date(sekans_times_spinup,unit = "6 hours")
    } else {
      sekans_times_spinup = sekans_times_spinup
    }
    
    #Date for the data!
    sekans_times_spinup_data = floor_date(sekans_times_spinup,unit = "1 day")
    sekans_times_data = ceiling_date(sekans_times,unit = "1 day")
    #Date for the data!
    datadates = c(sekans_times_spinup_data[1],sekans_times_data[length(sekans_times_data)])
    
    sekans_times_spinup_final=list()
    
    #Date for namelist
    for (i in 1:(length(sekans_times)-1)) {
      sekans_times_spinup_final[[i]] = c(sekans_times_spinup[i],sekans_times[i+1])
    }
    
    wps_predefined_lists = list()
    
    wps_predefined = wps_predefined_updated()
    
    for (i in 1:length(sekans_times_spinup_final)) {
      max_dom = max_dom()
      # max_dom = 2
      
      #WPS start_date indexes
      wps_s_index = str_detect(wps_predefined,"start_date")
      #WPS remove and replace the dates with the specified ones after equal sign
      wps_s_fes = str_locate(wps_predefined[wps_s_index],"=")[1]
      date_time_start_wps = strftime(sekans_times_spinup_final[[i]][1], "%Y-%m-%d_%H:%M:%S")
      date_time_start_wps = rep(date_time_start_wps, max_dom)
      date_time_start_wps = paste("'",paste(date_time_start_wps,collapse = "','"),"'",sep = "")
      
      wps_predefined[wps_s_index] = paste(substr(wps_predefined[wps_s_index],1,wps_s_fes),date_time_start_wps)
      
      #WPS end_date indexes
      wps_e_index = str_detect(wps_predefined,"end_date")
      wps_e_fes = str_locate(wps_predefined[wps_e_index],"=")[1]
      date_time_end_wps = strftime(sekans_times_spinup_final[[i]][2], "%Y-%m-%d_%H:%M:%S")
      date_time_end_wps = rep(date_time_end_wps, max_dom)
      date_time_end_wps =  paste("'",paste(date_time_end_wps,collapse = "','"),"'",sep = "")
      date_time_end_wps
      
      wps_predefined[wps_e_index] = paste(substr(wps_predefined[wps_e_index],1,wps_e_fes),date_time_end_wps)
      wps_predefined
      wps_predefined_lists[[i]] = wps_predefined
      
    }
    
    wrf_predefined_lists = list()

    for (i in 1:length(sekans_times_spinup_final)) {
      
      wrf_predefined = wrf_predefined_updated()
      max_dom_wrf = max_dom_wrf()

      #WRF date indexes, start_year, month, day, hour, end_year, month, day, hour
      date_time_start_wrf = sekans_times_spinup_final[[i]][1]
      start_year = paste(paste(rep(year(date_time_start_wrf),max_dom_wrf),collapse = ","),",",sep = "")
      
      start_month = month(date_time_start_wrf)
      start_month = ifelse(start_month<10, paste(0,start_month,sep = ""),start_month)
      start_month = paste(paste(rep(start_month,max_dom_wrf),collapse = ","),",",sep = "")
      
      start_day = day(date_time_start_wrf)
      start_day = ifelse(start_day<10, paste(0,start_day,sep = ""),start_day)
      start_day = paste(paste(rep(start_day,max_dom_wrf),collapse = ","),",",sep = "")
      
      start_hour = hour(date_time_start_wrf)
      start_hour = ifelse(start_hour<10, paste(0,start_hour,sep = ""),start_hour)
      start_hour = paste(paste(rep(start_hour,max_dom_wrf),collapse = ","),",",sep = "")
      
      wrf_predefined[str_detect(wrf_predefined,"start_year")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_year")],1,
                                                                             str_locate(wrf_predefined[str_detect(wrf_predefined,"start_year")],"=")[1]),
                                                                      start_year)
      
      wrf_predefined[str_detect(wrf_predefined,"start_month")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_month")],1,
                                                                              str_locate(wrf_predefined[str_detect(wrf_predefined,"start_month")],"=")[1]),
                                                                       start_month)
      
      wrf_predefined[str_detect(wrf_predefined,"start_day")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_day")],1,
                                                                            str_locate(wrf_predefined[str_detect(wrf_predefined,"start_day")],"=")[1]),
                                                                     start_day)
      
      wrf_predefined[str_detect(wrf_predefined,"start_hour")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"start_hour")],1,
                                                                             str_locate(wrf_predefined[str_detect(wrf_predefined,"start_hour")],"=")[1]),
                                                                      start_hour)
      
      #namelist.input ends!
      date_time_end_wrf = sekans_times_spinup_final[[i]][2]
      
      end_year = paste(paste(rep(year(date_time_end_wrf),max_dom_wrf),collapse = ","),",",sep = "")
      
      end_month = month(date_time_end_wrf)
      end_month = ifelse(end_month<10, paste(0,end_month,sep = ""),end_month)
      end_month = paste(paste(rep(end_month,max_dom_wrf),collapse = ","),",",sep = "")
      
      end_day = day(date_time_end_wrf)
      end_day = ifelse(end_day<10, paste(0,end_day,sep = ""),end_day)
      end_day = paste(paste(rep(end_day,max_dom_wrf),collapse = ","),",",sep = "")
      
      end_hour = hour(date_time_end_wrf)
      end_hour = ifelse(end_hour<10, paste(0,end_hour,sep = ""),end_hour)
      end_hour = paste(paste(rep(end_hour,max_dom_wrf),collapse = ","),",",sep = "")
      
      wrf_predefined[str_detect(wrf_predefined,"end_year")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_year")],1,
                                                                           str_locate(wrf_predefined[str_detect(wrf_predefined,"end_year")],"=")[1]),
                                                                    end_year)
      
      wrf_predefined[str_detect(wrf_predefined,"end_month")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_month")],1,
                                                                            str_locate(wrf_predefined[str_detect(wrf_predefined,"end_month")],"=")[1]),
                                                                     end_month)
      
      wrf_predefined[str_detect(wrf_predefined,"end_day")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_day")],1,
                                                                          str_locate(wrf_predefined[str_detect(wrf_predefined,"end_day")],"=")[1]),
                                                                   end_day)
      
      wrf_predefined[str_detect(wrf_predefined,"end_hour")] = paste(substr(wrf_predefined[str_detect(wrf_predefined,"end_hour")],1,
                                                                           str_locate(wrf_predefined[str_detect(wrf_predefined,"end_hour")],"=")[1]),
                                                                    end_hour)
      
      
      wrf_predefined_lists[[i]] = wrf_predefined
      
    }
    
    #DATA DOWNLOAD PART
    datadownload = datadownload()

    if (datadownload == "Yes") {
      
      datatype = datatype()
      dirpath_data = dirpath_data()
      
      if (datatype == "FNL") {
        
        # only 1999-07-30 until present available data
        # rda.ucar.edu account details:
        email = email
        pass = pass
        
        Sys.setenv(TZ="GMT")
        data_i = datadates[1]
        data_f = datadates[2]
        
        dir.create(paste(dirpath_data,"/FNL_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
        
        setwd(paste(dirpath_data,"/FNL_",data_i,"_to_",data_f, sep = ""))
        fnl_dirpath2 = paste0("cd ",dirpath_data,"/FNL_",data_i,"_to_",data_f)
        
        fnl_authentication = paste0("wget -O Authentication.log --save-cookies auth.rda_ucar_edu  --post-data 'email=", email, "&passwd=", pass, "&action=login' https://rda.ucar.edu/cgi-bin/login")
        
        seq_dates = seq(as.POSIXct(data_i), as.POSIXct(data_f), by = "6 hour")
        list_dates = format(seq_dates, "%Y%m%d_%H_00")
        file_name = paste0("fnl_", list_dates, ".grib2") #as.list(paste(my_url, file_name, sep = ""))
        
        if (data_i <= "2007-12-06" & data_f <= "2007-12-06") {
          
          down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib1/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib1")
          
        } else if (data_i >= "2007-12-06" & data_f >= "2007-12-06") {
          
          down_link = paste0("http://rda.ucar.edu/data/ds083.2/grib2/", format(seq_dates, "%Y"), "/", format(seq_dates, "%Y.%m"),"/fnl_", list_dates, ".grib2")
          
        }
        
        fnl_link = c()
        
        for (i in 1:length(file_name)){
          
          if (file.exists(file_name[i])) {
            
            cat(paste0(file_name[i], " - already downloaded. \n"))
            
          } else {
            
            # sink("fnl.log")
            a = paste0("wget -N --load-cookies auth.rda_ucar_edu ", down_link[i])
            fnl_link = append(fnl_link,a)
            # sink()
            
          }
        }
        
        fnl_link = paste(fnl_link,collapse = "\n")
        fnl_remove_auth = paste0("rm -rf Authentication.log auth.rda_ucar_edu")
        shouldbe_fnl = paste0("fnl_",list_dates[length(list_dates)], ".grib2")
        fnl_kontrol = paste0("if test -f '",shouldbe_fnl,"'; then echo 'FNL data sucessfuly downloaded!'; else echo 'FNL data is not downloaded!'; exit; fi")
        
        fnl_download_final = paste(fnl_dirpath2,fnl_authentication,fnl_link,fnl_remove_auth,fnl_kontrol, sep = "\n")
        
      } else 
      {
        
        #Surface data for ERA5
        # time sequence
        data_i = datadates[1]
        data_f = datadates[2]
        area = area()
        
        dir.create(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""), showWarnings = F)
        setwd(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""))
        era5_dirpath2 = paste0("cd ",dirpath_data,"/ERA5_",data_i,"_to_",data_f)
        
        list_dates = seq.Date(as.Date(data_i), as.Date(data_f), by = "day")
        list_dates = format(list_dates, "%Y%m%d")
        
        era5_surf_commands = c()
        
        for (i in 1:length(list_dates)) {
          
          if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".grib"))) {
            
            cat(paste0("ERA5_sfc_", list_dates[i], ".grib"), " - already downloaded. \n")
            
          } else if (file.exists(paste0("ERA5_sfc_", list_dates[i], ".nc"))) {
            
            cat(paste0("ERA5_sfc_", list_dates[i], ".nc"), " - already downloaded. \n")
            
          } else {
            
            txt = c("#!/usr/bin/env python", 
                    "import cdsapi",
                    "",
                    "c = cdsapi.Client()",
                    "",
                    "c.retrieve(",
                    "'reanalysis-era5-single-levels',",
                    "{",
                    "'product_type':'reanalysis',",
                    "'format':'grib',",
                    "'variable':[",
                    "'10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',",
                    "'2m_temperature','land_sea_mask','mean_sea_level_pressure',",
                    "'sea_ice_cover','sea_surface_temperature','skin_temperature',",
                    "'snow_depth','soil_temperature_level_1','soil_temperature_level_2',",
                    "'soil_temperature_level_3','soil_temperature_level_4','surface_pressure',",
                    "'volumetric_soil_water_layer_1','volumetric_soil_water_layer_2','volumetric_soil_water_layer_3',",
                    "'volumetric_soil_water_layer_4'",
                    "],",
                    paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
                    paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
                    paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
                    "'time': [",
                    "'00:00', '01:00', '02:00',",
                    "'03:00', '04:00', '05:00',",
                    "'06:00', '07:00', '08:00',",
                    "'09:00', '10:00', '11:00',",
                    "'12:00', '13:00', '14:00',",
                    "'15:00', '16:00', '17:00',",
                    "'18:00', '19:00', '20:00',",
                    "'21:00', '22:00', '23:00',",
                    "],",
                    "'area': [",
                    paste0(area[1],",",area[2],",",area[3],",",area[4]),
                    "],",
                    "},",
                    paste0("'ERA5_sfc_", list_dates[i], ".grib')")
            )
            
            writeLines(txt, paste0("ECMWF_ERA5_sfc_", list_dates[i], ".py"))
            
          }
          
          files_python = list.files(pattern = "*.py")
          files_python = files_python[length(files_python)]
          a = paste0("python ", files_python)
          era5_surf_commands = append(era5_surf_commands,a)
          
        }
        
        era5_surf_commands = paste(era5_surf_commands,collapse = "\n")
        
        shouldbe_era5_surf = paste0("'ERA5_sfc_", list_dates[length(list_dates)],".grib'")
        era5_surf_kontrol = paste0("if test -f ",shouldbe_era5_surf,"; then echo 'ERA5 surface data sucessfuly downloaded!'; else echo 'ERA5 surface data is not downloaded!'; exit; fi")
        era5_surf_download_final = paste(era5_dirpath2,era5_surf_commands,era5_surf_kontrol, sep = "\n")
        
        # Pressure level for ERA5
        # time sequence
        setwd(paste(dirpath_data,"/ERA5_",data_i,"_to_",data_f, sep = ""))
        era5_pl_commands = c()
        
        for (i in 1:length(list_dates)) {
          
          if (file.exists(paste0("ERA5_pl_", list_dates[i], ".grib"))) {
            
            cat(paste0("ERA5_pl_", list_dates[i], ".grib"), " - already downloaded. \n")
            
          } else if (file.exists(paste0("ERA5_pl_", list_dates[i], ".nc"))) {
            
            cat(paste0("ERA5_pl_", list_dates[i], ".nc"), " - already downloaded. \n")
            
          } else {
            
            txt = c("#!/usr/bin/env python", 
                    "import cdsapi",
                    "",
                    "c = cdsapi.Client()",
                    "",
                    "c.retrieve(",
                    "'reanalysis-era5-pressure-levels',",
                    "{",
                    "'product_type':'reanalysis',",
                    "'format':'grib',",
                    "'variable':[",
                    "'geopotential', 'relative_humidity', 'specific_humidity',",
                    "'temperature', 'u_component_of_wind', 'v_component_of_wind',",
                    "],",
                    "'pressure_level': [",
                    "'1', '2', '3',",
                    "'5', '7', '10',",
                    "'20', '30', '50',",
                    "'70', '100', '125',",
                    "'150', '175', '200',",
                    "'225', '250', '300',",
                    "'350', '400', '450',",
                    "'500', '550', '600',",
                    "'650', '700', '750',",
                    "'775', '800', '825',",
                    "'850', '875', '900',",
                    "'925', '950', '975',",
                    "'1000',",
                    "],",
                    paste0("'day'    : ['", ifelse(day(ymd(list_dates[i])) %in% 1:9, paste0(0,day(ymd(list_dates[i]))),day(ymd(list_dates[i]))),"'],"),
                    paste0("'month'    : ['", ifelse(month(ymd(list_dates[i])) %in% 1:9, paste0(0,month(ymd(list_dates[i]))),month(ymd(list_dates[i]))),"'],"),
                    paste0("'year'    : ['", year(ymd(list_dates[i])),"'],"),
                    "'time': [",
                    "'00:00', '01:00', '02:00',",
                    "'03:00', '04:00', '05:00',",
                    "'06:00', '07:00', '08:00',",
                    "'09:00', '10:00', '11:00',",
                    "'12:00', '13:00', '14:00',",
                    "'15:00', '16:00', '17:00',",
                    "'18:00', '19:00', '20:00',",
                    "'21:00', '22:00', '23:00',",
                    "],",
                    "'area': [",
                    paste0(area[1],",",area[2],",",area[3],",",area[4]),
                    "],",
                    "},",
                    paste0("'ERA5_pl_", list_dates[i], ".grib')")
            )
            
            writeLines(txt, paste0("ECMWF_ERA5_pl_",list_dates[i],".py"))
            
            
          }
          
          files_python = list.files(pattern = "ECMWF_ERA5_pl*")
          files_python = files_python[length(files_python)]
          a = paste0("python ", files_python)
          era5_pl_commands = append(era5_pl_commands,a)
          
        }
        
        era5_pl_commands = paste(era5_pl_commands,collapse = "\n")
        era5_remove_python = paste0("rm -rf *.py")
        
        shouldbe_era5_pl = paste0("'ERA5_pl_", list_dates[length(list_dates)],".grib'")
        era5_pl_kontrol = paste0("if test -f ",shouldbe_era5_pl,"; then echo 'ERA5 pressure level data sucessfuly downloaded!'; else echo 'ERA5 pressure level data is not downloaded!'; exit; fi")
        era5_pl_download_final = paste(era5_dirpath2,era5_pl_commands,era5_remove_python,era5_pl_kontrol, sep = "\n")
        era5_download_final = paste(era5_surf_download_final,era5_pl_download_final, sep = "\n")
        
        
      }
    } else {
      
      download_no = paste0("echo 'This script does not download any data. Data path should be provided on Data or WPS tab'")  
      
    }
    
    # Create NAMELIST.WPS ve NAMELIST.INPUT files------------------------------------------------------------
    # Save the namelist.wps ve namelist.input files inside WPS and WRF directories respectively
    dirpath_wps = dirpath_wps()
    
    sekans_dates_1 = strftime(sekans_times_spinup_final[[1]][1],"%Y%m%d_%H%M")
    sekans_dates_2 = strftime(sekans_times_spinup_final[[length(sekans_times_spinup_final)]][2], "%Y%m%d_%H%M")
    dir.create(paste0(dirpath_wps,"/namelists_wps_seqs_",sekans_dates_1,"_",sekans_dates_2))
    
    for (i in 1:length(wps_predefined_lists)) {
      
      writeLines(wps_predefined_lists[[i]], paste0(dirpath_wps,
                                                   paste0("/namelists_wps_seqs_",sekans_dates_1,"_",sekans_dates_2),
                                                   "/namelists.wps_",strftime(sekans_times_spinup_final[[i]][1],"%Y%m%d_%H%M"),"_",strftime(sekans_times_spinup_final[[i]][2], "%Y%m%d_%H%M")))
    }
    
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run")

    dir.create(paste0(dirpath_wrf,"/namelists_input_seqs_",sekans_dates_1,"_",sekans_dates_2))
    
    for (i in 1:length(wrf_predefined_lists)) {
      
      writeLines(wrf_predefined_lists[[i]], paste0(dirpath_wrf,
                                                   paste0("/namelists_input_seqs_",sekans_dates_1,"_",sekans_dates_2),
                                                   "/namelists.input_",strftime(sekans_times_spinup_final[[i]][1],"%Y%m%d_%H%M"),"_",strftime(sekans_times_spinup_final[[i]][2], "%Y%m%d_%H%M")))
    }
    
    #all namelist.wps files in bash script
    namelist_wpss = list.files(paste0(dirpath_wps, paste0("/namelists_wps_seqs_",sekans_dates_1,"_",sekans_dates_2)))
    namelist_wpss_copy = paste0("cp ",paste0(dirpath_wps,paste0("/namelists_wps_seqs_",sekans_dates_1,"_",sekans_dates_2,"/")), 
                                paste0(namelist_wpss),
                                " ",dirpath_wps,"/namelist.wps")
    
    #all namelist.input files in bash script
    namelist_inputs = list.files(paste0(dirpath_wrf, paste0("/namelists_input_seqs_",sekans_dates_1,"_",sekans_dates_2)))
    namelist_inputs_copy = paste0("cp ",paste0(dirpath_wrf,paste0("/namelists_input_seqs_",sekans_dates_1,"_",sekans_dates_2,"/")), 
                                  paste0(namelist_inputs),
                                  " ",dirpath_wrf,"/namelist.input")
    
    #WPS path
    dirpath_wps = dirpath_wps()
    
    #Geogrid control
    max_dom = max_dom()
    shouldbe_geo = paste("geo_em.d0",1:max_dom,".nc",sep = "")
    shouldbe_geo = shouldbe_geo[length(shouldbe_geo)]
    geogrid_kontrol = paste0("if test -f '",shouldbe_geo,"'; then echo 'Geogrid.exe completed!'; else echo 'Geogrid.exe is not completed!'; exit; fi")
    
    datatype = datatype()
    #Linkgrib data inside WPS
    if (as.character(unlist(input$datadir)[1])!= 0 & as.character(unlist(input$datadir2)[1])!= 0){
      dirpath_data2 = dirpath_data2()
      datalink = paste("./link_grib.csh ", dirpath_data2,"/*", sep = "")
    } else if (as.character(unlist(input$datadir2)[1])!= 0) {
      dirpath_data2 = dirpath_data2()
      datalink = paste("./link_grib.csh ", dirpath_data2,"/*", sep = "")
    } else {
      dirpath_data = dirpath_data()
      datalink = paste("./link_grib.csh ", dirpath_data,"/",datatype,"_",datadates[1],"_to_",datadates[2],"/*", sep = "")
    }
    
    #Vtable!
    if (datatype == "FNL") {
      vtable = "ln -sf ungrib/Variable_Tables/Vtable.GFS Vtable"
    } else {
      vtable = "ln -sf ungrib/Variable_Tables/Vtable.ERA-interim.pl Vtable"
    }
    
    #Ungrib control!
    ungrib_kontrol_lists = list()
    for (i in 1:length(sekans_times_spinup_final)) {
      date_time_start_wps = sekans_times_spinup_final[[i]][1]
      date_time_end_wps = sekans_times_spinup_final[[i]][2]
      shouldbe_ung = paste0("FILE:",strftime(date_time_end_wps, "%Y-%m-%d_%H"))
      ungrib_kontrol = paste0("if test -f '",shouldbe_ung,"'; then echo 'Ungrib.exe completed!'; else echo 'Ungrib.exe is not completed!'; exit; fi")
      ungrib_kontrol_lists[[i]] = ungrib_kontrol
    }
    
    #Metgrid control!
    metgrid_kontrol_lists = list()
    for (i in 1:length(sekans_times_spinup_final)) {
      max_dom = as.numeric(max_dom())
      date_time_start_wps = sekans_times_spinup_final[[i]][1]
      date_time_end_wps = sekans_times_spinup_final[[i]][2]
      shouldbe_met = paste0("met_em.d0",max_dom,".",strftime(date_time_end_wps, "%Y-%m-%d_%H:%M:%S"))
      metgrid_kontrol = paste0("if test -f '",shouldbe_met,"'; then echo 'Metgrid.exe completed!'; else echo 'Metgrid.exe is not completed!'; exit; fi")
      metgrid_kontrol_lists[[i]] = metgrid_kontrol  
    }
    
    #Real.exe
    dirpath_wrf = dirpath_wrf()
    dirpath_wrf = paste0(dirpath_wrf,"/run")
    
    #Real.exe control!
    max_dom_wrf = max_dom_wrf()
    shouldbe_real = paste("wrfinput_d0",1:max_dom_wrf,sep = "")
    shouldbe_real = shouldbe_real[length(shouldbe_real)]
    real_kontrol = paste0("if test -f '",shouldbe_real,"'; then echo 'Real.exe completed!'; else echo 'Real.exe is not completed!'; exit; fi")
    
    #Wrf.exe
    cpu_n = cpu_n()
    
    #Wrf.exe control!
    wrf_kontrol_lists = list()
    
    for (i in 1:length(sekans_times_spinup_final)) {
      
      max_dom_wrf = as.numeric(max_dom_wrf())
      date_time_start_wrf = sekans_times_spinup_final[[i]][1]
      date_time_end_wrf = sekans_times_spinup_final[[i]][2]
      shouldbe_wrf = paste0("wrfout_d0",max_dom,"_",strftime(date_time_end_wrf, "%Y-%m-%d_%H:%M:%S"))
      wrf_kontrol = paste0("if test -f '",shouldbe_wrf,"'; then echo 'Wrf.exe completed!'; else echo 'Wrf.exe is not completed!'; exit; fi")
      wrf_kontrol_lists[[i]] = wrf_kontrol
    }
    
    #Directory for WRF outputs
    wrf_out_dir = paste0("outputs_wrf_",
                         sekans_dates_1,"_",sekans_dates_2)
    
    bash_1 = paste("#!/bin/bash",
                   ifelse(datadownload == "Yes", ifelse(datatype == "FNL",fnl_download_final,era5_download_final),download_no),
                   "#Change directory to the WPS",
                   paste0("cd ",dirpath_wps),
                   "#Clean the directory",
                   paste0("rm geo_em*"),
                   paste0("rm FILE:*"),
                   paste0("rm met_em*"),
                   "#Copy namelist.wps",
                   namelist_wpss_copy[[1]],
                   "#Geogrid.exe",
                   paste0("./geogrid.exe"),
                   "#Geogrid check!",
                   geogrid_kontrol,
                   "#Link the data!",
                   datalink,
                   "#Link the related Vtable",
                   vtable,
                   "#Ungrib.exe",
                   paste0("./ungrib.exe"),
                   "#Ungrib check!",
                   ungrib_kontrol_lists[[1]],
                   "#Metgrid.exe",
                   paste0("./metgrid.exe"),
                   "#Metgrid control",
                   metgrid_kontrol_lists[[1]],
                   "#WRF",
                   paste0("cd ",dirpath_wrf),
                   "#Clean the files",
                   paste0("rm wrfinput*"),
                   paste0("rm wrfout*"),
                   "Copy namelist.input",
                   namelist_inputs_copy[[1]],
                   "#Met_em files are being linked!",
                   paste0("ln -sf ",dirpath_wps,"/met_em* ."),
                   "#Real.exe",
                   paste0("./real.exe"),
                   "#Real.exe check!",
                   real_kontrol,
                   "#Wrf.exe",
                   paste0("mpirun -np ",cpu_n," ./wrf.exe "),
                   "#Wrf.exe check!",
                   wrf_kontrol_lists[[1]],
                   "#Make a directory and move the wrf outputs inside it",
                   paste0("mkdir ",wrf_out_dir),
                   paste0("mv wrfout* ",wrf_out_dir),
                   sep = "\n")
    
    bash_2 = paste("#Change directory to the WPS",
                   paste0("cd ",dirpath_wps),
                   "#Clean the directory",
                   paste0("rm FILE:*"),
                   paste0("rm met_em*"),
                   "#Copy namelist.wps",
                   namelist_wpss_copy[-1],
                   "#Ungrib.exe",
                   paste0("./ungrib.exe"),
                   "#Ungrib check!",
                   ungrib_kontrol_lists[-1],
                   "#Metgrid.exe",
                   paste0("./metgrid.exe"),
                   "#Metgrid control",
                   metgrid_kontrol_lists[-1],
                   "#WRF",
                   paste0("cd ",dirpath_wrf),
                   "#Clean the files",
                   paste0("rm wrfinput*"),
                   paste0("rm wrfout*"),
                   "#Copy namelist.input",
                   namelist_inputs_copy[-1],
                   "#Met_em files are being linked!",
                   paste0("ln -sf ",dirpath_wps,"/met_em* ."),
                   "#Real.exe",
                   paste0("./real.exe"),
                   "#Real.exe check!",
                   real_kontrol,
                   "#Wrf.exe",
                   paste0("mpirun -np ",cpu_n," ./wrf.exe "),
                   "#Wrf.exe check!",
                   wrf_kontrol_lists[-1],
                   "#Make a directory and move the wrf outputs inside it",
                   paste0("mv wrfout* ",wrf_out_dir),
                   sep = "\n")
    
    bash_2 = paste(bash_2, collapse = "\n")
    
    bash_toplam = paste(bash_1, bash_2, sep = "\n")
    
  })
  
  #Bash2
  observeEvent(input$bash2_button, {
    bash2 = bash2()
    bash2
    
    output$bash_text = renderText(bash2)
  })
  
  #Bash script download
  observe({
    
    bash2 = bash2()
    daterange1 = ymd_hms(date_time_start_bash())
    daterange2 = ymd_hms(date_time_end_bash())
    
    download_script2 = function(data) {
      downloadHandler(
        filename = function() {
          paste("bash_sequence_wrf_",daterange1,"_",daterange2,".bash", sep = "")
        },
        content = function(file) {
          write(data, file, sep = "")
        }
      )
    }
    output$download_bash2 = download_script2(bash2)
  })
  
}

shinyApp(ui = ui, server = server)