# Make Shiny plots for analysis


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Functions

wy_to_y = function(wy, wyd){
  y = ifelse(wyd <= 92, wy - 1, wy)
  return(y)
}

wyd_to_yd = function(wyd, y){
  if (lubridate::leap_year(y) == TRUE){
    yd <- ifelse(wyd<=92, wyd+274, wyd-92)  
  } else {
    yd <- ifelse(wyd<=92, wyd+273, wyd-92)    
  }
  return(yd)
}

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

data_daily <- read_csv("output/data_daily.csv",
                       col_types = list(wy = col_character(),
                                        run = col_factor( levels = c("p301_100", "p301_80", "p301_50",
                                                                     "p303_100", "p303_80", "p303_50",
                                                                     "p304_100", "p304_80", "p304_50")),
                                        season = col_factor(levels = c("Jan-Mar",
                                                                       "Apr-Jun",
                                                                       "Jul-Sep",
                                                                       "Oct-Dec")),
                                        biomass = col_factor(),
                                        watershed = col_factor())) %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1)


diff_flux_daily <- read_rds("output/diff_flux_daily.rds") %>% 
  dplyr::bind_rows(.id="variable") %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1) %>% 
  tidyr::pivot_longer(c(absolute_50, absolute_80), names_to = "change_level", values_to = "absolute_change") %>% 
  dplyr::mutate(run = paste(watershed, "_", change_level, sep="")) %>% 
  dplyr::select(-c(wy, WYD, year, yd, relative_80, relative_50, `50`, `80`))
diff_flux_daily$run <- factor(diff_flux_daily$run, levels = c("p301_absolute_80", "p301_absolute_50",
                                                              "p303_absolute_80", "p303_absolute_50",
                                                              "p304_absolute_80", "p304_absolute_50"))


diff_storage_daily <- read_rds("output/diff_storage_daily.rds") %>% 
  dplyr::bind_rows(.id="variable") %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1) %>% 
  tidyr::pivot_longer(c(absolute_50, absolute_80), names_to = "change_level", values_to = "absolute_change") %>% 
  dplyr::mutate(run = paste(watershed, "_", change_level, sep="")) %>% 
  dplyr::select(-c(wy, WYD, year, yd, relative_80, relative_50, `50`, `80`))
diff_storage_daily$run <- factor(diff_storage_daily$run, levels = c("p301_absolute_80", "p301_absolute_50",
                                                                    "p303_absolute_80", "p303_absolute_50",
                                                                    "p304_absolute_80", "p304_absolute_50"))



# ------------------
# ------------------
# ------------------
# Create Shinyness


ui <- fluidPage(
  headerPanel('RHESSys Daily Output'),
  sidebarPanel(
    selectInput(
      inputId = "variable_type",
      label = "Which type of variable do you want to plot",
      choices = c("Standard Variable" = "standard",
                  "Flux Change Variable" = "flux", 
                  "Storage Change Variable" = "storage")
    ),
    sliderInput(
      inputId = "date_range", 
      label = "Choose Date Range:", 
      value = c(diff_flux_daily$date[1], max = diff_flux_daily$date[length(diff_flux_daily$date)]),
      min = diff_flux_daily$date[1], 
      max = diff_flux_daily$date[length(diff_flux_daily$date)]
    ),
    radioButtons(
      inputId = "variable_standard",
      label = "Select a standard variable.", 
      c("Temperature" = 1, 
        "Snow" = 2,
        "Evaporation" = 3,
        "Streamflow" = 4)
    ),
    radioButtons(
      inputId = "variable_flux",
      label = "Select a flux change variable.", 
      c("Streamflow Change" = 1,
        "Transpiration Change" = 2,
        "Evaporation Change" = 3,
        "PET Change" = 4,
        "Snow Sublimation Change" = 5,
        "Deleted Snowpack Change" = 6,
        "Baseflow Change" = 7,
        "ReturnFlow Change" = 8,
        "GW_out Change" = 9,
        "RZ_drain Change" = 10,
        "Unsat_drain Change" = 11,
        "CapRise Change" = 12,
        "R_Tfall Change" = 13,
        "S_Tfall Change" = 14,
        "Photosyn Change" = 15)
    ),
    radioButtons(
      inputId = "variable_storage",
      label = "Select a storage change variable.", 
      c("Snowpack Change" = 1,
        "LitterSto Change" = 2,
        "CanopySto Change" = 3,
        "RZ_sto Change" = 4,
        "Unsat_Sto Change" = 5,
        "SatDef_dep Change" = 6,
        "SatDef_Vol Change" = 7,
        "GW_sto Change" = 8,
        "Det_sto Change" = 9,
        "SatArea Change" = 10,
        "SCA Change" = 11,
        "LAI Change" = 12)
    )
  ),
  mainPanel(
    plotOutput('plot_main', height="1500px", width="auto")
  )
)

server <- function(input, output) {
  
  output$plot_main <- renderPlot({
    
    # ---------------------------
    if (input$variable_type == "standard"){
     
      run_id <- c(
        "p301_50" = "P301: 50% Treatment", "p301_80" = "P301: 20% Treatment", "p301_100" = "P301: Pre-Treatment",
        "p303_50" = "P303: 50% Treatment", "p303_80" = "P303: 20% Treatment", "p303_100" = "P303: Pre-Treatment",
        "p304_50" = "P304: 50% Treatment", "p304_80" = "P304: 20% Treatment", "p304_100" = "P304: Pre-Treatment"
      )
      
      # ------------------------
      if (input$variable_standard == 1){
        # Precip fluxes
        
        data_daily_precip <- data_daily %>% 
          dplyr::select(run, date, Tmax, Tmin)
        
        x <- data_daily_precip %>% 
          ggplot(.) +
          geom_line(aes(x=date,y=Tmax), color="red") +
          geom_line(aes(x=date,y=Tmin), color="blue") +
          geom_hline(yintercept = 0, color="black") +
          labs(title="Temperature",
               x="Date",
               y="Temp, C") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
      }
      
      # ------------------------
      if (input$variable_standard == 2){
        # Snow fluxes
        
        data_daily_snow <- data_daily %>% 
          dplyr::select(run, date, Snowfall, Snowpack)
        
        ylim.prim <- range(data_daily_snow$Snowfall)   # Smaller value
        ylim.sec <-  range(data_daily_snow$Snowpack)    # Larger value
        
        b <- diff(ylim.prim)/diff(ylim.sec)
        a <- b*(ylim.prim[1] - ylim.sec[1])
        
        x <- data_daily_snow %>% 
          ggplot(.) +
          geom_col(aes(x=date,y=Snowfall), color="gray20") +
          geom_line(aes(x=date,y=a+b*Snowpack), color="blue") +
          geom_hline(yintercept = 0, color="black") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          labs(title = "Snow Fluxes and Storage",
               x = "Date",
               y = "Flux (mm/day)") +
          scale_color_manual(values=c("black", "red", "blue")) +
          scale_y_continuous(name="Snowfall (mm)", sec.axis = sec_axis(~ (. - a)/b, name = "Snowpack (mm)")) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
      }
      
      # ------------------------
      if (input$variable_standard == 3){
        # ET fluxes
        
        data_daily_et <- data_daily %>% 
          dplyr::select(run, date, PET, Evap, `Trans+evap`, `Trans+evap+subl`) %>% 
          tidyr::pivot_longer(cols = -c(run, date, PET), names_to = "flux_type", values_to = "flux") %>% 
          dplyr::mutate(PET2 = PET/3)
        
        x <- data_daily_et %>% 
          ggplot(.) +
          geom_col(aes(x=date,y=PET2), color="gray90") +  
          geom_line(aes(x=date,y=flux, color=flux_type), size=1.5) +
          #geom_hline(yintercept = 0, color="black") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          labs(title = "Evaporative Fluxes and Demand",
               x = "Date",
               y = "Flux (mm/day)") +
          scale_color_manual(values=c("black", "red", "blue")) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
      }
      
      # ------------------------
      if (input$variable_standard == 4){
        # Streamflow fluxes
        
        data_daily_q <- data_daily %>% 
          dplyr::select(run, date, `Cum stream`, Streamflow)
        
        ylim.prim <- range(data_daily_q$Streamflow)   # Smaller value
        ylim.sec <-  range(data_daily_q$`Cum stream`)    # Larger value
        
        b <- diff(ylim.prim)/diff(ylim.sec)
        a <- b*(ylim.prim[1] - ylim.sec[1])
        
        x <- data_daily_q %>% 
          ggplot(.) +
          geom_col(aes(x=date,y=Streamflow), color="gray20") +
          geom_line(aes(x=date,y=a+b*`Cum stream`), color="blue") +
          geom_hline(yintercept = 0, color="black") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          labs(title = "Streamflow",
               x = "Date",
               y = "Streamflow (mm/day)") +
          scale_color_manual(values=c("black", "red", "blue")) +
          scale_y_continuous(name="Streamflow (mm)", sec.axis = sec_axis(~ (. - a)/b, name = "Cumulative Streamflow (mm)")) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
      }
    }
    
    # ---------------------------
    if (input$variable_type == "flux"){
      
      flux_change <- function(name_var, name_title, name_y_primary, name_y_secondary){
        
        diff_flux_tmp <- diff_flux_daily %>% 
          dplyr::filter(variable == name_var)
        
        ylim.prim <- range(diff_flux_tmp$`100`)    # Larger value
        ylim.sec <- range(diff_flux_tmp$absolute_change)   # Smaller value
        
        b <- diff(ylim.prim)/diff(ylim.sec)
        a <- b*(ylim.prim[1] - ylim.sec[1])
        
        run_id <- c(
          "p301_absolute_50" = "P301: 50% Treatment", "p301_absolute_80" = "P301: 20% Treatment",
          "p303_absolute_50" = "P303: 50% Treatment", "p303_absolute_80" = "P303: 20% Treatment",
          "p304_absolute_50" = "P304: 50% Treatment", "p304_absolute_80" = "P304: 20% Treatment"
        )
        
        x <- diff_flux_tmp %>% 
          ggplot(.) +
          geom_hline(aes(yintercept=0),color="black") +
          geom_col(aes(x=date, y=`100`), color="gray70") +
          geom_hline(aes(yintercept=a),color="black") +
          geom_line(aes(x=date,y=a+b*absolute_change), color="red") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          labs(title=name_title,
               x="Date") +
          scale_y_continuous(name=name_y_primary, sec.axis = sec_axis(~ (. - a)/b, name = name_y_secondary)) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
        
      }
      
      if (input$variable_flux == 1){
        name_var <- "Streamflow"
        name_title <- "Streamflow Change"
        name_y_primary <- "Streamflow (mm)"
        name_y_secondary <- "Streamflow Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      
      if (input$variable_flux == 2){
        name_var <- "Transp"
        name_title <- "Transpiration Change"
        name_y_primary <- "Transpiration (mm)"
        name_y_secondary <- "Transpiration Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 3){
        name_var <- "Evap"
        name_title <- "Evaporation Change"
        name_y_primary <- "Evaporation (mm)"
        name_y_secondary <- "Evaporation Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 4){
        name_var <- "PET"
        name_title <- "PET Change"
        name_y_primary <- "PET (mm)"
        name_y_secondary <- "PET Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 5){
        name_var <- "SnowSubl"
        name_title <- "Snow Sublimation Change"
        name_y_primary <- "Snow Sublimation (mm)"
        name_y_secondary <- "Snow Sublimation Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 6){
        name_var <- "del_snowpack"
        name_title <- "Deleted Snowpack Change"
        name_y_primary <- "Deleted Snowpack (mm)"
        name_y_secondary <- "Deleted Snowpack Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 7){
        name_var <- "Baseflow"
        name_title <- "Baseflow Change"
        name_y_primary <- "Baseflow (mm)"
        name_y_secondary <- "Baseflow Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 8){
        name_var <- "ReturnFlow"
        name_title <- "ReturnFlow Change"
        name_y_primary <- "ReturnFlow (mm)"
        name_y_secondary <- "ReturnFlow Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 9){
        name_var <- "GW_out"
        name_title <- "GW_out Change"
        name_y_primary <- "GW_out (mm)"
        name_y_secondary <- "GW_out Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 10){
        name_var <- "RZ_drain"
        name_title <- "RZ_drain Change"
        name_y_primary <- "RZ_drain (mm)"
        name_y_secondary <- "RZ_drain Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 11){
        name_var <- "Unsat_drain"
        name_title <- "Unsat_drain Change"
        name_y_primary <- "Unsat_drain (mm)"
        name_y_secondary <- "Unsat_drain Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 12){
        name_var <- "CapRise"
        name_title <- "CapRise Change"
        name_y_primary <- "CapRise (mm)"
        name_y_secondary <- "CapRise Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      
      if (input$variable_flux == 13){
        name_var <- "R_Tfall"
        name_title <- "R_Tfall Change"
        name_y_primary <- "R_Tfall (mm)"
        name_y_secondary <- "R_Tfall Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 14){
        name_var <- "S_Tfall"
        name_title <- "S_Tfall Change"
        name_y_primary <- "S_Tfall (mm)"
        name_y_secondary <- "S_Tfall Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_flux == 15){
        name_var <- "Photosyn"
        name_title <- "Photosyn Change"
        name_y_primary <- "Photosyn (mm)"
        name_y_secondary <- "Photosyn Change (mm)"
        
        flux_change(name_var=name_var, name_title=name_title,
                    name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
    }
    
    # ---------------------------
    if (input$variable_type == "storage"){
      
      storage_change <- function(name_var, name_title, name_y_primary, name_y_secondary){
        
        diff_flux_tmp <- diff_storage_daily %>% 
          dplyr::filter(variable == name_var)
        
        ylim.prim <- range(diff_flux_tmp$`100`)    # Larger value
        ylim.sec <- range(diff_flux_tmp$absolute_change)   # Smaller value
        
        b <- diff(ylim.prim)/diff(ylim.sec)
        a <- b*(ylim.prim[1] - ylim.sec[1])
        
        run_id <- c(
          "p301_absolute_50" = "P301: 50% Treatment", "p301_absolute_80" = "P301: 20% Treatment",
          "p303_absolute_50" = "P303: 50% Treatment", "p303_absolute_80" = "P303: 20% Treatment",
          "p304_absolute_50" = "P304: 50% Treatment", "p304_absolute_80" = "P304: 20% Treatment"
        )
        
        x <- diff_flux_tmp %>% 
          ggplot(.) +
          geom_hline(aes(yintercept=0),color="black") +
          geom_col(aes(x=date, y=`100`), color="gray70") +
          geom_hline(aes(yintercept=a),color="black") +
          geom_line(aes(x=date,y=a+b*absolute_change), color="red") +
          facet_grid(run~., labeller = labeller(.rows = run_id)) +
          labs(title=name_title,
               x="Date") +
          scale_y_continuous(name=name_y_primary, sec.axis = sec_axis(~ (. - a)/b, name = name_y_secondary)) +
          theme_bw(base_size = 14) +
          xlim(input$date_range) +
          NULL
        plot(x)
        
      }
      
      if (input$variable_storage == 1){
        name_var <- "Snowpack"
        name_title <- "Snowpack Change"
        name_y_primary <- "Snowpack (mm)"
        name_y_secondary <- "Snowpack Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      
      if (input$variable_storage == 2){
        name_var <- "LitterSto"
        name_title <- "LitterSto Change"
        name_y_primary <- "LitterSto (mm)"
        name_y_secondary <- "LitterSto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 3){
        name_var <- "CanopySto"
        name_title <- "CanopySto Change"
        name_y_primary <- "CanopySto (mm)"
        name_y_secondary <- "CanopySto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 4){
        name_var <- "RZ_sto"
        name_title <- "RZ_sto Change"
        name_y_primary <- "RZ_sto (mm)"
        name_y_secondary <- "RZ_sto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 5){
        name_var <- "Unsat_Sto"
        name_title <- "Unsat_Sto Change"
        name_y_primary <- "Unsat_Sto (mm)"
        name_y_secondary <- "Unsat_Sto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 6){
        name_var <- "SatDef_dep"
        name_title <- "SatDef_dep Change"
        name_y_primary <- "SatDef_dep (mm)"
        name_y_secondary <- "SatDef_dep Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 7){
        name_var <- "SatDef_Vol"
        name_title <- "SatDef_Vol Change"
        name_y_primary <- "SatDef_Vol (mm)"
        name_y_secondary <- "SatDef_Vol Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 8){
        name_var <- "GW_sto"
        name_title <- "GW_sto Change"
        name_y_primary <- "GW_sto (mm)"
        name_y_secondary <- "GW_sto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 9){
        name_var <- "Det_sto"
        name_title <- "Det_sto Change"
        name_y_primary <- "Det_sto (mm)"
        name_y_secondary <- "Det_sto Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 10){
        name_var <- "SatArea"
        name_title <- "SatArea Change"
        name_y_primary <- "SatArea (mm)"
        name_y_secondary <- "SatArea Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 11){
        name_var <- "SCA"
        name_title <- "SCA Change"
        name_y_primary <- "SCA (mm)"
        name_y_secondary <- "SCA Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
      if (input$variable_storage == 12){
        name_var <- "LAI"
        name_title <- "LAI Change"
        name_y_primary <- "LAI (mm)"
        name_y_secondary <- "LAI Change (mm)"
        
        storage_change(name_var=name_var, name_title=name_title,
                       name_y_primary=name_y_primary,name_y_secondary=name_y_secondary)
      }
      
    }
    
    
  })
}

shinyApp(ui = ui, server = server)



