# Utilities
# Includes libraries, files/directories, and functions


# ---------------------------------------------------------------------
# Libraries
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(broom)
library(stringr)
library(cowplot)
library(forcats)
library(shiny)
library(zoo)
library(patchwork)


# ---------------------------------------------------------------------
# Files and Directories



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


yd_to_month = function(yd, y){
  if (lubridate::leap_year(y) == TRUE){
    month <- case_when(
      yd %in% seq(1,31) ~ 1,
      yd %in% seq(32,60) ~ 2,
      yd %in% seq(61,91) ~ 3,
      yd %in% seq(92,121) ~ 4,
      yd %in% seq(122,152) ~ 5,
      yd %in% seq(153,182) ~ 6,
      yd %in% seq(183,213) ~ 7,
      yd %in% seq(214,244) ~ 8,
      yd %in% seq(245,274) ~ 9,
      yd %in% seq(275,305) ~ 10,
      yd %in% seq(306,335) ~ 11,
      yd %in% seq(336,366) ~ 12
    )
  } else {
    month <- case_when(
      yd %in% seq(1,31) ~ 1,
      yd %in% seq(32,59) ~ 2,
      yd %in% seq(60,90) ~ 3,
      yd %in% seq(91,120) ~ 4,
      yd %in% seq(121,151) ~ 5,
      yd %in% seq(152,181) ~ 6,
      yd %in% seq(182,212) ~ 7,
      yd %in% seq(213,243) ~ 8,
      yd %in% seq(244,273) ~ 9,
      yd %in% seq(274,304) ~ 10,
      yd %in% seq(305,334) ~ 11,
      yd %in% seq(335,365) ~ 12
    )
  }
  return(month)
}


# ---------------------------------------------------------------------
# Variables

flux_var <- c("Precip","Rainfall","Snowfall","SnowSubl","del_snowpack",
              "Evap","Transp","PET",
              "Streamflow","Baseflow","ReturnFlow","GW_out",
              "RZ_drain","Unsat_drain","CapRise","R_Tfall",
              "S_Tfall","Photosyn","WB_Residual","Evap_neg",
              "Transp_neg", "canopy_evap", "litter_evap")

storage_var <- c("Tmax","Tmin","Snowpack","LitterSto","CanopySto",
                 "RZ_sto","Unsat_Sto","SatDef_dep","SatDef_Vol","GW_sto",
                 "Det_sto","SatArea","SCA","LAI","Sat_sto","Total_sto")


# Color schemes
# source: https://personal.sron.nl/~pault/
# Bright: Purple, red, cyan, blue, green, yellow                    
colors_bright_6 = c("#AA3377","#EE6677","#66CCEE","#4477AA","#228833","#CCBB44")

# Bright: Cyan, blue, green, yellow                    
colors_bright_4 = c("#66CCEE","#4477AA","#228833","#CCBB44") 

# Bright: Blue, yellow
colors_bright_2 <- c("#4477AA","#CCBB44")

# Vibrant: Cyan, blue, orange, teal                    
colors_vibrant_4 = c("#33BBEE","#0077BB","#EE7733","#009988") 

# Vibrant: Orange, blue
colors_vibrant_2 <- c("#EE7733","#0077BB")

# Muted: Rose, indigo
colors_muted_2 <- c("#CC6677","#332288")






