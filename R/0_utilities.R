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


# ---------------------------------------------------------------------
# Files and Directories



# ---------------------------------------------------------------------
# Functions




# ---------------------------------------------------------------------
# Variables

flux_var <- c("Precip","Rainfall","Snowfall","SnowSubl","del_snowpack",
              "Evap","Transp","PET",
              "Streamflow","Baseflow","ReturnFlow","GW_out",
              "RZ_drain","Unsat_drain","CapRise","R_Tfall",
              "S_Tfall","Photosyn")

storage_var <- c("Tmax","Tmin","Snowpack","LitterSto","CanopySto",
                 "RZ_sto","Unsat_Sto","SatDef_dep","SatDef_Vol","GW_sto",
                 "Det_sto","SatArea","SCA","LAI")


