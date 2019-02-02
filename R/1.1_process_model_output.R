#Import model output (data originally in SigmaPlot)


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

wy <- seq(2004,2014)

generate_data <- function(wy, shed, biomass){
  # Generate file names of data to be imported 
  names <- map(.x=wy, 
               function(x)file.path("model_output",
                                    shed,
                                    paste(shed,"_",biomass,"_",x,".csv",sep="")))
  
  output_data <- names %>% 
    # Import data to list
    map(.,function(x)read_csv(x, n_max = 365)) %>% 
    # Set name of list
    setNames(., wy) %>% 
    dplyr::bind_rows(.id="wy") %>% 
    # Add column identifying the biomass level and watershed
    tibble::add_column(biomass = biomass, watershed = shed)
  
  return(output_data)
}

# Run data import function
rhessys_data <- list()
rhessys_data$p301_50 <- generate_data(wy=wy, shed="p301", biomass=50)
rhessys_data$p301_80 <- generate_data(wy=wy, shed="p301", biomass=80)
rhessys_data$p301_100 <- generate_data(wy=wy, shed="p301", biomass=100)

rhessys_data$p303_50 <- generate_data(wy=wy, shed="p303", biomass=50)
rhessys_data$p303_80 <- generate_data(wy=wy, shed="p303", biomass=80)
rhessys_data$p303_100 <- generate_data(wy=wy, shed="p303", biomass=100)

rhessys_data$p304_50 <- generate_data(wy=wy, shed="p304", biomass=50)
rhessys_data$p304_80 <- generate_data(wy=wy, shed="p304", biomass=80)
rhessys_data$p304_100 <- generate_data(wy=wy, shed="p304", biomass=100)

# Bind data together
r_data <- dplyr::bind_rows(rhessys_data, .id="run")

# ---------------------------------------------------------------------
# Plot year to year differences

r_data %>% 
  dplyr::group_by(wy, watershed) %>% 
  dplyr::summarise(snow = sum(Snowfall))

# ---------------------------------------------------------------------
# Plot year to year differences




