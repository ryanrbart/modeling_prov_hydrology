# Snow water balance figure
# Includes code for manuscript figure 7


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

# A single data frame with each variable
data_annual <- read_csv("output/data_annual.csv",
                        col_types = list(wy = col_character(),
                                         watershed = col_factor(),
                                         biomass = col_factor()))

# A list of data frames for each flux that has been differenced
diff_flux_annual <- read_rds("output/diff_flux_annual.rds")
diff_storage_annual <- read_rds("output/diff_storage_annual.rds")


data_daily <- read_csv("output/data_daily.csv",
                       col_types = list(wy = col_character(),
                                        run = col_factor(),
                                        season = col_factor(levels = c("Jan-Mar",
                                                                       "Apr-Jun",
                                                                       "Jul-Sep",
                                                                       "Oct-Dec")),
                                        biomass = col_factor(),
                                        watershed = col_factor()))

diff_flux_daily <- read_rds("output/diff_flux_daily.rds")
diff_storage_daily <- read_rds("output/diff_storage_daily.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Snow balance


diff_storage_daily <- read_rds("output/diff_storage_daily.rds") %>% 
  dplyr::bind_rows(.id="variable") %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1) %>% 
  tidyr::pivot_longer(c(absolute_50, absolute_80), names_to = "change_level", values_to = "absolute_change") %>% 
  dplyr::mutate(run = paste(watershed, "_", change_level, sep="")) %>% 
  dplyr::select(-c(year, yd, relative_80, relative_50, `50`, `80`))
diff_storage_daily$change_level <- factor(diff_storage_daily$change_level, levels = c("absolute_80", "absolute_50"))


diff_flux_tmp <- diff_storage_daily %>% 
  dplyr::filter(variable == "Snowpack",
                watershed=="p303",
                change_level == "absolute_50") %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::group_by(wetness, variable, WYD) %>% 
  dplyr::summarise(`100` = mean(`100`),
                   absolute_change  = mean(absolute_change))
diff_flux_tmp$wetness <- factor(diff_flux_tmp$wetness, levels = c("low", "middle", "high"))


ylim.prim <- range(diff_flux_tmp$`100`)    # Larger value
ylim.sec <- range(diff_flux_tmp$absolute_change)   # Smaller value

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate figure 7


wetness_id <- c(
  "low" = "Dry water years",
  "middle" = "Average water years",
  "high" = "Wet water years"
)

shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)

change_level_id <- c(
  "absolute_80" = "20% Thinning Scenario",
  "absolute_50" = "50% Thinning Scenario"
)


# Note: To line up the zero of the secondary axis with the primary axis
# Increase bottom of the primary axis range to -a
# Remove a from the geom_line and hte scale_y_continuous line


# Plot with same axis scales
x <- diff_flux_tmp %>% 
  ggplot(.) +
  geom_ribbon(aes(x=WYD, ymin=0, ymax = `100`, fill="Pre-thinning SWE")) +
  geom_line(aes(x=WYD,y=absolute_change, color="Change in\npost-thinning SWE"), size=0.8) +
  geom_hline(aes(yintercept=0),color="black") +
  facet_grid(wetness~., labeller = labeller(wetness = wetness_id)) +
  labs(x="Water year day", y="Snow water equivalent, mm") +
  scale_fill_manual(name = "",
                    breaks = c("Pre-thinning SWE"),
                    values = c("Pre-thinning SWE" = "gray70")) +
  scale_color_manual(name = "",
                     breaks = c("Change in\npost-thinning SWE"),
                     values = c("Change in\npost-thinning SWE" = "blue")) +
  scale_x_continuous(breaks = c(1,32,62,93,124,152,183,213,244,274,305,336),
                     minor_breaks = NULL,
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")) +
  theme_bw(base_size = 11) +
  #scale_color_identity(guide = "legend") +
  theme(axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        legend.position = "bottom") +
  NULL
#plot(x)

ggsave("output/manuscript_plots/plot_snow_water_balance_daily.jpg", plot=x, width = 4, height = 5)




