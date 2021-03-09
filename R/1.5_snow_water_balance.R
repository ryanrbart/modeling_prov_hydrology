# Snow water balance figure
# Includes code for manuscript figure 8


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
# Storage (snowpack) balance


diff_storage_daily <- diff_storage_daily %>% 
  dplyr::bind_rows(.id="variable") %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1) %>% 
  tidyr::pivot_longer(c(absolute_50, absolute_80), names_to = "change_level", values_to = "absolute_change") %>% 
  dplyr::mutate(run = paste(watershed, "_", change_level, sep="")) %>% 
  dplyr::select(-c(year, yd, relative_80, relative_50, `50`, `80`))
diff_storage_daily$change_level <- factor(diff_storage_daily$change_level, levels = c("absolute_80", "absolute_50"))


diff_snowpack_tmp <- diff_storage_daily %>% 
  dplyr::filter(variable == "Snowpack",
                watershed=="p303",
                change_level == "absolute_50") %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::group_by(wetness, variable, WYD) %>% 
  dplyr::summarise(`100` = mean(`100`),
                   absolute_change  = mean(absolute_change))
diff_snowpack_tmp$wetness <- factor(diff_snowpack_tmp$wetness, levels = c("low", "middle", "high"))


ylim.prim <- range(diff_snowpack_tmp$`100`)    # Larger value
ylim.sec <- range(diff_snowpack_tmp$absolute_change)   # Smaller value

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate snowpack portion of figure 8

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
x_snowpack <- diff_snowpack_tmp %>% 
  ggplot(.) +
  geom_ribbon(aes(x=WYD, ymin=0, ymax = `100`, fill="Pre-biomass-reduction SWE")) +
  geom_line(aes(x=WYD,y=absolute_change, color="Change in post-biomass-reduction SWE"), size=0.6) +
  geom_hline(aes(yintercept=0),color="black") +
  facet_grid(.~wetness, labeller = labeller(wetness = wetness_id)) +
  labs(title = "Snowpack", x="Water year day", y="Snow water equivalent, mm") +
  scale_fill_manual(name = "",
                    breaks = c("Pre-biomass-reduction SWE"),
                    values = c("Pre-biomass-reduction SWE" = "gray70")) +
  scale_color_manual(name = "",
                     breaks = c("Change in post-biomass-reduction SWE"),
                     values = c("Change in post-biomass-reduction SWE" = "blue")) +
  scale_x_continuous(breaks = c(1,32,62,93,124,152,183,213,244,274,305,336),
                     minor_breaks = NULL,
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")) +
  theme_bw(base_size = 11) +
  #scale_color_identity(guide = "legend") +
  theme(legend.text.align = 0,
        legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend(order = 1),color = guide_legend(order = 2)) +
  NULL
#plot(x_snowpack)

ggsave("output/manuscript_plots/plot_snowpack_water_balance_daily.jpg", plot=x_snowpack, width = 4, height = 5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Flux (snowmelt) balance


diff_flux_daily <- diff_flux_daily %>% 
  dplyr::bind_rows(.id="variable") %>% 
  dplyr::mutate(wy_wyd = paste(wy, "_", WYD, sep=""),
                year = wy_to_y(wy=as.numeric(wy), wyd=WYD),
                yd = wyd_to_yd(wyd=WYD, y=year)) %>% 
  dplyr::mutate(date = ymd(paste(year,"-01-01", sep="")) + yd -1) %>% 
  tidyr::pivot_longer(c(absolute_50, absolute_80), names_to = "change_level", values_to = "absolute_change") %>% 
  dplyr::mutate(run = paste(watershed, "_", change_level, sep="")) %>% 
  dplyr::select(-c(year, yd, relative_80, relative_50, `50`, `80`))
diff_flux_daily$change_level <- factor(diff_flux_daily$change_level, levels = c("absolute_80", "absolute_50"))


diff_snowmelt_tmp <- diff_flux_daily %>% 
  dplyr::filter(variable == "snowmelt",
                watershed=="p303",
                change_level == "absolute_50") %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::group_by(wetness, variable, WYD) %>% 
  dplyr::summarise(`100` = mean(`100`),
                   absolute_change  = mean(absolute_change))
diff_snowmelt_tmp$wetness <- factor(diff_snowmelt_tmp$wetness, levels = c("low", "middle", "high"))


ylim.prim <- range(diff_snowmelt_tmp$`100`)    # Larger value
ylim.sec <- range(diff_snowmelt_tmp$absolute_change)   # Smaller value

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate snowmelt portion of figure 8

# Plot with same axis scales
x_snowmelt <- diff_snowmelt_tmp %>% 
  ggplot(.) +
  geom_ribbon(aes(x=WYD, ymin=0, ymax = `100`, fill="Pre-biomass-reduction snowmelt")) +
  geom_line(aes(x=WYD,y=absolute_change, color="Change in post-biomass-reduction snowmelt"), size=0.6) +
  geom_hline(aes(yintercept=0),color="black") +
  facet_grid(.~wetness, labeller = labeller(wetness = wetness_id)) +
  labs(title = "Snowmelt", x="Water year day", y="Snowmelt, mm") +
  scale_fill_manual(name = "",
                    breaks = c("Pre-biomass-reduction snowmelt"),
                    values = c("Pre-biomass-reduction snowmelt" = "gray70")) +
  scale_color_manual(name = "",
                     breaks = c("Change in post-biomass-reduction snowmelt"),
                     values = c("Change in post-biomass-reduction snowmelt" = "blue")) +
  scale_x_continuous(breaks = c(1,32,62,93,124,152,183,213,244,274,305,336),
                     minor_breaks = NULL,
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")) +
  theme_bw(base_size = 11) +
  #scale_color_identity(guide = "legend") +
  theme(legend.text.align = 0,
        legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend(order = 1),color = guide_legend(order = 2)) +
  NULL
#plot(x_snowmelt)

ggsave("output/manuscript_plots/plot_snowmelt_water_balance_daily.jpg", plot=x_snowmelt, width = 4, height = 5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Flux (sublimation) balance


diff_sublimation_tmp <- diff_flux_daily %>% 
  dplyr::filter(variable == "SnowSubl",
                watershed=="p303",
                change_level == "absolute_50") %>% 
  dplyr::mutate(wetness = case_when(wy %in% c(2014, 2007, 2013) ~ "low",
                                    wy %in% c(2004, 2012, 2008, 2009) ~ "middle",
                                    wy %in% c(2010, 2005, 2006, 2011) ~ "high")) %>% 
  dplyr::group_by(wetness, variable, WYD) %>% 
  dplyr::summarise(`100` = mean(`100`),
                   absolute_change  = mean(absolute_change))
diff_sublimation_tmp$wetness <- factor(diff_sublimation_tmp$wetness, levels = c("low", "middle", "high"))


ylim.prim <- range(diff_sublimation_tmp$`100`)    # Larger value
ylim.sec <- range(diff_sublimation_tmp$absolute_change)   # Smaller value

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate sublimation portion of figure 8

# Plot with same axis scales
x_sublimation <- diff_sublimation_tmp %>% 
  ggplot(.) +
  geom_ribbon(aes(x=WYD, ymin=0, ymax = `100`, fill="Pre-biomass-reduction sublimation")) +
  geom_line(aes(x=WYD,y=absolute_change, color="Change in post-biomass-reduction sublimation"), size=0.6) +
  geom_hline(aes(yintercept=0),color="black") +
  facet_grid(.~wetness, labeller = labeller(wetness = wetness_id)) +
  labs(title = "Sublimation", x="Water year day", y="Sublimation, mm") +
  scale_fill_manual(name = "",
                    breaks = c("Pre-biomass-reduction sublimation"),
                    values = c("Pre-biomass-reduction sublimation" = "gray70")) +
  scale_color_manual(name = "",
                     breaks = c("Change in post-biomass-reduction sublimation"),
                     values = c("Change in post-biomass-reduction sublimation" = "blue")) +
  scale_x_continuous(breaks = c(1,32,62,93,124,152,183,213,244,274,305,336),
                     minor_breaks = NULL,
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")) +
  theme_bw(base_size = 11) +
  #scale_color_identity(guide = "legend") +
  theme(axis.text.x = element_text(angle = 315, hjust=0, vjust=0.6),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend(order = 1),color = guide_legend(order = 2)) +
  NULL
#plot(x_sublimation)

ggsave("output/manuscript_plots/plot_sublimation_water_balance_daily.jpg", plot=x_sublimation, width = 4, height = 5)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Patchwork it all together

plot_snow <- x_snowpack / x_snowmelt / x_sublimation  + 
  # plot_layout(guides = 'collect') +
  # plot_annotation(title = "Annual water balance with post-thinning percent changes") &
  plot_annotation(tag_levels = 'a')

#plot(plot_snow)
ggsave("output/manuscript_plots/plot_snow_water_balance_daily_all.jpg", plot=plot_snow, width = 8, height = 9)
ggsave("output/manuscript_plots/plot_snow_water_balance_daily_all.pdf", plot=plot_snow, width = 8, height = 9)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Get summaries

diff_snowpack_tmp %>% 
  summarise(pre = mean(`100`),
            pre_peak = max(`100`),
            reduction_mean = mean(absolute_change),
            reduction_max = max(absolute_change))

diff_snowmelt_tmp %>% 
  summarise(pre = sum(`100`),
            reduction = sum(absolute_change))

diff_snowmelt_tmp %>% 
  dplyr::filter(WYD <= 180, WYD > 90) %>% 
  summarise(pre = sum(`100`),
            reduction = sum(absolute_change))



diff_sublimation_tmp %>% 
  summarise(pre = sum(`100`),
            reduction = sum(absolute_change))

diff_sublimation_tmp %>% 
  dplyr::filter(WYD <= 180, WYD > 90) %>% 
  summarise(pre = sum(`100`),
            reduction = sum(absolute_change))



