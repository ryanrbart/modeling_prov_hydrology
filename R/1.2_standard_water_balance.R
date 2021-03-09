# Generate standard water balance figure
# Includes code for manuscript Figure 3


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


# Set up data so that it can be plotted
data_annual_stacked <- diff_flux_annual %>% 
  dplyr::bind_rows(.id="flux") %>% 
  dplyr::filter(flux %in% c("Streamflow", "Evap", "Transp", "WB_Residual"))
data_annual_stacked$flux <- factor(data_annual_stacked$flux, levels = c("WB_Residual", "Streamflow", "Evap", "Transp"))


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate plot components so that they may be grouped together

shed_id <- c(
  "p301" = "P301",
  "p303" = "P303",
  "p304" = "P304"
)

# Standard Water Balance (top plot)
standard_water_balance <- data_annual_stacked %>% 
  ggplot(.) +
  geom_col(aes(x=wy,y=`100`, fill=flux), width = .7, position = "stack") +
  facet_grid(.~watershed, labeller = labeller(watershed = shed_id)) +
  scale_fill_manual(name = "Flux", values = colors_vibrant_4, 
                    labels = c(expression('Change in Storage (d'*S[w]*')'), 
                               expression('Streamflow ('*Q[w]*')'),
                               expression('Evaporation ('*E[w]*')'), 
                               expression('Transpiration ('*T[w]*')'))) +
  labs(title = "Annual water balance", x="Water year", y="Annual flux, mm") +
  theme_bw(base_size = 11) +
  theme(#axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.text.align = 0,
        legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  NULL
#plot(standard_water_balance)


absolute_change_80 <- data_annual_stacked %>% 
  ggplot(.) +
  geom_col(aes(x=wy,y=absolute_80, fill=flux), width = .7, position = "stack") +
  facet_grid(.~watershed, labeller = labeller(watershed = shed_id)) +
  scale_fill_manual(name = "Flux", values = colors_vibrant_4, 
                    labels = c(expression('Change in Storage (d'*S[w]*')'), 
                               expression('Streamflow ('*Q[w]*')'),
                               expression('Evaporation ('*E[w]*')'), 
                               expression('Transpiration ('*T[w]*')'))) +
  labs(title = "20% Biomass reduction", x="Water year", y="Change in post-biomass-\nreduction annual flux, mm") +
  theme_bw(base_size = 11) +
  theme(#axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.text.align = 0,
        legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ylim(-250,250) +
  NULL
#plot(absolute_change_80)


absolute_change_50 <- data_annual_stacked %>% 
  ggplot(.) +
  geom_col(aes(x=wy,y=absolute_50, fill=flux), width = .7, position = "stack") +
  facet_grid(.~watershed, labeller = labeller(watershed = shed_id)) +
  scale_fill_manual(name = "Flux", values = colors_vibrant_4, 
                    labels = c(expression('Change in Storage (d'*S[w]*')'), 
                               expression('Streamflow ('*Q[w]*')'),
                               expression('Evaporation ('*E[w]*')'), 
                               expression('Transpiration ('*T[w]*')'))) +
  labs(title = "50% Biomass reduction", x="Water year", y="Change in post-biomass-\nreduction annual flux, mm") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 270, hjust=0, vjust=0.6),
        legend.text.align = 0,
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ylim(-250,250) +
  NULL
#plot(absolute_change_50)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Temporary patchwork fix (https://github.com/thomasp85/patchwork/issues/170)
# Standard water balance with absolute post-thinning changes 
standard_water_balance_guide <- standard_water_balance + guides(fill = "none")
absolute_change_80_guide <- absolute_change_80 + guides(fill = "none")

plot_annual_water_balance_absolute <- standard_water_balance_guide / absolute_change_80_guide / absolute_change_50  + 
  plot_layout(guides = "collect") &  plot_annotation(tag_levels = 'a') & theme(legend.position = "bottom")


ggsave("output/manuscript_plots/plot_standard_water_balance_absolute.jpg", plot=plot_annual_water_balance_absolute, width = 8, height = 9)
ggsave("output/manuscript_plots/plot_standard_water_balance_absolute.pdf", plot=plot_annual_water_balance_absolute, width = 8, height = 9)


write_csv(data_annual_stacked, path="output/data_annual_stacked.csv")










