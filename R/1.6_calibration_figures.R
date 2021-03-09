# Calibration figures

# Includes code for manuscript figure 2

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

data_cal <- read_csv("model_data/calibration/calibration_data.csv")

data_cal <- data_cal %>% 
  dplyr::mutate(date = mdy(date))


data_cal_q <- data_cal %>% 
  dplyr::select(-c(swe_modeled_basin, swe_modeled_patch, swe_obs_patch)) %>% 
  pivot_longer(c(q_p303_modeled, q_p303_obs), names_to = "q_names", values_to = "q")
data_cal_q$q_names <- factor(data_cal_q$q_names, levels=c("q_p303_obs", "q_p303_modeled"))

data_cal_swe <- data_cal %>% 
  dplyr::select(-c(q_p303_modeled, q_p303_obs)) %>% 
  pivot_longer(c(swe_modeled_basin, swe_modeled_patch, swe_obs_patch), names_to = "swe_names", values_to = "swe")
data_cal_swe$swe_names <- factor(data_cal_swe$swe_names, levels=c("swe_obs_patch", "swe_modeled_patch", "swe_modeled_basin"))

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate calibration figures (Figure 2)


x <- data_cal_q %>% 
  dplyr::filter(date < ymd("2008-10-1")) %>% 
  ggplot(data=.) +
  geom_line(aes(x=date, y=q, color=q_names), size=0.6) +
  scale_y_log10() +
  scale_color_manual(name = "",
                     values = c("black", "blue"),
                     labels = c("Observed\nstreamflow", "Modeled\nstreamflow")) +
  labs(title = "Streamflow",
       x="Year", 
       y="Streamflow, mm/day") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom") +
  NULL
#print(x)

y <- data_cal_swe %>% 
  dplyr::filter(date < ymd("2008-10-1")) %>% 
  ggplot(data=.) +
  geom_line(aes(x=date, y=swe, color=swe_names), size=0.6) +
  #scale_y_log10() +
  scale_color_manual(name = "",
                     values = c("black", "blue", "red"),
                     labels = c("Observed\nSWE (pillow)", "Modeled\nSWE (patch)", "Modeled\nSWE (watershed)")) +
  labs(title = "Snow Water Equivalent (SWE)",
       x="Year", 
       y="Snow water equivalent, mm") +
  theme_bw(base_size = 11) +  
  theme(legend.position = "bottom") +
  NULL
#print(y)

# Patchwork
z <- x / y +
  plot_annotation(tag_levels = 'a')

ggsave("output/manuscript_plots/plot_calibration.pdf", plot=z, width = 6.5, height = 6)




