library(sp)
library(tidyverse)
library(broom)
library(ggfortify)
library(lubridate)
library(maptools)
library(gganimate)
library(rmapshaper)


#setwd("C:/Users/331180/Documents/R/Other")

covid_age <- read_csv("V:/BPI_AA_PROJECT/policy_wider_analysis/cross_border/Will_N_vdi_tasks/other/ltla_2021-01-14.csv")

covid_age <- covid_age %>%
  filter(areaType == "ltla")

ltlas <- factor(covid_age$areaName)


blank_dates <- expand.grid(date = seq.Date(as.Date("2020-01-30"), as.Date("2020-05-14"), by = "day"),
                           newCasesBySpecimenDate = 0,
                           areaName = ltlas) %>%
                distinct()

covid_age <- full_join(blank_dates, covid_age)

# Data prep ---------------------------------------------------------------

df1 <- covid_age %>%
  mutate(week = floor_date(date, unit = "weeks")) %>%
  group_by(week, areaName) %>%
  summarise(total = sum(newCasesBySpecimenDate))
  
# ggplot(df1, aes(total)) +
#   geom_histogram(binwidth = 50) +
#   scale_y_log10() +
#   scale_x_continuous(breaks = seq(0, 5000, 100)) +
#   theme(axis.text = element_text(angle = 45))

geo <- raster::shapefile("V:/BPI_AA_PROJECT/policy_wider_analysis/cross_border/Will_N_vdi_tasks/other/Local_Authority_Districts__May_2020__Boundaries_UK_BFE.shp")



geo1 <- ms_simplify(geo, keep = 0.0005) # fine
# geo2 <- ms_simplify(geo, keep = 0.0001) # bit weak but passable
# geo3 <- ms_simplify(geo, keep = 0.00001) # starts to look like Porygon
# plot(geo1)
 
geo1@data <- geo1@data %>% 
  rename(areaName = lad20nm) 

geo1@data <- geo1@data %>% 
  mutate(areaName = str_replace(areaName, c("Cornwall|Isles of Scilly"), "Cornwall and Isles of Scilly")) %>%
  mutate(areaName = str_replace(areaName, "Na h-Eileanan Siar", "Comhairle nan Eilean Siar")) %>%
  mutate(areaName = str_replace(areaName, c("Hackney|City of London"), "Hackney and City of London")) %>%
  mutate(areaName = str_replace(areaName, "Buckinghamshire", "Chiltern"))



#gpclibPermit()

map.df <- fortify(geo1, region ="areaName")

# merge data
map.df <- left_join(map.df, df1, by=c('id'='areaName'))


map.df %>% select(id, total) %>% subset(is.na(total)) %>% distinct()

gg <- ggplot() +
  geom_polygon(data = map.df,
               aes(x = long, y = lat, group = group, fill = total),
               size = 0.25,
               colour = NA) +
  scale_fill_viridis_c(option = "plasma", trans = scales::pseudo_log_trans(sigma = 0.1), breaks=c(0,
                                                                                                  5,
                                                                                                  10,
                                                                                                  30,
                                                                                                  50, 
                                                                                                  100,
                                                                                                  150, 
                                                                                                  500, 
                                                                                                  750, 
                                                                                                  1000, 
                                                                                                  2000, 
                                                                                                  3000, 
                                                                                                  4000)) +
  coord_fixed(1) +
  theme_void() +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 20),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 14, hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2,1,2,1,"cm")) +
  transition_states(week, transition_length = 3, state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_fade() +
  labs(title='LTLA Covid-19 Cases in the UK',
       subtitle='Total Number of Cases Week Ending: {closest_state}',
       caption='
       Calculated as a weekly sum of total cases in an Lower Tier Local Authority
       N.B. Mass testing begins in May 2020.
       Cases data from: https://coronavirus.data.gov.uk/details/download
       Geospatial data from: https://geoportal.statistics.gov.uk/datasets (May 2020 boundaries)') +
  guides(fill = guide_legend(title="Weekly Cases", reverse = T))


#gg


animate(gg,
        fps = 25,
        duration = 30,
        start_pause = 50,
        end_pause = 50,
        width = 1200,
        height = 900,
        renderer = gifski_renderer("V:/BPI_AA_PROJECT/policy_wider_analysis/cross_border/Will_N_vdi_tasks/other/gganim_map_covid4.gif"))


# rm(covid_age, df1, geo, map.df)
# gc()

