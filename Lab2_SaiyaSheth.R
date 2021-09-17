install.packages('tidyverse')
install.packages('tidycensus')
install.packages('sf')

library(tidyverse)
library(tidycensus)
library(sf)
library(tmap) 
set.seed(717)

census_api_key("717274061bb926a3200b42feacfac59e646398de", overwrite = TRUE)

acs_variable_list.2019 <- load_variables(2019, "acs5", cache = TRUE)

vars <- load_variables(2019, "acs5")
View(vars)

#{r acs_vars, cache = FALSE, message = FALSE, warning = FALSE, results=FALSE}
acs_vars <- c("B99089_001E", # Estimate total allocation of vehicles avialble
              "B99163_001E", # Estimate total allocation ability to speak Eng
              "B01001_001E", # Estimate total sex by age
              "B01003_001E", # total population
              "B02001_001E") # total estimate race 

myTracts <- c("42101028300", 
              "42101028400", 
              "42101028500", 
              "42101028600", 
              "42101028700",
              "42101028800", 
              "42101028900")

#acsTractsPHL.2019.sf[acs_variable_list.2019$name == "Fred",]all the rows in which the values are Fred#

acsTractsPHL.2019.sf <- get_acs(geography = "tract",
                                year = 2019, 
                                variables = acs_vars, 
                                geometry = TRUE, 
                                state = "PA", 
                                county = "Philadelphia", 
                                output = "wide") %>% 
dplyr::select(GEOID, NAME, all_of(acs_vars)) %>%
  rename (total_vehicles.2019 = B99089_001E,
          total_english_2019 = B99163_001E,
          total_sex_by_age_2019 = B01001_001E,
          total_pop_2019 = B01003_001E,
          total_race_2019 = B02001_001E) %>%
  mutate(Neighborhood = ifelse(GEOID %in% myTracts,
                               "NeighborhoodPHL",
                               "REST OF PHILADELPHIA"))
view(acsTractsPHL.2019.sf)
options(tigris_use_cache = TRUE)

# rlang::last_error() #how to figure out an error in code
  
class(acsTractsPHL.2019.sf)

head(acsTractsPHL.2019.sf,2)

acsTractsPHL.2019.sf$geometry

st_crs(acsTractsPHL.2019.sf)

tm_shape(acsTractsPHL.2019.sf,
         projection = st_crs(acsTractsPHL.2019.sf)) +
  tm_fill() +
  tm_borders() +
  tm_layout(title= 'NAD83', 
            title.position = c('left', 'top'))

acsTractsPHL.2019.sf_UTM <- acsTractsPHL.2019.sf %>% 
  st_transform(crs = "EPSG:26918")

st_crs(acsTractsPHL.2019.sf_UTM)

acsTractsPHL.2019.sf_WGS84 <- acsTractsPHL.2019.sf %>% 
  st_transform(crs = "EPSG:4326")

st_crs(acsTractsPHL.2019.sf_WGS84)

tm_shape(acsTractsPHL.2019.sf_UTM, 
         projection = st_crs(acsTractsPHL.2016.sf_UTM)) +
  tm_fill() +
  tm_borders() +
  tm_layout(title= 'UTM 18N', 
            title.position = c('left', 'top'))

acsTractsPHL.2019.sf_Albers <- acsTractsPHL.2019.sf %>% 
  st_transform(crs = "ESRI:102003")

st_crs(acsTractsPHL.2019.sf_Albers)

tm_shape(acsTractsPHL.2019.sf_Albers, 
         projection = st_crs(acsTractsPHL.2019.sf_Albers)) +
  tm_fill() +
  tm_borders() +
  tm_layout(title= 'USA Contiguous albers', 
            title.position = c('left', 'top'))

acsTractsPHL.2019.sf_WGS84 <- acsTractsPHL.2019.sf %>% 
  st_transform(crs = "EPSG:4326")

st_crs(acsTractsPHL.2019.sf_WGS84)

tm_shape(acsTractsPHL.2019.sf_WGS84, 
         projection = st_crs(acsTractsPHL.2019.sf_WGS84)) +
  tm_fill() +
  tm_borders() +
  tm_layout(title= 'USA Contiguous albers', 
            title.position = c('left', 'top'))

PHL_data <- data.frame(point_ID = seq(1,300,1),
                       variable1 = rnorm(300,0,1)) %>% 
  mutate(latitude  = sample(seq(39.852,40.052,0.001),n(), replace = TRUE),
         longitude = sample(seq(-75.265,-75.065,0.001),n(), replace = TRUE))

head(PHL_data)

install.packages("viridis")
library(viridis)

scale_fill_viridis()

library(ggplot2)

#ggplot_geom_sf, warning = FALSE, echo = FALSE

colnames(acsTractsPHL.2019.sf)
ggplot(acsTractsPHL.2019.sf)+
    geom_sf(data = acsTractsPHL.2019.sf, aes(fill = total_english_2019),
            color = "transparent")+
    geom_sf(data = acsTractsPHL.2019.sf %>%
              filter(Neighborhood == "NeighborhoodPHL") %>%
              st_union(),
            color = "white",
            fill = "transparent")+
    labs(
      title = "Total English Speaking Population",
      subtitle = "ACS Data 2019",
      caption = "Why is this so difficult?")
  

