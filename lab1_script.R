# lab one - script


# 1.1 -

d.counties <- d.counties %>%
  dplyr:: group_by(STATEFP10) %>% 
  mutate(statearea = sum(ALAND10 + AWATER10))
d.counties <- d.counties %>%
  mutate(landareapercentage = (ALAND10/ statearea) *100)


# 1.2 - 
d.counties <- d.counties %>%
  mutate(countyAREA = (ALAND10 + AWATER10)) 
d.counties <- d.counties %>%
  mutate(waterasland = (AWATER10 / countyAREA))
maxWATER <- d.counties %>%
  dplyr:: group_by(STATEFP10) %>%
  slice_max(waterasland)


# 1.3 - 
d.counties %>%
  dplyr::group_by(STATEFP10) %>%
  dplyr::count(STATEFP10)


# 1.4 - 
d.stations <- d.stations %>%
  mutate(characterlength = (nchar(STATION_NA)))
d.stations %>%
  slice_min(characterlength)


# 2.1 - 
d.counties <- d.counties %>%
  mutate(stateName = (case_when(
    STATEFP10 == 10 ~ "Delaware",
    STATEFP10 == 11 ~ "District of Columbia",
    STATEFP10 == 24 ~ "Maryland",
    STATEFP10 == 36 ~ "New York",
    STATEFP10 == 42 ~ "Pennsylvania",
    STATEFP10 == 51 ~ "Virginia",
    STATEFP10 == 54 ~ "West Virginia")))

d.counties %>% ggplot(., aes(x= ALAND10, y= AWATER10, colour=stateName)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relationship between Land Area and Water Area in Chesapeake Water Bay Counties", subtitle = "By: Cadence Bowen", x="Land Area", y="Water Area",
       colour= "State Name")

# 2.2 -
d.stations %>% ggplot(., aes(x=Drainage_A)) +
  geom_histogram(fill = "pink") +
  geom_density(color = "white", linewidth = 1) +
  theme_minimal() +
  labs(title = "Drainage Area per Station", subtitle = "By: Cadence Bowen", x= "Drainage", y= "Count")

# 3.1 -
vectorCalculator <- function(value) {
  if (!is.numeric(value)) {
    stop("Please enter a numeric vector.")
  }
  
  return(list(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    sorted = sort(value, na.last = NA)  
  ))
}

# examples
exampleONEv <- c(0,1,-1)
vectorCalculator(exampleONEv)

exampleTWOv <- c(10,100,1000)
vectorCalculator(exampleTWOv)

exampleTHREEv <- c(.1,.001,1e8)
vectorCalculator(exampleTHREEv)

exampleFOURv <- c("a", "b", "c")
vectorCalculator(exampleFOURv)


# 4.1
d.counties %>%
  sf::st_is_valid()
d.stations %>%
  sf::st_is_valid()
d.counties <- d.counties %>%
  sf::st_make_valid()

CountyStation <- sf:: st_intersection(d.counties, d.stations)

CountyStation %>%
  as_tibble() %>%
  group_by(stateName) %>%
  summarise(STATIONSSTATE = n())

# 4.2
d.counties %>%
  as_tibble() %>%
  dplyr:: group_by(stateName) %>%
  dplyr:: filter(stateName == "New York") %>%
  summarise(AvgNYCounty = mean(Shape_Area))

# 4.3
CountyStation %>%
  as_tibble() %>%
  dplyr:: group_by(stateName) %>%
  summarise(avgDrain = mean(Drainage_A)) %>%
  dplyr:: slice_max(avgDrain)
