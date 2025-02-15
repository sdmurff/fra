library("dplyr")
library("tidyr")


census_crossings <- read.csv("/Users/murff/Downloads/crossings_census_join_US1.csv")


crossing_adjusted_pop <- census_crossings %>%
  group_by(GEOID20) %>%
  filter(any(!is.na(adjusted_population) & adjusted_population > 0)) %>%  # Ensure at least one valid value
  filter(adjusted_population == max(adjusted_population, na.rm = TRUE)) %>%  # Now it's safe
  slice(1) %>%  # Keep only one row per GEOID20
  ungroup() %>%
  group_by(Crossing.ID) %>%
  summarise(TOTAL_ADJ_POP20 = sum(adjusted_population)) %>%
  ungroup()
  
usa_crossings <- census_crossings %>%
  select(Revision.Date,
         Crossing.ID,
         State.Name,
         City.Name,
         Street,
         Highway.Name,
         THR.Request.No,
         Type.Of.Train.Service.IDs,
         Type.Of.Train.Service.1,
         Type.Of.Train.Service.2,
         Type.Of.Train.Service.3,
         Type.Of.Train.Service.4,
         Total.Switching.Trains,
         Total.Nighttime.Thru.Trains,
         Total.Daylight.Thru.Trains,
         Total.Transit.Trains,
         Total.Non.Transit,
         Development.Type,
         Crossing.Purpose,
         Track.Run.Down.Street,
         Number.Of.Yard.Tracks,
         Number.Of.Industry.Tracks,
         Number.Of.Main.Tracks,
         Number.Of.Transit.Tracks,
         Number.Of.Siding.Tracks,
         Highway.Paved,
         Road.At.Crossing,
         Road.At.Crossing.Type,
         Latitude,
         Longitude) %>%
  distinct()
  

usa_crossings1 <- usa_crossings %>%
  left_join(crossing_adjusted_pop, by = "Crossing.ID")

write.csv(usa_crossings1, "/Users/murff/Downloads/usa_crossings_adj_population.csv")








  dups <- census_crossings1 %>%
  group_by(GEOID20) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  ungroup() # Remove grouping for a clean result


census_crossings2 <- census_crossings1 %>%
  mutate(GEOID20 = as.character(GEOID20)) %>%  # Ensure GEOID20 is a character
  pivot_wider(names_from = BLOCKCE20,
              names_prefix = "GEO_",
              values_from = adjusted_population)




pivot_table <- crossings_gis2 %>%
  group_by(Development.Type) %>%
  summarise(CNT = n(),
            .groups = "drop") 

no_transit <- crossings_gis2 %>%
  group_by(Total.Non.Transit) %>%
  filter(Total.Non.Transit > -1) %>% # Keeps rows where the group size is greater than 1
  arrange(Total.Non.Transit) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result


census_crossings <- census_crossings |>
  filter(ALAND20>0)



