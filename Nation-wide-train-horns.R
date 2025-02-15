library(dplyr)
library(lubridate)
library(hms)
library(tidyr)
library(ggplot2)

# Next steps
# 1) Merge in quiet zon e data
# 2) Estimate Nation-wide how many people likely lose sleep due to the train horns
# 3) What percent of affected population (within 1k feet of crossing) live within a quiet zone
# 4)


crossings <- read.csv("/Users/murff/Downloads/Crossing_Inventory_Data__Form_71__-_Current_20241203.csv")

# Filter crossings to exclude
#   Not at-grade
#   Not closed
#   Not private
#   Does not run down the street
#   Is not a local transit train (e.g. light rail) or tourist train
#   Is paved

crossings_filtered <- crossings %>%
  filter(Crossing.Position=="At Grade", 
         Crossing.Closed=="No",
         !grepl("PVT|PRIV", Highway.Name, ignore.case = TRUE),
         Track.Run.Down.Street != "Yes",
         !grepl("\\b14\\b|\\b16\\b", Type.Of.Train.Service.IDs),
         Highway.Paved != "No") %>%
  select(Crossing.ID,
         Annual.Average.Daily.Traffic.Count,
         Revision.Date,
         Reason.Description,
         Reporting.Agency.Name,
         Railroad.Name,
         City.Name,
         State.Name,
         Type.Of.Train.Service.IDs,
         Type.Of.Train.Service.1,
         Type.Of.Train.Service.2,
         Type.Of.Train.Service.3,
         Type.Of.Train.Service.4,
         Total.Switching.Trains,
         Total.Nighttime.Thru.Trains,
         Total.Daylight.Thru.Trains,
         Total.Transit.Trains,
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
         Street,
         Highway.Name,
         Crossing.Type,
         Crossing.Position,
         Whistle.Ban,
         Whistle.Date,
         Quiet.Zone.ID,
         THR.Request.No,
         Latitude,
         Longitude,
         Channelization.Devices,
         Crossing.Closed,
         Url)

crossings_gis <- crossings_filtered |>
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Revision.Date = as.Date(Revision.Date, format = "%m/%d/%Y"),
         Total.Non.Transit = Total.Daylight.Thru.Trains + Total.Nighttime.Thru.Trains - Total.Transit.Trains) |>
         filter(!is.na(Latitude) & !is.na(Longitude)) |>
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
         Longitude)

duplicates <- crossings_gis %>%
  group_by(Crossing.ID) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Crossing.ID) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

crossings_gis1 <- crossings_gis %>%
  distinct(Crossing.ID, .keep_all = TRUE)

duplicates <- crossings_gis1 %>%
  group_by(Latitude, Longitude) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Latitude) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

crossings_gis2 <- crossings_gis1 %>%
  group_by(Latitude, Longitude) %>% # Group by all other columns
  slice_max(order_by = Revision.Date, n = 1) %>% # Keep the row with the most recent date
  distinct(Latitude, Longitude, .keep_all = TRUE) %>% # Removes duplicates if multiple rows have the same date
  ungroup() # Remove grouping for a clean result

# Check for final duplicates
duplicates <- crossings_gis2 %>%
  group_by(Latitude, Longitude) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Latitude) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

write.csv(crossings_gis2, "/Users/murff/Downloads/crossings_usa_clean1.csv", row.names = FALSE)

# Output cleaned up Utah Quiet Zones
utah_quiet_zones <- crossings_gis2 %>%
  filter(THR.Request.No %in% c("THR_Request_000000111217", "THR_Request_000000111670"))

write.csv(utah_quiet_zones, "/Users/murff/Downloads/quiet_zones_utah_clean.csv", row.names = FALSE)


pivot_table <- crossings_gis2 %>%
  mutate(quiet_zone = if_else(is.na(THR.Request.No) | THR.Request.No == "", "NO", "YES")) %>%
  group_by(State.Name, quiet_zone) %>%
  summarise(CNT = n(),
            NUM_QUIET_ZONES = n_distinct(THR.Request.No),
            .groups = "drop") %>%
  pivot_wider(names_from = quiet_zone, values_from = c(CNT, NUM_QUIET_ZONES), 
            names_prefix = "quiet_zone_") %>%
  mutate(PCT_QUIET_CROSSINGS = CNT_quiet_zone_YES/(CNT_quiet_zone_YES+CNT_quiet_zone_NO))



pivot_table <- crossings_filtered %>%
  group_by(State.Name, City.Name,  THR.Request.No, Whistle.Date) %>%
  summarise(CNT = n(), 
            TRAFFIC = sum(Annual.Average.Daily.Traffic.Count, na.rm = TRUE),
            .groups = "drop") 


###########

WoodsCross <- crossings_gis2 |>
  filter(THR.Request.No =="THR_Request_000000111670") |>
  select(Crossing.ID, City.Name, Street)

Lehi <- crossings_gis2 |>
  filter(THR.Request.No =="THR_Request_000000111217") |>
  select(Crossing.ID, City.Name, Street)

F57 <- read.csv("/Users/murff/Downloads/Highway-Rail_Grade_Crossing_Accident_Data__Form_57__20241212.csv")
F57_FILTERED <- F57 |>
  filter(Public.Private=="Public", Report.Year > 2005, Crossing.Users.Killed > 0) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Time1 = hms::as_hms(parse_date_time(Time, orders = "I:M p")),
         HourOfDay = hour(Time1), # Extract the hour
         TimeOfDay = ifelse(HourOfDay >= 7 & HourOfDay < 23, "Waking Hours", "Sleeping Hours"),
         Suicide = ifelse(Highway.User.Action=="Suicide/attempted suicide", "Suicide", "Other")) |>
  select(Grade.Crossing.ID,
         Whistle.Ban,
         Date,
         Report.Year,
         Time,
         Time1,
         HourOfDay,
         TimeOfDay,
         County.Name,
         State.Name,
         City.Name, 
         Highway.Name,
         Public.Private,
         Highway.User,
         Estimated.Vehicle.Speed,
         Highway.User.Position,
         Visibility,
         Equipment.Type,
         Train.Speed,
         Crossing.Warning.Expanded.1,
         Crossing.Warning.Expanded.2,
         Crossing.Warning.Expanded.3,
         Crossing.Warning.Expanded.4,
         Signaled.Crossing.Warning,
         User.Age,
         User.Gender,
         Highway.User.Action,
         Suicide,
         Crossing.Users.Killed,
         Crossing.Users.Injured,
         Employees.Killed,
         Employees.Injured,
         Passengers.Killed,
         Passengers.Injured,
         Narrative,
         Total.Killed.Form.57,
         Total.Injured.Form.57,
         Total.Killed.Form.55A,
         Total.Injured.Form.55A,
         Whistle.Ban,
         Reporting.Parent.Railroad.Name,
         Reporting.Railroad.Holding.Company,
         Url
  )

pivot_table <- F57_FILTERED %>%
  filter(Public.Private=="Public", Report.Year > 2015) %>%
  group_by(HourOfDay) %>%
  summarise(Total = sum(Crossing.Users.Killed), .groups = "drop") 

# Plot histogram with shaded region from 7AM to 11PM
ggplot(pivot_table, aes(x = HourOfDay, y = Total)) +
  geom_rect(aes(xmin = 7, xmax = 23, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.2) +  # Shaded region for 7 AM - 11 PM
  geom_col(fill = "steelblue", color = "black") +
  labs(title = "Total Crossing Users Killed by Hour of Day",
       x = "Hour of Day",
       y = "Total Deaths") +
  theme_minimal()



%>%
  pivot_wider(names_from = Report.Year, values_from = Total)
         



         Railroad.Name,
         Report.Year,
         
         City.Name,
         Street,
         THR.Request.No,
         Latitude,
         Longitude,
         Incident.Number,
         Date,
         Time,
         County.Name,
         State.Name,
         Highway.Name,
         Public.Private,
         Highway.User,
         Equipment.Struck,
         Train.Speed,
         User.Age,
         User.Gender,
         Highway.User.Action,
         View.Obstruction,
         Driver.In.Vehicle,
         Crossing.Users.Killed,
         Crossing.Users.Injured,
         Number.Vehicle.Occupants,
         Narrative,
         Total.Killed.Form.57,
         Total.Injured.Form.57,
         Total.Killed.Form.55A,
         Total.Injured.Form.55A
  )

