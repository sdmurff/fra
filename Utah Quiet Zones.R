library(dplyr)
install.packages("dplyr")
crossings <- read.csv("/Users/murff/Downloads/Crossing_Inventory_Data__Form_71__-_Current_20241203.csv")

crossings <- crossings |>
  select() 

quiet_zones <- crossings |>
  group_by(THR.Request.No) |>
    summarise(count = n())


WoodsCross <- crossings |>
  filter(THR.Request.No =="THR_Request_000000111670") |>
  select(Crossing.ID, City.Name, Street)

Lehi <- crossings |>
  filter(THR.Request.No =="THR_Request_000000111217") |>
  select(Crossing.ID, City.Name, Street)

utah_quiet_zones <- crossings |>
  filter(THR.Request.No %in% c("THR_Request_000000111217", "THR_Request_000000111670")) |>
  select(Crossing.ID, City.Name, Street, THR.Request.No, Latitude, Longitude)

write.csv(utah_quiet_zones, "/Users/murff/Downloads/utah_quiet_zones.csv")



crossings <- read.csv("/Users/murff/Downloads/Crossing_Inventory_Data__Form_71__-_Current_20241203.csv")
crossings_tx <- crossings |>
  filter(State.Name=="TEXAS") 

