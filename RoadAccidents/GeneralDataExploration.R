Data_Age <- Dataset %>%
  group_by(Driver_Age) %>%
  summarise(accident_count = n())
Data_Weather <- Dataset %>%
  group_by(Weather) %>%
  summarise(accident_count = n())
Data_Time <- Dataset %>%
  group_by(Time_of_Day) %>%
  summarise(accident_count = n())
Data_Condition <- Dataset %>%
  group_by(Road_Condition) %>%
  summarise(accident_count = n())
Data_Light <- Dataset %>%
  group_by(Road_Light_Condition) %>%
  summarise(accident_count = n())
Data_Severity <- Dataset %>%
  group_by(Accident_Severity) %>%
  summarise(accident_count = n())
