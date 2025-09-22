#Q1
day <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
snacks_spend <- c(30, 25, 40, 35, 50, 60, 45)  
travel_spend <- c(20, 30, 25, 40, 35, 50, 30) 
pocket <- data.frame(day, snacks_spend, travel_spend)

pocket$total_spend <- pocket$snacks_spend + pocket$travel_spend
print(pocket)


#Q2
#first 6 months using month.abb
month <- month.abb[1:6]   # January to June
amount <- rep(2000, 6)    
nav <- c(15.2, 15.5, 15.8, 16.0, 16.3, 16.5)
sip_log <- data.frame(month, amount, nav)
print(sip_log)

#Q3
date <- seq.Date(from = as.Date("2025-08-01"), by = "day", length.out = 10)
set.seed(1)
type <- sample(c("Debit", "Credit"), 10, replace = TRUE)
category <- sample(c("Groceries", "Fuel", "Entertainment"), 10, replace = TRUE)
amount <- sample(100:1000, 10, replace = TRUE)
cashbook <- data.frame(date, type, category, amount)
print(cashbook)

#Q4
month1 <- month.abb[1:12] #January to December
set.seed(2)
rain_mm = sample(10:100, 12, replace=TRUE)
raindays = sample(0:31, 12, replace=TRUE)
rain_data = data.frame(month1, rain_mm, raindays)
rain_data

#Q5
library(tibble)
day <- rep(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), times = 2)
city <- factor(rep(c("CityA", "CityB"), each = 7))
rain_mm <- c(5, 0, 12, 8, 15, 3, 0, 10, 5, 20, 0, 8, 12, 4) 
rain_week <- tibble(day, city, rain_mm)
rain_week

#Q6
station_id <- paste0("S", 1:3)
altitude_m <- c(5, 120, 250)
zone <- c("Coastal", "Inland", "Inland")
stations <- data.frame(station_id, altitude_m, zone)

dates <- as.Date("2025-08-01") + 0:4
readings <- expand.grid(date = dates, station_id = station_id)

set.seed(1)
readings$rain_mm <- sample(0:30, 15, replace = TRUE)

print(stations)
print(readings)


##DPLYR Practise
install.packages("dplyr")
library(dplyr)

#Q7
cashbook %>% select(date, category, amount) %>% arrange(desc(amount)) %>% slice_head(n = 5)  

#Q8
cashbook %>% filter(category %in% c("Groceries", "Fuel")) %>% summarise(total_spent = sum(amount))  

#Q9
sip_log %>% summarise(mean_nav = mean(nav), total_invested = sum(amount), months_count = n())

#Q10
pocket %>% mutate(over_100 = total_spend > 100) %>% summarise(days_over_100 = sum(over_100))

#Q11
rain_data %>% filter(raindays > 0) %>% summarise(total_rain = sum(rain_mm), avg_rain_per_day = sum(rain_mm) / sum(raindays))

#Q12
rain_data %>% arrange(desc(rain_mm)) %>% slice_head(n = 3)  

#Q13
rain_week %>% group_by(city) %>% summarise(mean_rain = mean(rain_mm)) 

#Q14
install.packages("tidyr")
library(tidyr)
library(dplyr)
rain_wide <- data.frame(day = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                        rain_cityA = c(5, 0, 12, 8, 15, 3, 0),
                        rain_cityB = c(10, 5, 20, 0, 8, 12, 4))

rain_long <- rain_wide %>% pivot_longer(
    cols = c(rain_cityA, rain_cityB),  
    names_to = "city",                  
    values_to = "rain_mm") %>% mutate(city = sub("rain_", "", city))  
rain_long

#Q15
readings_joined <- readings %>% left_join(stations, by = "station_id")
readings_joined %>% group_by(zone) %>% summarise(avg_rain = mean(rain_mm, na.rm = TRUE))

#Q16
cashbook %>% filter(type == "Debit") %>% arrange(desc(amount)) %>% slice_head(n = 1)

#Q17
rain_2024 <- data.frame(month = month.abb, rain_mm = sample(10:150, 12, replace = TRUE))

rain_2024 %>% mutate(season = case_when(
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",
    month %in% c("Mar", "Apr", "May") ~ "Pre-Monsoon",
    month %in% c("Jun", "Jul", "Aug", "Sep") ~ "Monsoon",
    month %in% c("Oct", "Nov") ~ "Post-Monsoon")) %>% group_by(season) %>% summarise(total_rain = sum(rain_mm))

#Q18
rain_2024$rain_mm[c(3, 7)] <- NA
mean(rain_2024$rain_mm, na.rm = TRUE)

library(ggplot2)

#Q19
ggplot(rain_2024, aes(x = month, y = rain_mm, group = 1)) +
  geom_line(color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Monthly Rainfall 2024", x = "Month", y = "Rain (mm)") +
  theme_minimal()

#Q20
cashbook %>% group_by(category) %>% summarise(total_amount = sum(amount)) %>%
  ggplot(aes(x = category, y = total_amount, fill = category)) +
  geom_col() +
  labs(title = "Total Spend by Category", x = "Category", y = "Total Amount") +
  theme_minimal()

#Q21
ggplot(pocket, aes(y = total_spend)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Daily Total Spend", y = "Total Spend") +
  theme_minimal()

#Q22
ggplot(rain_week, aes(x = day, y = rain_mm, fill = city)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) +
  labs(title = "Weekly Rainfall by City", x = "Day", y = "Rain (mm)") +
  theme_minimal()

#Q23
ggplot(sip_log, aes(x = month, y = nav, group = 1)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  labs(title = "SIP NAV Trend", x = "Month", y = "NAV") +
  theme_minimal()

#Q24
readings_joined %>% ggplot(aes(x = date, y = rain_mm, color = station_id, group = station_id)) +
                    geom_line() +
                    geom_point() +
                    facet_wrap(~ zone) +
                    labs(title = "Rainfall Readings by Station Zone", x = "Date", y = "Rain (mm)") +
                    theme_minimal()

#Q25
cashbook %>% group_by(category) %>% summarise(total_amount = sum(amount)) %>%
  ggplot(aes(x = reorder(category, -total_amount), y = total_amount, fill = category)) +
  geom_col() +
  labs(title = "Total Spend by Category (Ordered)", x = "Category", y = "Total Amount") +
  theme_minimal()

#Q26
ggplot(sip_log, aes(x = month, y = nav, group = 1)) +
  geom_point(color = "purple", size = 3) +
  geom_line(color = "purple") +
  labs(title = "SIP NAV Over Time",
       x = "Month (Jan–Jun)",
       y = "NAV Value") +
  theme_minimal()











