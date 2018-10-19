library(readr)
celtics_weather <- read_csv("celtics_weather.csv")
View(celtics_weather)
redsox_weather <- read_csv("redsox_weather.csv")
View(redsox_weather)
############################################################################################################

library(tidyverse)
### Celtics attendance
## Since there are extreme high temperatures in the dataset, I would exclude the rows with temperature higher than 70F
buoycel_norm <- celtics_weather %>% group_by(ATMP) %>% arrange(ATMP) %>% filter(ATMP<=70)
## Distribution of temperature
ggplot(data = buoycel_norm) +aes(x = ATMP) +geom_histogram(bins = 30, fill = "#0c4c8a") +theme_minimal()
## Overall attndence by temperature
ggplot(data = buoycel_norm) +aes(x = ATMP, y = ATTENDANCE) +geom_point(color = "#0c4c8a") +theme_minimal()
## Overall attendance by temperature, color-filled by Week_day
ggplot(data = buoycel_norm) +aes(x = ATMP, y = ATTENDANCE, color = Day_of_Week) +geom_point() +theme_minimal()
## Attendance for the coldest 20 days
cold10_cel <- buoycel_norm[1:20,]
ggplot(data = cold10_cel) +aes(x = ATMP, y = ATTENDANCE) +geom_point(color = "#0c4c8a") +theme_minimal()
## Attendance for the warmest 20 days
warm10_cel <- tail(buoycel_norm,20)
ggplot(data = warm10_cel) +aes(x = ATMP, y = ATTENDANCE) +geom_point(color = "#0c4c8a") +theme_minimal()
## Attendance for coldest 20 days and warmest 20 days in one graph
cold10_cel2 <- cold10_cel %>% select(ATMP, ATTENDANCE) %>% mutate(indicator = as.factor(rep(1,1)))
warm10_cel2 <- warm10_cel %>% select(ATMP, ATTENDANCE) %>% mutate(indicator = as.factor(rep(0,1)))
cwcel <- rbind(cold10_cel2,warm10_cel2)
ggplot(data = cwcel) +aes(x = ATMP, y = ATTENDANCE, color = indicator) +geom_point() +theme_minimal()
## Attendance by wind speed
ggplot(data = buoycel_norm) +aes(x = WSPD, y = ATTENDANCE) +geom_point(color = "#0c4c8a") +theme_minimal()

#############################################################################################################

### RedSox attendance
## Since there are extreme high temperatures in the dataset, I would exclude the rows with temperature higher than 70F
buoyrs_norm <- redsox_weather %>% group_by(ATMP) %>% arrange(ATMP) %>% filter(ATMP<=70)
## Distribution of temperature
ggplot(data = buoyrs_norm) +aes(x = ATMP) +geom_histogram(bins = 30, fill = "#0c4c8a") +theme_minimal()
## Overall attndence by temperature
ggplot(data = buoyrs_norm) +aes(x = ATMP, y = Attendance) +geom_point(color = "#0c4c8a") +theme_minimal()
## Overall attendance by temperature, color-filled by Week_day
ggplot(data = buoyrs_norm) +aes(x = ATMP, y = Attendance, color = Day_of_Week) +geom_point() +theme_minimal()
## Attendance for the coldest 20 days
cold10_rs <- buoyrs_norm[1:20,]
ggplot(data = cold10_rs) +aes(x = ATMP, y = Attendance) +geom_point(color = "#0c4c8a") +theme_minimal()
## Attendance for the warmest 20 days
warm10_rs <- tail(buoyrs_norm,20)
ggplot(data = warm10_rs) +aes(x = ATMP, y = Attendance) +geom_point(color = "#0c4c8a") +theme_minimal()
## Attendance for coldest 20 days and warmest 20 days in one graph
cold10_rs2 <- cold10_rs %>% select(ATMP, Attendance) %>% mutate(indicator = as.factor(rep(1,1)))
warm10_rs2 <- warm10_rs %>% select(ATMP, Attendance) %>% mutate(indicator = as.factor(rep(0,1)))
cwrs <- rbind(cold10_rs2,warm10_rs2)
ggplot(data = cwrs) +aes(x = ATMP, y = Attendance, color = indicator) +geom_point() +theme_minimal()
## Attenance by wind speed
ggplot(data = buoyrs_norm) +aes(x = WSPD, y = Attendance) +geom_point(color = "#0c4c8a") +theme_minimal()




library(esquisse)
esquisser(cwrs)


