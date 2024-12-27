library(tidyverse)
library(ggplot2)
library(ggrepel)

astro <- read.csv("C:/Users/hilary.barker/Downloads/astro_1_row.csv")
missions <- read.csv("C:/Users/hilary.barker/Downloads/missions.csv")



astronauts_separated <- astro %>%
  separate_rows(Missions, sep = "/")

combined_data <- astronauts_separated %>%
  left_join(missions, by = c("Missions" = "HSFTAG"))



convert_to_seconds <- function(duration) {
  parts <- strsplit(duration, ":")[[1]]
  days <- as.numeric(parts[1])
  hours <- as.numeric(parts[2])
  minutes <- as.numeric(parts[3])
  seconds <- as.numeric(parts[4])
  total_seconds <- days * 86400 + hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}

bin_duration <- function(duration) {
  if (is.na(duration)) {
    return(NA)
  } else if (duration < 3600) {
    return('less than 1 hour')
  } else if (duration <= 604800) {  # 1 week = 604800 seconds
    return('1 hour to 1 week')
  } else if (duration <= 4320000) {  # 50 days = 4320000 seconds
    return('1 week to 50 days')
  } else if (duration <= 31536000) {  # 1 year = 31536000 seconds
    return('50 days to 1 year')
  } else {
    return('more than 1 year')
  }
}

#1110:15:00:11

clean_citizen <- function(citizen) {
  if (citizen %in% c("US", "USA", "US?")) {
    return("USA")
  } else if (citizen == "RU") {
    return("Russia")
  } else if (citizen == "CN") {
    return("China")
  } else {
    return("Other")
  }
}

combined_data2 <- combined_data %>%
  mutate(DurationSeconds = sapply(Duration, convert_to_seconds),
         CitizenCleaned = sapply(Citizen, clean_citizen))

combined_data3 <- combined_data2 %>%
  group_by(Name) %>%
  summarise(
    FirstMissionYear = min(LDate, na.rm = TRUE),
    LastMissionYear = max(LDate, na.rm = TRUE)
  )

astro2 <- astro %>%
  left_join(combined_data3, by = "Name")

astro2 <- astro2 %>%
  mutate(DurationSeconds = sapply(Duration, convert_to_seconds),
         CitizenCleaned = sapply(Citizen, clean_citizen))

astro2 <- astro2 %>%
  mutate(DurationBinned = sapply(DurationSeconds, bin_duration))


ggplot(astro2, aes(x = FirstMissionYear, y = DurationSeconds, color = CitizenCleaned, group = CitizenCleaned)) +
  geom_line() +
  labs(title = "Duration Over Years by Citizen",
       x = "Year",
       y = "Duration",
       color = "Citizen") +
  theme_minimal()

astro2 <- astro2 %>%
  filter(!is.na(DurationBinned))

astro2$DurationBinned <- factor(astro2$DurationBinned, levels = c('less than 1 hour', '1 hour to 1 week', '1 week to 50 days', '50 days to 1 year', 'more than 1 year'))

astronauts_count <- astro2 %>%
  group_by(FirstMissionYear, CitizenCleaned, DurationBinned) %>%
  summarise(Count = n())

labels <- astronauts_count %>%
  group_by(CitizenCleaned, DurationBinned) %>%
  filter(FirstMissionYear == max(FirstMissionYear)) %>%
  mutate(FirstMissionYear = 2035) %>%
  ungroup() %>%
  as.data.frame()

custom_colors <- c("less than 1 hour" = "#FF4500", "1 hour to 1 week" = "#FF8C00", "1 week to 50 days" = "gray", "50 days to 1 year" = "#1E90FF", "more than 1 year" = "#0000FF")

ggplot(astronauts_count, aes(x = FirstMissionYear, y = Count, color = DurationBinned, group = DurationBinned)) +
  geom_line(size = 1.3) +
 # geom_text_repel(data = labels, aes(label = DurationBinned), nudge_x = 0.5, direction = "y", hjust = "right") + # Add labels at the end of the lines
  facet_wrap(~ CitizenCleaned) +
  scale_color_manual(values = custom_colors) + # Use custom colors
  labs(title = "Count of astronauts summarized by their time spent in space.",
       x = "Year",
       y = "Number of Astronauts",
       color = "Duration in Space") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 14, face = "bold"), # Make facet labels larger and bold
        axis.line.x = element_blank(), # Remove x-axis lines
        panel.grid.major.x = element_blank(), # Remove major x-axis grid lines
        panel.grid.minor.x = element_blank(), # Remove minor x-axis grid lines
        panel.grid.major.y = element_line(color = "grey90"), # Keep major y-axis grid lines
        panel.grid.minor.y = element_line(color = "grey90"), # Keep minor y-axis grid lines
        axis.title = element_text(size = 14, face = "bold"), # Increase font size and make axis titles bold
        axis.text = element_text(size = 12)) # Increase font size for axis labels


write.csv(astronauts_count, "C:/Users/hilary.barker/Downloads/astronaut_summary.csv")
