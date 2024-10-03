# Load necessary libraries
library(tidyverse)

# Read in the Spotify dataset from Kaggle
spotify_data <- read_csv("data/spotify_songs.csv")

# View the first few rows of the data (optional)
head(spotify_data)

# Summary of the response variable (track_popularity)
popularity_summary <- spotify_data %>%
  summarize(
    mean_popularity = mean(track_popularity, na.rm = TRUE),
    median_popularity = median(track_popularity, na.rm = TRUE),
    min_popularity = min(track_popularity, na.rm = TRUE),
    max_popularity = max(track_popularity, na.rm = TRUE),
    sd_popularity = sd(track_popularity, na.rm = TRUE)
  )
print("Popularity Summary")
print(popularity_summary)

# Summary of the predictor variables (duration, tempo, valence, speechiness, loudness)
predictor_summary <- spotify_data %>%
  summarize(
    mean_duration = mean(duration_ms, na.rm = TRUE),
    median_duration = median(duration_ms, na.rm = TRUE),
    sd_duration = sd(duration_ms, na.rm = TRUE),
    
    mean_tempo = mean(tempo, na.rm = TRUE),
    median_tempo = median(tempo, na.rm = TRUE),
    sd_tempo = sd(tempo, na.rm = TRUE),
    
    mean_valence = mean(valence, na.rm = TRUE),
    median_valence = median(valence, na.rm = TRUE),
    sd_valence = sd(valence, na.rm = TRUE),
    
    mean_speechiness = mean(speechiness, na.rm = TRUE),
    median_speechiness = median(speechiness, na.rm = TRUE),
    sd_speechiness = sd(speechiness, na.rm = TRUE),
    
    mean_loudness = mean(loudness, na.rm = TRUE),
    median_loudness = median(loudness, na.rm = TRUE),
    sd_loudness = sd(loudness, na.rm = TRUE)
  )
print("Predictor Variables Summary")
print(predictor_summary)

# Visualize distributions of predictors (optional)
spotify_data %>%
  select(duration_ms, tempo, valence, speechiness, loudness) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Predictor Variables", x = "Value", y = "Frequency")
