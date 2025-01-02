library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinydashboard) 
library(scales)

#Source: https://www.kaggle.com/datasets/asaniczka/tmdb-movies-dataset-2023-930k-movies
setwd('C:/Users/vlad7/NYCDS/R/Shiny Project')

#Load dataset and make a copy
movies_raw = read.csv('TMDB_movie_dataset_v11.csv')
movies = movies_raw

#Data cleaning and preparation
#Convert release date to date type
movies$release_date = as.Date(movies$release_date)

#Move revenue and budget columns to the end
movies = movies %>% select(-revenue, revenue) %>%
  select(-budget, budget)

#Add profit column (revenue - budget)
movies = movies %>% mutate(profit = revenue - budget)

#Obtain the first element of the genres, spoken_languages, production_companies, and production_countries columns
movies = movies %>%
  mutate(main_genre = sapply(str_split(genres, ", "), `[`, 1),
         main_spoken_lang = sapply(str_split(spoken_languages, ", "), `[`, 1),
         main_prod_comp = sapply(str_split(production_companies, ", "), `[`, 1),
         main_prod_country = sapply(str_split(production_countries, ", "), `[`, 1))

#Create month and year columns
movies = movies %>% mutate(Month = month(release_date), Year = year(release_date))

#Create filtered table for app
movies_shiny = movies

#Remove unnecessary columns
movies = movies %>% select(-id, -backdrop_path, -poster_path, -imdb_id)

#Remove unnecessary columns
movies_shiny = movies_shiny %>% select(-id, -adult, -original_language, -original_title,
                                       -overview, -tagline, -spoken_languages, -keywords,
                                       -backdrop_path, -poster_path, -imdb_id)

#Filter out movies with release year after 2024 and movies with genre
movies = movies %>% filter(Year >= 1900 & Year <= 2024, genres != "")
movies_shiny = movies_shiny %>% filter(year(release_date) >= 1900 & year(release_date) <= 2024, genres != "")

########################################################################################################
#Summary table for ratings distribution  
summary_table <- movies_shiny %>%
  filter(vote_count > 0) %>%
  group_by(main_genre) %>%
  summarise(
    Mean_Rating = mean(vote_average, na.rm = TRUE),
    Median_Rating = median(vote_average, na.rm = TRUE),
    SD_Rating = sd(vote_average, na.rm = TRUE),
    Min_Rating = min(vote_average, na.rm = TRUE),
    Max_Rating = max(vote_average, na.rm = TRUE),
    Count = n()
  ) %>%
  mutate(
    Mean_Rating = format(round(Mean_Rating, 2), nsmall = 2),
    Median_Rating = format(round(Median_Rating, 2), nsmall = 2),
    SD_Rating = format(round(SD_Rating, 2), nsmall = 2),
    Min_Rating = Min_Rating,
    Max_Rating = Max_Rating
  ) %>%
  rename(
    Genre = main_genre,
    `Average Rating` = Mean_Rating,
    `Median Rating` = Median_Rating,
    `Standard Deviation` = SD_Rating,
    `Minimum Rating` = Min_Rating,
    `Maximum Rating` = Max_Rating,
    `Movie Count` = Count
  )


#Correlation matrix
cor_matrix <- movies_shiny %>%
  select(vote_average, budget, revenue, profit, runtime) %>%
  rename(
    `Average Rating` = vote_average,
    `Budget` = budget,
    `Revenue` = revenue,
    `Profit` = profit,
    `Runtime` = runtime
  ) %>%
  na.omit() %>%
  cor()








