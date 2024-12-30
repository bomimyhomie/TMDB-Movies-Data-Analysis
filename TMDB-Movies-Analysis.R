library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

setwd('C:/Users/vlad7/NYCDS/R/Shiny Project')

movies_raw = read.csv('TMDB_movie_dataset_v11.csv')
movies = movies_raw

#Initial exploratory data analysis
dim(movies)
summary(movies)

unique(movies$status)
unique(movies$adult)
unique(movies$original_language)
unique(movies$production_countries)

#Data cleaning and preparation
#Convert release date to date type
movies$release_date = as.Date(movies$release_date)
class(movies$release_date)

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

#Filter out movies with release year after 2024 and movies with genre
movies = movies %>% filter(Year >= 1900 & Year <= 2024, genres != "")

#Count numbwer of movies per year and plot as line chart
movies %>% group_by(Year) %>%
  summarise(movie_count = n()) %>%
  ggplot(aes(x=Year, y=movie_count)) +
  geom_line(color = 'blue') +
  labs(title = 'Number of Movies Per Year', x = 'Year', y = 'Movie Count') +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color='black'),
    axis.ticks = element_line(color = "black")
  )

#User input genre and can see number of movies per year
movies %>% group_by(Year, main_genre) %>%
  summarise(movie_count = n()) %>%
  filter(main_genre == 'Action')
#use filter function with user input to select year?

#Filter out movies with no genre, number of films by genre
movies %>% filter(main_genre != "") %>%
  group_by(main_genre, year) %>%
  summarise(num_movies = n()) %>%
  ggplot(aes(x=main_genre))





