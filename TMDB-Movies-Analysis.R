library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)
library(viridis)

setwd('C:/Users/vlad7/NYCDS/R/Shiny Project')

#Source: https://www.kaggle.com/datasets/asaniczka/tmdb-movies-dataset-2023-930k-movies

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

#Create filtered table for app
movies_shiny = movies

#Remove unnecessary columns
movies = movies %>% select(-id, -backdrop_path, -poster_path, -imdb_id)

#Remove unnecessary columns
movies_shiny = movies_shiny %>% select(-id, -adult, -original_language, -original_title,
                                       -overview, -tagline, -spoken_languages, -keywords,
                                       -backdrop_path, -poster_path, -imdb_id)

#Create month and year columns
movies = movies %>% mutate(Month = month(release_date), Year = year(release_date))

#Filter out movies with release year between 1900 and 2024 and movies with no genre
movies = movies %>% filter(Year >= 1900 & Year <= 2024, genres != "")
movies_shiny = movies_shiny %>% filter(year(release_date) >= 1900 & year(release_date) <= 2024, genres != "")

#Filter out movies with release year between 1900 and 2024 and movies with no genre
movies = movies %>% filter(Year >= 1900 & Year <= 2024, genres != "")

#Count number of movies per year and plot as line chart
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

#Bar plot of movies by genre over time
movies %>%
  group_by(Year, main_genre) %>%
  summarise(movie_count = n()) %>%
  ggplot(aes(x = Year, y = movie_count, fill = main_genre)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = 'Number of Movies by Genre Over Time', x = 'Year', y = 'Movie Count') +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = 'black'),
    axis.ticks = element_line(color = "black")
  )

#User input genre and can see number of movies per year
movies %>% group_by(Year, main_genre) %>%
  summarise(movie_count = n()) %>%
  filter(main_genre == 'Action')
#use filter function with user input to select year?

#Create bar plots for Top 10 Genres
movies_shiny %>%
  group_by(main_genre) %>%
  summarise(Budget = sum(as.numeric(budget), na.rm = TRUE)) %>%
  arrange(desc(Budget)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = Budget, y = main_genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Genres by Budget", x = "Budget", y = "Genre") +
  theme_minimal()

#Scatter plot (y = vote_average)
ggplot(data = movies,mapping = aes(x = vote_average,y = revenue)) +
  theme_bw() +
  geom_point() +
  ggtitle("Average Vote vs. Revenue") +
  xlab("Average Vote") +
  ylab("Revenue ($)")

movies_shiny %>%
  ggplot(aes(x = log(profit + 1), y = vote_average)) +
  geom_point()

#Violin plot of ratings distribution for each genre
movies_shiny %>%
  filter(vote_count > 0) %>%
ggplot(aes(x = main_genre, y = vote_average, fill = main_genre)) +
  geom_violin(trim = FALSE) + 
  labs(title = "Distribution of Average Rating by Genre", 
       x = "Genre", 
       y = "Average Rating") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = 'black'),
    axis.ticks = element_line(color = "black"),
    legend.position = "none"
  )


movies_shiny %>%
  filter(vote_count > 0) %>%
  group_by(main_genre, Year) %>%
  summarise(Average_Rating = mean(vote_average, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = main_genre, fill = Average_Rating)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma") +
  labs(title = "Heatmap of Average Rating by Genre and Year", 
       x = "Year", 
       y = "Genre") +
  theme_minimal()


cor_matrix <- movies_shiny %>%
  select(vote_average, budget, revenue, profit, runtime) %>%
  na.omit() %>%
  cor()


