# Library
library(tidyverse)
library(gt)

# Dataset
tt <- tidytuesdayR::tt_load("2020-08-25")
chopped_data <- tt$chopped

# Tidy Data
tidy_data <- chopped_data %>% 
  select(season, season_episode, episode_name, episode_rating,
         episode_name, appetizer, entree, dessert, air_date) %>% 
  group_by(season) %>% 
  mutate(appetizer = str_to_title(appetizer),
         entree = str_to_title(entree),
         dessert = str_to_title(dessert),
  ) %>%
  ungroup() %>% 
  rename(
    'Season' = season,
    'Name' = episode_name,
    'Rating' = episode_rating,
    'Number' = season_episode,
    'Appetizer' = appetizer,
    'Entree' = entree,
    'Dessert' = dessert,
    'Date' = air_date)

# Table
tidy_data %>% 
  filter(Rating >= 9.1) %>% 
  gt() %>% 
  tab_header(
    title = md("**The Best Episodes of Chopped**"),
    subtitle = md("Chopped is an American reality-based cooking television game show series created by Michael Krupat, Dave Noll and Linda Lea. It is hosted by Ted Allen. The series pits four chefs against each other as they compete for a chance to win $10,000. The format of the show pits four chefs against each other for a chance to win $10,000. There are three rounds - appetizer, entree, and dessert. The chefs receive a basket containing four mystery ingredients that they must use to create a dish. One contestant is eliminated at the end of each round until the last one standing wins.")
  ) %>% 
  tab_source_note(
    source_note = md("<img src = https://upload.wikimedia.org/wikipedia/commons/7/7c/Kaggle_logo.png width = 45/> <img src = https://upload.wikimedia.org/wikipedia/commons/thumb/6/69/IMDB_Logo_2016.svg/1200px-IMDB_Logo_2016.svg.png width = 45/>")
  ) %>% 
  tab_spanner(
    label = "Episode",
    columns = vars(Season, Number, Name, Rating)
  ) %>% 
  tab_spanner(
    label = "Round",
    columns = vars(Appetizer, Entree, Dessert)
  ) %>% 
  cols_align(
    align = "center",
    columns = vars(Number, Name, Rating, Appetizer, 
                   Entree, Dessert, Date, Season)
  ) %>% 
  tab_style(
    style = cell_text(size = px(16),
                      font = google_font(name = "Merriweather"),
                      color = "#af4a14"),
    locations = cells_body(
      columns = vars(Number, Name, Rating, Appetizer, 
                     Entree, Dessert, Date, Season)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "#963f11",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )%>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "#963f11",
        weight = px(3)
      )
    ),
    locations = list(
      cells_title("subtitle")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        font = google_font(name = "Merriweather"),
        align = "left",
        size = px(26),
        color = "#af4a14",
        weight = "bold",
        transform = "uppercase"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        font = google_font(name = "Merriweather"),
        align = "left",
        size = px(18),
        color = "#EF7215"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>% 
  tab_style(
    style = cell_text(size = px(16),
                      font = google_font(name = "Merriweather"),
                      color = "#af4a14",
                      weight = "bold"),
    locations = cells_column_spanners(
      c("Episode", "Round")
    )
  ) %>% 
  tab_style(
    style = cell_text(size = px(16),
                      font = google_font(name = "Merriweather"),
                      color = "#af4a14",
                      weight = "bold"),
    locations = cells_column_labels(
      columns = gt::everything()
    )
  ) %>% 
  tab_options(
    table.background.color = "#FEDEBE",
    table.width = "100%"
  ) %>% 
  gtsave("chopped_table.png")