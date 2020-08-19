library(tidyverse)
library(extrafont)
loadfonts()

# Dataset
plants <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

# Tidy Data

tidy_data <- function(plants){
  
  plants %>% 
  dplyr::select(binomial_name, country, continent, 
                year_last_seen, red_list_category) %>%
  dplyr::filter(!is.na(year_last_seen),
                red_list_category == "Extinct in the Wild") %>%
  dplyr::mutate(year_last_seen = factor(year_last_seen, 
                                 levels = c("Before 1900",
                                            "1900-1919",
                                            "1920-1939",
                                            "1940-1959",
                                            "1960-1979",
                                            "1980-1999",
                                            "2000-2020")
                                 )
                ) %>% 
  dplyr::mutate(country =
                  dplyr::case_when(country=="United States" ~ "USA",
                     country=="Viet Nam" ~ "Vietnam",
                     country=="Trinidad and Tobago" ~ "Trinidad",
                     country=="Saint Helena, Ascension and Tristan da Cunha"
                     ~ "Saint Helena",
                     country=="Côte d'Ivoire" ~ "Ivory Coast",
                     country=="Cabo Verde" ~ "Cape Verde",
                     country=="Congo" ~ "Democratic Republic of the Congo",
                     country == "Pitcairn" ~ "Pitcairn Islands",
                     TRUE ~ as.character(country)
                     )
  ) %>% 
  dplyr::group_by(year_last_seen)

}

# Graph

plot_plant <- 
  function(tidy_data){
    ggplot2::ggplot(tidy_data, aes(year_last_seen, 
                                   country, 
                                   label = binomial_name, 
                                   country = country)) +
    ggplot2::geom_point(color = "#012D04", size = 2.0) +
    ggrepel::geom_text_repel(aes(label = binomial_name, 
                                 family = "Garamond"),
                    alpha = 1.0,
                    size = 2.5,
                    hjust = 0,
                    vjust = 0.5,
                    angle = 0,
                    segment.color = "#012D04") +
    ggplot2::theme_minimal(base_family = "Garamond", base_size = 14) +
    ggplot2::labs(title = "Plants extinct in the wild",
                  y = NULL,
                  x = NULL,
                  caption = "Source: IUCN Red List | Graphic: Kenji") +
    ggplot2::theme(
      plot.background = element_rect(fill= "#AAB952", 
                                     color = "#AAB952"),
      panel.grid.major = element_line(color = "#D5D170"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, 
                                vjust = 0.5, 
                                size = 18, 
                                face = "bold", 
                                margin = margin (0,0,20,0),
                                color = "#012D04"),
      axis.text.x = element_text(hjust = 0.5, 
                                 vjust = 0.5, 
                                 size = 12,
                                 margin = margin (0,0,20,0),
                                 color = "#012D04"),
      axis.text.y = element_text(hjust = 0.5, 
                                 vjust = 0.5, 
                                 size = 12,
                                 margin = margin (0,0,20,0),
                                 color = "#012D04"),
      plot.caption = element_text(size = 10,
                                  color = "#012D04")
    )
}

plot_plant(tidy_data(plants)) +
  ggsave("plants.png", dpi = 320, width = 10, height = 12)
