# plots_small.R

library(hrbrthemes)
library(ggforce)



plot_1 <- function(data) {
  
  ggplot(
    drop_na(data, mpaa_rating),
    aes(fct_reorder(genre, mpaa_rating, function(x) mean(x == "R")),
      fill = mpaa_rating
    )
  ) +
    geom_bar(position = "fill", color = NA) +
    scale_fill_viridis_d("MPAA rating", 
                         option = "E",
                         end=0.9,
      guide = guide_legend(
        title.position = "top",
        direction = "horizontal",
        reverse = TRUE
      )
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_ipsum() +
    labs(
      y = "Percentage of movies relased",
      x = "Genre",
      title = "Hollywood movies by MPAA rating and genre",
    ) +
    theme(legend.position = "bottom")
}


plot_2 <- function(data) {
  
  data <- data %>%
    group_by(season,genre) %>%
    summarize(ratio = mean(ratio))
  
  ggplot(data, aes(x = ratio, y = fct_rev(season), color = genre)) +
    geom_point(size = 8) +
    theme_ipsum() +
    geom_vline(xintercept = 1) +
    scale_color_viridis_d("Genre",
      guide = guide_legend(direction = "horizontal")
    ) +
    labs(
      x = "Worldwide gross/Budget",
      y = "Season",
      title = "Return on investment of movies by season",
    ) +
    scale_x_continuous(breaks = c(1, 5, 10), limits = c(0, NA)) +
    theme(legend.position = "bottom")
}


plot_3 <- function(data) {
  
  ggplot(data, aes(y = fct_rev(fct_infreq(genre)), x = season)) +
    geom_bin2d() +
    scale_fill_viridis_c("Number of movies",
      guide = guide_legend(reverse = TRUE)
    ) +
    theme_ipsum() +
    labs(
      x = "Season",
      y = "Genre",
      title = "Number of movies by season and genre"
    )
}



plot_4 <- function(data) {
  
  data <- data %>%
    group_by(decade) %>%
    summarize(ratio = mean(domestic_gross / worldwide_gross, na.rm = TRUE))


  ggplot(data, aes(decade, ratio)) +
    geom_point(size = 8, color = "skyblue") +
    theme_ipsum() +
    geom_hline(yintercept = 0.5, size = 1) +
    labs(
      x = "Decade",
      y = "Domestic gross percentage of the total",
      title = "Domestic and worldwide gross compared"
    ) +
    scale_y_continuous(
      labels = scales::label_percent(),
      limits = c(0.4, 1),
      breaks = c(0.5, 0.75, 1)
    )
}


plot_5 <- function(data) {
  
  ggplot(data = subset(data, ratio < 150), aes(year, ratio)) +
    geom_jitter(alpha = 0.7, color = "grey") +
    scale_y_continuous(breaks = c(1, 100, 200, 300, 400), limits = c(1, 500)) +
    theme_ipsum() +
    geom_mark_ellipse(
      data = subset(data, ratio > 150),
      aes(
        filter = ratio > 400,
        description = "The Blair Witch Project 1999 \nParanormal Activity 2007"
      ),
      label.fill = NA,
      con.type = "straight",
      label.family = "Arial Narrow",
      label.fontsize = 11
    ) +
    geom_jitter(
      data = subset(data, ratio > 150),
      aes(year, ratio, color = genre),
      size = 3
    ) +
    scale_color_viridis_d("Genre",limits = unique(data$genre),
                          breaks = unique(subset(data, ratio>150)$genre)) +
    geom_mark_ellipse(
      data = subset(data, ratio > 150),
      aes(
        filter = ratio > 180 & ratio < 400,
        description = "American Graffitti 1973\nRocky 1976\nHalloween 1978"
      ),
      con.type = "straight",
      label.fill = NA,
      label.family = "Arial Narrow",
      label.fontsize = 11
    ) +
    labs(
      x = "Year",
      y = "Worldwide Gross / Budget",
      title = "Most profitable movies"
    )
}



plot_6 <- function(data) {
  
  data <- data %>%
    group_by(mpaa_rating, genre) %>%
    summarise(count = n()) %>%
    gather_set_data(c(1:2)) %>%
    drop_na()


  ggplot(data, aes(x, id = id, split = y, value = count)) +
    geom_parallel_sets(aes(fill = mpaa_rating),
      strength = 0.5,
      axis.width = 0.32,
      alpha = 0.9,
      color = "grey50"
    ) +
    geom_parallel_sets_axes() +
    geom_parallel_sets_labels(angle = 0) +
    theme_ipsum() +
    scale_fill_viridis_d("MPAA rating",option="E",end=0.9,
      guide = guide_legend(direction = "vertical")
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_discrete(labels = c("Genre", "MPAA rating")) +
    labs(
      title = "Hollywood movies by MPAA rating and genre"
    )
}
