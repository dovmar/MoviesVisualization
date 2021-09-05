# plots_panel.R


library(ggrepel)
library(vapoRwave)


panel_1 <- function(data) {
  

  data <- data %>%
    group_by(genre) %>%
    summarize(total = sum(worldwide_gross)) %>%
    mutate(perc = round(total / sum(total) * 100, 0)) %>%
    mutate(label = paste0(round(total / sum(total) * 100, 0), "%")) %>%
    arrange(desc(genre))

  height <- cumsum(data$perc) - 5
  height[1] <- height[1] + 5
  height[5] <- height[5] - 15
  height[4] <- height[4] - 5
  df <- data.frame(x = 2.5, y = height, label = data$label, perc = data$perc)

  p1 <- ggplot(data, aes(1, y = perc, fill = genre)) +
    geom_bar(stat = "identity", width = 1.2) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(0,0)) +
    xlim(-0.9, 2.2) +
    scale_fill_vapoRwave("") +
    theme_void(base_size = 20, base_family = "Roboto Condensed") +
    coord_polar(theta = "y") +
    guides(label = "none") +
    labs(title = "Percentage of total gross by genre") +
    theme(plot.margin = margin(0, 0, 0, 0), legend.position = "none") +
    geom_text(data = df, aes(x = 2.2, y = height, label = label), inherit.aes = FALSE, color = "grey20", size = 6)
  p1
}



panel_2 <- function(data) {
  
  top3 <- data %>%
    arrange(desc(production_budget)) %>%
    slice_max(production_budget, n = 3)
  top3$movie[2] <- "Who Framed\nRoger Rabbit?"
  top3$movie <- paste0(top3$movie, "\n", "(", top3$year, ")")
  top3$movie <- factor(top3$movie, levels = top3$movie[c(2, 1, 3)])

  yloc <- top3$production_budget
  yloc[1] <- yloc[1] * 1.01
  yloc[2] <- yloc[2] / 1.01
  yloc[3] <- yloc[3] / 1.02

  top3$genre <- factor(top3$genre, levels = c(
    "Drama", "Comedy", "Adventure", "Horror",
    "Action"
  ), )
  p2 <- ggplot(top3, aes(movie, production_budget, fill = genre)) +
    geom_bar(stat = "identity", alpha = 1) +
    theme_minimal(base_size = 20, base_family = "Roboto Condensed") +
    scale_fill_vapoRwave(drop = FALSE) +
    guides(fill = "none", color = "none") +
    labs(x = "", y = "", title = "Movies with the highest budget") +
    scale_y_continuous(
      breaks = yloc,
      labels = paste(round(top3$production_budget / 10^6, 1), "mil."),
      limits = c(0, max(top3$production_budget) * 1.06)
    ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(10^8 * 1.1, NA))
  p2
}


panel_3 <- function(data) {
  
  xintercepts <- as.Date(c("1980-01-01", "1990-01-01"))

  p3 <- ggplot(data, aes(release_date, worldwide_gross, color = genre, size = worldwide_gross, alpha = worldwide_gross)) +
    geom_point() +
    theme_minimal(base_size = 20, base_family = "Roboto Condensed") +
    scale_color_vapoRwave("") +
    theme(legend.position = "none", panel.grid.major = element_blank(), plot.margin = margin(t = 0, 5.5, b = 0, 5.5)) +
    annotate(
      geom = "rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1990-01-01"),
      ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.05
    ) +
    labs(x = "", y = "Worldwide gross", title = "Wordwide gross of movies") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_alpha(range = c(0.5, 1)) +
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20) * 10^8, labels = paste(c("0", "0.5", "1", "1.5", "2"), "mlrd."), limits = c(0, 24 * 10^8)) +
    geom_vline(data = as.data.frame(xintercepts), aes(xintercept = xintercepts)) +
    scale_size_area(max_size = 12) +
    geom_text_repel(data = slice_max(data, worldwide_gross, n = 8), aes(release_date, worldwide_gross, label = movie), inherit.aes = FALSE, nudge_y = 300000000, min.segment.length = 3, size = 5)
  p3
}



panel_4 <- function(data) {
  
  
  xintercepts <- as.Date(c("1980-01-01", "1990-01-01"))
  
  p4<-ggplot(
    data,
    aes(release_date, production_budget, color = genre, size = production_budget, alpha = worldwide_gross)
  ) +
    geom_point() +
    theme_minimal(base_size = 20, base_family = "Roboto Condensed") +
    guides(alpha = "none", size = "none") +
    scale_color_vapoRwave("", guide = guide_legend(
      keyheight = unit(7, units = "mm"),
      keywidth = unit(15, units = "mm"),
      label.position = "bottom", title.position = "top",
      nrow = 1, title.hjust = 0.5, override.aes = list(size = 10)
    )) +
    theme(legend.position = "bottom", panel.grid.major = element_blank(), plot.margin = margin(t = 0)) +
    annotate(
      geom = "rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1990-01-01"),
      ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.05
    ) +
    labs(x = "Year", y = "Budget", title = "Movie budgets") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
    scale_alpha(range = c(0.5, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1, 1.5) * 10^8, labels = paste(c("0", "50", "100", "150"), "mil."), limits = c(0, 1.7 * 10^8)) +
    geom_vline(data = as.data.frame(xintercepts), aes(xintercept = xintercepts)) +
    scale_size_area(max_size = 8)
  p4
}
