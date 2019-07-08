#' @title Plots the number of CRAN packages over time



plot_cran_packages <- function(){

if(!require("pacman")) install.packages("pacman")
library(pacman)

## install other packages
p_load(zoo,
       plotly,
       tidyverse,
       rvest,
       XML,
       plotly
       )

## get packages data url
url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"

## extract data from url
page <- read_html(url)

page %>%
  html_node("table") %>%
  html_table() %>%
  mutate(count = rev(1:nrow(.))) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Month = format(Date, format="%Y-%m")) %>%
  group_by(Month) %>%
  summarise(published = min(count)) %>%
  mutate(Date = as.Date(as.yearmon(Month))) -> pkgs

## pkgs_summary
pkgs_summary <- pkgs %>%
  mutate(
  Year = lubridate::year(Date)
  ) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarize(total_pkgs_year = mean(published)) %>%
  dplyr::filter(Year != "2019")

## plot
plot <- pkgs_summary %>%
  ggplot(aes(x = Year, y = total_pkgs_year)) + 
  geom_point(size = 2.5, colour = "darkblue") + 
  geom_line(colour = "steelblue", size = 1.5) + 
  theme_bw() +
  labs(title = paste("CRAN packages published from",
                     min(pkgs_summary$Year), "to", Sys.Date())) +
  xlab("Year") +
  ylab("Total packages/year") +
  scale_x_continuous(breaks=seq(2006, 2018, 2)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
  
# plot

#ggsave("./images/cran_pkgs_published.png", plot = plot)
#margins = list(l = 100, r = 100, b = 100, t = 100, pad = 4)

return(plot)

}
