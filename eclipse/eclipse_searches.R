
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(janitor)
library(patchwork)
library(rnaturalearth)
library(rvest)
library(shadowtext)



# Read data ---------------------------------------------------------------

# Spain map data
spain <- 
  ne_countries(scale = "medium") |> 
  filter(sovereignt == "Spain") |> 
  ggplot() +
  geom_sf(fill = "grey10") +
  ylim(c(35, 45)) +
  xlim(c(-10, 5)) +
  theme_void()

# Search data from Google Trends:
# https://trends.google.com/explore?q=%22me%20duelen%20los%20ojos%22&date=now%201-d&geo=ES
trends <- 
  tibble(
    time = c("19:20", "19:36", "19:52", "20:08", "20:24", "20:40", "20:56", "21:12", "21:28", "21:44", "22:00", "22:16", "22:32", "22:48", "23:04"),
    n = c(0, 0, 31, 37, 81, 100, 72, 37, 28, 33, 36, 20, 27, 0, 0)
  ) |> 
  mutate(time = as_datetime(paste0("2026-08-12 ", time, ":00")))

# Wikipedia data on solar eclipse times
url <- "https://en.wikipedia.org/wiki/Solar_eclipse_of_August_12,_2026"

# Get table of times from Wikipedia article
eclipse <- 
  read_html(url) |> 
  html_table()

# Filter to Spain only and clean up data
spain_eclipse <- 
  eclipse |> 
  _[[5]] |> 
  clean_names() |> 
  filter(country_or_territory == "Spain") |> 
  mutate(end_of_partial_eclipse = str_remove(end_of_partial_eclipse, " (sunset)")) |> 
  mutate(across(3:7, \(x) as_datetime(paste0("2026-08-12 ", x))))

# Minute-wise timing across relevant time window
times <- 
  tibble(
    time = expand_grid(h = c(19, 20, 21, 22), 
                       m = str_pad(seq(0, 59), width = 2, pad = "0")) |> 
      mutate(d = paste0(h, ":", m)) |> 
      pull(d)
  ) |> 
  mutate(time = as_datetime(paste0("2026-08-12 ", time, ":00")))

# Match partial eclipse times and coverage (cities listed)
partial_times <-
  times |> 
  left_join(spain_eclipse, by = join_by(between(time, start_of_partial_eclipse, end_of_partial_eclipse))) |> 
  summarize(n = n(), .by = time) |> 
  mutate(prop = n / max(n))

# Match total eclipse times and coverage (cities listed)
total_times <-
  times |> 
  left_join(spain_eclipse, by = join_by(between(time, start_of_total_eclipse, end_of_total_eclipse))) |> 
  summarize(n = n(), .by = time) |> 
  mutate(prop = n / max(partial_times$n))



# Plot data ---------------------------------------------------------------

# Plot eclipse time series and search trends
eclipse_plot <- 
  ggplot() +
  geom_bar(data = partial_times,
           aes(x = time, y = prop * 100, fill = prop, alpha = prop),
           stat = "identity",
           show.legend = FALSE) +
  geom_rect(data = spain_eclipse,
            aes(xmin = start_of_total_eclipse, xmax = end_of_total_eclipse, ymin = 0, ymax = 100),
            fill = "black", alpha = .3) +
  geom_line(data = trends, aes(x = time, y = n), color = "firebrick", linewidth = 1.5) +
  geom_point(data = trends, aes(x = time, y = n), color = "firebrick", size = 3) +
  annotate("text", x = I(c(.27, .5)), y = I(.07), label = "Partial\neclipse", 
           color = "orange2", size = 3.5, lineheight = .65) +
  annotate("text", x = I(.5 - (.5 -.27) / 2), y = I(.07), label = "Total\neclipse", 
           color = "grey30", size = 3.5, lineheight = .68) +
  annotate("text", x = I(.67), y = I(.2), label = "Number of Google\nsearches in Spain", 
           color = "firebrick", hjust = 0, size = 4.5, lineheight = .7) +
  annotate("shadowtext", x = I(.2), y = I(.88), label = "Passage & coverage\nof eclipse across\nSpain", 
           color = "orange", bg.color = "lightyellow2", hjust = 0, size = 3, lineheight = .7) +
  scale_fill_gradient2(low = "lightyellow", mid = "yellow", high = "orange", midpoint = .3) +
  scale_x_datetime(limits = as_datetime(c("2026-08-12 19:00:00", "2026-08-12 23:00:00"))) +
  scale_y_continuous(limits = c(-10, 100)) +
  labs(x = "Local time", y = NULL, caption = "<span style='color:grey40'>Data:</span> Google Trends & Wikipedia | <span style='color:grey40'>Packages:</span>  {tidyverse, ggtext, janitor, patchwork, rnaturalearth, rvest, shadowtext} | <span style='color:grey40'>Visualization:</span> C. Börstell",
       title = "Google searches for the phrase \"*<span style='color:firebrick'>me duelen los ojos</span>*\" ('my eyes hurt') in Spain around the solar eclipse on August 12, 2026") +
  guides(fill = "none",
         x = guide_axis(cap = "both"),
         y = guide_axis(cap = "both")) +
  theme_classic(base_size = 16, base_family = "Archivo Narrow", paper = "aliceblue") +
  theme(axis.line = element_line(lineend = "square"),
        axis.ticks = element_line(lineend = "square"),
        axis.title.x = element_text(size = rel(.8), 
                                    hjust = .98,
                                    color = "grey30"),
        plot.caption = element_markdown(size = rel(.5), color = "grey60", hjust = 0),
        plot.title = element_textbox(size = rel(1.2), 
                                     width = 1, family = "Archivo Narrow"),
        plot.title.position = "plot")

# Combine with Spain map and add an eclipse to Spain
eclipse_plot + inset_element(spain, 
                             left = I(.7), 
                             bottom = I(.3), 
                             right = I(1), 
                             top = I(1.1)) +
  annotate("point", x = I(.45), y = I(.5), color = "white", size = 15) +
  annotate("point", x = I(.45), y = I(.5), color = "orange", size = 14.5, alpha = .2) +
  annotate("point", x = I(.45), y = I(.5), color = "grey20", size = 14)


# Save plot
ggsave("eclipse.png", width = 7.1, height = 4, units = "in", dpi = 600)


