library(tidyverse)
library(readxl)
library(directlabels)
library(extrafont)


#GDP per capita in current LCU
gdp_lcu_cur <-
  read_excel("scripts_n_data/1_gdp_per_capita/chart_data/gdp_per_capita.xlsx",
             sheet = "gdp_per_cap_lcu_cur")

#GDP deflator
deflator <-
  read_excel("scripts_n_data/1_gdp_per_capita/chart_data/gdp_per_capita.xlsx",
             sheet = "deflator")

#LCU to EUR exchange rate in 2016
ex_rate <-
  read_excel("scripts_n_data/1_gdp_per_capita/chart_data/gdp_per_capita.xlsx",
             sheet = "lcu_to_eur")

#countries' names in ua
ua_names <-
  read_excel("reference_data/country_names.xlsx", sheet = 1)

#GDP per capita in constant_2016 EUR
gdp_eur_con <-
  gdp_lcu_cur %>% left_join(deflator, by = c("country", "year")) %>%
  left_join(ex_rate, by = "country") %>%
  mutate(gdp_per_cap_eur_con = gdp_per_cap_lcu_cur * def_2016 / lcu_to_eur)


#countries to show on chart close to Ukraine
nabes <-
  gdp_eur_con[order(-gdp_eur_con$year, gdp_eur_con$gdp_per_cap_eur_con),] %>%
  head(12) %>%
  filter(!(country %in% c("Greece", "Croatia")))

#tidy data
gdp_eur_con <-
  gdp_eur_con %>% left_join(ua_names, by = "country") %>%
  mutate(gdp_per_cap_eur_con_ths = gdp_per_cap_eur_con / 1000)


#stripes like here
#https://www.inwt-statistics.com/read-blog/a-not-so-simple-bar-plot-example-using-ggplot2-507.html
#stripes can be made only after you've seen the plot
rect <- data.frame(
  ymin = seq(0, 14, 2),
  ymax = seq(2, 16, 2),
  xmin = 2008,
  xmax = 2016,
  color = rep(c(
    grDevices::rgb(239, 242, 244, maxColorValue = 255),
    grDevices::rgb(227, 230, 234, maxColorValue = 255)
  ),
  length.out = 8)
)

#subsetting df to make slopechart
data_lines <- gdp_eur_con %>% filter(country %in% nabes$country)
data_points <-
  gdp_eur_con %>% filter(country %in% nabes$country  &
                           (year == 2008 |
                              year == 2016))


#main plot
ggplot(data_lines) +

  #stripes
  geom_rect(
    data = rect,
    mapping = aes(
      ymin = ymin,
      ymax = ymax,
      xmin = xmin,
      xmax = xmax
    ),
    fill = rect$color
  ) +
  #gridlines
  geom_vline(
    xintercept = 2008,
    linetype = "solid",
    size = .25,
    color = "#CCD0D7"
  ) +
  geom_vline(
    xintercept = 2016,
    linetype = "solid",
    size = .25,
    color = "#CCD0D7"
  ) +

  #slopechart
  geom_line(
    mapping = aes(y = gdp_per_cap_eur_con_ths,
                  x = year,
                  group = country),
    color = ifelse(data$country == "Ukraine", "#3A3F4A", "#FAA61A"),
    size = .5
  ) +
  geom_point(
    data = data_points,
    mapping = aes(y = gdp_per_cap_eur_con_ths,
                  x = year),
    color = ifelse(data_points$country == "Ukraine", "#3A3F4A", "#FAA61A"),
    size = .75
  ) +

  #labels
  #https://stackoverflow.com/questions/21004491/avoiding-overlapping-of-labels-with-direct-labels-and-ggplot2
  geom_dl(
    mapping = aes(y = gdp_per_cap_eur_con_ths,
                  x = year, label = country_ua),
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x + 0.1),
      "last.bumpup",
      cex = 0.6,
      fontfamily = "Roboto Condensed"
    )
  ) +

  geom_dl(
    mapping = aes(y = gdp_per_cap_eur_con_ths,
                  x = year, label = country_ua),
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x - 0.1),
      "first.bumpup",
      cex = 0.6,
      fontfamily = "Roboto Condensed"
    )
  ) +

  #scales
  scale_y_continuous(
    breaks = seq(from = 0, to = 16, by = 2),
    limits = c(0, NA),
    expand = c(0, 0.5)
  ) +
  scale_x_continuous(
    breaks = c(seq(2008, 2016, by = 2)),
    labels = c("'08", "'10", "'12", "'14", "2016"),
    expand = c(0.35, 0.35)
  ) +

  #titles
  labs(title = "Економіка України тривалий час падає",
       subtitle = "Реальний ВВП на душу населення, тис. EUR (в цінах 2016р.)",
       caption = "За даними World Bank і Eurostat") +

  #theme
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    text = element_text(
      family = "Roboto Condensed",
      face = "plain",
      color = "#3A3F4A",
      size = 10 #absolute
    ),

    #titles
    plot.title = element_text(face = "bold",
                              margin = margin(b = 10, t = 20)),
    plot.subtitle = element_text(
      size = rel(0.8),
      face = "plain",
      margin = margin(b = 35)
    ),
    plot.caption = element_text(
      size = rel(0.65),
      margin = margin(t = 20),
      hjust = 1,
      color = '#5D646F'
    ),

    #axis
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(
      color = "#3A3F4A",
      size = .25,
      linetype = "solid"
    ),
    axis.text = element_text(size = rel(0.65)),

    #general layout
    plot.background = element_rect(fill = "#EFF2F4"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.25, 0.4, 0.25, 0.4), "in")
  )


ggsave(
  "story_charts/1_gdp_per_capita.png",
  device = "png",
  units = "in",
  dpi = 600,
  width = 4,
  height = 6
)
