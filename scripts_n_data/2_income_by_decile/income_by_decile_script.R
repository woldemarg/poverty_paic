library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(directlabels)


income_uah <-
  read_excel("scripts_n_data/2_income_by_decile/chart_data/data_set.xlsx",
             sheet = "income")
inflation <-
  read_excel("scripts_n_data/2_income_by_decile/chart_data/data_set.xlsx",
             sheet = "inflation")
self_assesment <-
  read_excel("scripts_n_data/2_income_by_decile/chart_data/data_set.xlsx",
             sheet = "sbj_level")
cost_of_living_adj <-
  read_excel("scripts_n_data/2_income_by_decile/chart_data/data_set.xlsx",
             sheet = "cost_of_living")
#average number of resons per household
#consider in 2008 the same number as in 2016
#to exclude family-size factor from analysis
family_size_2016 <-
  read_excel("scripts_n_data/2_income_by_decile/chart_data/data_set.xlsx",
             sheet = "family_size_2016")


#adjust income for 2016
income_adj <-
  income_uah %>% left_join(inflation, by = "year") %>%
  mutate(income = value * index / 28.30065) %>% #UAH to EUR
  select(-annual, -index, -value)


#EU methodology
#60% of median income
level_md <-
  income_adj %>% filter(decile == 5) %>%
  inner_join(family_size_2016, by = c("year", "decile")) %>%
  mutate(lvl_md = income / size * 0.6) %>% #per capita
  select(year, lvl_md)


#subjective poverty line
#respondets were asked about cash income, so adjust
#it for non-cash share to calculate aggregate amount
level_sbj <-
  self_assesment %>%
  group_by(country, year, decile) %>%
  summarise(value = weighted.mean(income, weight) / 28.30065) %>% #UAH to EUR
  mutate(ctg = "lvl_sbj") %>%
  left_join(inflation, by = "year") %>%
  mutate(value = value * index) %>% #adjust for 2016
  select(-index, -annual)


#cost of living is the same for all deciles
cost_of_living_eur <-
  cost_of_living_adj %>% mutate(value = value_real / 28.30065) %>% #UAH to EUR
  select(-4, -6) %>%
  slice(rep(1:n(), each = 10)) %>%
  mutate(decile = rep(1:10, length.out = 100))


#combine and melt all tables
data <-
  income_adj %>% left_join(level_md, by = "year") %>%
  gather(ctg, value, -country, -decile, -year) %>%
  bind_rows(level_sbj) %>%
  inner_join(family_size_2016, by = c("year", "decile")) %>%
  mutate(value = ifelse(ctg != "lvl_md", value / size, value)) %>% #per capita
  select(-size) %>%
  bind_rows(cost_of_living_eur)


#fake data to crate two panels
#these panels will be removed
#to make space for labels
fake_data_1 <-
  data %>% filter(decile == 1 &
                    (year == 2008 | year == 2016) & ctg == "income")
fake_data_1$decile <- -1
fake_data_2 <-
  data %>% filter(decile == 2 &
                    (year == 2008 | year == 2016) & ctg == "income")
fake_data_2$decile <- 0

data <- data %>% bind_rows(fake_data_1, fake_data_2)


#convert deciles' names to factors to preserve the right order
decile_levels <- tibble(
  decile = c(-1:10),
  name = c(
    "fake1",
    #fake panel to be removed from plot
    "fake2",
    #fake panel to be removed from plot
    "нижча\nгрупа",
    "2-а група",
    "3-а група",
    "4-а група",
    "медіана",
    "6-а група",
    "7-а група",
    "8-а група",
    "9-а група",
    "вища\nгрупа"
  )
)

data <- data %>% left_join(decile_levels, by = "decile")
data$name = factor(data$name, levels = decile_levels$name, ordered = TRUE)


#tip to create gridlines only where the data is presented
#this can be made only after you've seen the plot
grid <-
  data %>% filter(ctg == "income" &
                    (year == "2016" | year == "2008")) %>%
  group_by(decile) %>%
  summarize(val = ceiling(max(value) / 25))


#https://stackoverflow.com/questions/30602821/r-creating-a-sequence-table-from-two-columns
grid_ext <- data.frame(decile = rep(grid$decile, grid$val),
                       line = unlist(mapply(seq, 25, grid$val * 25, 25)))

grid_ext <-
  grid_ext %>% left_join(decile_levels, by = "decile") %>%
  select(-1)

grid_ext$name = factor(grid_ext$name, levels = decile_levels$name, ordered = TRUE)


#subsetting table
data_flt <- data %>% filter(year == 2008 | year == 2016)


#data for labels
labels <-
  data_flt %>% filter(year == 2008 & decile == 1 & ctg != "income")

labels$value[labels$ctg == "live"] <-
  labels$value[labels$ctg == "live"] / 2

#annotations n comments to plot
labels$text <-
  c(
    "відносна бідність\n(60% від медіани)",
    "суб'єктивна бідність\n(за самооцінкою)",
    "абсолютна бідність\n(прожитковий мінімум)"
  )

annotation <-
  data.frame(
    name = c("нижча\nгрупа", "6-а група"),
    year = c(2008, 2008),
    value = c(115, 175),
    text = c(
      "фактичний дохід\n(в цінах 2016р.)",
      "60% українців отримували в 2016 році\nдохід,  менший за той,  який вважали\nдостатнім, аби не почуватися бідними"
    )
  )

curve <-
  data.frame(
    name = c("нижча\nгрупа", "6-а група"),
    year = c(2008, 2008),
    value = c(115, 175),
    year_2 = c(2010, 2016),
    value_2 = c(69, 106)
  )

plot <- ggplot(mapping = aes(x = year)) +

  #areas are overlaid themselves, so that
  #the bottom one becomes the most contrast
  geom_ribbon(
    data = subset(data_flt, ctg != "income"),
    mapping = aes(ymax = value, ymin = 0, group = ctg),
    fill = "#FAA61A",
    alpha = 0.15
  ) +

  #labels in place of first and second panels
  geom_dl(
    data = labels,
    mapping = aes(y = value, label = text),
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x - 0.15, y = y + 0.01),
      "first.bumpup",
      cex = 0.45,
      fontfamily = "Roboto Condensed"
    )
  ) +

  #line to separate layers
  geom_line(
    data = subset(data_flt, ctg != "income"),
    mapping = aes(y = value, group = ctg),
    size = .5,
    color = "#EFF2F4"
  ) +

  #gridlines
  geom_hline(
    data = grid_ext,
    mapping = aes(yintercept = line),
    linetype = "dashed",
    size = .25,
    color = "#CCD0D7"
  ) +

  #main lines
  geom_line(
    data = subset(data_flt, ctg == "income"),
    mapping = aes(y = value, group = ctg),
    size = .5,
    color = "#3A3F4A"
  ) +

  geom_dl(
    data = annotation,
    mapping = aes(y = value, label = text),
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x - 0.15, y = y + 0.2),
      "first.bumpup",
      cex = 0.45,
      fontfamily = "Roboto Condensed"
    )
  ) +

  geom_curve(
    data = curve,
    aes(xend = year_2,
        y = value,
        yend = value_2),
    color = "#7F8590",
    size = .15,
    curvature = -0.25,
    arrow = arrow(length = unit(0.125, "npc"))
  ) +

  facet_grid(. ~ name) +

  #scales
  scale_x_continuous(
    breaks = seq(2009, 2015, 6),
    #to ensure that label are "inside" x-axis
    labels = c("'08", "'16"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits =  c(0, NA),
    breaks = (seq(0, 300, 50)),
    position = "right",
    expand = c(0, 0, 0.025, 0)
  ) +
  #titles
  labs(title = "Разом із падінням економіки українці стають біднішими",
       subtitle = "Реальні доходи населення за децильними (10%-ми) групами, EUR/міс. на 1 особу (в цінах 2016р.)",
       caption = "За даними Держстату і Верховної Ради України") +


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

    strip.text = element_text(size = rel(0.65), color = "#5D646F"),

    #general layout
    plot.background = element_rect(fill = "#EFF2F4"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(0.2, "lines"),
    plot.margin = unit(c(0.25, 0.4, 0.25, 0.4), "in")
  )

#modify plot on low-level
grob <- ggplotGrob(plot)

#see info in grob
print(grob)
# ...
# 45  2 ( 6- 6,14-14)  strip-t-6                                 gtable[strip]
# 46  2 ( 6- 6,16-16)  strip-t-7                                 gtable[strip]
# 47  2 ( 6- 6,18-18)  strip-t-8                                 gtable[strip]
# ...


#bold font for 5th decile facet label
#https://stackoverflow.com/questions/46905774/ggplot-using-strip-text-x-element-text-for-making-only-one-element-of-the-fac?noredirect=1&lq=1
#5th strip grob is at position 46
k <- 46

#set bold font
grob$grobs[[k]]$grobs[[1]]$children[[2]]$children[[1]]$gp$font <-
  as.integer(2)

attr(grob$grobs[[k]]$grobs[[1]]$children[[2]]$children[[1]]$gp$font, "names") <-
  "bold"

#turn off clipping for third panel to show labels
#https://stackoverflow.com/questions/41065829/ggplot2-issue-with-facets-and-turning-off-clipping
#https://stackoverflow.com/questions/38088966/directlabels-package-in-r-labels-do-not-fit-in-plot-area
grob$layout$clip[grob$layout$name == "panel-3-1"] = "off"
grob$layout$clip[grob$layout$name == "panel-8-1"] = "off"


#completle remove first and second panel with fake data
#https://stackoverflow.com/questions/30372368/adding-empty-graphs-to-facet-wrap-in-ggplot2
rm_grobs <-
  grob$layout$name %in% c("panel-1-1",
                          "panel-2-1",
                          "strip-t-1",
                          "strip-t-2",
                          "axis-b-1",
                          "axis-b-2")
#remove grobs
grob$grobs[rm_grobs] <- NULL
grob$layout <- grob$layout[!rm_grobs,]


#inches to pixels with given dpi
#http://auctionrepair.com/pixels.html
png(
  "story_charts/2_income_by_decile.png",
  width = 3600,
  height = 3600,
  res = 600
)

grid.draw(grob)

dev.off()
