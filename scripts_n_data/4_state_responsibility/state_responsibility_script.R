library(readxl)
library(tidyverse)
library(extrafont)
library(ggalt)
library(directlabels)


df_raw <-
  read_excel("scripts_n_data/4_state_responsibility/chart_data/data.xlsx",
             1)

#transform and simplify data
df_reshaped <- df_raw %>% spread(key = year, value = value)

df_collapsed <- df_reshaped %>% mutate(param = fct_collapse(
  param,
  state = c("повністю держава", "переважно держава"),
  together = c("як держава, так і вони самі"),
  self = c("переважно вони", "виключно вони")
))

df_sum <- df_collapsed %>% group_by(param, decile) %>%
  summarise(`2008` = sum(`2008`),
            `2016` = sum(`2016`)) %>% filter(decile != "avg")

#convert deciles' names to factors to preserve the right order
decile_levels <- tibble(
  decile = c(1:10),
  name = c(
    "нижча",
    "2-а",
    "3-а",
    "4-а",
    "медіана",
    "6-а",
    "7-а",
    "8-а",
    "9-а",
    "вища\nгрупа"
  )
)

df_sum$decile <- as.integer(df_sum$decile)

df_sum <- df_sum %>% left_join(decile_levels, by = "decile")

#reverse order to show on plot
df_sum$name <- factor(df_sum$name, levels = decile_levels$name)

data <- subset(df_sum, param == "state")

ggplot(data)  +

  #https://cran.r-project.org/web/packages/ggalt/vignettes/ggalt_examples.html
  geom_dumbbell(
    mapping = aes(
      x = `2008`,
      xend = `2016`,
      y = name,
      group = name
    ),
    colour = "#FAA61A",
    size = 0.5,
    size_x = 1.5,
    size_xend = 1.5,
    colour_xend = "#3A3F4A",
    dot_guide = TRUE,
    dot_guide_size = 0.25,
    dot_guide_colour = "#CCD0D7"
  ) +

  geom_dl(
    data = subset(data, decile == 10),
    mapping = aes(y = name, x = `2008`),
    label = colnames(data)[3],
    #2008
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x + 0.1, y = y + 0.3),
      "last.bumpup",
      cex = 0.6,
      fontfamily = "Roboto Condensed"
    )
  ) +

  geom_dl(
    data = subset(data, decile == 10),
    mapping = aes(y = name, x = `2016`),
    label = colnames(data)[4],
    #2016
    color = "#3A3F4A",
    method = list(
      dl.trans(x = x + 0.1 , y = y - 0.3),
      "last.bumpup",
      cex = 0.6,
      fontfamily = "Roboto Condensed"
    )
  ) +

  scale_x_continuous(limits = c(0, max(data$`2008`, data$`2016`)),
                     expand = c(0, 0.35)) +

  #space for years' labels
  scale_y_discrete(expand = c(0, 1)) +

  #titles
  labs(#y = "групи",
    title = "Бідні українці покладаються на державу",
    subtitle = "Населення по децильних (10%-их) групах за доходами,\nяке вважає, що за його матеріальний добробут несе\nвідповідальність повністю або переважно держава, %",
    caption = "За даними Держстату України") +

  #geom_dumbbell is originally for horizontal layout
  coord_flip() +

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
      margin = margin(t = 10),
      hjust = 1,
      color = '#5D646F'
    ),

    #axis
    #axis.title.x = element_text(size = rel(0.7)),
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
  "story_charts/4_state_responsibility.png",
  device = "png",
  units = "in",
  dpi = 600,
  width = 4,
  height = 6
)
