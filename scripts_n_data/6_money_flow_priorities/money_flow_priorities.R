library(readxl)
library(tidyverse)
library(extrafont)
library(directlabels) #http://directlabels.r-forge.r-project.org/docs/index.html
library(grid)


df <-
  read_excel("scripts_n_data/6_money_flow_priorities/chart_data/flow_priorities.xlsx",
             1)

#ordered factors to show panels in right order
#https://stackoverflow.com/questions/14262497/fixing-the-order-of-facets-in-ggplot
df$status_f = factor(df$status, levels = c("нижча група", "медіана", "вища група"))

#leave two years in dataframe
df_flt <- df %>% filter(year == 2008 |  year == 2016)


plot <- ggplot(data = df_flt,
               mapping = aes(x = year,
                             y = share)) +

  geom_line(mapping = aes(group = item,
                          color = my_color),
            size = .4) +

  #label
  geom_dl(
    mapping = aes(label = item),
    color = "#3A3F4A",
    alpha = ifelse(df_flt$my_color == "show", 1, 0.5),
    method = list(
      dl.trans(x = x + 0.1),
      "last.bumpup",
      cex = 0.5,
      fontfamily = "Roboto Condensed"
    )
  ) +

  facet_grid(. ~ status_f) +


  #transparent colors (two last digits)
  #https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4
  scale_color_manual(values = c("#FAA61A80", "#3A3F4ACC"),
                     guide = "none") +

  scale_y_continuous(expand = c(0, 0.1),
                     limits = c(0, max(df_flt$share))) +

  scale_x_continuous(
    breaks = seq(2008.5, 2016.5, 7),
    labels = c("'08", "'16"),
    expand = c(0, 0)
  ) +

  #titles
  labs(title = "Українці без освіти і підприємницького духу не піднімуть економіку",
       subtitle = "Пріоритети витрачання додаткових коштів при значному збільшенні доходів, % населення",
       caption = "За даними Держстату України") +

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
    axis.text = element_text(size = rel(0.6)),

    strip.text = element_text(size = rel(0.65), color = "#5D646F"),

    #general layout
    plot.background = element_rect(fill = "#EFF2F4"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(0.85, "in"),
    plot.margin = unit(c(0.25, 1.05, 0.25, 0.4), "in")
  )

#modify plot on low-level
grob <- ggplotGrob(plot)


#turn off clipping
#https://stackoverflow.com/questions/41065829/ggplot2-issue-with-facets-and-turning-off-clipping
grob$layout$clip = "off"

#saving plot
png(
  "story_charts/6_money_flow_priorities.png",
  width = 3600,
  height = 3600,
  res = 600
)

grid.draw(grob)

dev.off()
