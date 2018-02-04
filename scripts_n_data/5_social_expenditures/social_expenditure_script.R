library(readxl)
library(tidyverse)
library(modelr)
library(directlabels)
library(extrafont)

#get historic data to build models
eu_raw <-
  read_csv("scripts_n_data/5_social_expenditures/chart_data/eu_gov_exp.csv") %>%
  select(-3, -4, -6, -8)

#sum up expenditures for health and social protection
eu_raw$Value <- as.double(eu_raw$Value)

eu_sum <- eu_raw %>% group_by(TIME, GEO) %>%
  summarize(share = sum(Value, na.rm = TRUE))

eu_sum$share[eu_sum$share == 0] <- NA

#build many models (for each country) to forecast value for 2016
#http://r4ds.had.co.nz/many-models.html
by_country <- eu_sum %>%
  group_by(GEO) %>%
  nest()

country_model <- function(df) {
  lm(share ~ TIME, data = df)
}

by_country <- by_country %>%
  mutate(model = map(data, country_model))

#add predicitons
by_country <- by_country %>%
  mutate(predicts = map2(data, model, add_predictions))

predicts <- unnest(by_country, predicts) %>%
  select(-share)

#get values for 2016 where they are missed
#as predictions from corresponding model
eu_2016 <- eu_sum %>% full_join(predicts, by = c("TIME", "GEO"))

eu_2016$share <-
  ifelse(is.na(eu_2016$share), eu_2016$pred, eu_2016$share)

eu_2016 <- eu_2016 %>% select(-4:-5)


#general government expenditures in Ukraine
ua_gov_exp <-
  read_excel("scripts_n_data/5_social_expenditures/chart_data/ua_gov_exp.xlsx") %>%
  select(-2:-8) %>%
  mutate(share = share * 100, GEO = "Ukraine") %>%
  rename(TIME = year)

#combined dataframe
gov_exp <- bind_rows(ua_gov_exp, eu_2016) %>%
  arrange(TIME, GEO) %>%
  filter(TIME >= 2008)


#countries' names in ua
ua_names <-
  read_excel("reference_data/country_names.xlsx", sheet = 1)


data <-
  gov_exp %>% left_join(ua_names, by = c("GEO" = "country"))


#selected labels: extrems + nabes of Ukraine
labels <-
  c(
    "Latvia",
    "Romania",
    "Poland",
    "Bulgaria",
    "Ukraine",
    "Germany (until 1990 former territory of the FRG)",
    "France",
    "Sweden",
    "Czech Republic",
    "Hungary"
  )

data_labs <- data %>% filter(GEO %in% labels & TIME == 2016)


ggplot(mapping = aes(x = TIME, y = share, group = GEO)) +

  geom_path(
    data = subset(data, GEO != "Ukraine"),
    color = "#FAA61A",
    size = .5,
    alpha = 0.2
  ) +

  geom_path(
    data = subset(data, GEO == "Ukraine"),
    color = "#3A3F4A",
    size = .5,
    alpha = .8
  ) +

  geom_dl(
    data = data_labs,
    mapping = aes(x = TIME,
                  y = share,
                  label = country_ua),
    color = "#3A3F4A",
    alpha = ifelse(data_labs$GEO == "Ukraine", 1, 0.5),
    method = list(
      dl.trans(x = x + 0.1),
      "last.bumpup",
      cex = 0.5,
      fontfamily = "Roboto Condensed"
    )
  ) +

  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(gov_exp$share))) +

  #assymetrical expand
  #https://github.com/tidyverse/ggplot2/issues/1669
  scale_x_continuous(
    breaks = c(seq(2008, 2016, by = 2)),
    labels = c("'08", "'10", "'12", "'14", "2016"),
    expand = c(0.01, 0, 0.2, 0)
  ) +

  #titles
  labs(title = "Ресурси уряду обмежуються розміром ВВП",
       subtitle = "Видатки бюджету і спеціальних фондів в Україні і ЄС\nна охорону здоров'я та соціальних захист, % від ВВП",
       caption = "За даними Eurostat, Держстату і Держказначейства України") +

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
  "story_charts/5_social_expenditures.png",
  device = "png",
  units = "in",
  dpi = 600,
  width = 4,
  height = 6
)
