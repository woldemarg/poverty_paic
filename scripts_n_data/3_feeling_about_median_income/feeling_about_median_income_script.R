library(tidyverse)
library(readxl)
library(extrafont)


proj_dir_path <- getwd()

#set directory to loop through data files
setwd(
  paste(
    proj_dir_path,
    "/scripts_n_data/3_feeling_about_median_income/chart_data/",
    sep = ""
  )
)


#EU data
files_list <- list.files(pattern = "ess.*.xlsx")
eu_all_deciles <-
  lapply(files_list, read_excel, sheet = 1) %>% bind_rows()


#Ukraine data
ua_5th_decile <- read_excel("ukr_poors.xlsx", sheet = "5th_decile")


#countries' names in ua
ua_names <-
  read_excel("../../../reference_data/country_names.xlsx", sheet = 1)


#get 5th decile's data for countries presented ia all surveys
median_income <-
  eu_all_deciles %>% separate(
    "Household's total net income, all sources",
    into = c("code", "decile"),
    sep = " - "
  ) %>%
  filter(code == "F") %>%
  group_by(Country) %>% #select countries that have presence in all surveys
  filter(n() == 5) %>% #letter code for 5th decile
  ungroup() %>%
  mutate(poors = `Difficult on present income` + `Very difficult on present income`) %>%
  select(-2:-7) %>%
  filter(!(
    Country %in% c("Israel", "Czech Republic", "Switzerland", "Norway") #non european countries or countries puffed from gem\neral tendency
  )) %>%
  bind_rows(ua_5th_decile)

#get ukrainian countries' names for labels
median_income <-
  median_income %>% left_join(ua_names, by = c("Country" = "country"))

#this is to reveal the general trend
country_levels <- median_income[median_income$Year == 2008,] %>%
  arrange(desc(poors)) %>%
  select(country_ua)

#ordered factors
median_income$country_ua <-
  factor(median_income$country_ua, levels = country_levels$country_ua)


#add colors
median_income <-
  median_income %>% mutate(mycolor = ifelse(Country != "Ukraine", "eu", "ua"))


ggplot(data = median_income, mapping = aes(x = Year)) +

  geom_line(aes(y = poors, color = mycolor), size = 0.5) +

  facet_grid(. ~ country_ua) +

  scale_x_continuous(
    breaks = seq(2009, 2015, 6),
    labels = c("'08", "'16"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits =  c(0, max(median_income$poors)),
                     expand = c(0, 0)) +

  scale_colour_manual(values = c("#FAA61A", "#3A3F4A"), guide = FALSE) +
  scale_fill_manual(values = c("#FAA61A", "#3A3F4A"), guide = FALSE) +

  #titles
  labs(title = "Середній дохід не забезпечує пристойного життя в Україні",
       subtitle = "Населення із медіанним доходом, яке за оцінкою матеріального добробуту вважає себе бідним, %",
       caption = "За даними Держстату України і European Social Survey") +

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
      size = rel(0.6),
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

setwd(proj_dir_path)


ggsave(
  "story_charts/3_feeling_about_median_income.png",
  device = "png",
  units = "in",
  dpi = 600,
  width = 6,
  height = 4
)
