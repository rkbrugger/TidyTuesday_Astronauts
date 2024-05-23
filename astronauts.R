library(tidyverse)
library(jpeg)
library(ggpubr)
library(grid)
library(png)
library(ggtext)


astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts <- astronauts %>% mutate_if(is.character, as.factor)

# background

img <- readJPEG("space2.jpg")
img2 <- readPNG("space8.png")

## do later mission have more eva hours

mis_number_evahours <- astronauts %>% 
  filter(eva_hrs_mission != 0) %>% 
  ggplot(aes(factor(mission_number), eva_hrs_mission)) +
  geom_boxplot() +
  theme_light()

## do males / females different eva hours

astronauts_byind <- astronauts %>% 
  select(number, name, sex, year_of_birth, nationality, military_civilian, selection, 
         year_of_selection, total_number_of_missions, occupation, total_hrs_sum, total_eva_hrs) %>% 
  distinct(number, .keep_all = TRUE) %>% 
  mutate(occupation_c = case_when(occupation %in% c("flight engineer", "Flight engineer") ~ "flight engineer",
                                  occupation %in% c("pilot", "Pilot") ~ "pilot",
                                  occupation %in% c("Other (space tourist)", "Other (Journalist)", "Other (Space tourist)", "Space tourist", "spaceflight participant") ~ "other",
                                  occupation == "MSP" ~ "mission specialist",
                                  occupation == "PSP" ~ "payload specialist",
                                  occupation == "commander" ~ "commander"))

astronauts_byind$occupation_c <- as.factor(astronauts_byind$occupation_c)

astronauts_byind <- astronauts_byind %>% group_by(occupation_c) %>% 
  mutate(mean_eva_byocc = mean(total_eva_hrs)) %>% ungroup() %>% 
  mutate(occupation_c = fct_reorder(occupation_c, -mean_eva_byocc))
                                  

sex_toteva <- astronauts_byind %>% 
  ggplot(aes(sex, total_eva_hrs)) +
  geom_boxplot() +
  theme_light()

numbmission_eva <- astronauts_byind %>% 
  ggplot(aes(factor(total_number_of_missions), total_eva_hrs)) +
  geom_violin() +
  theme_light()


texts  <- tibble(
  total_eva_hrs = c(70, 30, 48),
  occupation_c = c(2.5, 5, 4.2),
  text = c("P. A. Whitson (biochemist & anstronaut!) was the first female commandor of the ISS and holds the record amongst women for most time in space (nearly 666 days) as well as most hours of extravehicular activities with 60 hours 21 min. total.",
           "N. A. Armstrong commandor of the Appolo 11 and first human on the moon spend 2 hours and 31 minutes on the moons surface. He and Buzz ALdrin collected 21.5 kg of lunar material on this mission.",
           "R. L. Curbeam Jr. is the record holder of most spacewalks during a single spaceflight. He completed 4 spacewalks during the STS-116 mission in 2006. In total he spend 45 hours and 34 min. on EVA"),
  vjust = c(.5, 0.5, 0.5)
) 

set.seed(1)
occup_eva <- astronauts_byind %>% 
  #filter(total_eva_hrs != 0) %>% 
  ggplot(aes(occupation_c, total_eva_hrs, label = number)) +
  annotation_custom(rasterGrob(img2), 
                              xmin = 4, ymin = 55) +
  coord_flip() +
  geom_jitter(aes(size = total_number_of_missions), color = "white", width = 0.3, alpha = 0.6) +

  stat_summary(aes(color = occupation_c), fun = mean, geom = "point", size = 10, alpha = 0.8) +
  scale_color_manual(values = c("#202076", "#8B2079", "#D03D6B", "#F97558", "#FFB651", "#F9F871")) +
  annotate("curve", y = 70, yend = 60.8, x = 2, xend = 1.15, curvature = -0.2, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#797585") +
  annotate("curve", y = 25, yend = 2.6, x = 5.3, xend = 1.9, curvature = 0.2, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#797585") +
  annotate("curve", y = 48, yend = 45.55, x = 4, xend = 0.85, curvature = 0.2, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#797585") +
  labs(title = "Extravehicular activity by occupation",
       subtitle = "Each dot signifies one astronaut and their summed-up duration of extravehicular activities from all missions they were part of. 
The larger the dot the more missions the astronaut participated in (range 1 - 7 missions). The large colored dots show the mean duration of extravehicular activity by occupation on the mission.
       
As expected, mission specialists spend the most time outside of the spacecraft (EVA), with an average of 12.5 hours accumulated over all missions. 
Space tourists (part of category other) are (luckily) not performing any extravehicular activities! :)
       
Caveat (!!): Some astronauts had multiple occupations which is not reflected in this visualization.",
       y = "Total duration of all extravehicular activities [h]",
       caption =  "\n\n#TidyTuesday | Graphic: @rkbruegger | Data:  Mariya Stavnichuk and Tatsuya Corlett")  +
  geom_textbox(data = texts,
               aes(occupation_c, total_eva_hrs, 
                   label = text,
                   vjust = vjust),
               colour = "white",
               fill = "black",
               box.colour = "black",
               family = "Open Sans",
               size = 3,
               maxwidth = unit(8, "lines"),
               hjust = .5,
               show.legend = F) +
  theme(
    plot.title =  element_text(family = "Bungee Shade", size = 22, color = "white",  margin = margin(b = 14)),
    plot.subtitle = element_text(face = "italic", size = rel(0.9)),
    axis.ticks = element_blank(), 
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text =  element_text(family = "Open Sans", color = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(1.5), color = "white"),
    axis.title.x = element_text(hjust = 1, margin = margin(t = 14), color = "white", size = rel(1.2)),
    axis.text.x = element_text(size = rel(1.25), color = "white"),
    legend.position = "none") 
occup_eva

ggsave("eva_by_occ_astronauts1.png", occup_eva, height = 9.5, width = 14, units = c("in"), dpi = 300)
