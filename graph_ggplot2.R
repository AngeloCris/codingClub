library(tidyverse)
library(gridExtra)

LPI <- read_csv("CC-4-Datavis-master/CC-4-Datavis-master/LPIdata_CC.csv")
head(LPI)

glimpse(LPI)
LPI$`1970` <- as.numeric(LPI$`1970`)

LPI2 <- 
  LPI %>% 
  pivot_longer(names_to = "year",
               values_to = "abundance", 
               cols = -c(1:8))

glimpse(LPI2)

LPI2 <- 
  LPI2 %>% 
  mutate(year = as.numeric(year))

unique(LPI2$`Common Name`)

vulture <- LPI2 %>% 
  dplyr::filter(`Common Name` == "Griffon vulture / Eurasian griffon")

vulture <- na.omit(vulture)

vulture %>% 
  ggplot(aes(x = abundance)) +
  geom_histogram(bindwidth = 250, colour = "#FFFFFF", fill = "#BDBDBD")+
  geom_vline(aes(xintercept = mean(abundance)), 
             colour = "red", 
             linetype = "dashed", 
             size = 1) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = "cm"))


# Scaterring plot

vultureITCR <- 
  vulture %>% 
  filter(`Country list` %in% c("Croatia", "Italy"))

vultureITCR %>% 
  ggplot(aes(x = year, y = abundance, colour = `Country list`)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = `Country list`)) +
  theme_bw() + 
  scale_fill_manual(values = c("#BFBFBF", "#7D7D7D")) +
  scale_color_manual(values = c("#BFBFBF", "#7D7D7D")) +
  labs( y = "Griffon vulture abundance", 
        x = "") +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 0.9),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = "cm"),
        legend.text = element_text(size = 12, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.9))


test <- 
  vultureITCR %>% 
  mutate(abundance2 = abundance) %>% 
  mutate(abundance2 =
           case_when(`Country list` == "Croatia" ~ abundance2 * 100,
                     TRUE ~ as.double(abundance2)))
  

test %>% 
  ggplot(aes(x = year, y = abundance2,  colour = `Country list`)) +
  geom_point() +
  geom_line(aes(group = `Country list`))




