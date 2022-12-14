library(dplyr)
library(ggplot2)
library(data.table)
library(showtext)
library(readr)
library(magrittr)
library(formattable)
library(nflplotR)

df <- read.csv("Arquivos/manager_appointments.csv")
View(df)

df1 <- df %>% 
  mutate(nacional = if_else(team_name == country_name, "Nativo", "Estrangeiro")) %>% 
  select(tournament_id, tournament_name, team_name, family_name, given_name, country_name, nacional) %>% 
  mutate(tournament_id = case_when(tournament_id == "WC-1930" ~ "1930", 
                                   tournament_id == "WC-1934" ~ "1934", 
                                   tournament_id == "WC-1938" ~ "1938", 
                                   tournament_id == "WC-1950" ~ "1950", 
                                   tournament_id == "WC-1954" ~ "1954",
                                   tournament_id == "WC-1958" ~ "1958", 
                                   tournament_id == "WC-1962" ~ "1962", 
                                   tournament_id == "WC-1966" ~ "1966", 
                                   tournament_id == "WC-1970" ~ "1970", 
                                   tournament_id == "WC-1974" ~ "1974",
                                   tournament_id == "WC-1978" ~ "1978", 
                                   tournament_id == "WC-1982" ~ "1982", 
                                   tournament_id == "WC-1986" ~ "1986", 
                                   tournament_id == "WC-1990" ~ "1990", 
                                   tournament_id == "WC-1994" ~ "1994",
                                   tournament_id == "WC-1998" ~ "1998", 
                                   tournament_id == "WC-2002" ~ "2002", 
                                   tournament_id == "WC-2006" ~ "2006",
                                   tournament_id == "WC-2010" ~ "2010", 
                                   tournament_id == "WC-2014" ~ "2014",
                                   tournament_id == "WC-2018" ~ "2018")) %>% 
  rename("tournament_year" = "tournament_id")

df1 <- setDT(df1)
df1 <- df1[, .SD[!duplicated(team_name)], tournament_name]

df1 <- df1 %>% 
  group_by(tournament_name) %>% 
  mutate(team_count = n()) %>% 
  group_by(nacional, tournament_name) %>% 
  mutate(count = n()) %>% 
  mutate(pct = count / team_count * 100)

         
breaks = c(5, 10, 15, 20, 25, 30, 35)
labels = as.character(breaks)



ggplot(df1, aes(y = tournament_year))+
geom_bar(aes(fill = nacional),  position = position_stack(reverse = TRUE),
         color='black',width=0.9)+
scale_x_continuous(breaks= breaks, labels = labels)+
scale_fill_manual(values = c("#c51f5d", "#394859"))+
  labs(title = "Nacionalidades dos treinadores das seleções ao longo das\nedições da Copa do Mundo",
      subtitle = "@rafael_flima || Data: https://github.com/jfjelstul/worldcup \n",
      x = "Número de seleções",
      y = "Edições")+
  theme(plot.background = element_rect(fill = "#be9b7b", color = "#be9b7b"),
       panel.background = element_rect("#be9b7b", color = "#be9b7b"),
       panel.grid.minor = element_blank(),
       axis.ticks = element_blank(),
     axis.title.y = element_blank(),
       axis.title = element_text(colour = "#241531", family = "montserrat", size = 11, face = "bold"),
    panel.grid.major = element_blank(),
       axis.text = element_text(colour = "#241531", family = "montserrat", size = 11, face = "bold"),
       plot.title = element_text(hjust = .5, size = 15.5, colour = "#241531", family = "josefinsans", face = "bold"),
       plot.subtitle = element_text(hjust = .5, size = 11, colour = "#241531", family = "josefinsans", face = "bold"),
       legend.position = "bottom",
       legend.title = element_blank(),
       legend.background = element_blank(),
       legend.key=element_rect(fill='transparent'),
       legend.text = element_text(size=9.3, color = "#241531", family = "josefinsans", face = "bold"))+
       annotate("text", x = c(23,23, 23, 23, 23, 23, 15.5, 15.5, 15.5, 15.5, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), 
                y = c(21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 
                      label = c("62.50%", "56.25%", "62.50%", "53.13%", "75%",
                                "68.75%", "70.83%", "79.17%", "75%", "75%",
                                "87.50%", "81.25%", "75%", "75%", "75%" ,"81.25%" ,"62.50%",
                                "84.62%", "66.67%", "75%", "69,23%") , color="white", 
                      size=2.8, angle=0, fontface="bold")+
      annotate(nflplotR::GeomFromPath,
               x = 29.5, y = 4,
               path = "Downloads/108-1080979_the-original-jules-rimet-trophy-jules-rimet-trophy.png",
               width = 0.04)+
  annotate(nflplotR::GeomFromPath,
           x = 31.4, y = 4,
           path = "Downloads/5842fe21a6515b1e0ad75b3e.png",
           width = 0.06)+
  annotate("rect", xmin = 28.7, xmax = 32.3, ymin = 0.5, ymax = 7.4,
           alpha = .1, fill = "white")


  
  
  
         
