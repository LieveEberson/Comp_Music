library(tidyverse)
library(spotifyr)
library(ggplot2)
library(dplyr)

lunch <- get_playlist_audio_features("","15XTzdCk80I6KrHwrRgR0L")
rock_fissa <- get_playlist_audio_features("", "7fLp6h0KxTCnNWsbBpSWXi")

bar_playlists <-
  bind_rows(
    lunch %>% mutate(category = "Lunch"),
    rock_fissa %>% mutate(category = "Avond")
  )

plot 1 <- bar_playlists %>% mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) %>%ggplot(                     
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +              
  geom_rug(size = 0.1) +      
  geom_text(                  
    aes(
      x = valence,
      y = energy,
      label = label
    ),
    data = 
      tibble(
        label = c("", ""),
        category = c("Avond", "Lunch"),
        valence = c(0.090, 0.123),
        energy = c(0.101, 0.967)
      ),
    colour = "black",        
    size = 3,                
    hjust = "left",          
    vjust = "bottom",        
    nudge_x = -0.05,          
    nudge_y = 0.02            
  ) +
  facet_wrap(~category) +     
  scale_x_continuous(         
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   
    minor_breaks = NULL       
  ) +
  scale_y_continuous(         
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        
    type = "qual",            
    palette = "Paired"        
  ) +
  scale_size_continuous(      
    trans = "exp",            
    guide = "none"            
  ) +
  theme_light() +             
  labs(                       
    x = "Danceability",
    y = "Energy",
    colour = "Mode"
  )
