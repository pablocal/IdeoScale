# Metadata ----------------------------------------------------------------
# Title: Measurement issues with ideology scale (1-10)
# Purpose: Check consistency of the scale using a CIS panel
# Author(s): @pablocal
# Date Created: 2019-09-24
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------

rm(list = ls())
library(tidyverse)

# 1. Data -----------------------------------------------------------------
panel <- haven::read_sav("data/panel2011.sav")
panel <- haven::as_factor(panel)


# 2. Prepare vars ---------------------------------------------------------
 panel %>% 
  select(p25, X40) %>% 
  sjmisc::frq()


table(panel$p25, panel$X40)
levels(panel$p25)

panel <- panel %>%
  mutate(ideo_pre = recode(p25, "Extrema izquierda" = "Izquierda",
                           "Extrema derecha" = "Derecha",
                           "N.S." = "NS/NC", "N.C." = "NS/NC"),
         ideo_pre_red = recode(ideo_pre, "Izquierda" = "Izquierda (1-3)",
                               "2" = "Izquierda (1-3)",
                               "3" = "Izquierda (1-3)",
                               "4" = "Centro izq. (4-5)",
                               "5" = "Centro izq. (4-5)",
                               "6" = "Centro der. (6-7)",
                               "7" = "Centro der. (6-7)",
                               "8" = "Derecha (8-10)",
                               "9" = "Derecha (8-10)",
                               "Derecha" = "Derecha (8-10)"),
         ideo_pos = recode(X40, "N.S." = "NS/NC", "N.C." = "NS/NC"),
         ideo_pos_red = recode(ideo_pos, "Izquierda" = "Izquierda (1-3)",
                               "2" = "Izquierda (1-3)",
                               "3" = "Izquierda (1-3)",
                               "4" = "Centro izq. (4-5)",
                               "5" = "Centro izq. (4-5)",
                               "6" = "Centro der. (6-7)",
                               "7" = "Centro der. (6-7)",
                               "8" = "Derecha (8-10)",
                               "9" = "Derecha (8-10)",
                               "Derecha" = "Derecha (8-10)"))

# checks 
table(panel$p25, panel$ideo_pre)
table(panel$p25, panel$ideo_pre_red)

table(panel$X40, panel$ideo_pos)
table(panel$X40, panel$ideo_pos_red)


# 3. Full plot ------------------------------------------------------------
full <- panel %>% 
  group_by(ideo_pre, ideo_pos) %>% 
  summarise(count = n(), peso = sum(PESO)) %>% 
  mutate(per = round(peso/6082*100, 1))


plot_full <- ggplot(full, aes(x = ideo_pos, y = fct_rev(ideo_pre), label = per, fill = per)) +
  geom_text(col = "black", fontface = "bold", size = 5, family = "Josefin Sans") +
  geom_tile(alpha = .3, col = "white") +
  scale_fill_gradient2("per", limits = c(0, 21), 
                       low = "grey", high = "darkgreen") +
  scale_x_discrete(position = "top") +
  labs(title = "Problemas de la escala idológica 1-10",
       caption = "@pablocalv - panel CIS elecciones 2011 (2915 y 2920)",
       x = "...tras las elecciones",
       y = "Los que antes de las\n elecciones dijeron...") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 9, family = "Josefin Sans", face = "bold"),
        axis.title = element_text(size = 12, family = "Josefin Sans", face = "bold"),
        plot.title = element_text(size = 12, family = "Josefin Sans", face = "bold", hjust = -.4),
        plot.caption = element_text(size = 8, family = "Josefin Sans", color = "grey40"),
        panel.background = element_blank(),
        legend.position = "none")


# 4. Reduced plot ---------------------------------------------------------

red <- panel %>% 
  group_by(ideo_pre_red, ideo_pos_red) %>% 
  summarise(count = n(), peso = sum(PESO)) %>% 
  mutate(per = round(peso/6082*100, 1))


plot_red <- ggplot(red, aes(x = ideo_pos_red, y = fct_rev(ideo_pre_red), label = per, fill = per)) +
  geom_text(col = "black", fontface = "bold", size = 5, family = "Josefin Sans") +
  geom_tile(alpha = .3, col = "white") +
  scale_fill_gradient2("per", limits = c(0, 21), 
                      low = "grey", high = "darkgreen") +
  scale_x_discrete(position = "top") +
  labs(title = "Problemas de la escala idológica 1-10",
       caption = "@pablocalv - panel CIS elecciones 2011 (2915 y 2920)",
       x = "...tras las elecciones",
       y = "Los que antes de las\n elecciones dijeron...") + 
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 9, family = "Josefin Sans", face = "bold"),
        axis.title = element_text(size = 12, family = "Josefin Sans", face = "bold"),
        plot.title = element_text(size = 12, family = "Josefin Sans", face = "bold", hjust = -.4),
        plot.caption = element_text(size = 8, family = "Josefin Sans", color = "grey40"),
        panel.background = element_blank(),
        legend.position = "none")
plot_red

ggsave("escala_ideo_red.png")
