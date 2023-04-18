# -----
# proj: polypharmacy
# author: evan flack
# desc: defines plot themes for ggplot2 objects
# -----

# my_theme -----
# desc: base theme for all ggplot objects
my_theme <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = .5, size = 20,
                                  margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 20, l = 0)),
        plot.subtitle = element_text(hjust = .5, size = 15,
                                  margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 10, l = 0)),
        axis.text = element_text(hjust = .5, size = 10),
  			axis.title = element_text(hjust = .5, size = 15),
        strip.text = element_text(hjust = .5, size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0,
                                                             b = 0, l = 0)))


my_theme_paper <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = .5, size = 15,
                                  margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 20, l = 0)),
        plot.subtitle = element_text(hjust = .5, size = 10,
                                     margin = ggplot2::margin(t = 0, r = 0,
                                                              b = 10, l = 0)),
        axis.title = element_text(hjust = .5, size = 9, face = "plain"),
        axis.text = element_text(hjust = .5, size = 9, face = "plain"),
        strip.text = element_text(hjust = .5, size = 9),
        legend.title = element_text(size = 8, face = "plain"),
        legend.text = element_text(size = 8),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0,
                                                             b = 0, l = 0)))
