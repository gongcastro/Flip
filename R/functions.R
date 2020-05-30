#### functions #####################

theme_custom <- theme(
  panel.background = element_rect(fill = "white", colour = "grey"),
  panel.border      = element_rect(fill = "transparent", colour  = "black"),
  panel.grid       = element_blank(),
  text             = element_text(colour = "black", size = 20),
  axis.text        = element_text(colour = "black"),
  strip.background = element_rect(fill = "transparent", colour = "transparent"),
  strip.placement  = "outside"
) 


# add "," as thousands mark
add_big_mark <- function(x) format(x, big.mark = ",", scientific = FALSE)
