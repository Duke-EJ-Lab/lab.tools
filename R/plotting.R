

#' Anne's favorite ggplot theme
#'
#' A pretty simple theme, based on the tufte theme from ggthemes, but with a transparent background and simpler axes
#' @import ggplot2
#' @import ggthemes
#' @param font The font
#' @param size Text size
theme_anne = function(font="Avenir", size=10) {
  theme_tufte(base_size=size, base_family=font) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="transparent", colour=NA), 
      axis.line.x = element_line(color="black", size = .2), 
      axis.line.y = element_line(color="black", size = .2), 
      plot.title = element_text(hjust = 0.5)
    )
}

#' A truly blank ggplot theme
#'
#' A really blank theme - no axes, no ticks, no title, no axis labels, nothing. Mostly useful for maps. 
#' @import ggplot2
#' @import ggthemes
#' @param font The font
#' @param size Text size
theme_blank = function(font="Avenir", size=10) {
  theme_tufte(base_size=size, base_family=font) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(), 
      plot.title = element_blank(), 
      axis.line=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      legend.position="none"
    )
}