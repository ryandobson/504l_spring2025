


theme504 <- function(base_size = 24, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) + #%+replace%
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.text.y = ggplot2::element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.ticks = ggplot2::element_line(color = "black", size = 0.2),
      axis.title.x = ggplot2::element_text(size = base_size, color = "black", margin = ggplot2::margin(10, 0, 0, 0)),
      axis.title.y = ggplot2::element_text(size = base_size, color = "black", angle = 90, margin = ggplot2::margin(0, 10, 0, 0)),
      axis.ticks.length = ggplot2::unit(0.3, "lines"),
      legend.background = ggplot2::element_rect(color = NA, fill = "#ffffff"),
      legend.key = ggplot2::element_rect(color = "black", fill = "#ffffff"),
      legend.key.size = ggplot2::unit(2, "lines"),
      legend.text = ggplot2::element_text(size = base_size * 0.8, color = "black"),
      legend.title = ggplot2::element_text(size = base_size * 0.8, face = "bold", color = "black"),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.border = ggplot2::element_rect(fill = NA, color = "black"),
      panel.grid.major = ggplot2::element_line(color = "#ffffff"),
      panel.grid.minor = ggplot2::element_line(color = "#ffffff"),
      panel.spacing = ggplot2::unit(2, "lines"),
      strip.background = ggplot2::element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = ggplot2::element_text(size = base_size * 0.8, color = "black"),
      strip.text.y = ggplot2::element_text(size = base_size * 0.8, color = "black", angle = -90),
      plot.background = ggplot2::element_rect(color = "#ffffff", fill = "#ffffff"),
      plot.title = ggplot2::element_text(size = base_size * 1.2, color = "black"),
      plot.margin = ggplot2::unit(rep(1, 4), "lines")
    )
}
