#'
#' Create an Hexagonal Sticker for the Package
#'


# install.packages(c("png", "ggplot2", "hexSticker", "grid", "ggpubr"))


rlogo <- png::readPNG(here::here("inst", "package-sticker", "logo.png"))
rlogo <- grid::rasterGrob(rlogo, interpolate = TRUE)

p <- ggplot2::ggplot() +
  ggplot2::annotation_custom(rlogo, xmin = -Inf, xmax = Inf, ymin = -Inf,
                             ymax = Inf) +
  ggplot2::theme_void() +
  ggpubr::theme_transparent()

hexSticker::sticker(

  subplot   = p,
  package   = "MetaSynthesis",
  filename  = here::here("man", "figures", "package-sticker.png"),
  dpi       = 800,

  p_size    = 38.0,         # Title
  u_size    =  5.0,         # URL
  p_family  = "Aller_Rg",

  p_color   = "#32436F",   # Title
  h_fill    = "#FFFFFF",   # Background
  h_color   = "#1064B2",   # Border
  u_color   = "#32436F",   # URL

  p_x       = 1.00,        # Title
  p_y       = 0.60,        # Title
  s_x       = 0.90,        # Subplot
  s_y       = 1.05,        # Subplot

  s_width   = 1.75,        # Subplot
  s_height  = 5.75,        # Subplot

  url       = "https://github.com/",

  spotlight = TRUE,
  l_alpha   = 0.10,
  l_width   = 4,
  l_height  = 4
)
