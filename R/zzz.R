.onLoad <- function(libname, pkgname) {

  # Construct the path to the font file
  font_path <- system.file("fonts/FFF-AcidGrotesk-Book.woff2",
                           package = pkgname)

  # Add the font using sysfonts
  sysfonts::font_add("acidgrotesk-book", regular = font_path)

  # Automatically use showtext for all plots
  showtext::showtext_auto()
}
