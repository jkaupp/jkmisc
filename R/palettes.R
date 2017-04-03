#' Lumina color palette
#' @export
.lumina <- c("#EDDAEB","#AD8CAE","#4F93B8","#306489","#222B4C")

#' Queen's tri-color palette
#' @export
.tricolour <- c("#9d1939", "#eebd31", "#11335d")

#' Graduate Attribute Colors
#' @export
.ga_colors <- c("#E12493", "#067F0A", "#08829F", "#F0A305", "#9E56E5", "#845508", "#B21C42", "#214A1E", "#B8AACC", "#97B438", "#B988EF", "#12B2B3") %>%
  stats::setNames(c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))

#' AU Colors
#' @export
.au_colors <- RColorBrewer::brewer.pal(5, "Set1") %>%
  stats::setNames(c("Math","NS","CS","ES","ED"))

# FOCUS PALETTES
# Red as highlight
#' @export
.redfocus = c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

# Green as highlight
#' @export
.greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

# Blue as highlight
#' @export
.bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

# EQUAL WEIGHT
# Generated with rainbow(12, s = 0.6, v = 0.75)
#' @export
.rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")
#' @export
.rainbow10equal = c("#BF4D4D", "#BF914D", "#A8BF4D", "#63BF4D", "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", "#BF4D91")
#' @export
.rainbow8equal = c("#BF4D4D", "#BFA34D", "#86BF4D", "#4DBF69", "#4DBFBF", "#4D69BF", "#864DBF", "#BF4DA3")
#' @export
.rainbow6equal = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF", "#4D4DBF", "#BF4DBF")

# Generated with package "gplots" function rich.colors(12)
#' @export
.rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", "#FFC200", "#FF8500", "#FF3300")
#' @export
.rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", "#FF3300")
#' @export
.rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")
#' @export
.rich6equal = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", "#FFCC00", "#FF3300")

# Generated with package "fields" function tim.colors(12), which is said to emulate the default matlab colorset
#' @export
.tim12equal = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000")
#' @export
.tim10equal = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#800000")
#' @export
.tim8equal = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000")
#' @export
.tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000")

# Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2
#' @export
.dark8equal = c("#1B9E77", "#666666", "#66A61E", "#7570B3", "#A6761D", "#D95F02", "#E6AB02", "#E7298A")
#' @export
.dark6equal = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E6AB02", "#E7298A")
#' @export
.set8equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")
#' @export
.set6equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", "#FC8D62", "#FFD92F")

# Qualitative color schemes by Paul Tol
#' @export
.tol1qualitative=c("#4477AA")
#' @export
.tol2qualitative=c("#4477AA", "#CC6677")
#' @export
.tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
#' @export
.tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
#' @export
.tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
#' @export
.tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
#' @export
.tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
#' @export
.tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
#' @export
.tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
