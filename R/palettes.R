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
.redfocus <- c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

# Green as highlight
#' @export
.greenfocus <- c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

# Blue as highlight
#' @export
.bluefocus <- c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")


# Qualitative color schemes by Paul Tol
#' @export
.tol1qualitative <- c("#4477AA")
#' @export
.tol2qualitative <- c("#4477AA", "#CC6677")
#' @export
.tol3qualitative <- c("#4477AA", "#DDCC77", "#CC6677")
#' @export
.tol4qualitative <- c("#4477AA", "#117733", "#DDCC77", "#CC6677")
#' @export
.tol5qualitative <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
#' @export
.tol6qualitative <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol7qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol8qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
#' @export
.tol9qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
#' @export
.tol10qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
#' @export
.tol11qualitative <- c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
#' @export
.tol12qualitative <-c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
