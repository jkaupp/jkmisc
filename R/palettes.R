#Lumina color palette
lumina <- c("#EDDAEB","#AD8CAE","#4F93B8","#306489","#222B4C")

# Queen's tri-color palette
tricolour <- c("#9d1939", "#eebd31", "#11335d")

#Graduate Attribute Colors
ga_colors <- c("#E12493", "#067F0A", "#08829F", "#F0A305", "#9E56E5", "#845508", "#B21C42", "#214A1E", "#B8AACC", "#97B438", "#B988EF", "#12B2B3") %>%
  stats::setNames(c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))

#AU Colors
au_colors <- RColorBrewer::brewer.pal(5, "Set1") %>%
  stats::setNames(c("Math","NS","CS","ES","ED"))
