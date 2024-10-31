#####FINAL PROJECT PLANT BEE INTERACTION ####


#Required libraries:
library(bipartite)
library(dplyr)

#Set your working directoru
setwd("Z:/Andres/Plantbee_web/")
getwd()

####Tribe level####

#Reading file with network matrix

D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
nam <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                  full.names = F)
Finalnam <- paste0("C:/Users/a124h117/Desktop/networks/output/metrics/metrics_", nam) #Create output filder
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_values <- matriz[,2:length(matriz)]
  final_values = as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  net <-networklevel(final_values)
  write.csv(net, Finalnam[i], row.names = TRUE)
}


#plotweb from all your matrices
D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
output_plotweb <- ("C:/Users/a124h117/Desktop/networks/output/plots/plotweb/")
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values <- as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  tiff_filename <- paste(output_plotweb,"plot", i, ".tiff", sep = "")
  tiff(filename = tiff_filename, width = 13, height = 9.5, units = "in", res = 600)
  plotweb(sortweb(final_values, sort.order="dec"),
          method="normal", 
          col.high = "darkgreen", 
          bor.col.high = "darkgreen",
          col.low = "darkorange", 
          bor.col.low = "darkorange",
          bor.col.interaction = "gray70",
          text.rot = 90,
          labsize =1.5)
  dev.off()
}

#visweb all your matrices
D <- list.files(path = "C:/Users/a124h117/Desktop/networks/input/", pattern = ".csv$", 
                full.names = T)
output_visweb <- ("C:/Users/a124h117/Desktop/networks/output/plots/visweb/")
for (i in 1:length(D)){
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values <- as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  tiff_filename <- paste(output_visweb,"plot", i, ".tiff", sep = "")
  tiff(filename = tiff_filename, width = 6, height = 6, units = "in", res = 600)
  ncolo <- length(table(final_values))
  col <- hcl.colors(ncolo-1, palette = "YlOrRd", rev = TRUE)
  rgb_cols <- c("#FFFFFFFF",col)
  visweb(final_values, 
         square="defined", 
         def.col=rgb_cols, 
         labsize = 1.5,
         box.border="grey")
  dev.off()
}


### Tribes strenghts ###

#Imput data
D <- list.files(path = "Z:/Andres/Plantbee_web/input/tribes/", pattern = ".csv$", 
                full.names = T)
nam <- list.files(path = "Z:/Andres/Plantbee_web/input/tribes/", pattern = ".csv$", 
                  full.names = F)

### Plants ###
Finalnam <- paste0("Z:/Andres/Plantbee_web/output_strength/plants/strength_", nam) #Create output filder
for (i in 1:length(D)){
  library(dplyr)
  library(bipartite)
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values = as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  strength <-strength(final_values,type = "Barrat")
  write.csv(strength, Finalnam[i], row.names = TRUE)
}

### Tribes ###
Finalnam <- paste0("Z:/Andres/Plantbee_web/output_strength/bees/strength_", nam) #Create output filder
for (i in 1:length(D)){
  library(dplyr)
  library(bipartite)
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  matriz <- matriz %>%
    group_by(Tribe) %>%
    summarise(across(everything(), sum, na.rm=TRUE))
  matriz_names <- matriz[1]
  matriz_names
  matriz_values <- matriz[,2:length(matriz)]
  final_values = as.matrix(matriz_values)
  rownames(final_values) = matriz$Tribe
  strength <-strength(t(final_values), type = "Barrat")
  write.csv(strength, Finalnam[i], row.names = TRUE)
}


#Saving multiple plots tribes
D <- list.files(path = "Z:/Andres/Plantbee_web/input_plot_strength/bees/", pattern = ".csv$", 
                full.names = T)
setwd("Z:/Andres/Plantbee_web/output_plot_strength/bees/")
for (i in 1:length(D)){
  library(ggplot2)
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  p <-ggplot(matriz, aes(x = reorder(Tribe, -Strength), y = Strength)) +
    geom_bar(stat = "identity", fill = "darkred") +
    labs(x = "Bee tribe") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text=element_text(color = "black"),
          axis.title=element_text(face="bold")) 
  tiff_filename <- paste("ggplot", i, ".tiff", sep = "")
  ggsave(filename = tiff_filename, plot = p, width = 6, height = 4, units = "in", dpi = 300)
}

#Saving multiple plots plants
#Saving multiple plots
setwd("Z:/Andres/Plantbee_web/output_plot_strength/plants/")
D <- list.files(path = "Z:/Andres/Plantbee_web/input_plot_strength/plants/", pattern = ".csv$", 
                full.names = T)
for (i in 1:length(D)){
  library(ggplot2)
  matriz <- read.csv(D[i])
  matriz <- as.data.frame(matriz)
  p <-ggplot(matriz, aes(x = reorder(Family, -Strength), y = Strength)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(x = "Plant family") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.text=element_text(color = "black"),
          axis.title=element_text(face="bold")) 
  tiff_filename <- paste("ggplot", i, ".tiff", sep = "")
  ggsave(filename = tiff_filename, plot = p, width = 6, height = 4, units = "in", dpi = 300)
}


############################### circle #######################################
library(circlize)

#Scrub
t_scrub <- read.csv("Z:/Andres/Plantbee_web/input/tribes/Scrub_all.csv", header=TRUE, ",")
scrub <- t_scrub %>%
  group_by(Tribe) %>%
  summarise(across(everything(), sum, na.rm=TRUE))
scrub <- as.data.frame(scrub)
names_scrub <- scrub[1]
names_scrub
values_scrub <- scrub[,2:length(scrub)]
final_scrub <- as.matrix(values_scrub)
rownames(final_scrub) <- names_scrub$Tribe
final_scrub

setwd("Z:/Andres/Plantbee_web/circles/")
tiff(filename = "scrub.tiff", width = 6, height = 6, units = "in", res = 600)
color <- c(Apocynaceae ="forestgreen", Asteraceae ="forestgreen",Boraginaceae ="forestgreen",
           Burseraceae ="forestgreen",Convolvulaceae ="forestgreen",Cucurbitaceae ="forestgreen",
           Euphorbiaceae ="forestgreen",Leguminosae ="forestgreen",Lamiaceae ="forestgreen",
           Malpighiaceae ="forestgreen", Malvaceae ="forestgreen",Nyctaginaceae ="forestgreen")
chordDiagram(final_scrub, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.7, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)
dev.off()

#Forest
t_forest <- read.csv("Z:/Andres/Plantbee_web/input/tribes/Forest_all.csv", header=TRUE, ",")
forest <- t_forest %>%
  group_by(Tribe) %>%
  summarise(across(everything(), sum, na.rm=TRUE))
forest <- as.data.frame(forest)
names_forest<- forest[1]
names_forest 
values_forest <- forest [,2:length(forest)]
final_forest<- as.matrix(values_forest)
rownames(final_forest ) <- names_forest$Tribe
final_forest 

setwd("Z:/Andres/Plantbee_web/circles/")
tiff(filename = "Forest.tiff", width = 6, height = 6, units = "in", res = 600)

color <- c(Apocynaceae ="forestgreen", Asteraceae ="forestgreen",Bignoniaceae ="forestgreen",
           Boraginaceae ="forestgreen", Burseraceae ="forestgreen",Convolvulaceae ="forestgreen",
           Euphorbiaceae ="forestgreen",Leguminosae ="forestgreen",Loasaceae ="forestgreen",
           Malpighiaceae ="forestgreen", Malvaceae ="forestgreen",Passifloraceae ="forestgreen",
           Solanaceae = "forestgreen", Vervenaceae = "forestgreen")

chordDiagram(final_forest, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.7, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)
dev.off()


#Taganga
t_taganga <- read.csv("Z:/Andres/Plantbee_web/input/tribes/Taganga_all.csv", header=TRUE, ",")
taganga <- t_taganga %>%
  group_by(Tribe) %>%
  summarise(across(everything(), sum, na.rm=TRUE))
taganga <- as.data.frame(taganga)
names_taganga<- taganga[1]
names_taganga 
values_taganga <- taganga[,2:length(taganga)]
final_taganga<- as.matrix(values_taganga)
rownames(final_taganga) <- names_taganga$Tribe
final_taganga 

setwd("Z:/Andres/Plantbee_web/circles/")
tiff(filename = "Taganga.tiff", width = 6, height = 6, units = "in", res = 600)
color <- c(Apocynaceae ="forestgreen", Convolvulaceae ="forestgreen",Nyctaginaceae ="forestgreen",
           Polygonaceae ="forestgreen",Leguminosae ="forestgreen",Malvaceae ="forestgreen",Bignoniaceae ="forestgreen",
           Cucurbitaceae ="forestgreen",Meliaceae ="forestgreen",Zygophyllaceae ="forestgreen",Capparaceae ="forestgreen",
           Scrophulariaceae ="forestgreen")

chordDiagram(final_taganga, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.7, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)
dev.off()

#Tatacoa
t_tatacoa <- read.csv("Z:/Andres/Plantbee_web/input/tribes/Tatacoa_all.csv", header=TRUE, ",")
tatacoa <- t_tatacoa %>%
  group_by(Tribe) %>%
  summarise(across(everything(), sum, na.rm=TRUE))
tatacoa <- as.data.frame(tatacoa)
names_tatacoa <- tatacoa[1]
names_tatacoa 
values_tatacoa <- tatacoa[,2:length(tatacoa)]
final_tatacoa <- as.matrix(values_tatacoa)
rownames(final_tatacoa) <- names_tatacoa$Tribe
final_tatacoa 

setwd("Z:/Andres/Plantbee_web/circles/")

tiff(filename = "Tatacoa.tiff", width = 6, height = 6, units = "in", res = 600)
color <- c(Boraginaceae ="forestgreen", Verbenaceae ="forestgreen",Convolvulaceae ="forestgreen",
           Euphorbiaceae ="forestgreen",Portulacaceae ="forestgreen",Malvaceae ="forestgreen",Rubiaceae ="forestgreen",
           Leguminosae ="forestgreen",Nyctaginaceae ="forestgreen",Solanaceae ="forestgreen",
           Capparaceae ="forestgreen",Anacardiaceae ="forestgreen",Apocynaceae ="forestgreen",Rutaceae ="forestgreen",
           Cactaceae ="forestgreen",Passifloraceae ="forestgreen")

chordDiagram(final_tatacoa, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.7, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)
dev.off()

#Besotes
t_besotes <- read.csv("Z:/Andres/Plantbee_web/input/tribes/Besotes_all_pollen.csv", header=TRUE, ",")
besotes <- t_besotes %>%
  group_by(Tribe) %>%
  summarise(across(everything(), sum, na.rm=TRUE))
besotes <- as.data.frame(besotes)
names_besotes <- besotes[1]
names_besotes 
values_besotes <- besotes[,2:length(besotes)]
final_besotes <- as.matrix(values_besotes)
rownames(final_besotes) <- names_besotes$Tribe
final_besotes

setwd("Z:/Andres/Plantbee_web/circles/")

tiff(filename = "besotes.tiff", width = 6, height = 6, units = "in", res = 600)

color <- c(Acanthaceae ="forestgreen",	Amaranthaceae ="forestgreen",	Anacardiaceae ="forestgreen",	Apocynaceae ="forestgreen",	
           Araceae ="forestgreen",	Arecaceae ="forestgreen",	Asteraceae ="forestgreen",Bignoniaceae ="forestgreen",Bombacaceae ="forestgreen",	
           Brasicaceae ="forestgreen",	Bromelia ="forestgreen",	Burseraceae ="forestgreen",	Cactaceae ="forestgreen",	Cochlospermaceae ="forestgreen",	
           Combretaceae ="forestgreen",	Cyperaceae ="forestgreen",	Euphorbiaceae ="forestgreen",	Leguminosae ="forestgreen",	Loranthaceae ="forestgreen",
           Malpigiaceae ="forestgreen",	Melastomataceae ="forestgreen",	Moraceae ="forestgreen",	Myrtaceae ="forestgreen",	Onagraceae ="forestgreen",	
           Passifloraceae ="forestgreen",	Piperacae ="forestgreen",	Poaceae ="forestgreen",	Podocarpaceae ="forestgreen",	Portulacaceae ="forestgreen",	
           Proteaceae ="forestgreen",	Pteridophyta ="forestgreen",	Rubiaceae ="forestgreen",	Rutaceae ="forestgreen",	Sapindaceae ="forestgreen",	Solanaceae ="forestgreen")

chordDiagram(final_besotes, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.3, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)
dev.off()

### par demo
par(mar = c(.1, .1, .1, .1),mfrow = c(3,2))


chordDiagram(final_forest, annotationTrack = "grid",
             preAllocateTracks = 1, grid.col = color)
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", cex = 0.7, niceFacing = TRUE, adj = c(0, 0.1))
}, bg.border = NA)



getwd()

library(knitr)
