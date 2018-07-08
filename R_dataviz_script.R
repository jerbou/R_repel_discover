# ==== chargement des lib ====

library(plyr)

library(httr)

library(jsonlite)

library(magrittr)

library(dplyr)

library(data.tree)

library(tidyr)

library(dplyr)

library(XML)

library(plyr)

library(xmlparsedata)

library(data.table)

library(dtplyr)

library(WriteXLS)

library(xlsx)

library(XML2R)

library(xml2)

# options(encoding = "UTF-8")

library(readr)

library(dplyr)

library(magrittr)

library(purrr)

library(stringr)

library(stringi)

library(purrr)

library(addinslist)

library(questionr)

library(sqldf)

library(rgdal)

library(readr)

library(tibble)

library(ggplot2)

library(ggrepel)

# ==== 00 set du wd et creation des df ====

setwd("C:/COPY_data_local/data_gouv_fr/budget_commune/data_in")

 

df_2016 <- read_csv("2016(1).csv")

# str(df_2016)

 

df_2016 <- as_data_frame(df_2016)

?as_data_frame

# View(df_2016)

dim(df_2016)

 

# select <- subset(df_2016, df_2016$commune == 'SAPOIS')

# select1 <- subset(df_2016, df_2016$depcom == '88075')

 

df0 <- subset(df_2016, dep == 21 | dep == 25 | dep == 39 | dep == 58 | dep == 70 | dep == 71 | dep == 89 | dep == 90)

names(df0)

View(subset(df0, df0$commune=='AGEY'))

df1 <- select(df0, population , produits_total, taxe_habitation)

df3l <- select(df3k, population , produits_total, taxe_habitation)

 

pairs(df3l)

 

dim(df0)

str(df0)

 

# ==== dataviz =====

 

 

# ==== produit total ====

g1 <- ggplot(data=df0, aes(x = factor(df0$dep), y=df0$produits_total, fill=df0$dep))

g1 +geom_bar(stat="identity",width = 0.8) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

 

g1 + geom_boxplot()

geom_label_repel()

 

# g1 + geom_density(aes(y=df0$produits_total))

 

summary(df0)

 

 

 

# ==== Etude repartition population TO COMPLETE ========

#

hist(df0$population)

 

# exemple ggplot(diamonds, aes(price, colour = cut)) +  geom_freqpoly(binwidth = 500)

ggplot(df0, aes(x=df0$population, colour = df0$dep)) + geom_freqpoly(binwidth = 200)

# distribution toute écrasé car des communes à plus de 150 000 pers.

 

# TO DO =faire un graphe by summary, retrouver pyramides ages pour fixer le nombre, et l'etendu des classes.

dfpyr <- cut(df0$population, breaks = seq(0, max(df0$population), 10000, right = FALSE))

p1 <- ggplot(data=df0, aes(x = factor(df0$dep), y=df0$population, fill=df0$dep))

p1 <- ggplot(data=df0) +geom_density(data=df0, aes(df0$population), adjust = 2, size=1) + labs(x="Montant de Cotisation", y = "densité")

p1

summary(df0$population)

# moyenne à 700 hab

 

# on va purger le nom des communes de plus

 

p2 <- ggplot(data=df0, aes(y=df0$population, x=factor(df0$dep))) # + geom_boxplot(alpha=0.3)

p2 + labs(x="département", y = "population") +geom_jitter(alpha=0.1)

# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

 

 

p3 <- ggplot(data=df0, aes(y=df0$population, x=factor(df0$dep), label =df0$commune)) + geom_jitter(color = ifelse(df0$population > 15000, "red", "gray50"))

# pb de labeller les subset puisque pas la meme dimensiion on passe par un ifelse

 

p31 <- ggplot(data=df0, aes(y=df0$population, x=df0$depcom)) # + geom_boxplot(alpha=0.3)

p32 <- p31 + labs(x="département", y = "population") +geom_point(alpha=0.3) + scale_x_discrete(breaks=NULL)

df15 <- subset(df0, df0$population > 15000)

df3k <- subset(df0, df0$population > 3500)

 

p33 <- p32 + geom_point(data=df3k,aes(y=df3k$population, x=df3k$depcom) , color = "orange")

p34 <- p33  + geom_point(data=df15,aes(y=df15$population, x=df15$depcom) , color = "red")

# p32 <- ggplot(data=df0, aes(y=df0$population, x=df0$depcom), label =df0$commune) + geom_point(color = ifelse(df0$population > 15000, "red", "gray50"))

# p32 + geom_text_repel(aes(label = df0$commune))

p34 + geom_label_repel(data=df15, aes(y=df15$population, x=df15$depcom) , label = df15$commune)

p34

 

names(df0)

i30 <- ggplot(data=df0, aes(y=df0$produits_total, x=factor(df0$dep))) + geom_boxplot(alpha=0.3)

i31  <- ggplot(data=df0, aes(y=df0$produits_total, x=df0$depcom))

i32 <- i31 + labs(x="commune", y = "produits total") +geom_point(alpha=0.3) + scale_x_discrete(breaks=NULL)

i32

 

 

# TO DO : mettre un ggarrange pour cote cote plusieurs graf -------------------------------------------------------------------------------------

# ggarrange(p34, i32)

 

 

# p4 <- p3 + geom_jitter(color = ifelse(df15$population > 15000, "red", "gray50"))

p4 + geom_text_repel(aes(label = df15$commune))

 

p3 <- ggplot(data=df0, aes(y=df0$population, x=factor(df0$dep), label =df0$commune))

p4 <- p3 + geom_jitter(color = ifelse(df0$population > 15000, "red", "gray50"))

p4 + geom_text_repel(data = subset(df0, df0$population > 15000),    aes(label = commune))

 

# methode de contournement en ecrivant directly les position en jitter

# https://github.com/slowkow/ggrepel/issues/52

 

 

df15 <-  subset(df0,  df0$population > 15000)

g2 <- ggplot(data=df15, aes(x = df15$population, y=df15$produits_total, label = df15$commune, color = df15$dep))

g3 <- g2 + geom_point()

# https://ggplot2.tidyverse.org/reference/geom_text.html

# g3 + geom_label(aes(fill = factor(commune)), colour = "white", fontface = "bold") 

# g3 + geom_text_repel(show.legend = FALSE,  fontface = "bold")

g3 + geom_label_repel(show.legend = FALSE,  fontface = "bold")

# + geom_smooth(aes(x = df15$population, y=df15$produits_total))

 

dfsub15 <-  subset(df0,  df0$population < 15000)

g2 <- ggplot(data=dfsub15, aes(x = dfsub15$population, y=dfsub15$produits_total, label = dfsub15$commune, color = dfsub15$dep))

g3_sub <- g2 + geom_point()

 

 

pairs(df3k)

 

 

g2 <- ggplot(data=df0, aes(x = population, y=produits_total, label = commune, color = dep))

g3 <- g2 + geom_point()

g3 + geom_label_repel(show.legend = FALSE,  fontface = "bold")

 

 

 

# https://stackoverflow.com/questions/34643380/automatically-vary-the-positions-of-labels-with-geom-text-when-they-overlie-each

 

 

# ==== 02 : QUESTIONS

# === relation entre variables ?

# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

 

a <- df0[5:47]

cor(a)

is.numeric(a)

is.na(a) <- 0

 

# http://r.789695.n4.nabble.com/How-to-exclude-a-column-by-name-td892627.html

 

b <- a[,-which(names(a) == "avance_tresor")]

# on test toutes les variables identifié comme numériques

#  na, à traiter en que zéro

cormat <- cor(b)

 

library(reshape2)

melted_cormat <- melt(cor(b))

 

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +

  geom_tile()

 

# fonctions -------------------------------------------------------------------------------------------------------------------------------------

# creation des fonctions

get_upper_tri <- function(cormat){

  cormat[lower.tri(cormat)]<- NA

  return(cormat)

}

 

reorder_cormat <- function(cormat){

  # Use correlation between variables as distance

  dd <- as.dist((1-cormat)/2)

  hc <- hclust(dd)

  cormat <-cormat[hc$order, hc$order]

}

 

upper_tri <- get_upper_tri(cormat)

upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

 

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+

  geom_tile(color = "white")+

  scale_fill_gradient2(low = "blue", high = "red", mid = "white",

                       midpoint = 0, limit = c(-1,1), space = "Lab",

                       name="Pearson\nCorrelation") +

# theme_minimal()+

  theme(axis.text.x = element_text(angle = 45, vjust = 1,

                                   size = 12, hjust = 1))+

  coord_fixed()

 

 

# ======= on va limiter a quelques variables

View(names(df0))

 

 

cormat <- cor(df1)

cormat <- cor(df3l)

 

# ====

cormat <- reorder_cormat(cormat)

upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+

  geom_tile(color = "white")+

  scale_fill_gradient2(low = "blue", high = "red", mid = "white",

                       midpoint = 0, limit = c(-1,1), space = "Lab",

                       name="Pearson\nCorrelation") +

  theme_minimal()+ # minimal theme

  theme(axis.text.x = element_text(angle = 45, vjust = 1,

                                   size = 12, hjust = 1))+

  coord_fixed()

# Print the heatmap

print(ggheatmap)

 

ggheatmap +

  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +

  theme(

    axis.title.x = element_blank(),

    axis.title.y = element_blank(),

    panel.grid.major = element_blank(),

    panel.border = element_blank(),

    panel.background = element_blank(),

    axis.ticks = element_blank(),

    legend.justification = c(1, 0),

    legend.position = c(0.6, 0.7),

    legend.direction = "horizontal")+

  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,

                               title.position = "top", title.hjust = 0.5))

 

 

pairs(df1)

cor.test(~ df1$population + df1$taxe_habitation, data = df1)

 

cor.test(~ df1$population + df1$taxe_habitation, data = df1)

 

# ========== BOXPLOT de qq variables interressante ==========

g4 <- ggplot(data=df0, aes(x = factor(df0$dep), y=df0$produits_total, fill=df0$dep, label=df0$commune))

g4 + geom_boxplot()

g4 +  geom_label_repel(data = subset(df0, df0$produits_total > 50000), aes(label=commune) , show.legend = FALSE,  fontface = "bold")
