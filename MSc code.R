rm(list=ls())

setwd("C:/Users/mendi/Documents/MSc Stuff/ Dissertation")


#install packages
library(ggplot2)

#DOC vs latitude

doc_all<- read.csv ("DOC main doc.csv", row.names=1)

sml2022<-subset(doc_all, Group == "SML22")
sml2023<-subset(doc_all, Group == "SML23")
ssw2022<-subset(doc_all, Group == "SSW22")
ssw2023<-subset(doc_all, Group == "SSW23")

#Hydrophilic DOC vs Latitude
colours <- c("sml2022" = "cyan3", "ssw2022" = "darkorchid", "sml2022" = "goldenrod2", "ssw2023" = "forestgreen")
shapes <- c("sml2022" = 16, "ssw2022" =16, "sml2023" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=Latitude, y=Hydrophilic_DOC, color="sml2022", shape="sml2022")) + 
  geom_point(data=ssw2022, aes(x=Latitude, y=Hydrophilic_DOC, color="ssw2022", shape="ssw2022")) +
  geom_line(data=sml2022, aes(x=Latitude, y=Hydrophilic_DOC, color="sml2022"), group = 1) +
  geom_line(data=ssw2022, aes(x=Latitude, y=Hydrophilic_DOC, color="ssw2022"), group = 1) +
  
  geom_point(data=sml2023, aes(x=Latitude, y=Hydrophilic_DOC, color="sml2023", shape="sml_2023")) + 
  geom_point(data=ssw2023, aes(x=Latitude, y=Hydrophilic_DOC, color="ssw2023", shape="ssw_2023")) +
  geom_line(data=sml2023, aes(x=Latitude, y=Hydrophilic_DOC, color="sml2023"), group = 1) +
  geom_line(data=ssw2023, aes(x=Latitude, y=Hydrophilic_DOC, color="ssw2023"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Hydrophilic DOC (ppb))")

#Pearson
sml2022$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022$Hydrophilic_DOC <- as.numeric(as.character(sml2022$Hydrophilic_DOC))

res <- cor.test(sml2022$Latitude, sml2022$Hydrophilic_DOC, 
                method = "pearson")
res

ssw2022$Latitude <- as.numeric(as.character(ssw2022$Latitude))
ssw2022$Hydrophilic_DOC <- as.numeric(as.character(ssw2022$Hydrophilic_DOC))

res <- cor.test(ssw2022$Latitude, ssw2022$Hydrophilic_DOC, 
                method = "pearson")
res


sml2023$Latitude <- as.numeric(as.character(sml2023$Latitude))
sml2023$Hydrophilic_DOC <- as.numeric(as.character(sml2023$Hydrophilic_DOC))

res <- cor.test(sml2023$Latitude, sml2023$Hydrophilic_DOC, 
                method = "pearson")
res

ssw2023$Latitude <- as.numeric(as.character(ssw2023$Latitude))
ssw2023$Hydrophilic_DOC <- as.numeric(as.character(ssw2023$Hydrophilic_DOC))

res <- cor.test(ssw2023$Latitude, ssw2023$Hydrophilic_DOC, 
                method = "pearson")
res


#Biopolymers vs Latitude
colours <- c("sml2022" = "cyan3", "ssw2022" = "darkorchid", "sml2022" = "goldenrod2", "ssw2023" = "forestgreen")
shapes <- c("sml2022" = 16, "ssw2022" =16, "sml2023" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=Latitude, y=Biopolymers, color="sml2022", shape="sml2022")) + 
  geom_point(data=ssw2022, aes(x=Latitude, y=Biopolymers, color="ssw2022", shape="ssw2022")) +
  geom_line(data=sml2022, aes(x=Latitude, y=Biopolymers, color="sml2022"), group = 1) +
  geom_line(data=ssw2022, aes(x=Latitude, y=Biopolymers, color="ssw2022"), group = 1) +
  
  geom_point(data=sml2023, aes(x=Latitude, y=Biopolymers, color="sml2023", shape="sml_2023")) + 
  geom_point(data=ssw2023, aes(x=Latitude, y=Biopolymers, color="ssw2023", shape="ssw_2023")) +
  geom_line(data=sml2023, aes(x=Latitude, y=Biopolymers, color="sml2023"), group = 1) +
  geom_line(data=ssw2023, aes(x=Latitude, y=Biopolymers, color="ssw2023"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Biopolymers (ppb))")

#Pearson Correlation 
sml2022$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022$Biopolymers <- as.numeric(as.character(sml2022$Biopolymers))

res <- cor.test(sml2022$Latitude, sml2022$Biopolymers, 
                method = "pearson")
res

ssw2022$Latitude <- as.numeric(as.character(ssw2022$Latitude))
ssw2022$Biopolymers <- as.numeric(as.character(ssw2022$Biopolymers))

res <- cor.test(ssw2022$Latitude, ssw2022$Biopolymers, 
                method = "pearson")
res


sml2023$Latitude <- as.numeric(as.character(sml2023$Latitude))
sml2023$Biopolymers <- as.numeric(as.character(sml2023$Biopolymers))

res <- cor.test(sml2023$Latitude, sml2023$Biopolymers, 
                method = "pearson")
res

ssw2023$Latitude <- as.numeric(as.character(ssw2023$Latitude))
ssw2023$Biopolymers <- as.numeric(as.character(ssw2023$Biopolymers))

res <- cor.test(ssw2023$Latitude, ssw2023$Biopolymers, 
                method = "pearson")
res

#Humic Substances vs Latitude

colours <- c("sml2022" = "cyan3", "ssw2022" = "darkorchid", "sml2022" = "goldenrod2", "ssw2023" = "forestgreen")
shapes <- c("sml2022" = 16, "ssw2022" =16, "sml2023" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=Latitude, y=Humics, color="sml2022", shape="sml2022")) + 
  geom_point(data=ssw2022, aes(x=Latitude, y=Humics, color="ssw2022", shape="ssw2022")) +
  geom_line(data=sml2022, aes(x=Latitude, y=Humics, color="sml2022"), group = 1) +
  geom_line(data=ssw2022, aes(x=Latitude, y=Humics, color="ssw2022"), group = 1) +
  
  geom_point(data=sml2023, aes(x=Latitude, y=Humics, color="sml2023", shape="sml2023")) + 
  geom_point(data=ssw2023, aes(x=Latitude, y=Humics, color="ssw2023", shape="ssw22023")) +
  geom_line(data=sml2023, aes(x=Latitude, y=Humics, color="sml2023"), group = 1) +
  geom_line(data=ssw2023, aes(x=Latitude, y=Humics, color="ssw2023"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Humic Substances (ppb))")

#Pearson Correlation 
sml2022$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022$Humics <- as.numeric(as.character(sml2022$Humics))

res <- cor.test(sml2022$Latitude, sml2022$Humics, 
                method = "pearson")
res

ssw2022$Latitude <- as.numeric(as.character(ssw2022$Latitude))
ssw2022$Humics <- as.numeric(as.character(ssw2022$Humics))

res <- cor.test(ssw2022$Latitude, ssw2022$Humics, 
                method = "pearson")
res


sml2023$Latitude <- as.numeric(as.character(sml2023$Latitude))
sml2023$Humics <- as.numeric(as.character(sml2023$Humics))

res <- cor.test(sml2023$Latitude, sml2023$Humics, 
                method = "pearson")
res

ssw2023$Latitude <- as.numeric(as.character(ssw2023$Latitude))
ssw2023$Humics <- as.numeric(as.character(ssw2023$Humics))

res <- cor.test(ssw2023$Latitude, ssw2023$Humics, 
                method = "pearson")
res

#Building blocks vs Latitude

colours <- c("sml2022" = "cyan3", "ssw2022" = "darkorchid", "sml2022" = "goldenrod2", "ssw2023" = "forestgreen")
shapes <- c("sml2022" = 16, "ssw2022" =16, "sml2023" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=Latitude, y=Building_blocks, color="sml2022", shape="sml2022")) + 
  geom_point(data=ssw2022, aes(x=Latitude, y=Building_blocks, color="ssw2022", shape="ssw2022")) +
  geom_line(data=sml2022, aes(x=Latitude, y=Building_blocks, color="sml2022"), group = 1) +
  geom_line(data=ssw2022, aes(x=Latitude, y=Building_blocks, color="ssw2022"), group = 1) +
  
  geom_point(data=sml2023, aes(x=Latitude, y=Building_blocks, color="sml2023", shape="sml2023")) + 
  geom_point(data=ssw2023, aes(x=Latitude, y=Building_blocks, color="ssw2023", shape="ssw2023")) +
  geom_line(data=sml2023, aes(x=Latitude, y=Building_blocks, color="sml2023"), group = 1) +
  geom_line(data=ssw2023, aes(x=Latitude, y=Building_blocks, color="ssw2023"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Building blocks (ppb))")

#Pearson Correlation 
sml2022$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022$Building_blocks <- as.numeric(as.character(sml2022$Building_blocks))

res <- cor.test(sml2022$Latitude, sml2022$Building_blocks, 
                method = "pearson")
res

ssw2022$Latitude <- as.numeric(as.character(ssw2022$Latitude))
ssw2022$Building_blocks <- as.numeric(as.character(ssw2022$Building_blocks))

res <- cor.test(ssw2022$Latitude, ssw2022$Building_blocks, 
                method = "pearson")
res


sml2023$Latitude <- as.numeric(as.character(sml2023$Latitude))
sml2023$Building_blocks <- as.numeric(as.character(sml2023$Building_blocks))

res <- cor.test(sml2023$Latitude, sml2023$Building_blocks, 
                method = "pearson")
res

ssw2023$Latitude <- as.numeric(as.character(ssw2023$Latitude))
ssw2023$Building_blocks<- as.numeric(as.character(ssw2023$Building_blocks))

res <- cor.test(ssw2023$Latitude, ssw2023$Building_blocks, 
                method = "pearson")
res

#LMW Neutrals vs Latitude

colours <- c("sml2022" = "cyan3", "ssw2022" = "darkorchid", "sml2022" = "goldenrod2", "ssw2023" = "forestgreen")
shapes <- c("sml2022" = 16, "ssw2022" =16, "sml2023" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=Latitude, y=LMW_Neutrals, color="sml2022", shape="sml2022")) + 
  geom_point(data=ssw2022, aes(x=Latitude, y=LMW_Neutrals, color="ssw2022", shape="ssw2022")) +
  geom_line(data=sml2022, aes(x=Latitude, y=LMW_Neutrals, color="sml2022"), group = 1) +
  geom_line(data=ssw2022, aes(x=Latitude, y=LMW_Neutrals, color="ssw2022"), group = 1) +
  
  geom_point(data=sml2023, aes(x=Latitude, y=LMW_Neutrals, color="sml2023", shape="sml2023")) + 
  geom_point(data=ssw2023, aes(x=Latitude, y=LMW_Neutrals, color="ssw2023", shape="ssw2023")) +
  geom_line(data=sml2023, aes(x=Latitude, y=LMW_Neutrals, color="sml2023"), group = 1) +
  geom_line(data=ssw2023, aes(x=Latitude, y=LMW_Neutrals, color="ssw2023"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("LMW Neutrals (ppb))")

#Pearson Correlation 
sml2022$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022$LMW_Neutrals <- as.numeric(as.character(sml2022$LMW_Neutrals))

res <- cor.test(sml2022$Latitude, sml2022$LMW_Neutrals, 
                method = "pearson")
res

ssw2022$Latitude <- as.numeric(as.character(ssw2022$Latitude))
ssw2022$LMW_Neutrals <- as.numeric(as.character(ssw2022$LMW_Neutrals))

res <- cor.test(ssw2022$Latitude, ssw2022$LMW_Neutrals, 
                method = "pearson")
res


sml2023$Latitude <- as.numeric(as.character(sml2023$Latitude))
sml2023$LMW_Neutrals <- as.numeric(as.character(sml2023$LMW_Neutrals))

res <- cor.test(sml2023$Latitude, sml2023$LMW_Neutrals, 
                method = "pearson")
res

ssw2023$Latitude <- as.numeric(as.character(ssw2023$Latitude))
ssw2023$LMW_Neutrals<- as.numeric(as.character(ssw2023$LMW_Neutrals))

res <- cor.test(ssw2023$Latitude, ssw2023$LMW_Neutrals, 
                method = "pearson")
res


#DON
don_all<- read.csv ("DON all.csv", row.names=1)
sml2022_DON<-subset(don_all, Group == "SML2022")
sml2023_DON<-subset(don_all, Group == "SML2023")
ssw2022_DON<-subset(don_all, Group == "SSW2022")
ssw2023_DON<-subset(don_all, Group == "SSW2023")

#DON Biopolymers vs Latitude
colours <- c("sml2022_DON" = "cyan3", "ssw2022_DON" = "darkorchid", "sml2022_DON" = "goldenrod2", "ssw2023_DON" = "forestgreen")
shapes <- c("sml2022_DON" = 16, "ssw2022_DON" =16, "sml2023_DON" = 16, "ssw2023_DON" = 16)

ggplot() +
  geom_point(data=sml2022_DON, aes(x=Latitude, y=Biopolymer_DON, color="sml2022_DON", shape="sml2022_DON")) + 
  geom_point(data=ssw2022_DON, aes(x=Latitude, y=Biopolymer_DON, color="ssw2022_DON", shape="ssw2022_DON")) +
  geom_line(data=sml2022_DON, aes(x=Latitude, y=Biopolymer_DON, color="sml2022_DON"), group = 1) +
  geom_line(data=ssw2022_DON, aes(x=Latitude, y=Biopolymer_DON, color="ssw2022_DON"), group = 1) +
  
  geom_point(data=sml2023_DON, aes(x=Latitude, y=Biopolymer_DON, color="sml2023_DON", shape="sml_2023_DON")) + 
  geom_point(data=ssw2023_DON, aes(x=Latitude, y=Biopolymer_DON, color="ssw2023_DON", shape="ssw2023_DON")) +
  geom_line(data=sml2023_DON, aes(x=Latitude, y=Biopolymer_DON, color="sml2023_DON"), group = 1) +
  geom_line(data=ssw2023_DON, aes(x=Latitude, y=Biopolymer_DON, color="ssw2023_DON"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Biopolymers (ppb))")

#Pearson Correlation 
sml2022_DON$Latitude <- as.numeric(as.character(sml2022_DON$Latitude))
sml2022_DON$Biopolymer_DON <- as.numeric(as.character(sml2022_DON$Biopolymer_DON))

res <- cor.test(sml2022_DON$Latitude, sml2022$Biopolymer_DON, 
                method = "pearson")
res

ssw2022_DON$Latitude <- as.numeric(as.character(ssw2022_DON$Latitude))
ssw2022_DON$Biopolymer_DON <- as.numeric(as.character(ssw2022_DON$Biopolymer_DON))

res <- cor.test(ssw2022_DON$Latitude, ssw2022_DON$Biopolymer_DON, 
                method = "pearson")
res


sml2023_DON$Latitude <- as.numeric(as.character(sml2023_DON$Latitude))
sml2023_DON$Biopolymer_DON <- as.numeric(as.character(sml2023_DON$Biopolymer_DON))

res <- cor.test(sml2023_DON$Latitude, sml2023_DON$Biopolymer_DON, 
                method = "pearson")
res

ssw2023_DON$Latitude <- as.numeric(as.character(ssw2023_DON$Latitude))
ssw2023_DON$Biopolymer_DON <- as.numeric(as.character(ssw2023_DON$Biopolymer_DON))

res <- cor.test(ssw2023_DON$Latitude, ssw2023_DON$Biopolymer_DON, 
                method = "pearson")
res


#DON Humic Substances vs Latitude

ggplot() +
  geom_point(data=sml2022_DON, aes(x=Latitude, y=Humic_DON, color="sml2022_DON", shape="sml2022_DON")) + 
  geom_point(data=ssw2022_DON, aes(x=Latitude, y=Humic_DON, color="ssw2022_DON", shape="ssw2022_DON")) +
  geom_line(data=sml2022_DON, aes(x=Latitude, y=Humic_DON, color="sml2022_DON"), group = 1) +
  geom_line(data=ssw2022_DON, aes(x=Latitude, y=Humic_DON, color="ssw2022_DON"), group = 1) +
  
  geom_point(data=sml2023_DON, aes(x=Latitude, y=Humic_DON, color="sml2023_DON", shape="sml2023_DON")) + 
  geom_point(data=ssw2023_DON, aes(x=Latitude, y=Humic_DON, color="ssw2023_DON", shape="ssw2023_DON")) +
  geom_line(data=sml2023_DON, aes(x=Latitude, y=Humic_DON, color="sml2023_DON"), group = 1) +
  geom_line(data=ssw2023_DON, aes(x=Latitude, y=Humic_DON, color="ssw2023_DON"),  group = 1) +
  
  scale_color_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c("cyan3", "goldenrod2", "darkorchid", "forestgreen")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("SML 2022", "SML 2023", "SSW 2022", "SSW 2023"),
    values = c(16, 16, 16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Humic Substances (ppb))")

#Pearson Correlation 
sml2022_DON$Latitude <- as.numeric(as.character(sml2022$Latitude))
sml2022_DON$Humic_DON <- as.numeric(as.character(sml2022$Humics))

res <- cor.test(sml2022$Latitude, sml2022$Humics, 
                method = "pearson")
res

ssw2022_DON$Latitude <- as.numeric(as.character(ssw2022_DON$Latitude))
ssw2022_DON$Humic_DON <- as.numeric(as.character(ssw2022_DON$Humic_DON))

res <- cor.test(ssw2022_DON$Latitude, ssw2022_DON$Humic_DON, 
                method = "pearson")
res


sml2023_DON$Latitude <- as.numeric(as.character(sml2023_DON$Latitude))
sml2023_DON$Humic_DON <- as.numeric(as.character(sml2023_DON$Humic_DON))

res <- cor.test(sml2023_DON$Latitude, sml2023_DON$Humic_DON, 
                method = "pearson")
res

ssw2023_DON$Latitude <- as.numeric(as.character(ssw2023_DON$Latitude))
ssw2023_DON$Humic_DON <- as.numeric(as.character(ssw2023_DON$Humic_DON))

res <- cor.test(ssw2023_DON$Latitude, ssw2023_DON$Humic_DON, 
                method = "pearson")
res


#Surfactant Enrichment Factor
SA_EF<- read.csv("enrichment.csv", row.names=NULL)
EF_2022<-subset(SA_EF, Year == "2022")
EF_2023<-subset(SA_EF, Year == "2023")

colours <- c("EF_2022" = "darkseagreen2", "EF_2023" = "deepskyblue")
shapes <- c("EF_2022" = 16, "EF_2023" = 16)

ggplot() +
  geom_point(data=EF_2022, aes(x=Latitude, y=EF, color="EF_2022", shape="EF_2022")) + 
  geom_point(data=EF_2023, aes(x=Latitude, y=EF, color="EF_2023", shape="EF_2023")) +
  geom_line(data=EF_2022, aes(x=Latitude, y=EF, color="EF_2022"),  group = 1) +
  geom_line(data=EF_2023, aes(x=Latitude, y=EF, color="EF_2023"),  group = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("darkseagreen2", "deepskyblue")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Latitude (°N)") +
  ylab("Enrichment Factor)")

#Pearson Correlation

res <- cor.test(EF_2022$Latitude, EF_2022$EF, 
                method = "pearson")
res

res <- cor.test(EF_2023$Latitude, EF_2023$EF, 
                method = "pearson")
res


#EF vs Longhurst23
EF_longhurst23<- read.csv ("longhurst2023.csv", row.names = 1)
EF_longhurst23$Longhurst <- factor(
  EF_longhurst23$Longhurst,
  levels = c("NATR", "NAST(E)", "NADR")
)
boxplot(EF~Longhurst,data=EF_longhurst23, xlab= "Longhurst Biogeographical Province", ylab= "Enrichment Factor", col = c("#CD4F39","#8B5A2B","deeppink3"))    

kruskal.test(EF ~ Longhurst, data = EF_longhurst23)

#EF vs Longhurst 22
EF_longhurst22<- read.csv ("longhurst 2022.csv", row.names = 1)
EF_longhurst22$Longhurst <- factor(
  EF_longhurst22$Longhurst,
  levels = c("NATR", "NAST(E)", "CNRY", "NADR", "NECS")
)
boxplot(EF~Longhurst,data=EF_longhurst22, xlab= "Longhurst Biogeographical Province", ylab= "Enrichment Factor", col = c("#CD4F39","#8B5A2B","salmon", "deeppink3", "#00C5CD"))    

kruskal.test(EF ~ Longhurst, data = EF_longhurst22)


#SA vs Longhurst 
longhurstsa_2022<- read.csv ("Longhurst SA 2022.csv", row.names = 1)
longhurst22_sml <- subset(longhurstsa_2022, DEPTH == "SML")
longhurst22_ssw <- subset(longhurstsa_2022, DEPTH == "SSW")

longhurst22_sml$Longhurst <- factor(
  longhurst22_sml$Longhurst,
  levels = c("NATR", "NAST(E)", "CNRY", "NADR", "NECS")
)
boxplot(SA~Longhurst,data=longhurst22_sml, xlab= "Longhurst Biogeographical Province", ylab= "Surfactant Activity (µg)", col = c("#CD4F39","#8B5A2B","salmon", "deeppink3", "#00C5CD"))    

kruskal.test(SA ~ Longhurst, data = longhurst22_sml)


longhurst22_ssw$Longhurst <- factor(
  longhurst22_ssw$Longhurst,
  levels = c("NATR", "NAST(E)", "CNRY", "NADR", "NECS")
)

boxplot(SA~Longhurst,data=longhurst22_ssw, xlab= "Longhurst Biogeographical Province", ylab= "Surfactant Activity (µg)", col = c("#CD4F39","#8B5A2B","salmon", "deeppink3", "#00C5CD"))    

kruskal.test(SA ~ Longhurst, data = longhurst22_ssw)

longhurst23_sa<- read.csv("Longhurst SA 2023.csv", row.names=1)

longhurst23_sml<- subset(longhurst23_sa, DEPTH == "SML")
longhurst23_ssw <-  subset(longhurst23_sa, DEPTH == "SSW")

longhurst23_sml$Longhurst <- factor(
  longhurst23_sml$Longhurst,
  levels = c("NATR", "NAST(E)", "NADR")
)
boxplot(SA~Longhurst,data=longhurst23_sml, xlab= "Longhurst Biogeographical Province", ylab= "Surfactant Activity (µg)", col = c("#CD4F39","salmon", "deeppink3"))    

kruskal.test(SA ~ Longhurst, data = longhurst23_sml)


longhurst23_ssw$Longhurst <- factor(
  longhurst23_ssw$Longhurst,
  levels = c("NATR", "NAST(E)", "NADR")
)

boxplot(SA~Longhurst,data=longhurst23_ssw, xlab= "Longhurst Biogeographical Province", ylab= "Surfactant Activity (µg)", col = c("#CD4F39","salmon", "deeppink3"))    

kruskal.test(SA ~ Longhurst, data = longhurst23_ssw)


#SML DOC vs SA
sml_doc_sa<- read.csv ("doc vs sa sml.csv", row.names = 1)

sml_2022<-subset(sml_doc_sa, Year == "2022")
sml_2023<- subset(sml_doc_sa, Year == "2023")

colours <- c("sml_2022" = "salmon", "sml_2023" = "darkmagenta")
shapes <- c("sml_2022" = 16, "sml_2023" = 16)

ggplot() +
  geom_point(data=sml_2022, aes(x=DOC, y=SA, color="sml_2022", shape="sml_2022")) + 
  geom_point(data=sml_2023, aes(x=DOC, y=SA, color="sml_2023", shape="sml_2023")) +
  geom_smooth(data=sml_2022, aes(x=DOC, y=SA, color="sml_2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=sml_2023, aes(x=DOC, y=SA, color="sml_2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("salmon", "darkmagenta")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("DOC (ppb)") +
  ylab("Surfactant Activity (µg))")

#Pearson correlation test 2022
res <- cor.test(sml_2022$DOC, sml_2022$SA, 
                method = "pearson")
res

sml_2022_outlier<-subset(sml_2022, Outlier == "NO")

res <- cor.test(sml_2022_outlier$DOC, sml_2022_outlier$SA, 
                method = "pearson")
res

#Pearson correlation test 2023
res <- cor.test(ssw_2023$DOC, ssw_2023$SA, 
                method = "pearson")
res



#SSW DOC vs SA 
ssw_doc_sa<- read.csv ("sa vs doc ssw.csv", row.names = 1)

ssw_2022<-subset(ssw_doc_sa, Year == "2022")
ssw_2023<- subset(ssw_doc_sa, Year == "2023")

colours <- c("ssw_2022" = "darkolivegreen2", "ssw_2023" = "darkslategray3")
shapes <- c("ssw_2022" = 16, "ssw_2023" = 16)

ggplot() +
  geom_point(data=ssw_2022, aes(x=DOC, y=SA, color="ssw_2022", shape="ssw_2022")) + 
  geom_point(data=ssw_2023, aes(x=DOC, y=SA, color="ssw_2023", shape="ssw_2023")) +
  geom_smooth(data=ssw_2022, aes(x=DOC, y=SA, color="ssw_2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=ssw_2023, aes(x=DOC, y=SA, color="ssw_2023"), method="lm", se=FALSE, linetype="solid") +
    
    scale_color_manual(
      name = "",
      labels = c("2022", "2023"),
      values = c("darkolivegreen2", "darkslategray3")
    ) +
    scale_shape_manual(
      name = "",
      labels = c("2022", "2023"),
      values = c(16, 16))+

  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("DOC (ppb)") +
  ylab("Surfactant Activity (µg))")
  
#Pearson correlation test 2022
res <- cor.test(ssw_2022$DOC, ssw_2022$SA, 
                  method = "pearson")
res

ssw_2022_outlier<-subset(ssw_2022, Outlier == "NO")

res <- cor.test(ssw_2022_outlier$DOC, ssw_2022_outlier$SA, 
                method = "pearson")
res

#Pearson correlation test 2023
res <- cor.test(ssw_2023$DOC, ssw_2023$SA, 
                method = "pearson")
res


#DOM DRIVERS SML
#biopolymers

colours <- c("sml2022" = "salmon", "sml2023" = "darkmagenta")
shapes <- c("sml2022" = 16, "sml2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=SA, y=Biopolymers, color="sml2022", shape="sml2022")) + 
  geom_point(data=sml2023, aes(x=SA, y=Biopolymers, color="sml2023", shape="sml2023")) +
  geom_smooth(data=sml2022, aes(x=SA, y=Biopolymers, color="sml2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=sml2023, aes(x=SA, y=Biopolymers, color="sml2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("salmon", "darkmagenta")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Biopolymers (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(sml2022$SA, sml2022$Biopolymers, 
                method = "pearson")
res

res <- cor.test(sml2023$SA, sml2023$Biopolymers, 
                method = "pearson")
res

#Humics
colours <- c("sml2022" = "salmon", "sml2023" = "darkmagenta")
shapes <- c("sml2022" = 16, "sml2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=SA, y=Humics, color="sml2022", shape="sml2022")) + 
  geom_point(data=sml2023, aes(x=SA, y=Humics, color="sml2023", shape="sml2023")) +
  geom_smooth(data=sml2022, aes(x=SA, y=Humics, color="sml2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=sml2023, aes(x=SA, y=Humics, color="sml2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("salmon", "darkmagenta")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Humic Substances (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(sml2022$SA, sml2022$Humics, 
                method = "pearson")
res

res <- cor.test(sml2023$SA, sml2023$Humics, 
                method = "pearson")
res

#Building Blocks
colours <- c("sml2022" = "salmon", "sml2023" = "darkmagenta")
shapes <- c("sml2022" = 16, "sml2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=SA, y=Building_blocks, color="sml2022", shape="sml2022")) + 
  geom_point(data=sml2023, aes(x=SA, y=Building_blocks, color="sml2023", shape="sml2023")) +
  geom_smooth(data=sml2022, aes(x=SA, y=Building_blocks, color="sml2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=sml2023, aes(x=SA, y=Building_blocks, color="sml2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("salmon", "darkmagenta")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Building Blocks (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(sml2022$SA, sml2022$Building_blocks, 
                method = "pearson")
res


res <- cor.test(sml2023$SA, sml2023$Building_blocks, 
                method = "pearson")
res

#LMW Neutrals
colours <- c("sml2022" = "salmon", "sml2023" = "darkmagenta")
shapes <- c("sml2022" = 16, "sml2023" = 16)

ggplot() +
  geom_point(data=sml2022, aes(x=SA, y=LMW_Neutrals, color="sml2022", shape="sml2022")) + 
  geom_point(data=sml2023, aes(x=SA, y=LMW_Neutrals, color="sml2023", shape="sml2023")) +
  geom_smooth(data=sml2022, aes(x=SA, y=LMW_Neutrals, color="sml2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=sml2023, aes(x=SA, y=LMW_Neutrals, color="sml2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("salmon", "darkmagenta")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("LMW Neutrals(ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(sml2022$SA, sml2022$LMW_Neutrals, 
                method = "pearson")
res


res <- cor.test(sml2023$SA, sml2023$LMW_Neutrals, 
                method = "pearson")
res

#DOM DRIVERS SSW
colours <- c("ssw2022" = "darkslategray3", "sml2023" = "darkolivegreen2")
shapes <- c("ssw2022" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=ssw2022, aes(x=SA, y=Biopolymers, color="ssw2022", shape="ssw2022")) + 
  geom_point(data=ssw2023, aes(x=SA, y=Biopolymers, color="ssw2023", shape="ssw2023")) +
  geom_smooth(data=ssw2022, aes(x=SA, y=Biopolymers, color="ssw2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=ssw2023, aes(x=SA, y=Biopolymers, color="ssw2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("darkslategray3", "darkolivegreen2")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Biopolymers (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(ssw2022$SA, ssw2022$Biopolymers, 
                method = "pearson")
res

res <- cor.test(ssw2023$SA, ssw2023$Biopolymers, 
                method = "pearson")
res

#Humics
colours <- c("ssw2022" = "darkslategray3", "ssw2023" = "darkolivegreen2")
shapes <- c("ssw2022" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=ssw2022, aes(x=SA, y=Humics, color="ssw2022", shape="ssw2022")) + 
  geom_point(data=ssw2023, aes(x=SA, y=Humics, color="ssw2023", shape="ssw2023")) +
  geom_smooth(data=ssw2022, aes(x=SA, y=Humics, color="ssw2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=ssw2023, aes(x=SA, y=Humics, color="ssw2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("darkslategray3", "darkolivegreen2")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Humic Substances (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(ssw2022$SA, ssw2022$Humics, 
                method = "pearson")
res

res <- cor.test(sml2023$SA, sml2023$Humics, 
                method = "pearson")
res

#Building Blocks
colours <- c("ssw2022" = "darkslategray3", "ssw2023" = "darkolivegreen2")
shapes <- c("ssw2022" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=ssw2022, aes(x=SA, y=Building_blocks, color="ssw2022", shape="ssw2022")) + 
  geom_point(data=ssw2023, aes(x=SA, y=Building_blocks, color="ssw2023", shape="ssw2023")) +
  geom_smooth(data=ssw2022, aes(x=SA, y=Building_blocks, color="ssw2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=ssw2023, aes(x=SA, y=Building_blocks, color="ssw2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("darkslategray3", "darkolivegreen2")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Building Blocks (ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(ssw2022$SA, ssw2022$Building_blocks, 
                method = "pearson")
res


res <- cor.test(ssw2023$SA, ssw2023$Building_blocks, 
                method = "pearson")
res

#LMW Neutrals
colours <- c("ssw2022" = "darkslategray3", "ssw2023" = "darkolivegreen2")
shapes <- c("ssw2022" = 16, "ssw2023" = 16)

ggplot() +
  geom_point(data=ssw2022, aes(x=SA, y=LMW_Neutrals, color="ssw2022", shape="ssw2022")) + 
  geom_point(data=ssw2023, aes(x=SA, y=LMW_Neutrals, color="ssw2023", shape="ssw2023")) +
  geom_smooth(data=ssw2022, aes(x=SA, y=LMW_Neutrals, color="ssw2022"), method="lm", se=FALSE, linetype="solid") +
  geom_smooth(data=ssw2023, aes(x=SA, y=LMW_Neutrals, color="ssw2023"), method="lm", se=FALSE, linetype="solid") +
  
  scale_color_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c("darkslategray3", "darkolivegreen2")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2022", "2023"),
    values = c(16, 16))+
  
  
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("LMW Neutrals(ppb)") +
  xlab("Surfactant Activity (µg))")

res <- cor.test(ssw2022$SA, ssw2022$LMW_Neutrals, 
                method = "pearson")
res


res <- cor.test(ssw2023$SA, ssw2023$LMW_Neutrals, 
                method = "pearson")
res



#kw comparison
kw_sml<- read.csv ("kw_sml.csv", row.names = 1)

kw_sml_2014 <- subset(kw_sml, Year == "2014")
kw_sml_2022 <-subset(kw_sml, Year == "2022")
kw_sml_2023 <-subset(kw_sml, Year == "2023")

kw_sml_2014$Latitude <- as.numeric(kw_sml_2014$Latitude)
kw_sml_2022$Latitude <- as.numeric(kw_sml_2022$Latitude)
kw_sml_2023$Latitude <- as.numeric(kw_sml_2023$Latitude)

#PLOT kw vs latitude 
colours <- c("kw_sml_2014" = "deeppink4", "kw_sml_2022" = "deeppink", "kw_sml_2023"= "darksalmon")
shapes <- c("kw_sml_2014" = 16, "kw_sml_2022" = 16, "kw_sml_2023" = 16)


colours <- c("kw_sml_2014" = "deeppink4", "kw_sml_2022" = "deeppink", "kw_sml_2023"= "darksalmon")
shapes <- c("kw_sml_2014" = 16, "kw_sml_2022" = 16, "kw_sml_2023" = 16)

ggplot() +
  geom_point(data = kw_sml_2014, aes(x = Latitude, y = Kw, color = "kw_sml_2014", shape = "kw_sml_2014")) + 
  geom_point(data = kw_sml_2022, aes(x = Latitude, y = Kw, color = "kw_sml_2022", shape = "kw_sml_2022")) +
  geom_point(data = kw_sml_2023, aes(x = Latitude, y = Kw, color = "kw_sml_2023", shape = "kw_sml_2023")) +
  
  geom_smooth(data = kw_sml_2014, aes(x = Latitude, y = Kw), method = "lm", se = FALSE, linetype = "solid", color = "deeppink4") +
  geom_smooth(data = kw_sml_2022, aes(x = Latitude, y = Kw), method = "lm", se = FALSE, linetype = "solid", color = "deeppink") +
  geom_smooth(data = kw_sml_2023, aes(x = Latitude, y = Kw), method = "lm", se = FALSE, linetype = "solid", color = "darksalmon") +
  
  scale_color_manual(
    name = "",
    labels = c("2014", "2022", "2023"),
    values = colours
  ) +
  scale_shape_manual(
    name = "",
    labels = c("2014", "2022", "2023"),
    values = shapes
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  xlab("Latitude (N)") +
  ylab("Kw Suppression (%)")

#Pearson correlation
res <- cor.test(kw_sml_2014$Latitude, kw_sml_2014$Kw, 
                method = "pearson")
res

res <- cor.test(kw_sml_2022$Latitude, kw_sml_2022$Kw, 
                method = "pearson")
res

res <- cor.test(kw_sml_2023$Latitude, kw_sml_2023$Kw, 
                method = "pearson")
res