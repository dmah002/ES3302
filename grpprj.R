library("readxl") # Read excel file

# load("/Users/davidmah/Desktop/ES3302/BTNR/CTFSRPackage.rdata")

CleanTech <- read_excel("/Users/davidmah/Desktop/ES3302/grp proj/CleanTech.xlsx", sheet = "Sheet1", col_names = TRUE,na = "", trim_ws = TRUE, skip = 0, n_max = Inf)
Meranti <- read_excel("/Users/davidmah/Desktop/ES3302/grp proj/CleanTech.xlsx", sheet = "Sheet2", col_names = TRUE,na = "", trim_ws = TRUE, skip = 0, n_max = Inf)
library(ggplot2)
library(dplyr)
library(ggthemes)



# Bubble Plot of CleanTech
pbp_CT<- ggplot(CleanTech, aes(x=gx, y=gy, size = height, color=sp)) +
  geom_point(alpha=0.6) +
  scale_size(range = c(2, 14), name="height (m)") + 
  scale_x_reverse() + 
  labs(title="Spatial plot of CleanTech Forest trees",x="Transect Distance", y="Distance from Edge") + 
  theme(
  plot.title = element_text(size=15, face="bold"),
  axis.title.x = element_text(size=12),
  axis.title.y = element_text(size=12)
  ) +
  theme_bw()

# Geom-Smooth Plot of CleanTech
pgs_CT<- ggplot(CleanTech, aes(x=gy, y=height)) + 
  geom_smooth(model=lm) +
  ggtitle("CleanTech: Plot of Canopy Height (m) against Distance from Edge (m)") +
  xlab("Distance from Edge (m)") + ylab("Canopy Height (m)") +
  theme_economist() + scale_fill_economist()


# Bubble Plot of Meranti
pbp_M<- ggplot(Meranti, aes(x=gx, y=gy, size = height, color=sp)) +
  geom_point(alpha=0.6) +
  scale_size(range = c(2, 14), name="height (m)") + 
  scale_x_reverse() + 
  labs(title="Spatial plot of Meranti Forest trees",x="Transect Distance", y="Distance from Edge") + 
  theme(
    plot.title = element_text(size=15, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  ) +
  theme_bw()

pgs_M<- ggplot(Meranti, aes(x=gy, y=height)) + 
  geom_smooth(model=lm) +
  ggtitle("Meranti: Plot of Canopy Height (m) against Distance from Edge (m)") +
  xlab("Distance from Edge (m)") + ylab("Canopy Height (m)")+ 
  theme_economist() + scale_fill_economist()

library("vegan") #diversity index function

CleanTech$richness <- length(unique(CleanTech$sp))
CleanTech$abundance <- length(CleanTech)

CT_div <- data.frame(count(CleanTech, vars = sp))
div <- diversity(CT_div[-1], index="shannon")

M_div <- data.frame(count(Meranti, vars = sp))
div <- data.frame(rbind(div,diversity(M_div[-1], index="shannon")))


div <- data.frame(Forest=c("CleanTech","Meranti"),
                 Shannon=c(div))
colnames(div) <- c("Forest", "ShannonDI")

ggplot(data=div, aes(x=Forest, y=ShannonDI)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=ShannonDI), vjust=1.6, color="white", size=3.5)+
  theme_minimal()


plot(pbp_M)


