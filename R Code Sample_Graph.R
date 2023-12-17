# empty workspace 
rm(list = ls())

# install required packages（uncomment below to install any of the packages）
# install.packages("haven")
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("tidyverse")

# load required packages
library(haven) # for reading in data
library(maps) # for mapping
library(ggplot2) # for graphing
library(tidyverse) # for data sub-setting


# disable scientific notation
options(scipen = 999) 

# *** change working directory ***
setwd("C:/Users/huhu/Desktop/重构code/out/fig")


#F1C <- read_dta("C:/Users/huhu/Desktop/重构code/out/F1_coast.dta")

# F3
F3G1_A <- read_dta("C:/Users/huhu/Desktop/重构code/out/F1_joint.dta")
F3G1_B <- read_dta("C:/Users/huhu/Desktop/重构code/out/F1_urban.dta")
F3G1_C <- read_dta("C:/Users/huhu/Desktop/重构code/out/F1_coast.dta")

#data_text_f3 <- data.frame(gender = c(0, 1), label_text = c("A", "B"))

Figure_3 <- ggplot() + 
  geom_abline(color = "red", lty = 1, alpha=0.3) +
  geom_point(data=F3G1_A, aes(x=coef_finc, y=coef_minc, group=prov), color="gray60", alpha=0.6, size=0.8) +
  geom_point(data=F3G1_B, aes(x=coef_finc, y=coef_minc, group=urban, color=as.factor(urban), shape=as.factor(urban)), size=1.4, alpha=0.95) + 
  geom_point(data=F3G1_C, aes(x=coef_finc, y=coef_minc, group=coast, color=as.factor(coast), shape=as.factor(coast)), size=1.4, alpha=0.95) +
  scale_shape_manual("Region",
                     values = c(0,1,2,3),
                     labels=c("Rural","Urban","Inland","Coast")) +
  scale_color_manual("Region", 
                     values = c("#D43F3A", "black","#5CB85C", "darkorange3"),
                     labels=c("Rural","Urban","Inland","Coast")) +
  theme_bw() + 
  labs(x=expression(paste("Fathers' income positions net of mothers' positions (coefficient)")), 
       y=expression(paste("Mothers' income positions net of fathers' positions (coefficient)"))) +
  scale_y_continuous(limits=c(-0.1, 0.3),  breaks=seq(-0.1, 0.3, 0.04)) + 
  scale_x_continuous(limits=c(-0.1, 0.3),  breaks=seq(-0.1, 0.3, 0.04)) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major= element_blank(),
        axis.text.x = element_text(color="black", angle=90, size=7, vjust = 0.5), 
        axis.text.y = element_text(color="black", size=7),
        strip.background = element_blank(), 
        strip.text = element_text(color="black", face="bold", size=7), 
        axis.title.x = element_text(size=7, face="bold"),
        axis.title.y = element_text(size=7, face="bold"),
        panel.spacing = unit(-0.1, "lines"), 
        text = element_text(size = 7, color="black"),
        legend.position="bottom", 
        legend.margin=margin(0,0,0,0), 
        legend.text=element_text(size=7),
        legend.title=element_text(size=7), 
        legend.spacing.y = unit(-0.25, 'cm'), 
        legend.box.margin=margin(-8,-8,-8,-8)) +
  guides(color=guide_legend(nrow=1,byrow=TRUE), 
         size=guide_legend(nrow=1,byrow=TRUE), 
         shape=guide_legend(nrow=1,byrow=TRUE))

ggsave("C:/Users/huhu/Desktop/重构code/out/fig/F1.pdf", Figure_3, height = 3.46457, width = 3.46457, dpi = 600, device = cairo_pdf)
