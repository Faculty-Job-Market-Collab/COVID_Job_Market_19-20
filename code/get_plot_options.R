library(scales)
library(RColorBrewer)
library(cowplot)
#library(patchwork)

#preferred themes----
my_theme <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_horiz <- theme_classic()+ 
  theme(legend.position = "none", axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        plot.caption = element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5),
        panel.border = element_blank())

my_theme_leg <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_left <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left", plot.caption=element_text(size=12, hjust=.5),
        plot.subtitle = element_text(size = 12, hjust = .5))

my_theme_leg_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"),
        plot.subtitle = element_text(size = 12, hjust = .5))

#adding proportion/count labels to barchart
prop_lab_low <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = 1)

count_lab_low <- geom_text(stat = "count", aes(label=..count..), vjust = 1)

prop_lab_high <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = -1)

count_lab_high <- geom_text(stat = "count", aes(label=..count..), vjust = -1)

#colorblind palettes
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #grey, goldenrod, light blue, dark green, gold, navy blue, orange rust, pink

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #black

#settings----
gen_levels <- c("none", "male", "female")

gen_ed_levels <- c("female", "male")

gen_breaks <- c("NA", "male", "female")

gen_labels <- c("none" = "Unknown", "male" = "Men",
                "female" = "Women")

gen_linetype <- c("solid", "dashed", "dotted")

gen_ed_labels <- c("Women", "Men")

gen_colors <- c("none" = "#999999", "male" = "#0072B2",
                "female" = "#D55E00")

gen_ed_colors <- c("#D55E00", "#0072B2")

gen_x_replace <- scale_x_discrete(breaks=gen_levels,
                 labels=gen_labels)

gen_ed_facet <- function(x){
  ifelse(x == "female", "Women Editors", "Men Editors")
}

gen_rev_facet <- function(x){
  ifelse(x == "female", "Women Reviewers", "Men Reviewers")
}


#plotting functions----

#plot feature weights
feature_box_plot <- function(df){
  plot <- ggplot(df)+
    geom_boxplot(aes(x = clean_feat, y = weight))+
    coord_flip()+
    labs(x = "\nLogistic Regression Variables", y = "Weight (Absolute value)")+
    my_theme_horiz
  
  return(plot)
}
