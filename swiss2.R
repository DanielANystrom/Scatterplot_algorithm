#Name: Swiss Scatterplot Algorithm ####
#Date: July 30, 2020
#Description: Based on the 'swiss' data set in the 'datasets'
#package. The goal of this analysis is to demonstrate
#various visualization packages, particularly ggplot,
#and develop a relatively easy algorithm for producing
#multiple customized scatterplots with the same aesthetic

# INSTALLING PACKAGES AND DATA #####


#Install pacman package: For powerful functionality
if(!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,psych,rio,tidyverse,datasets,
               corrplot,plyr)

#Information on datasets and the swiss dataset
library(help = "datasets")
?swiss

#Import the data
df <- datasets::swiss %>%
  as_tibble() %>%
  print()
 
# DESCRIPTIVE STATISTICS WITH GGPLOT####

#Always best practice to start with some numeric summary data
summary(df)
describe(df)

#And then the correlations between variables either as a correlation matrix:
matrix <- cor(df) %>%
  round(digits = 2) %>%
  print()

#And even make a nice visualization using the corrplot() library
corrplot(matrix, type = "upper", method = "color",
         order = "hclust", 
         tl.col = "black", tl.srt = 35, tl.offset = 1,
         title = "Correlation Matrix for 'swiss'",
         addgrid.col=FALSE,
         sig.level = 0.05,
         addCoef.col = TRUE,
         mar = c(1,3,4,4))
?corrplot


#Or, to leverage the strengths of ggplot, a scatterplot matrix:
scatter <- ggpairs(df) +
  labs(title = "Scatterplot Matrix for Swiss Fertility Data",
       subtitle = "Scatterplots, density plots, and correlation values for six variables",
       caption = "Data credited to the 'swiss' data set in the R 'datasets' package") +
  theme(plot.title = element_text(face = "bold", 
                                  color = "black", 
                                  size = 17),
        plot.subtitle = element_text(face = "italic", 
                                     color = "black", 
                                     size = 14),
        panel.background = element_rect(color = "black",
                                       linetype = "solid",
                                       size = 1))
print(scatter) #printing the plot is a little slow, but worth it

#But, given that's its hard to truly grasp the nuanace in a relationship
#in a scatterplot matrix (due to small size), its helpful to focus on specific
#relationships to visualize their impact through a full-size scatterplot
#but, in a plot like this, with several variables and many high levels
#of correlation, this becomes tedious.
#So here's one way to leverage the power of variables to allow the user
#to program the style and formatting once, and then simply adjust the
#now-smaller set of variables:

df <- df #Declare the dataframe
independent <- df$Education #Declare the independent variable
dependent <- df$Fertility #Declare the dependent variable
i.name <- "Education" #Enter the name of the independent variable, capitalize
i.units <- "Proportion" #Enter the units of the independent variable, else type NULL
d.name <- "Fertility" #Enter the name of the dependent variable, capitalize
d.units <- NULL #Enter the units of the dependent variable, else type NULL
#If do not want to credit data, DO NOT RUN or declare as NULL:
credit <- "Data credited to the 'swiss' data set in the R 'datasets' package"


sctr1 <- ggplot(df, aes(independent, dependent)) +
  geom_point(position = "jitter", color = "black", size = 3.5) +
  geom_smooth(method = "lm", level = 0.95, color = "coral3", size = 2.5) +
  labs(title = paste("Visualizing the Effect of",i.name,"on",d.name,sep=" "),
       subtitle = "Scatterplot with linear regression line and 95% confidence intervals",
       caption = if(!is.null(credit)){ #this conditional triggers the caption
         credit
       }) + 
  theme(plot.title = element_text(face = "bold", color = "azure", size = 20,
                                  family = "serif"), #sans by default
        plot.subtitle = element_text(face = "italic",color = "azure", size = 14,
                                     family = "serif"),
        plot.caption = element_text(face = "bold.italic", color = "azure", size = 12,
                                    family = "serif"),
        axis.title = element_text(face = "bold", color = "azure", size = 14,
                                  family = "serif"),
        axis.text = element_text(face = "bold", color = "azure", size = 12,
                                 family = "serif"),
        panel.background = element_rect(color = "black", linetype = "solid", size=0.5,
                                        fill = "azure3"),
        plot.background = element_rect(fill="darkblue")) +
  scale_x_continuous(breaks=seq(min(independent),
                             max(independent),
                             round((max(independent)-min(independent))/5, digits=0))) +
                              #this final part(at leasT), should be replaced by an algorithm
                              #to auto-scale to different variable ranges (in a stylish way)
  scale_y_continuous(breaks=seq(min(dependent),
                                max(dependent),
                                round((max(dependent)-min(dependent))/5, digits=0))) +
  xlab(paste(i.name,if(!is.null(i.units)){ #this conditional triggers the x label
    paste("(",i.units,")",sep="")
  },sep=" ")) +
  ylab(paste(d.name,if(!is.null(d.units)){ #this conditional defines the y label
    paste("(",d.units,")",sep="")
  },sep=" "))
print(sctr1)
