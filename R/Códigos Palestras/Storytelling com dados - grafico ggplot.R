

if (!require("pacman")) install.packages("pacman"); library("pacman")

pacman::p_load(irtoys,dplyr,highcharter,readxl,ggplot2,knitr,writexl,bigstatsr)


# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)


#Grafico usualmente feito 
data %>%
  tail(10) %>%
  ggplot( aes(x = date, y=value)) +
  geom_line() +
  geom_point()


#Grafico dessaturado 

data %>%
  tail(10) %>%
  ggplot( aes(x = date, y=value  , label = round(value,0))) +
  geom_line(color = 'blue') +   geom_label() + 
  theme_classic() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
       panel.grid.major.x  = element_line(size = 0.5, colour = "grey80",linetype = "dashed"))
   
