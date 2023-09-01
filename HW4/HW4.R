#Question 4
data <- read.csv(file="C:/Users/user/Desktop/HW4-4.csv")
data
# install.packages("ICSNP")
library(ICSNP)
HotellingsT2(data, mu=c(7,11))

#Question 5
turtle <- read.csv(file="C:/Users/user/Desktop/HW4-5.csv")
turtle
# install.packages("dplyr")
set.seed(1234)
dplyr::sample_n(turtle, 48)
res.man <- manova(cbind(length, width, height) ~ sex, data = turtle)
summary(res.man)
summary.aov(res.man)

#Question 6
#let's SS be 1, JL be 2, LP be 3
#let's + be1, - be 2
seedlings <- read.csv(file="C:/Users/user/Desktop/HW4-6.csv")
seedlings

CM_mean <- data.frame(x1 = c(10.35, 13.41, 7.78, 10.4, 17.78, 10.4),
                      x2 = c(25.93, 38.63, 25.15, 24.25, 41.45, 29.2),
                      species = factor(c(1, 2, 3, 1, 2, 3)),
                      nutrient = factor(c(1, 1, 1, 2, 2, 2)))

# MANOVA for the species effect
species_manova <- manova(cbind(x1, x2) ~ species, data = CM_mean)
summary(species_manova)

# MANOVA for the nutrient effect
nutrient_manova <- manova(cbind(x1, x2) ~ nutrient, data = CM_mean)
summary(nutrient_manova)

# two-way ANOVA for 560CM
anov560 <- aov(CM_mean$x1 ~ CM_mean$species + CM_mean$nutrient + CM_mean$species:CM_mean$nutrient)
summary(anov560)

# two-way ANOVA for 720CM
anov720 <- aov(CM_mean$x2 ~ CM_mean$species + CM_mean$nutrient + CM_mean$species:CM_mean$nutrient)
summary(anov720)

