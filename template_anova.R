# https://statsandr.com/blog/anova-in-r/
# Learn ANOVA with post hoc

library(palmerpenguins)
library(tidyverse)

dat <- penguins %>%
  select(species, flipper_length_mm)

summary(dat)

library(ggplot2)

ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")

# Run anova
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

# Check normality
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)


shapiro.test(res_aov$residuals)

# Boxplot
boxplot(flipper_length_mm ~ species,
        data = dat
)

# Dotplot
library("lattice")

dotplot(flipper_length_mm ~ species,
        data = dat
)

# Check for normality
library(car)

leveneTest(flipper_length_mm ~ species,
           data = dat
)

par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(res_aov, which = 1)

# 2. Normality
plot(res_aov, which = 2)

boxplot(flipper_length_mm ~ species,
        data = dat
)


ggplot(dat) +
  aes(x = species, y = flipper_length_mm) +
  geom_boxplot()

aggregate(flipper_length_mm ~ species,
          data = dat,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)


library(dplyr)

group_by(dat, species) %>%
  summarise(
    mean = mean(flipper_length_mm, na.rm = TRUE),
    sd = sd(flipper_length_mm, na.rm = TRUE)
  )

# 1st method:
oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = TRUE # assuming equal variances
)

# 2nd method:
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

summary(res_aov)

oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = FALSE # assuming unequal variances
)

# install.packages("remotes")
# remotes::install_github("easystats/report") # You only need to do that once
library("report") # Load the package every time you start R

report(res_aov)

# Post hocs
library(multcomp)

# Tukey HSD test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Tukey")
)

summary(post_test)


# The results of post hoc
par(mar = c(3, 8, 3, 3))
plot(post_test)

TukeyHSD(res_aov)
plot(TukeyHSD(res_aov))

# Dunnett's test allows to only make comparisons with a reference group, but with more power
