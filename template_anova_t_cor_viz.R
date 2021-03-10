# Mission - To build templates for one way ANOVA, t test and correlations
# Dataviz is also partof the module
# @ author - Rahul Venugopal on 09.03.2021

# Loading libraries
library(palmerpenguins)
library(tidyverse)
library(car)
library(report)
library(multcomp)
library(lattice)
library(see)
library(ggtext)
library(psych)
library(naniar)
library(ggpubr)
library(WRS2)
library(DescTools)

# Loading the data and selecting relevant columns or variables
data <- penguins %>% dplyr::select(species, flipper_length_mm,bill_length_mm)

##################### Get to know the data #####################

# Summarising data
summary(data)

# Descriptive stats
describe(data) #Risky if categorical variables are there
# A quick dirty plot
plot(data)

# Finding missing data and visualising the same
gg_miss_var(data) + 
  labs(y = "Look at all the missing ones") + 
  theme_radar()
ggsave("MissingOnes.jpeg",width = 8, height = 6, dpi = 300)

# Check normality and homogeneity of variation
# Basic qqplot
ggqqplot(data$bill_length_mm, color = "steelblue")
ggsave("ggqq.jpeg",width = 8, height = 6, dpi = 300)
ggqqplot(data$flipper_length_mm, color = "steelblue")

# Shapiro-Wilk's method
# If the p-value 0.05, we can assume the normality
shapiro.test(data$bill_length_mm)
shapiro.test(data$flipper_length_mm)

# Pick Levene or Bartlett test based on normality
bartlett.test(bill_length_mm ~ species, data = data)
leveneTest(flipper_length_mm ~ species, data = data)
fligner.test(bill_length_mm ~ species, data = data)

# change global theme settings (for all following plots)
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

#################### Seeing the data #####################

# Reordering the groups
data$species <- factor(data$species, levels = c("Chinstrap","Gentoo","Adelie"))

# change global theme settings (for all following plots)
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

# modify plot elements globally (for all following plots)
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(1, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  axis.text.x = element_text(color = "grey20", size = 20),
  axis.text.y = element_text(color = "grey20", size = 20),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 12, color = "grey30"),
  plot.caption = element_text(size = 9, margin = margin(t = 15))
)

# dataviz
data_plot <- ggplot(data = data,
                    aes(x= species, y = flipper_length_mm, fill = species)) + 
  
  geom_boxplot(width = 0.1,
               position = position_nudge(x = -0.3, y = 0),
               outlier.shape = 21,
               show.legend = FALSE) + 
  scale_fill_manual(values=c("#DEC3B6", "#317372", "#D33534")) + 
  
  geom_violindot(size_dots = 8,
                 trim = FALSE,
                 show.legend = FALSE) +
  
  # custom labels
  labs(
    title = 'Flipper length across three species of penguins',
    subtitle = 'Data distribution, Median, Inter quartile range, Scatter plot',
    caption = 'Data: Scholar | PhD work',
    x = ' ', 
    y = 'Flipper length'
  ) + 
  
  theme(
    plot.title = ggtext::element_markdown(),
    plot.caption = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  )

# Adding color layer
data_plot_2 <- data_plot +
  labs(title = 'Whatever whatever between
       <i style="color:#DEC3B6;">Group1</i> , <i style="color:#317372;">Group2</i> group
       and <i style="color:#D33534;">Group3</i>')

# Alignments, white space around borders
data_plot_3 <- data_plot_2 + 
  theme(plot.title.position = "plot") + 
  theme(plot.margin = margin(t = 25, r = 25, b = 10, l = 25))

ggsave("Three Penguins.jpeg", width = 9, height = 8, dpi = 300)

##################### Robust correlations #####################

# Robust correlations with scatter plot with annotation
# Compute percent bend correlation
cor_values <- pbcor(data$bill_length_mm, data$flipper_length_mm,
                    beta = 0.1, ci = TRUE, nboot = 5000)
# Get the texts ready
corr_value = paste('r value  ',as.character(round(cor_values[[1]],digits=2)))
p_value = paste('p value  ',as.character(round(cor_values[[3]], digits=2)))
ci_cor = paste('95% CI ',paste(as.character(round(cor_values[[5]], digits = 4)),
                               collapse = " - "))
to_be_pasted_as_title = paste(corr_value, p_value, ci_cor, sep="\n")

# Viz
ggplot(data, aes(x=bill_length_mm, y=flipper_length_mm)) +
  geom_point(color="steelblue") +
  geom_rug(color = "indianred") + 
  geom_smooth(method=lm,
              color = "grey70",
              fill = "grey80") + 
  ggtitle(to_be_pasted_as_title) + 
  labs(
    subtitle = 'Write a small sumamry here',
    caption = 'Data: Scholar | PhD work',
    x = 'Write x-axis title here ', 
    y = 'Write y-axis title here)'
  )

ggsave("correlation_plot_new.jpeg",width = 6, height = 6, dpi = 300)

##################### Run one way ANOVA#####################

# Re-levelling the data
# Change reference category:
dat$species <- relevel(dat$species, ref = "Gentoo")

# Check that Gentoo is the reference category:
levels(dat$species)

res_aov <- aov(flipper_length_mm ~ species, data = data)

# Effect size eta-squared
EtaSq(res_aov)

# A more objective way
shapiro.test(res_aov$residuals)

# Dotplot to see the variance/range in/of data
library("lattice")

dotplot(flipper_length_mm ~ species,
        data = data
)

# 1st method: Welch ANOVA
oneway.test(flipper_length_mm ~ species,
            data = data,
            var.equal = TRUE # assuming equal variances
)

# 2nd method:
res_aov <- aov(flipper_length_mm ~ species,
               data = data)

summary(res_aov)

oneway.test(flipper_length_mm ~ species,
            data = data,
            var.equal = FALSE # assuming unequal variances
)

# Creates a neat written summary of ANOVA
report(res_aov)

# Post hocs
# Tukey HSD test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Tukey"))

summary(post_test)


# The results of post hoc
# Method I
par(mar = c(3, 8, 3, 3))
plot(post_test, col="indianred")


# Method II
TukeyHSD(res_aov)
plot(TukeyHSD(res_aov))

# Dunnett's test allows to only make comparisons with a reference group, but with more power
# Dunnett's test:
post_test <- glht(res_aov,
                  linfct = mcp(species = "Dunnett"))

summary(post_test)
# Visualising the CIs
par(mar = c(3, 8, 3, 3))
plot(post_test, col="indianred")

# Single ANOVA results diagram
# Edit from here
x <- which(names(data) == "species") # name of grouping variable
y <- which(
  names(data) == "flipper_length_mm" # names of variables to test
)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("Chinstrap", "Adelie"), c("Gentoo", "Adelie"), c("Gentoo", "Chinstrap")) # comparisons for post-hoc tests

# Loop
for (i in y) {
  for (j in x) {
    p <- ggboxplot(data,
                   x = colnames(data[j]), y = colnames(data[i]),
                   color = colnames(data[j]),
                   legend = "none",
                   palette = "npg", #palette =c("#00AFBB", "#E7B800", "#FC4E07")
                   add = "jitter",
                   xlab = "Species of Penguins",
                   ylab = "Flipper length (mm)",
                   title = "We studied three species of penguins and guess what"
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = method1, label.y = max(data[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}
# Refer http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/82-ggplot2-easy-way-to-change-graphical-parameters/

##################### Robust t-test ####################
formula = flipper_length_mm ~ species

df <- data %>% filter(species != "Gentoo")
# Robust yuens t test with bootstrapping
yuenbt(formula, data=df, tr = 0.2, nboot = 1000)
# Effect size
yuen.effect.ci(formula, data=df, tr = 0.2,nboot = 1000,alpha = 0.05)
# Prett good resource
# https://rstudio-pubs-static.s3.amazonaws.com/261629_d9295c8bd9314cc9abcda6c9f587c0e1.html
yuen.effect.ci(formula, data=df, tr = 0.2,nboot = 1000,alpha = 0.05)

# Viz of two groups
# Reordering the groups
data$species <- factor(data$species, levels = c("Chinstrap","Gentoo","Adelie"))

# change global theme settings (for all following plots)
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))

# modify plot elements globally (for all following plots)
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(1, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  axis.text.x = element_text(color = "grey20", size = 20),
  axis.text.y = element_text(color = "grey20", size = 40),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 12, color = "grey30"),
  plot.caption = element_text(size = 9, margin = margin(t = 15))
)

# dataviz
data_plot <- ggplot(data = df,
                    aes(x= species, y = flipper_length_mm, fill = species)) + 
  
  geom_boxplot(width = 0.1,
               position = position_nudge(x = -0.3, y = 0),
               outlier.shape = 21,
               show.legend = FALSE) + 
  scale_fill_manual(values=c("#317372", "#D33534")) + 
  
  geom_violindot(size_dots = 8,
                 trim = FALSE,
                 show.legend = FALSE) +
  
  # custom labels
  labs(
    title = 'Flipper length across two species of penguins',
    subtitle = 'Data distribution, Median, Inter quartile range, Scatter plot',
    caption = 'Data: Scholar | PhD work',
    x = ' ', 
    y = 'Flipper length'
  ) + 
  
  theme(
    plot.title = ggtext::element_markdown(),
    plot.caption = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  )

# Adding color layer
data_plot_2 <- data_plot +
  labs(title = 'Whatever whatever between
       <i style="color:#317372;">Group1</i> and <i style="color:#D33534;">Group2</i>')

# Alignments, white space around borders
data_plot_3 <- data_plot_2 + 
  theme(plot.title.position = "plot") + 
  theme(plot.margin = margin(t = 25, r = 25, b = 10, l = 25))

ggsave("Two groups.jpeg", width = 9, height = 8, dpi = 300)