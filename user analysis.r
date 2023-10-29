# user analysis
library(dplyr)
library(ggplot2)
library(broom)
library(reshape2)

dem_answers <- read.csv("C:\\Users\\z3258367\\OneDrive - UNSW\\#PhD\\Walkability\\Workshops\\Cedric\\map-me_dem_answers_08-06-2023_01-26.csv", stringsAsFactors = FALSE)
neighbourhoods <- read.csv("C:\\Users\\z3258367\\OneDrive - UNSW\\#PhD\\Walkability\\Workshops\\map-me-walking\\q34hullsizes_17_9.csv", stringsAsFactors = FALSE)

data <- dem_answers %>% mutate('question' = case_when(
    id_dem_question == 24021 | id_dem_question ==  24040 | id_dem_question == 24110 | id_dem_question == 24117 ~ 1, 
    id_dem_question == 24022 | id_dem_question ==  24041 | id_dem_question == 24111 | id_dem_question == 24118 ~ 2,
    id_dem_question == 24023 | id_dem_question ==  24042 | id_dem_question == 24112 | id_dem_question == 24119 ~ 3,
    id_dem_question == 24024 | id_dem_question ==  24043 | id_dem_question == 24113 | id_dem_question == 24120 ~ 4,
    id_dem_question == 24025 | id_dem_question ==  24044 | id_dem_question == 24114 | id_dem_question == 24121 ~ 5,
    id_dem_question == 24026 | id_dem_question ==  24045 | id_dem_question == 24115 | id_dem_question == 24122 ~ 6,
    id_dem_question == 24027 | id_dem_question ==  24046 | id_dem_question == 24116 | id_dem_question == 24123 ~ 7,
    TRUE ~ 0)) %>%
    mutate(id_website = NULL, id_dem_question = NULL)

# this removes my answers entered when testing the system
wide <- wide %>% filter(id_person > 81913)

# in data, count responses per person_id
data %>% group_by(id_person) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(20)

# the reshape throws some warnings because there are 7 people who filled out the dem questions more than once, thus more than one of some person-question pairs, but for all 7 their answers are consistent each time, so it's ok that this just keeps the first one
wide <- data %>% reshape(timevar = "question", idvar = c("id_person"), direction = "wide")

wide <- wide %>% left_join(neighbourhoods, by = c("id_person" = "id_person"))

# this adds columns for area in km2 (I know I could probably do this at visualisation time instead)
wide$q3area <- wide$q3size/1000000
wide$q4area <- wide$q4size/1000000

filterdata <- wide %>% filter(q3size>0)

#quartiles for q3size vs dem_answer.5
wide %>% group_by(dem_answer.5) %>% summarise(quantile(q3size, na.rm = TRUE)) %>% print(n=50) 
boxplot(q4size ~ dem_answer.5, data = wide, range=0)

# function for number of observations 
give.n <- function(x){
  return(c(y = mean(x)*0.7, label = ""))#length(x)))
  # experiment with the multiplier to find the perfect position
}

# box plot of q3size and q4size
areas <- wide %>% select(q3area, q4area)
ggplot(stack(areas), aes(x = ind, y = values)) +
  geom_boxplot()+
  scale_y_continuous(trans='log10')

lengths <- wide %>% select(hausdorff3, hausdorff4)
ggplot(stack(lengths), aes(x = ind, y = values)) +
  geom_boxplot()+
  scale_y_continuous(trans='log10')

### lots of exploratory plotting from here
ggplot(wide, aes(x=q3size)) +
geom_histogram(binwidth = 0.1) +
scale_x_continuous(trans='log10')

ggplot(filterdata, aes(y=q3area, x=dem_answer.6, colour=dem_answer.6)) +
  geom_boxplot(size=1) +
  #stat_summary(fun.data = give.n, geom = "text", size=10) +
  scale_y_continuous(trans='log10') + 
  labs(x="Participants normal mode of transport", y="Question 3 area (km2)", colour="") +
  theme_grey(base_size = 22) + 
  theme(aspect.ratio=1,legend.position="none") +
  geom_jitter(width=0.2, size=4)

# Area vs length, for both questions, with 95% confidence ellipses
ggplot(filterdata, aes(y=q4area, x=hausdorff4)) +
  geom_point(aes(colour = "q4"),size=4) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  stat_ellipse(geom = "polygon", level=0.95, 
              fill = "#00BFC4", alpha = 0.15) +
  geom_point(data=wide, aes(y=q3area, x=hausdorff3, colour = "q3"), size=4) +
  stat_ellipse(data=wide, aes(y=q3area, x=hausdorff3),fill = "#F8766D",geom = "polygon",
               level=0.95,  alpha = 0.15) +
  labs(x="Centroid to furthest edge length (hausdorff distance, m)", y="Area (km2)") + 
  scale_color_manual(values=c(q3="#F8766D",q4="#00BFC4"), labels=c("Question 3: Current walking area", "Question 4: Potential walking area")) +
  theme_grey(base_size = 22) + 
  theme(aspect.ratio=1, legend.position = c(0.75, 0.15), legend.title = element_blank())

# boxplot of area for both questions, with jitter points
dat.m <- melt(filterdata,id.vars='id_person', measure.vars=c('q3area','q4area'))
ggplot(dat.m, aes(x=variable, y=value, color=variable)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_x_discrete(labels=c("Question 3", "Question 4")) +
  labs(x="Area (km2)",y="") +
  theme_grey(base_size = 50) + 
  theme(aspect.ratio=1, legend.position = "none", legend.title = element_blank()) +
  geom_jitter(width=0.2, size = 5)

# t test of lengths
t.test(wide$hausdorff3, wide$hausdorff4)

# boxplot of length for both questions, with jitter points
dat.l <- melt(filterdata,id.vars='id_person', measure.vars=c('hausdorff3','hausdorff4'))
ggplot(dat.l, aes(x=variable, y=value, color=variable)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_x_discrete(labels=c("Question 3", "Question 4")) +
  labs(x="Length (m)",y="") +
  theme_grey(base_size = 50) + 
  theme(aspect.ratio=1, legend.position = "none", legend.title = element_blank()) +
  geom_jitter(width=0.2, size = 5)

# q3 area vs q4 area, with points coloured by normal mode of transport
ggplot(filterdata, aes(y=q4area, x=q3area, color=dem_answer.6)) +
  geom_point(size=4) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10', limits=c(0.04, max(wide$q4area,na.rm=TRUE))) +
  geom_abline(intercept=0, slope=1) +
  labs(x="Question 3 area (km2)", y="Question 4 area (km2)", colour="Most common mode used \n for non-commute trips") +
  theme_grey(base_size = 22) +
  theme(aspect.ratio=1, legend.position = c(0.75, 0.15))

library(corrr)
cor.test(wide$q3area, wide$q4area)

# q3 length vs q4 length
ggplot(wide, aes(y=hausdorff4, x=hausdorff3)) +
  geom_point(fill = "grey80", size=4) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  geom_abline(intercept=0, slope=1) +
                        theme_grey(base_size = 22)

# anova comparisons
commute_aov <- tidy(aov(q3size ~ dem_answer.5, data = wide))
mode_aov <- tidy(aov(q3size ~ dem_answer.6, data = wide))
age_aov <- tidy(aov(q3size ~ dem_answer.7, data = wide))
source_aov <- tidy(aov(q3size ~ dem_answer.1, data = wide))
days_aov <- tidy(aov(q3size ~ dem_answer.3, data = wide))
commute_4_aov <- tidy(aov(q4size ~ dem_answer.5, data = wide))
mode_4_aov <- tidy(aov(q4size ~ dem_answer.6, data = wide))
age_4_aov <- tidy(aov(q4size ~ dem_answer.7, data = wide))
source_4_aov <- tidy(aov(q4size ~ dem_answer.1, data = wide))
days_4_aov <- tidy(aov(q4size ~ dem_answer.3, data = wide))
commute_aov <- tidy(aov(q3size ~ dem_answer.5, data = wide))
mode_aov_l <- tidy(aov(hausdorff3 ~ dem_answer.6, data = wide))
age_aov_l <- tidy(aov(hausdorff3 ~ dem_answer.7, data = wide))
source_aov_l <- tidy(aov(hausdorff3 ~ dem_answer.1, data = wide))
days_aov_l <- tidy(aov(hausdorff3 ~ dem_answer.3, data = wide))
commute_4_aov_l <- tidy(aov(hausdorff4 ~ dem_answer.5, data = wide))
mode_4_aov_l <- tidy(aov(hausdorff4 ~ dem_answer.6, data = wide))
age_4_aov_l <- tidy(aov(hausdorff4 ~ dem_answer.7, data = wide))
source_4_aov_l <- tidy(aov(hausdorff4 ~ dem_answer.1, data = wide))
days_4_aov_l <- tidy(aov(hausdorff4 ~ dem_answer.3, data = wide))

library(ggpubr)

dat <- wide
# visualisation routine
x <- which(names(dat) == "dem_answer.7") # name of grouping variable
y <- which(names(dat) == "hausdorff3") # names of variables to test)
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("18-30", "31-40"), c("18-30", "41-50"), c("18-30", "51-60"),
    c("31-40", "51-60")) # comparisons for post-hoc tests
# Edit until here
# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(dat,
      x = colnames(dat[j]), y = colnames(dat[i]),
      color = colnames(dat[j]),
      legend = "none",
      palette = "npg",
      add = "jitter"
    )
    print(p) +
    #print(
      # p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format.., " (", ifelse(..p.adj.. > 0.05, "not significant", ..p.signif..), ")")),
      #   method = method1, label.y = max(dat[, i], na.rm = TRUE)
      # )
      stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # only relevant if p-value of ANOVA or Kruskal-Wallis test < 0.05
    #)
  }
}
