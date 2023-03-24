# install.packages('tidyverse')
# install.packages('survey')
# install.packages('marginaleffects')
# install.packages("showtext")

library(tidyverse)
library(survey)
library(marginaleffects)
library(showtext)


# Loading Data
anes20 <- read_csv("data/anes_20.csv")

# Constructing Data Frame
vote_duty_age <- anes20 %>%
  select(duty_raw = V201225x,
         education_raw = V201510,
         age_raw = V201507x,
         weights = V200010a,
         psu = V200010c,
         stratum = V200010d) %>%
  mutate(across(everything(),\(x){ifelse(x < 0 | x > 80,NA,x)}))

## checking that all dk/no & others are out.
map(as.list(vote_duty_age[1:3]),table)

## checking weights
hist(vote_duty_age$weights)

## Making cohort conditions
### Rather than writing out every condition...
low_bound <- seq(25,80,5)
high_bound <- lead(low_bound)-1

low_bound <- low_bound[1:11]
high_bound <- high_bound[1:11]

conditions <- 
  pmap(list(low_bound, high_bound, c(1:11)),
       \(x,y,i){quo(age_raw %in% !!x:!!y ~ !!i)})

conditions <- 
  append(conditions, quo(age_raw >= 80 ~ 12L))

## Making coarsened categories
vote_duty_age <- 
  vote_duty_age %>% 
  mutate(cohort = case_when(!!!conditions),
         generations = case_when(
           age_raw %in% 24:39 ~ 1,
           age_raw %in% 40:55 ~ 2,
           age_raw %in% 56:74 ~ 3,
           age_raw %in% 74:80 ~ 4
         ),
         duty_bin = ifelse(duty_raw < 4,1,0),
         college = ifelse(education_raw > 5,1,0)) %>%
  mutate(cohort = as.factor(cohort),
         generations = as.factor(generations)) %>%
  drop_na()


# Modeling time! 

## Setting up survey design
anes_dsgn<- svydesign(id = ~psu, weights = ~weights, nest = TRUE, 
          strata = ~stratum, data = vote_duty_age)

## svyglm

model1 <-  svyglm(formula = duty_bin ~ generations*college, 
                  design = anes_dsgn, family = 'binomial')

model2 <- svyglm(formula = duty_bin ~ cohort*college, 
                 design = anes_dsgn, family = 'binomial')

## 
preds1 <- predictions(model1, newdata = datagrid(generations = 1:4,
                                                 college = c(0,1)))
preds2 <- predictions(model2, newdata = datagrid(cohort = 1:12,
                                       college = c(0,1)))


# Graphing time!


font_add_google("Vollkorn","vollkorn")
font_add_google("Archivo","archivo")

showtext_auto()

method_cap <- "Points are predicted probabilities from a logistic regression model incorporating survey design. Bars are 95% confidence intervals. "

### Generations
  ggplot(preds1,aes(x = generations, y = estimate, 
                   fill = as.factor(college))) +
  geom_col(position = position_dodge()) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(.9),
                  width = .2,
                   alpha = .4) +
    scale_fill_manual(name = "",
                       labels = c("Less than\nCollege Education", "College Educated"),
                       values = c("#6a9871","#818BBD")) +
    scale_x_discrete(breaks = 1:4,labels = c("Millennials", "Gen-X", "Baby\nBoomers", "Greatest\nGeneration")) +
    scale_y_continuous(breaks = seq(.2,.9,.1))+
    xlab("") +
    ylab("Probability of\nseeing voting\nas a duty") +
    labs(title = "Educated people are more likely to see voting as  \"duty\" vs a \"choice\" across all generations",
         subtitle = "Older people are more likely to see voting as a duty overall, and the gap between college-educated \nand non-college educated is roughly the same for all groups apart from Millennials\n",
         caption =  paste0("Peter Licari\n\nData: ANES 2020\n",
                           stringr::str_wrap(width = 70, 
                                             method_cap)))+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(family = "Vollkorn", face = "bold",
                                    color = "#071014", size = 25),
          plot.subtitle = element_text(hjust = .5, size = 15),
          axis.title = element_text(color = "#283038"),
          text = element_text(family = "Archivo", color = "#283038"),
          panel.background = element_rect(fill ="#F9f9f9"),
          plot.background = element_rect(fill = "#F9f9f9"),
          legend.background = element_rect(fill = "#F9f9f9"),
          legend.key = element_rect(fill = "#F9f9f9"),
          legend.position = "top",
          axis.ticks = element_blank(),
          axis.title.y = element_text(hjust = 0, angle = 0, vjust = 1.05,
                                      margin = margin(r=10)),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#a1abb5")
          ) 

  ### Age cohorts 
  
  xaxlabs <- map2_chr(low_bound, high_bound, ~paste0(.x,"-",.y))
  xaxlabs <- c(xaxlabs,"80+")  
  
  ggplot(preds2,aes(x = cohort, y = estimate, 
                    fill = as.factor(college))) +
    geom_col(position = position_dodge()) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(.9),
                  width = .2,
                  alpha = .4) +
    scale_fill_manual(name = "",
                      labels = c("Less than\nCollege Education", "College Educated"),
                      values = c("#6a9871","#818BBD")) +
    scale_x_discrete(breaks = 1:12,labels =xaxlabs) +
    scale_y_continuous(breaks = seq(.2,.9,.1))+
    xlab("Age") +
    ylab("Probability of\nseeing voting\nas a duty") +
    labs(title = "Educated people are more likely to see voting as  \"duty\" vs a \"choice\" across all generations",
         subtitle = "Older people are more likely to see voting as a duty overall, and the gap between college-educated \nand non-college educated is roughly the same sans a couple, younger cohorts\n",
         caption =  paste0("Peter Licari\n\nData: ANES 2020\n",
                           stringr::str_wrap(width = 70, 
                                             method_cap)))+
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 12),
          plot.title = element_text(family = "Vollkorn", face = "bold",
                                    color = "#071014", size = 25),
          plot.subtitle = element_text(hjust = .5, size = 15),
          axis.title = element_text(color = "#283038"),
          text = element_text(family = "Archivo", color = "#283038"),
          panel.background = element_rect(fill ="#F9f9f9"),
          plot.background = element_rect(fill = "#F9f9f9"),
          legend.background = element_rect(fill = "#F9f9f9"),
          legend.key = element_rect(fill = "#F9f9f9"),
          legend.position = "top",
          axis.ticks = element_blank(),
          axis.title.y = element_text(hjust = 0, angle = 0, vjust = 1.05,
                                      margin = margin(r=10)),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#a1abb5")
    ) 
  
  
  
  
  xaxlabs <- map2_chr(low_bound, high_bound, ~paste0(.x,"-",.y))
  xaxlabs <- c(xaxlabs,"80+")
