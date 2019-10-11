library(dslabs)
data("research_funding_rates")
library(tidyverse)
options(digits = 3)
research_funding_rates

table <- research_funding_rates %>% 
  mutate(men_not = applications_men - awards_men, women_not = applications_women - awards_women) %>%
  select(discipline, awards_men, men_not, awards_women, women_not)

table
sum(table$men_not)
sum(table$women_not)

sum(table$awards_men)/sum(table$men_not + table$awards_men) * 100
sum(table$awards_women)/sum(table$women_not + table$awards_women) * 100

## 2 by 2 table
two_by_two <- data.frame(awarded = c("no", "yes"),
  men = c(no = sum(table$men_not), yes = sum(table$awards_men)),
  women = c(no = sum(table$women_not), yes = sum(table$awards_women))
)
two_by_two


## Chi squared test

chisq_test <- two_by_two %>% select(-awarded) %>%
  chisq.test()
chisq_test
