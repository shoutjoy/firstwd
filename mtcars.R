


#tstatix 이용
library(rstatix)
mtcars %>% group_by(cyl, am) %>% get_summary_stats(mpg, type="mean_sd")

#dplyr
library(dplyr, warn.conflicts = FALSE)
mtcars %>% group_by(cyl, am) %>% summarise(n=n(),mean=mean(mpg), sd=sd(mpg))
