#reverse code a variable

library(psych)
library(dplyr)
dat <- data.frame(x = c(1, 2, 3, 4, 5))
dat$x

dat$xr <- reverse.code(dat$x, 
                       keys = -1) %>% as.numeric()


dat
