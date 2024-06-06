

# TODO
# look into correlations for likert items
#https://rpubs.com/JBean/840128

dat <- read_xlsx("UConn TGN Measure Validation dataset and codebook  18apr2024.xlsx", 
                 sheet = "Raw data")
dem <- read_xlsx("UConn TGN Measure Validation dataset and codebook  18apr2024.xlsx", 
                 sheet = "prolific demographics")


dem_nb <- read.csv("prolific_export_64eee7348f37e7eabaef3c13 (5).csv")
dem_tr <- read.csv("prolific_export_64eee57e11a73795c75fad6c (15).csv")


dem_nb$Participant.id
length(unique(dem_nb$Participant.id))
length(unique(dem_tr$Participant.id))


names(dem_nb) == names(dem_tr)




dem_comb <- rbind(dem_nb, dem_tr)


dat<- clean_names(dat)
#dat_comb <- clean_dat = dat_comb <- clean_names(dem_comb)

names(dat)[1] <- "participant_id"
names(dat_comb)
dat2 <- left_join(dat, dat_comb, by = "participant_id")

#----------------------------------#

library(gtsummary)
dat2$class_content_1


# class content
class_cont <- dat2 %>% dplyr::select(contains("class_")& !contains("rep")) 
names(class_cont)
dat2 %>% dplyr::select(contains("class_")& !contains("rep")) %>%
  tbl_summary()

library(psych)

psych::describe(class_cont)

psych::alpha(class_cont, check.keys = TRUE)

fa1 <- fa(class_cont[, -3], nfactors = 1)
fa2 <- fa(class_cont[, -3], nfactors = 2)
fa3 <- fa(class_cont[, -3], nfactors = 3)

fa1
fa2
# 1 and 2 hang together
# 4 and 5 hang together

write.csv(dat2, "combined_data.csv", row.names = FALSE)

# policies
policies_disc <- dat2 %>% dplyr::select(contains("policies_disc")) 
names(class_cont)


dat2 %>% dplyr::select(contains("policies_disc")) %>%
  tbl_summary()

policies_disc[, 1:8] <- apply(policies_disc[, 1:8], 2, 
                              function(x) ifelse(x==-99, NA, x))


dev.new()
hist(policies_disc$policies_disc_4)

# remove failed Attention Checks
policies_disc <- policies_disc %>% filter(policies_disc_4==4)
policies_disc <-policies_disc %>% dplyr::select(-policies_disc_4)

psych::describe(policies_disc)

psych::alpha(policies_disc, check.keys = TRUE)

ev <- eigen(cor(policies_disc, use="pairwise.complete.obs")) 
scree(policies_disc, pc=FALSE)  
fa.parallel(policies_disc, fa="fa")

fa1 <- fa(policies_disc[, c(-2, -4)], nfactors = 1)
fa3 <- fa(policies_disc[, c(-2, -4)], nfactors = 3)


fa2 <- fa(policies_disc[, c(-2,  -4)], nfactors = 2)
fa2

# check alpha for fa2 without policies_disc_1
psych::alpha(policies_disc[, c(-1, -2, -4)], check.keys = TRUE)

## policies_prac
policies_prac <- dat2 %>% dplyr::select(contains("policies_prac")) 

policies_prac[, 1:3] <- apply(policies_prac[, 1:3], 2, 
                              function(x) ifelse(x==-99, NA, x))

psych::alpha(policies_prac, check.keys = TRUE)

cor(policies_prac, use = "pair") %>% round(2)
mean(c(0.60, 0.63, 0.71))

## policies_inc
policies_inc <- dat2 %>% dplyr::select(contains("policies_inc")) 


policies_inc[, 1:6] <- apply(policies_inc[, 1:6], 2, 
                              function(x) ifelse(x==-99, NA, x))
psych::alpha(policies_inc, check.keys = TRUE)


psych::describe(policies_inc)

# policies_inc[, 1:6] <- apply(policies_inc[, 1:6], 2, 
#                              function(x) factor(x, levels = c("1","2", 
#                                                               "3", "4","5"),
#                                                 ordered = TRUE))
# 
# policies_inc <- policies_inc %>% mutate(across(everything(), ~as.factor()))


policies_inc$policies_inc_1 <- as.factor(policies_inc$policies_inc_1)
policies_inc$policies_inc_2 <- as.factor(policies_inc$policies_inc_2)
policies_inc$policies_inc_3 <- as.factor(policies_inc$policies_inc_3)
policies_inc$policies_inc_4 <- as.factor(policies_inc$policies_inc_4)
policies_inc$policies_inc_5 <- as.factor(policies_inc$policies_inc_5)
policies_inc$policies_inc_6 <- as.factor(policies_inc$policies_inc_6)


str(policies_inc)

library(polycor)
cor1 <- cor(policies_inc, use = "pairwise")

cor2<-hetcor(policies_inc %>% data.frame(), use= "pairwise")


scree(cor2, pc=FALSE)  
fa.parallel(cor2, fa="fa")


eigen(cor2$correlations)


(22*84) + (55*50)


4920



fa2 <- fa(cor2$correlations, nfactors = 2)
fa2
fa3
