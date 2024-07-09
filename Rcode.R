#Install packages needed in these exercises

install.packages(c("directlabels", "haven", "tidyverse", "ggeffects", 
                   "lme4", "merTools", "labelled", "sjPlot", "Metrics", "here"))

library(directlabels)
library(haven)
library(tidyverse)
library(ggeffects)
library(lme4)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)
library(here)

#PRACTICAL 1

tutorial <- read_dta(here("tutorial.dta"))

str(tutorial)
summary(tutorial)

tutorial <- tutorial[order(tutorial$school, 
                           tutorial$student),]

#Checking the summary statistics
summary(tutorial$normexam)
sd(tutorial$normexam)

ggplot(data = tutorial, aes(x = normexam))+
  geom_histogram(fill = "#FF6885", color = "#FF6885", alpha = 0.6)

View(tutorial)

tutorial %>%
  group_by(school) %>%
  summarise(max = max(student)) %>% 
  summarise(total = sum(max))
  

summary(tutorial)

tutorial[3,1]



nullmodel <- lmer(normexam ~ (1 | school), data=tutorial)
summary(nullmodel)

sum <- lm(normexam~1, data=tutorial)
AIC(nullmodel, singlelevel)

randomint <- lmer(normexam ~ standlrt + (1 | school), data=tutorial)
summary(randomint)

reEX <- REsim(randomint)
plotREsim(reEX, labs=TRUE)

predexam <- fitted(randomint)
install.packages("directlabels")
library("directlabels")

ggplot(tutorial, aes(x=standlrt, y=predexam, group=as.factor(school)
                     , color=as.factor(school))) +
  geom_line() +
  scale_colour_discrete(guide = 'none') +
  geom_dl(aes(label=school), method="last.points")

randomint2 <- lmer(normexam ~ standlrt + avslrt + (1 | school), data=tutorial)
summary(randomint2)

tab_model(singlelevel, nullmodel, randomint, randomint2)


#PRACTICAL 2
-----------------

tut <- read_dta(here("TutorialData.dta"))

tut$stratum <- 10000 * tut$sex + 1000 * tut$race + 100 * tut$education + 10 * tut$income + 1 * tut$age

tut <- unlabelled(tut)

table(tut$stratum)

tut <- tut %>%
  group_by(stratum) %>%
  mutate(strataN = n())

table(tut$sex)
table(tut$race)
table(tut$education)
table(tut$income)
table(tut$age)

summary(tut$HbA1c)
sd(tut$HbA1c)

ggplot(tut, aes(x=HbA1c)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=2, boundary=22) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of Individuals") +
  geom_vline(aes(xintercept=40.22)) +
  annotate("text", x=53, y=0.1, label="Sample Mean=40.2 mmol/mol") +
  annotate("text", x=11, y=0.01, label="Min = 10.3 mmol/mol") +
  annotate("text", x=80, y=0.01, label="Max = 101.2 mmol/mol")

model1A <- lmer(HbA1c ~ (1|stratum), data=tut)

tut$m1Am <- predict(model1A)

model1B <- lmer(HbA1c ~ sex + race + education + income + age + (1|stratum), data=tut)


library(doParallel)
registerDoParallel(cores=4)
m1Bm <- predictInterval(model1B, level=0.95, include.resid.var=FALSE)


#create ids for each dataset
m1Bm <- mutate(m1Bm, id=row_number())
tut$id <- seq.int(nrow(tut))

#Merge the datasets
tut2 <- merge(tut, m1Bm, by="id")

tut2 <- tut2 %>%
  rename(
    m1Bmfit=fit,
    m1Bmupr= upr,
    m1Bmlwr=lwr
  )

stratum_level <- aggregate(x=tut2[c("HbA1c")], 
	by=tut2[c("sex", "race", "education", "income", "age",
	"stratum", "strataN", "m1Am", "m1Bmfit", "m1Bmupr", "m1Bmlwr")], FUN=mean)

#Produce table
tab_model(model1A, model1B, p.style="stars")

#Graph stratums
stratum_level <- stratum_level %>%
  mutate(rank=rank(m1Bmfit))

ggplot(stratum_level, aes(y=m1Bmfit, x=rank)) +
  geom_point() +
  geom_pointrange(aes(ymin=m1Bmlwr, ymax=m1Bmupr)) +
  ylab("Predicted HbA1c, Model 1B") +
  xlab("Stratum Rank") + 
  theme_bw()

stratum_level <- stratum_level[order(stratum_level$rank),]

head(stratum_level)
tail(stratum_level)

m1Bu <- REsim(model1B)

p <-plotREsim(m1Bu) +
  xlab("Stratum Rank") +
  ylab("Predicted stratum Random Effect in HbA1c (mmol/mol)") +
  ggtitle("") +
  theme(
    strip.background=element_blank(),
    strip.text.x = element_blank()
  )
p

m1Bucut <- p[["data"]]

m1Bucut <- m1Bucut %>%
  filter(sig=="TRUE") %>%
  mutate(xvar=as.factor(xvar))

ggplot(m1Bucut, aes(x=xvar, y=median, label=groupID)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=ymin, ymax=ymax)) + 
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_text(hjust=0, vjust=5) +
  xlab("Stratum Rank") +
  ylab("Predicted stratum Random Effect in HbA1c (mmol/mol)")+
  theme_bw()

