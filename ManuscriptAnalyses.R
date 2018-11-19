library(papaja)
library(readr)
library(readxl)
library(xtable)
library(knitr)
library(broom)
library(tidyverse)
library(ggplot2)
library(mediation)
library(ggrepel)
library(MBESS)
library(checkmate)


#importing data
Shortcuts <- read_csv("./ShortcutAnalysisData.csv")
Shortcuts$StateAB <- state.abb

#importing reliability and other descriptives
DescriptivesTable <- read_csv("./DescriptiveStatistics.csv")

#calculating correlation between income and gini
gini_income_cor1 <- cor.test(Shortcuts$Gini, Shortcuts$Percapi)
gini_income_cor1_r <- round(gini_income_cor1$estimate, 2)
gini_income_cor1_p <- round(gini_income_cor1$p.value, 2)
gini_income_cor2 <- cor.test(Shortcuts$Gini, Shortcuts$MedIncome)
gini_income_cor2_r <- round(gini_income_cor2$estimate, 2)
gini_income_cor2_p <- round(gini_income_cor2$p.value, 2)

#calculating correlation between gini and shortcut / learning searching
ginishortcuts <- apa_print(cor.test(Shortcuts$Gini, Shortcuts$Shortcut))
ginilearn <- apa_print(cor.test(Shortcuts$Gini, Shortcuts$Learn))

#looking at quartiles
InequalityShortcuts <- (lm(Shortcut ~ q50_q10 + q90_q50, data = Shortcuts))
summary(InequalityShortcuts)
apa_print(InequalityShortcuts)

#creating numbers for Texas and Iowa
IncomeStats2015 <- read_csv("./Income_SAIPE2005-2015.csv")
Iowa <- dplyr::filter(IncomeStats2015, StateName == "Iowa")
Texas <- dplyr::filter(IncomeStats2015, StateName == "Texas")


## Making a nice figure that shows that Gini predicts achievement outcomes
Shortcuts_std <- Shortcuts
Shortcuts_std[, 2:22] <- scale(Shortcuts_std[, 2:22])
View(Shortcuts_std)


ggplot(Shortcuts_std, aes(Gini)) + 
  geom_point(aes(y = Math8, colour = "Math8")) + 
  geom_point(aes(y = Reading8, colour = "Reading8"))+
  geom_point(aes(y = Science8, colour = "Science8"))+
  geom_point(aes(y = Vocab8, colour = "Vocabulary8"))


ggplot(Shortcuts_std, mapping = aes(x = Gini_MC, y = Math8))+
  geom_text_repel(aes(label=StateAB))+
  geom_abline()+
  labs(x = "Gini Index of Inequality", y = "Average NAEP Math Scores")+
  theme_apa(base_family = )  

ggsave("InequalityShortcuts.pdf", scale = 1.6)

ggplot(Shortcuts_std, mapping = aes(x = Gini, y = Shortcut))+
  geom_text_repel(aes(label=StateAB))+
  geom_abline()+
  labs(x = "Gini Index of Inequality", y = "Searches for Academic Shortcuts")+
  theme_apa(base_family = )

ggsave("InequalityShortcuts.pdf", scale = 1.6)

ggplot(Shortcuts_std, mapping = aes(x = Gini, y = Shortcut))+
  geom_text_repel(aes(label=StateAB, family = "serif"), box.padding = .04, point.padding = .098)+
  labs(x = "Gini Index of Inequality", y = "Searches for Academic Shortcuts")+
  theme_apa()  +
  theme(text = element_text(family = "serif", size = 12))+
  ggtitle("People Search More for Academic Shortcuts in Unequal US States")

# ggsave("mtcars.pdf", width = 4, height = 4)


#mean centering
GiniM <- mean(Shortcuts$Gini)
PercapiM <- mean(Shortcuts$Percapi)
MedIncomeM <- mean(Shortcuts$MedIncome)

Shortcuts$Gini_MC <- (Shortcuts$Gini - GiniM)
Shortcuts$Percapi_MC <- (Shortcuts$Percapi - PercapiM)
Shortcuts$MedIncome_MC <- (Shortcuts$MedIncome - MedIncomeM)

View(Shortcuts)

### Mediation analyses without income

#4th grade
#Math4
med.fit.M4 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.M4 <- lm(Math4 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.M4 <- mediation::mediate(med.fit.M4, out.fit.M4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.M4)
summary(out.fit.M4)

M4a <- med.fit.M4$coefficients[2]
M4b <- out.fit.M4$coefficients[2]
M4c_TE <- med.out.M4$tau.coef
M4cprime_ADE <- med.out.M4$z0
M4_ACME <- med.out.M4$d.avg
M4_PropMed <- med.out.M4$n0

#Reading4
med.fit.R4 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.R4 <- lm(Reading4 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.R4 <- mediation::mediate(med.fit.R4, out.fit.R4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.R4)
summary(out.fit.R4)

R4a <- med.fit.R4$coefficients[2]
R4b <- out.fit.R4$coefficients[2]
R4c_TE <- med.out.R4$tau.coef
R4cprime_ADE <- med.out.R4$z0
R4_ACME <- med.out.R4$d.avg
R4_PropMed <- med.out.R4$n0

#Vocab4
med.fit.V4 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.V4 <- lm(Vocab4 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.V4 <- mediation::mediate(med.fit.V4, out.fit.V4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.V4)
summary(out.fit.V4)

V4a <- med.fit.V4$coefficients[2]
V4b <- out.fit.V4$coefficients[2]
V4c_TE <- med.out.V4$tau.coef
V4cprime_ADE <- med.out.V4$z0
V4_ACME <- med.out.V4$d.avg
V4_PropMed <- med.out.V4$n0

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science4 != "NA")
                               
#Science4
med.fit.S4 <- lm(Shortcut~ Gini_MC, data = Shortcuts_science)
out.fit.S4 <- lm(Science4 ~ Shortcut + Gini_MC, data = Shortcuts_science)
med.out.S4 <- mediation::mediate(med.fit.S4, out.fit.S4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.S4)
summary(out.fit.S4)

S4a <- med.fit.S4$coefficients[2]
S4b <- out.fit.S4$coefficients[2]
S4c_TE <- med.out.S4$tau.coef
S4cprime_ADE <- med.out.S4$z0
S4_ACME <- med.out.S4$d.avg
S4_PropMed <- med.out.S4$n0


#8th grade

#Math8
med.fit.M8 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.M8 <- lm(Math8 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.M8 <- mediation::mediate(med.fit.M8, out.fit.M8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.M8)
summary(out.fit.M8)

M8a <- med.fit.M8$coefficients[2]
M8b <- out.fit.M8$coefficients[2]
M8c_TE <- med.out.M8$tau.coef
M8cprime_ADE <- med.out.M8$z0
M8_ACME <- med.out.M8$d.avg
M8_PropMed <- med.out.M8$n0

#Reading8
med.fit.R8 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.R8 <- lm(Reading8 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)

R8a <- med.fit.R8$coefficients[2]
R8b <- out.fit.R8$coefficients[2]
R8c_TE <- med.out.R8$tau.coef
R8cprime_ADE <- med.out.R8$z0
R8_ACME <- med.out.R8$d.avg
R8_PropMed <- med.out.R8$n0

#Vocab8
med.fit.V8 <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.V8 <- lm(Vocab8 ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.V8 <- mediation::mediate(med.fit.V8, out.fit.V8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.V8)
summary(out.fit.V8)

V8a <- med.fit.V8$coefficients[2]
V8b <- out.fit.V8$coefficients[2]
V8c_TE <- med.out.V8$tau.coef
V8cprime_ADE <- med.out.V8$z0
V8_ACME <- med.out.V8$d.avg
V8_PropMed <- med.out.V8$n0

#Science8

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science8 != "NA")

med.fit.S8 <- lm(Shortcut ~ Gini_MC, data = Shortcuts_science)
out.fit.S8 <- lm(Science8 ~ Shortcut + Gini_MC, data = Shortcuts_science)
med.out.S8 <- mediation::mediate(med.fit.S8, out.fit.S8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.S8)
summary(out.fit.S8)

S8a <- med.fit.S8$coefficients[2]
S8b <- out.fit.S8$coefficients[2]
S8c_TE <- med.out.S8$tau.coef
S8cprime_ADE <- med.out.S8$z0
S8_ACME <- med.out.S8$d.avg
S8_PropMed <- med.out.S8$n0

#Adult attainment

#Less9th
med.fit.9th <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.9th <- lm(Less9th ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.9th <- mediation::mediate(med.fit.9th, out.fit.9th, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.9th)
summary(out.fit.9th)

Less9tha <- med.fit.9th$coefficients[2]
Less9thb <- out.fit.9th$coefficients[2]
Less9thc_TE <- med.out.9th$tau.coef
Less9thcprime_ADE <- med.out.9th$z0
Less9th_ACME <- med.out.9th$d.avg
Less9th_PropMed <- med.out.9th$n0

#HS drop
med.fit.HSdrop <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.HSdrop <- lm(HSnoDiploma ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.HSdrop <- mediation::mediate(med.fit.HSdrop, out.fit.HSdrop, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.HSdrop)
summary(out.fit.HSdrop)

HSdropa <- med.fit.HSdrop$coefficients[2]
HSdropb <- out.fit.HSdrop$coefficients[2]
HSdropc_TE <- med.out.HSdrop$tau.coef
HSdropcprime_ADE <- med.out.HSdrop$z0
HSdrop_ACME <- med.out.HSdrop$d.avg
HSdrop_PropMed <- med.out.HSdrop$n0

#HS
med.fit.HSgrad <- lm(Shortcut ~ Gini_MC , data = Shortcuts)
out.fit.HSgrad <- lm(Hsgradormore ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.HSgrad <- mediation::mediate(med.fit.HSgrad, out.fit.HSgrad, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.HSgrad)
summary(out.fit.HSgrad)

HSgrada <- med.fit.HSgrad$coefficients[2]
HSgradb <- out.fit.HSgrad$coefficients[2]
HSgradc_TE <- med.out.HSgrad$tau.coef
HSgradcprime_ADE <- med.out.HSgrad$z0
HSgrad_ACME <- med.out.HSgrad$d.avg
HSgrad_PropMed <- med.out.HSgrad$n0

#BA
med.fit.BA <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.BA <- lm(BachDegormore ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.BA <- mediation::mediate(med.fit.BA, out.fit.BA, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.BA)
summary(out.fit.BA)

BAa <- med.fit.BA$coefficients[2]
BAb <- out.fit.BA$coefficients[2]
BAc_TE <- med.out.BA$tau.coef
BAcprime_ADE <- med.out.BA$z0
BA_ACME <- med.out.BA$d.avg
BA_PropMed <- med.out.BA$n0

#MA
med.fit.MA <- lm(Shortcut ~ Gini_MC, data = Shortcuts)
out.fit.MA <- lm(GradDeg ~ Shortcut + Gini_MC, data = Shortcuts)
med.out.MA <- mediation::mediate(med.fit.MA, out.fit.MA, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.MA)
summary(out.fit.MA)

MAa <- med.fit.MA$coefficients[2]
MAb <- out.fit.MA$coefficients[2]
MAc_TE <- med.out.MA$tau.coef
MAcprime_ADE <- med.out.MA$z0
MA_ACME <- med.out.MA$d.avg
MA_PropMed <- med.out.MA$n0

#Making a tables with the results

#4th
Mediation4_noincome_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M4a, R4a, S4a, V4a),
  b = c(M4b, R4b, S4b, V4b),
  c = c(M4c_TE, R4c_TE, S4c_TE, V4c_TE),
  cprime = c(M4cprime_ADE, R4cprime_ADE, S4cprime_ADE, V4cprime_ADE),
  ACME = c(M4_ACME, R4_ACME, S4_ACME, V4_ACME),
  pb = c(M4_PropMed, R4_PropMed, S4_PropMed, V4_PropMed))

colnames(Mediation4_noincome_table) <- c("Fourth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation4_noincome_table)
#exporting
write.csv(Mediation4_noincome_table, "Mediation4_noincome_table.csv", row.names = FALSE)

#8th
Mediation8_noincome_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M8a, R8a, S8a, V8a),
  b = c(M8b, R8b, S8b, V8b),
  c = c(M8c_TE, R8c_TE, S8c_TE, V8c_TE),
  cprime = c(M8cprime_ADE, R8cprime_ADE, S8cprime_ADE, V8cprime_ADE),
  ACME = c(M8_ACME, R8_ACME, S8_ACME, V8_ACME),
  pb = c(M8_PropMed, R8_PropMed, S8_PropMed, V8_PropMed))

colnames(Mediation8_noincome_table) <- c("Eighth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation8_noincome_table)
#exporting
write.csv(Mediation8_noincome_table, "Mediation8_noincome_table.csv", row.names = FALSE)

#Attainment
MediationAttain_noincome_table <- data.frame(
  Outcome = c("Less than 9th", "Less than HS", "HS grad", "BA", "MA"),
  a = c(Less9tha, HSdropa, HSgrada, BAa, MAa),
  b = c(Less9thb, HSdropb, HSgradb, BAb, MAb),
  c = c(Less9thc_TE, HSdropc_TE, HSgradc_TE, BAc_TE, MAc_TE),
  cprime = c(Less9thcprime_ADE, HSdropcprime_ADE, HSgradcprime_ADE, BAcprime_ADE, MAcprime_ADE),
  ACME = c(Less9th_ACME, HSdrop_ACME, HSgrad_ACME, BA_ACME, MA_ACME),
  pb = c(Less9th_PropMed, HSdrop_PropMed, HSgrad_PropMed, BA_PropMed, MA_PropMed))

colnames(MediationAttain_noincome_table) <- c("Educational Attainment", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(MediationAttain_noincome_table)
#exporting
write.csv(MediationAttain_noincome_table, "MediationAttain_noincome_table.csv", row.names = FALSE)



### Mediation analyses with income MC

#4th grade
#Math4
med.fit.M4 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.M4 <- lm(Math4 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.M4 <- mediation::mediate(med.fit.M4, out.fit.M4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.M4)
summary(out.fit.M4)

M4a <- med.fit.M4$coefficients[2]
M4b <- out.fit.M4$coefficients[2]
M4c_TE <- med.out.M4$tau.coef
M4cprime_ADE <- med.out.M4$z0
M4_ACME <- med.out.M4$d.avg
M4_PropMed <- med.out.M4$n0

#Reading4
med.fit.R4 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.R4 <- lm(Reading4 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.R4 <- mediation::mediate(med.fit.R4, out.fit.R4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.R4)
summary(out.fit.R4)

R4a <- med.fit.R4$coefficients[2]
R4b <- out.fit.R4$coefficients[2]
R4c_TE <- med.out.R4$tau.coef
R4cprime_ADE <- med.out.R4$z0
R4_ACME <- med.out.R4$d.avg
R4_PropMed <- med.out.R4$n0

#Vocab4
med.fit.V4 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.V4 <- lm(Vocab4 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.V4 <- mediation::mediate(med.fit.V4, out.fit.V4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.V4)
summary(out.fit.V4)

V4a <- med.fit.V4$coefficients[2]
V4b <- out.fit.V4$coefficients[2]
V4c_TE <- med.out.V4$tau.coef
V4cprime_ADE <- med.out.V4$z0
V4_ACME <- med.out.V4$d.avg
V4_PropMed <- med.out.V4$n0

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science4 != "NA")

#Science4
med.fit.S4 <- lm(Shortcut~ Gini_MC + Percapi_MC, data = Shortcuts_science)
out.fit.S4 <- lm(Science4 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts_science)
med.out.S4 <- mediation::mediate(med.fit.S4, out.fit.S4, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.S4)
summary(out.fit.S4)

S4a <- med.fit.S4$coefficients[2]
S4b <- out.fit.S4$coefficients[2]
S4c_TE <- med.out.S4$tau.coef
S4cprime_ADE <- med.out.S4$z0
S4_ACME <- med.out.S4$d.avg
S4_PropMed <- med.out.S4$n0


#8th grade

#Math8
med.fit.M8 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.M8 <- lm(Math8 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.M8 <- mediation::mediate(med.fit.M8, out.fit.M8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.M8)
summary(out.fit.M8)

M8a <- med.fit.M8$coefficients[2]
M8b <- out.fit.M8$coefficients[2]
M8c_TE <- med.out.M8$tau.coef
M8cprime_ADE <- med.out.M8$z0
M8_ACME <- med.out.M8$d.avg
M8_PropMed <- med.out.M8$n0

#Reading8
med.fit.R8 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.R8 <- lm(Reading8 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)

R8a <- med.fit.R8$coefficients[2]
R8b <- out.fit.R8$coefficients[2]
R8c_TE <- med.out.R8$tau.coef
R8cprime_ADE <- med.out.R8$z0
R8_ACME <- med.out.R8$d.avg
R8_PropMed <- med.out.R8$n0

#Vocab8
med.fit.V8 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.V8 <- lm(Vocab8 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.V8 <- mediation::mediate(med.fit.V8, out.fit.V8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.V8)
summary(out.fit.V8)

V8a <- med.fit.V8$coefficients[2]
V8b <- out.fit.V8$coefficients[2]
V8c_TE <- med.out.V8$tau.coef
V8cprime_ADE <- med.out.V8$z0
V8_ACME <- med.out.V8$d.avg
V8_PropMed <- med.out.V8$n0

#Science8

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science8 != "NA")

med.fit.S8 <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts_science)
out.fit.S8 <- lm(Science8 ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts_science)
med.out.S8 <- mediation::mediate(med.fit.S8, out.fit.S8, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.S8)
summary(out.fit.S8)

S8a <- med.fit.S8$coefficients[2]
S8b <- out.fit.S8$coefficients[2]
S8c_TE <- med.out.S8$tau.coef
S8cprime_ADE <- med.out.S8$z0
S8_ACME <- med.out.S8$d.avg
S8_PropMed <- med.out.S8$n0

#Adult attainment

#Less9th
med.fit.9th <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.9th <- lm(Less9th ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.9th <- mediation::mediate(med.fit.9th, out.fit.9th, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.9th)
summary(out.fit.9th)

Less9tha <- med.fit.9th$coefficients[2]
Less9thb <- out.fit.9th$coefficients[2]
Less9thc_TE <- med.out.9th$tau.coef
Less9thcprime_ADE <- med.out.9th$z0
Less9th_ACME <- med.out.9th$d.avg
Less9th_PropMed <- med.out.9th$n0

#HS drop
med.fit.HSdrop <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.HSdrop <- lm(HSnoDiploma ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.HSdrop <- mediation::mediate(med.fit.HSdrop, out.fit.HSdrop, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.HSdrop)
summary(out.fit.HSdrop)

HSdropa <- med.fit.HSdrop$coefficients[2]
HSdropb <- out.fit.HSdrop$coefficients[2]
HSdropc_TE <- med.out.HSdrop$tau.coef
HSdropcprime_ADE <- med.out.HSdrop$z0
HSdrop_ACME <- med.out.HSdrop$d.avg
HSdrop_PropMed <- med.out.HSdrop$n0

#HS
med.fit.HSgrad <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.HSgrad <- lm(Hsgradormore ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.HSgrad <- mediation::mediate(med.fit.HSgrad, out.fit.HSgrad, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.HSgrad)
summary(out.fit.HSgrad)

HSgrada <- med.fit.HSgrad$coefficients[2]
HSgradb <- out.fit.HSgrad$coefficients[2]
HSgradc_TE <- med.out.HSgrad$tau.coef
HSgradcprime_ADE <- med.out.HSgrad$z0
HSgrad_ACME <- med.out.HSgrad$d.avg
HSgrad_PropMed <- med.out.HSgrad$n0

#BA
med.fit.BA <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.BA <- lm(BachDegormore ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.BA <- mediation::mediate(med.fit.BA, out.fit.BA, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.BA)
summary(out.fit.BA)

BAa <- med.fit.BA$coefficients[2]
BAb <- out.fit.BA$coefficients[2]
BAc_TE <- med.out.BA$tau.coef
BAcprime_ADE <- med.out.BA$z0
BA_ACME <- med.out.BA$d.avg
BA_PropMed <- med.out.BA$n0

#MA
med.fit.MA <- lm(Shortcut ~ Gini_MC + Percapi_MC, data = Shortcuts)
out.fit.MA <- lm(GradDeg ~ Shortcut + Gini_MC + Percapi_MC, data = Shortcuts)
med.out.MA <- mediation::mediate(med.fit.MA, out.fit.MA, mediator = "Shortcut", treat = "Gini_MC", boot = FALSE)
summary(med.out.MA)
summary(out.fit.MA)

MAa <- med.fit.MA$coefficients[2]
MAb <- out.fit.MA$coefficients[2]
MAc_TE <- med.out.MA$tau.coef
MAcprime_ADE <- med.out.MA$z0
MA_ACME <- med.out.MA$d.avg
MA_PropMed <- med.out.MA$n0

#Making a tables with the results

#4th
Mediation4_income_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M4a, R4a, S4a, V4a),
  b = c(M4b, R4b, S4b, V4b),
  c = c(M4c_TE, R4c_TE, S4c_TE, V4c_TE),
  cprime = c(M4cprime_ADE, R4cprime_ADE, S4cprime_ADE, V4cprime_ADE),
  ACME = c(M4_ACME, R4_ACME, S4_ACME, V4_ACME),
  pb = c(M4_PropMed, R4_PropMed, S4_PropMed, V4_PropMed))

colnames(Mediation4_income_table) <- c("Fourth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation4_income_table)
#exporting
write.csv(Mediation4_income_table, "Mediation4_income_table.csv", row.names = FALSE)

#8th
Mediation8_income_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M8a, R8a, S8a, V8a),
  b = c(M8b, R8b, S8b, V8b),
  c = c(M8c_TE, R8c_TE, S8c_TE, V8c_TE),
  cprime = c(M8cprime_ADE, R8cprime_ADE, S8cprime_ADE, V8cprime_ADE),
  ACME = c(M8_ACME, R8_ACME, S8_ACME, V8_ACME),
  pb = c(M8_PropMed, R8_PropMed, S8_PropMed, V8_PropMed))

colnames(Mediation8_income_table) <- c("Eighth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation8_income_table)
#exporting
write.csv(Mediation8_income_table, "Mediation8_income_table.csv", row.names = FALSE)

#Attainment
MediationAttain_income_table <- data.frame(
  Outcome = c("Less than 9th", "Less than HS", "HS grad", "BA", "MA"),
  a = c(Less9tha, HSdropa, HSgrada, BAa, MAa),
  b = c(Less9thb, HSdropb, HSgradb, BAb, MAb),
  c = c(Less9thc_TE, HSdropc_TE, HSgradc_TE, BAc_TE, MAc_TE),
  cprime = c(Less9thcprime_ADE, HSdropcprime_ADE, HSgradcprime_ADE, BAcprime_ADE, MAcprime_ADE),
  ACME = c(Less9th_ACME, HSdrop_ACME, HSgrad_ACME, BA_ACME, MA_ACME),
  pb = c(Less9th_PropMed, HSdrop_PropMed, HSgrad_PropMed, BA_PropMed, MA_PropMed))

colnames(MediationAttain_income_table) <- c("Educational Attainment", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(MediationAttain_income_table)
#exporting
write.csv(MediationAttain_income_table, "MediationAttain_income_table.csv", row.names = FALSE)




### Mediation analyses with ratios rather than Gini

#4th grade
#Math4
med.fit.M4 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.M4 <- lm(Math4 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.M4 <- mediation::mediate(med.fit.M4, out.fit.M4, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.M4)
summary(out.fit.M4)

M4a <- med.fit.M4$coefficients[2]
M4b <- out.fit.M4$coefficients[2]
M4c_TE <- med.out.M4$tau.coef
M4cprime_ADE <- med.out.M4$z0
M4_ACME <- med.out.M4$d.avg
M4_PropMed <- med.out.M4$n0

#Reading4
med.fit.R4 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.R4 <- lm(Reading4 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.R4 <- mediation::mediate(med.fit.R4, out.fit.R4, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.R4)
summary(out.fit.R4)

R4a <- med.fit.R4$coefficients[2]
R4b <- out.fit.R4$coefficients[2]
R4c_TE <- med.out.R4$tau.coef
R4cprime_ADE <- med.out.R4$z0
R4_ACME <- med.out.R4$d.avg
R4_PropMed <- med.out.R4$n0

#Vocab4
med.fit.V4 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.V4 <- lm(Vocab4 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.V4 <- mediation::mediate(med.fit.V4, out.fit.V4, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.V4)
summary(out.fit.V4)

V4a <- med.fit.V4$coefficients[2]
V4b <- out.fit.V4$coefficients[2]
V4c_TE <- med.out.V4$tau.coef
V4cprime_ADE <- med.out.V4$z0
V4_ACME <- med.out.V4$d.avg
V4_PropMed <- med.out.V4$n0

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science4 != "NA")

#Science4
med.fit.S4 <- lm(Shortcut~ q90_q50 + q50_q10, data = Shortcuts_science)
out.fit.S4 <- lm(Science4 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts_science)
med.out.S4 <- mediation::mediate(med.fit.S4, out.fit.S4, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.S4)
summary(out.fit.S4)

S4a <- med.fit.S4$coefficients[2]
S4b <- out.fit.S4$coefficients[2]
S4c_TE <- med.out.S4$tau.coef
S4cprime_ADE <- med.out.S4$z0
S4_ACME <- med.out.S4$d.avg
S4_PropMed <- med.out.S4$n0


#8th grade

#Math8
med.fit.M8 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.M8 <- lm(Math8 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.M8 <- mediation::mediate(med.fit.M8, out.fit.M8, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.M8)
summary(out.fit.M8)

M8a <- med.fit.M8$coefficients[2]
M8b <- out.fit.M8$coefficients[2]
M8c_TE <- med.out.M8$tau.coef
M8cprime_ADE <- med.out.M8$z0
M8_ACME <- med.out.M8$d.avg
M8_PropMed <- med.out.M8$n0

#Reading8
med.fit.R8 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.R8 <- lm(Reading8 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)

R8a <- med.fit.R8$coefficients[2]
R8b <- out.fit.R8$coefficients[2]
R8c_TE <- med.out.R8$tau.coef
R8cprime_ADE <- med.out.R8$z0
R8_ACME <- med.out.R8$d.avg
R8_PropMed <- med.out.R8$n0

#Vocab8
med.fit.V8 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.V8 <- lm(Vocab8 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.V8 <- mediation::mediate(med.fit.V8, out.fit.V8, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.V8)
summary(out.fit.V8)

V8a <- med.fit.V8$coefficients[2]
V8b <- out.fit.V8$coefficients[2]
V8c_TE <- med.out.V8$tau.coef
V8cprime_ADE <- med.out.V8$z0
V8_ACME <- med.out.V8$d.avg
V8_PropMed <- med.out.V8$n0

#Science8

##Limiting to just the states with science
Shortcuts_science <- dplyr::filter(Shortcuts, Science8 != "NA")

med.fit.S8 <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts_science)
out.fit.S8 <- lm(Science8 ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts_science)
med.out.S8 <- mediation::mediate(med.fit.S8, out.fit.S8, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.S8)
summary(out.fit.S8)

S8a <- med.fit.S8$coefficients[2]
S8b <- out.fit.S8$coefficients[2]
S8c_TE <- med.out.S8$tau.coef
S8cprime_ADE <- med.out.S8$z0
S8_ACME <- med.out.S8$d.avg
S8_PropMed <- med.out.S8$n0

#Adult attainment

#Less9th
med.fit.9th <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.9th <- lm(Less9th ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.9th <- mediation::mediate(med.fit.9th, out.fit.9th, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.9th)
summary(out.fit.9th)

Less9tha <- med.fit.9th$coefficients[2]
Less9thb <- out.fit.9th$coefficients[2]
Less9thc_TE <- med.out.9th$tau.coef
Less9thcprime_ADE <- med.out.9th$z0
Less9th_ACME <- med.out.9th$d.avg
Less9th_PropMed <- med.out.9th$n0

#HS drop
med.fit.HSdrop <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.HSdrop <- lm(HSnoDiploma ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.HSdrop <- mediation::mediate(med.fit.HSdrop, out.fit.HSdrop, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.HSdrop)
summary(out.fit.HSdrop)

HSdropa <- med.fit.HSdrop$coefficients[2]
HSdropb <- out.fit.HSdrop$coefficients[2]
HSdropc_TE <- med.out.HSdrop$tau.coef
HSdropcprime_ADE <- med.out.HSdrop$z0
HSdrop_ACME <- med.out.HSdrop$d.avg
HSdrop_PropMed <- med.out.HSdrop$n0

#HS
med.fit.HSgrad <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.HSgrad <- lm(Hsgradormore ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.HSgrad <- mediation::mediate(med.fit.HSgrad, out.fit.HSgrad, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.HSgrad)
summary(out.fit.HSgrad)

HSgrada <- med.fit.HSgrad$coefficients[2]
HSgradb <- out.fit.HSgrad$coefficients[2]
HSgradc_TE <- med.out.HSgrad$tau.coef
HSgradcprime_ADE <- med.out.HSgrad$z0
HSgrad_ACME <- med.out.HSgrad$d.avg
HSgrad_PropMed <- med.out.HSgrad$n0

#BA
med.fit.BA <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.BA <- lm(BachDegormore ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.BA <- mediation::mediate(med.fit.BA, out.fit.BA, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.BA)
summary(out.fit.BA)

BAa <- med.fit.BA$coefficients[2]
BAb <- out.fit.BA$coefficients[2]
BAc_TE <- med.out.BA$tau.coef
BAcprime_ADE <- med.out.BA$z0
BA_ACME <- med.out.BA$d.avg
BA_PropMed <- med.out.BA$n0


#MA
med.fit.MA <- lm(Shortcut ~ q90_q50 + q50_q10, data = Shortcuts)
out.fit.MA <- lm(GradDeg ~ Shortcut + q90_q50 + q50_q10, data = Shortcuts)
med.out.MA <- mediation::mediate(med.fit.MA, out.fit.MA, mediator = "Shortcut", treat = "q90_q50", boot = FALSE)
summary(med.out.MA)
summary(out.fit.MA)

MAa <- med.fit.MA$coefficients[2]
MAb <- out.fit.MA$coefficients[2]
MAc_TE <- med.out.MA$tau.coef
MAcprime_ADE <- med.out.MA$z0
MA_ACME <- med.out.MA$d.avg
MA_PropMed <- med.out.MA$n0

#Making a tables with the results

#4th
Mediation4_ratios_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M4a, R4a, S4a, V4a),
  b = c(M4b, R4b, S4b, V4b),
  c = c(M4c_TE, R4c_TE, S4c_TE, V4c_TE),
  cprime = c(M4cprime_ADE, R4cprime_ADE, S4cprime_ADE, V4cprime_ADE),
  ACME = c(M4_ACME, R4_ACME, S4_ACME, V4_ACME),
  pb = c(M4_PropMed, R4_PropMed, S4_PropMed, V4_PropMed))

colnames(Mediation4_ratios_table) <- c("Fourth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation4_ratios_table)
#exporting
write.csv(Mediation4_ratios_table, "Mediation4_ratios_table.csv", row.names = FALSE)

#8th
Mediation8_ratios_table <- data.frame(
  Outcome = c("Math", "Reading", "Science", "Vocab"),
  a = c(M8a, R8a, S8a, V8a),
  b = c(M8b, R8b, S8b, V8b),
  c = c(M8c_TE, R8c_TE, S8c_TE, V8c_TE),
  cprime = c(M8cprime_ADE, R8cprime_ADE, S8cprime_ADE, V8cprime_ADE),
  ACME = c(M8_ACME, R8_ACME, S8_ACME, V8_ACME),
  pb = c(M8_PropMed, R8_PropMed, S8_PropMed, V8_PropMed))

colnames(Mediation8_ratios_table) <- c("Eighth Grade Subject", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(Mediation8_ratios_table)
#exporting
write.csv(Mediation8_ratios_table, "Mediation8_ratios_table.csv", row.names = FALSE)


#Attainment
MediationAttain_ratios_table <- data.frame(
  Outcome = c("Less than 9th", "Less than HS", "HS grad", "BA", "MA"),
  a = c(Less9tha, HSdropa, HSgrada, BAa, MAa),
  b = c(Less9thb, HSdropb, HSgradb, BAb, MAb),
  c = c(Less9thc_TE, HSdropc_TE, HSgradc_TE, BAc_TE, MAc_TE),
  cprime = c(Less9thcprime_ADE, HSdropcprime_ADE, HSgradcprime_ADE, BAcprime_ADE, MAcprime_ADE),
  ACME = c(Less9th_ACME, HSdrop_ACME, HSgrad_ACME, BA_ACME, MA_ACME),
  pb = c(Less9th_PropMed, HSdrop_PropMed, HSgrad_PropMed, BA_PropMed, MA_PropMed))

colnames(MediationAttain_ratios_table) <- c("Educational Attainment", "a", "b", "c", "c'", "ACME", "Prop. Med.")
#View(MediationAttain_ratios_table)
#exporting
write.csv(MediationAttain_ratios_table, "MediationAttain_ratios_table.csv", row.names = FALSE)



####### Checking 4th grade correlations with income

math4_income_cor1 <- cor.test(Shortcuts$Math4, Shortcuts$Percapi)
math4_income_cor1_r <- round(math4_income_cor1$estimate, 2)

vocab4_income_cor1 <- cor.test(Shortcuts$Vocab4, Shortcuts$Percapi)
vocab4_income_cor1_r <- round(vocab4_income_cor1$estimate, 2)

reading4_income_cor1 <- cor.test(Shortcuts$Reading4, Shortcuts$Percapi)
reading4_income_cor1_r <- round(reading4_income_cor1$estimate, 2)

science4_income_cor1 <- cor.test(Shortcuts$Science4, Shortcuts$Percapi)
science4_income_cor1_r <- round(science4_income_cor1$estimate, 2)

### versus gini

math4_gini_cor1 <- cor.test(Shortcuts$Math4, Shortcuts$Gini)
math4_gini_cor1_r <- round(math4_gini_cor1$estimate, 2)

vocab4_gini_cor1 <- cor.test(Shortcuts$Vocab4, Shortcuts$Gini)
vocab4_gini_cor1_r <- round(vocab4_gini_cor1$estimate, 2)

reading4_gini_cor1 <- cor.test(Shortcuts$Reading4, Shortcuts$Gini)
reading4_gini_cor1_r <- round(reading4_gini_cor1$estimate, 2)

science4_gini_cor1 <- cor.test(Shortcuts$Science4, Shortcuts$Gini)
science4_gini_cor1_r <- round(science4_gini_cor1$estimate, 2)

math4_gini_cor1_r
math4_income_cor1_r
vocab4_gini_cor1_r
vocab4_income_cor1_r
reading4_gini_cor1_r
reading4_income_cor1_r
science4_gini_cor1_r
science4_income_cor1_r


###### same but for 8th grade


math8_income_cor1 <- cor.test(Shortcuts$Math8, Shortcuts$Percapi)
math8_income_cor1_r <- round(math8_income_cor1$estimate, 2)

vocab8_income_cor1 <- cor.test(Shortcuts$Vocab8, Shortcuts$Percapi)
vocab8_income_cor1_r <- round(vocab8_income_cor1$estimate, 2)

reading8_income_cor1 <- cor.test(Shortcuts$Reading8, Shortcuts$Percapi)
reading8_income_cor1_r <- round(reading8_income_cor1$estimate, 2)

science8_income_cor1 <- cor.test(Shortcuts$Science8, Shortcuts$Percapi)
science8_income_cor1_r <- round(science8_income_cor1$estimate, 2)

### versus gini

math8_gini_cor1 <- cor.test(Shortcuts$Math8, Shortcuts$Gini)
math8_gini_cor1_r <- round(math8_gini_cor1$estimate, 2)

vocab8_gini_cor1 <- cor.test(Shortcuts$Vocab8, Shortcuts$Gini)
vocab8_gini_cor1_r <- round(vocab8_gini_cor1$estimate, 2)

reading8_gini_cor1 <- cor.test(Shortcuts$Reading8, Shortcuts$Gini)
reading8_gini_cor1_r <- round(reading8_gini_cor1$estimate, 2)

science8_gini_cor1 <- cor.test(Shortcuts$Science8, Shortcuts$Gini)
science8_gini_cor1_r <- round(science8_gini_cor1$estimate, 2)

math8_gini_cor1_r
math8_income_cor1_r
vocab8_gini_cor1_r
vocab8_income_cor1_r
reading8_gini_cor1_r
reading8_income_cor1_r
science8_gini_cor1_r
science8_income_cor1_r


########Analyses

#Checking for interaction bw income and gini====

####### Consistent with the theory we've laid out, there is an interaction between income and inequality for each out of our coutcomes. Given that statistically controlling for a confound requires that there is no interaction between the confound and the other variables, we are conducting mediation without covarying income out.

##Note: pattern is the same when we use Median income

#Ach8
Ach8_interact <- (lm(Achievement8 ~ Gini*Percapi,data = Shortcuts))
summary(Ach8_interact)
#Ach8
Ach4_interact <- (lm(Achievement4 ~ Gini*Percapi,data = Shortcuts))
summary(Ach4_interact)
#Less9th *No interaction
Less9th_interact <- (lm(Less9th ~ Gini*Percapi,data = Shortcuts))
summary(Less9th_interact)
#HSnoDiploma 
HSno_interact <- (lm(HSnoDiploma  ~ Gini*Percapi,data = Shortcuts))
summary(HSno_interact)
#Hsgradormore 
Hsgrad_interact <- (lm(Hsgradormore  ~ Gini*Percapi,data = Shortcuts))
summary(Hsgrad_interact)
#BachDegormore *Nothing sig
BA_interact <- (lm(BachDegormore ~ Gini*Percapi,data = Shortcuts))
summary(BA_interact)
#GradDeg *Nothing sig
MA_interact <- (lm(GradDeg ~ Gini*Percapi,data = Shortcuts))
summary(MA_interact)

#### Additional achievement outcomes

#Vocab4 *Nothing sig
Vocab4_interact <- (lm(Vocab4 ~ Gini*Percapi,data = Shortcuts))
summary(Vocab4_interact)
#Vocab8 
Vocab8_interact <- (lm(Vocab8 ~ Gini*Percapi,data = Shortcuts))
summary(Vocab8_interact)
#Sci4 *Nothing sig
Sci4_interact <- (lm(Science4 ~ Gini*Percapi,data = Shortcuts))
summary(Sci4_interact)
#Sci8
Sci8_interact <- (lm(Science8 ~ Gini*Percapi,data = Shortcuts))
summary(Sci8_interact)
#Math4 *No interaction
Math4_interact <- (lm(Math4 ~ Gini*Percapi,data = Shortcuts))
summary(Math4_interact)
#Math8 
Math8_interact <- (lm(Math8 ~ Gini*Percapi,data = Shortcuts))
summary(Math8_interact)
#Reading4 
Reading4_interact <- (lm(Reading4 ~ Gini*Percapi,data = Shortcuts))
summary(Reading4_interact)
#Reading8 
Reading8_interact <- (lm(Reading8 ~ Gini*Percapi,data = Shortcuts))
summary(Reading8_interact)

## Tables ====


#Creating values to populate tables

#Means
Vocab4M <- Vocab4_alpha$total$mean
Vocab8M <- Vocab8_alpha$total$mean
Science4M <- Science4_alpha$total$mean
Science8M <- Science8_alpha$total$mean
Math4M <- Math4_alpha$total$mean
Math8M <- Math8_alpha$total$mean
Reading4M <- Reading4_alpha$total$mean
Reading8M <- Reading8_alpha$total$mean

#Standard deviations
Vocab4SD <- Vocab4_alpha$total$sd
Vocab8SD <- Vocab8_alpha$total$sd
Science4SD <- Science4_alpha$total$sd
Science8SD <- Science8_alpha$total$sd
Math4SD <- Math4_alpha$total$sd
Math8SD <- Math8_alpha$total$sd
Reading4SD <- Reading4_alpha$total$sd
Reading8SD <- Reading8_alpha$total$sd

#zero order correlations
Vocab4rGini <- cor(Shortcuts$Vocab4, Shortcuts$Gini)
Vocab8rGini <- cor(Shortcuts$Vocab8, Shortcuts$Gini)
Science4rGini <- cor(Shortcuts$Science4, Shortcuts$Gini, use = "pairwise.complete.obs")
Science8rGini <- cor(Shortcuts$Science8, Shortcuts$Gini, use = "pairwise.complete.obs")
Math4rGini <- cor(Shortcuts$Math4, Shortcuts$Gini)
Math8rGini <- cor(Shortcuts$Math8, Shortcuts$Gini)
Reading4rGini <- cor(Shortcuts$Reading4, Shortcuts$Gini)
Reading8rGini <- cor(Shortcuts$Reading8, Shortcuts$Gini)


cor.test(Shortcuts$Vocab8, Shortcuts$Gini)

#betas w controlls (including interaction)
Vocab4bGini <- Vocab4_interact$estimate$Gini_MC
Vocab8bGini <- Vocab8_interact$estimate$Gini_MC
Science4bGini <- Sci4_interact$estimate$Gini_MC
Science8bGini <- Sci8_interact$estimate$Gini_MC
Math4bGini <- Math4_interact$estimate$Gini_MC
Math8bGini <- Math8_interact$estimate$Gini_MC
Reading4bGini <- Reading4_interact$estimate$Gini_MC
Reading8bGini <- Reading8_interact$estimate$Gini_MC

#Pvalues for table
Vocab4p <- Vocab4_interact$statistic$Gini_MC
Vocab8p <- Vocab8_interact$statistic$Gini_MC
Science4p <- Sci4_interact$statistic$Gini_MC
Science8p <- Sci8_interact$statistic$Gini_MC
Math4p <- Math4_interact$statistic$Gini_MC
Math8p <- Math8_interact$statistic$Gini_MC
Reading4p <- Reading4_interact$statistic$Gini_MC
Reading8p <- Reading8_interact$statistic$Gini_MC

#Alphas
Vocab4A <- Vocab4_alpha$total$raw_alpha
Vocab8A <- Vocab8_alpha$total$raw_alpha
Science4A <- Science4_alpha$total$raw_alpha
Science8A <- Science8_alpha$total$raw_alpha
Math4A <- Math4_alpha$total$raw_alpha
Math8A <- Math8_alpha$total$raw_alpha
Reading4A <- Reading4_alpha$total$raw_alpha
Reading8A <- Reading8_alpha$total$raw_alpha


#apa_table(Learn_interact$table, caption = "Learn")

EducationalAchieve8Table <- data.frame(
  EighthGradeSubj = c("Vocabulary", "Science", "Reading", "Math"),
  MeanStateAvg = c(Vocab8M, Science8M, Math8M, Reading8M),
  SDStateAvg = c(Vocab8SD, Science8SD, Math8SD, Reading8SD),
  CronbachsAlpha = c(Vocab8A, Science8A, Math8A, Reading8A),
  rGini = c(Vocab8rGini, Science8rGini,  Reading8rGini, Math8rGini),
  bGiniwcontrols = c(Vocab8bGini, Science8bGini, Math8bGini, Reading8bGini),
  pb = c(Vocab8p, Science8p, Math8p, Reading8p))

colnames(EducationalAchieve8Table) <- c("Eigth Grade Subject", "Mean State Average", "SD State Average", "Cronbach's Alpha", "r Gini", "b Gini with covariates", "pb")

View(EducationalAchieve8Table)

#exporting
write.csv(EducationalAchieve8Table, "EducationalAchieve8Table.csv", row.names = FALSE)

#4th grade


EducationalAchieve4Table <- data.frame(
  EighthGradeSubj = c("Vocabulary", "Science", "Reading", "Math"),
  MeanStateAvg = c(Vocab4M, Science4M, Math4M, Reading4M),
  SDStateAvg = c(Vocab4SD, Science4SD, Math4SD, Reading4SD),
  CronbachsAlpha = c(Vocab4A, Science4A, Math4A, Reading4A),
  rGini = c(Vocab4rGini, Science4rGini,  Reading4rGini, Math4rGini),
  bGiniwcontrols = c(Vocab4bGini, Science4bGini, Math4bGini, Reading4bGini),
  pb = c(Vocab4p, Science4p, Math4p, Reading4p))

colnames(EducationalAchieve4Table) <- c("Fourth Grade Subject", "Mean State Average", "SD State Average", "Cronbach's Alpha", "r Gini", "b Gini with covariates", "pb")

View(EducationalAchieve4Table)

#exporting
write.csv(EducationalAchieve4Table, "EducationalAchieve4Table.csv", row.names = FALSE)


##attinament tables


#Means
Less9thM <- Less9th_alpha$total$mean
HSnoDiplomaM <- HSnoDiploma_alpha$total$mean
HsgradormoreM <- Hsgradormore_alpha$total$mean
BachDegormoreM <- BachDegormore_alpha$total$mean
GradDegM <- GradDeg_alpha$total$mean


#Standard deviations
Less9thSD <- Less9th_alpha$total$sd
HSnoDiplomaSD <- HSnoDiploma_alpha$total$sd
HsgradormoreSD <- Hsgradormore_alpha$total$sd
BachDegormoreSD <- BachDegormore_alpha$total$sd
GradDegSD <- GradDeg_alpha$total$sd

#zero order correlations
Less9thrGini <- cor(Shortcuts$Less9th, Shortcuts$Gini, use = "pairwise.complete.obs")
HSnoDiplomarGini <- cor(Shortcuts$HSnoDiploma, Shortcuts$Gini, use = "pairwise.complete.obs")
HsgradormorerGini <- cor(Shortcuts$Hsgradormore, Shortcuts$Gini, use = "pairwise.complete.obs")
BachDegormorerGini <- cor(Shortcuts$BachDegormore, Shortcuts$Gini, use = "pairwise.complete.obs")
GradDegrGini <- cor(Shortcuts$GradDeg, Shortcuts$Gini, use = "pairwise.complete.obs")



#betas w controlls
Less9thbGini <- Less9th_interact$estimate$Gini_MC
HSnoDiplomabGini <- HSno_interact$estimate$Gini_MC
HsgradormorebGini <- Hsgrad_interact$estimate$Gini_MC
BachDegormorebGini <- BA_interact$estimate$Gini_MC
GradDegbGini <- MA_interact$estimate$Gini_MC

#Pvalues for table
Less9thp <- Less9th_interact$statistic$Gini_MC
HSnoDiplomap <- HSno_interact$statistic$Gini_MC
Hsgradormorep <- Hsgrad_interact$statistic$Gini_MC
BachDegormorep <- BA_interact$statistic$Gini_MC
GradDegp <- MA_interact$statistic$Gini_MC

#Alphas
Less9thA <- Less9th_alpha$total$raw_alpha
HSnoDiplomaA <- HSnoDiploma_alpha$total$raw_alpha
HsgradormoreA <- Hsgradormore_alpha$total$raw_alpha
BachDegormoreA <- BachDegormore_alpha$total$raw_alpha
GradDegA <- GradDeg_alpha$total$raw_alpha


EducationalAttainTable <- data.frame(
  Var = c("Less than 9th grade", "Some high school", "High school diploma", "College degree", "Graduate degree"),
  Mean = c(Less9thM, HSnoDiplomaM, HsgradormoreM, BachDegormoreM, GradDegM),
  SD = c(Less9thSD, HSnoDiplomaSD, HsgradormoreSD, BachDegormoreSD, GradDegSD),
  Alpha = c(Less9thA, HSnoDiplomaA, HsgradormoreA, BachDegormoreA, GradDegA),
  rGini = c(Less9thrGini, HSnoDiplomarGini,  HsgradormorerGini, BachDegormorerGini, GradDegrGini),
  b = c(Less9thbGini, HSnoDiplomabGini, HsgradormorebGini, BachDegormorebGini, GradDegbGini),
  pb = c(Less9thp, HSnoDiplomap, Hsgradormorep, BachDegormorep, GradDegp))

colnames(EducationalAttainTable) <- c("Fourth Grade Subject", "Mean State Average", "SD State Average", "Cronbach's Alpha", "r Gini", "b Gini with covariates", "pb")


View(EducationalAttainTable)

#exporting
write.csv(EducationalAttainTable, "EducationalAttainTable.csv", row.names = FALSE)

mean(Shortcuts$MedIncome)
sd(Shortcuts$MedIncome)

#Mediation analyses (Shortcut)====


#Less9th
med.fit.9th <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.9th <- lm(Less9th ~ Shortcut + Gini, data = Shortcuts)
med.out.9th <- mediation::mediate(med.fit.9th, out.fit.9th, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.9th)
summary(out.fit.9th)

#HS drop
med.fit.HSdrop <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.HSdrop <- lm(HSnoDiploma ~ Shortcut + Gini, data = Shortcuts)
med.out.HSdrop <- mediation::mediate(med.fit.HSdrop, out.fit.HSdrop, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.HSdrop)
summary(out.fit.HSdrop)

#HS
med.fit.HSgrad <- lm(Shortcut ~ Gini , data = Shortcuts)
out.fit.HSgrad <- lm(Hsgradormore ~ Shortcut + Gini, data = Shortcuts)
med.out.HSgrad <- mediation::mediate(med.fit.HSgrad, out.fit.HSgrad, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.HSgrad)
summary(out.fit.HSgrad)



#BA
med.fit.BA <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.BA <- lm(BachDegormore ~ Shortcut + Gini, data = Shortcuts)
med.out.BA <- mediation::mediate(med.fit.BA, out.fit.BA, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.BA)
summary(out.fit.BA)


#MA
med.fit.MA <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.MA <- lm(GradDeg ~ Shortcut + Gini, data = Shortcuts)
med.out.MA <- mediation::mediate(med.fit.MA, out.fit.MA, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.MA)
summary(out.fit.MA)

#Math4
med.fit.R8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.R8 <- lm(Math4 ~ Shortcut + Gini, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)


#Math8
med.fit.M8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.M8 <- lm(Math8 ~ Shortcut + Gini, data = Shortcuts)
med.out.M8 <- mediation::mediate(med.fit.M8, out.fit.M8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.M8)
summary(out.fit.M8)

#Math4
med.fit.R8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.R8 <- lm(Math4 ~ Shortcut + Gini, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)


#Reading8
med.fit.R8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.R8 <- lm(Reading8 ~ Shortcut + Gini, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)

#Reading4
med.fit.R4 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.R4 <- lm(Reading4 ~ Shortcut + Gini, data = Shortcuts)
med.out.R4 <- mediation::mediate(med.fit.R4, out.fit.R4, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.R4)
summary(out.fit.R4)

#Achievement8
med.fit.8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.8 <- lm(Achievement8 ~ Shortcut + Gini, data = Shortcuts)
med.out.8 <- mediation::mediate(med.fit.8, out.fit.8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.8)


#Additional variables

#Vocab4
med.fit.V4 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.V4 <- lm(Vocab4 ~ Shortcut + Gini, data = Shortcuts)
med.out.V4 <- mediation::mediate(med.fit.V4, out.fit.V4, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.V4)
summary(out.fit.V4)

#Vocab8
med.fit.V8 <- lm(Shortcut ~ Gini, data = Shortcuts)
out.fit.V8 <- lm(Vocab8 ~ Shortcut + Gini, data = Shortcuts)
med.out.V8 <- mediation::mediate(med.fit.V8, out.fit.V8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.V8)
summary(out.fit.V8)

##Limiting to just the states with science

Shortcuts_science <- dplyr::filter(Shortcuts, Science4 != "NA")
View(Shortcuts_science)                                     

#Science8
med.fit.S8 <- lm(Shortcut ~ Gini, data = Shortcuts_science)
out.fit.S8 <- lm(Science8 ~ Shortcut + Gini, data = Shortcuts_science)
med.out.S8 <- mediation::mediate(med.fit.S8, out.fit.S8, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.S8)
summary(out.fit.S8)
summary(lm(Shortcut~Gini, data = Shortcuts_science))

#Science4
med.fit.S4 <- lm(Shortcut~ Gini, data = Shortcuts_science)
out.fit.S4 <- lm(Science4 ~ Shortcut + Gini, data = Shortcuts_science)
med.out.S4 <- mediation::mediate(med.fit.S4, out.fit.S4, mediator = "Shortcut", treat = "Gini", boot = FALSE)
summary(med.out.S4)
summary(out.fit.S4)

#Mediation analyses (Learning)====


##Limiting to just the states with science

Shortcuts_science <- dplyr::filter(Shortcuts, Science4 != "NA")
View(Shortcuts_science)                                     

#Science8
med.fit.S8 <- lm(Learn ~ Gini, data = Shortcuts_science)
out.fit.S8 <- lm(Science8 ~ Learn + Gini, data = Shortcuts_science)
med.out.S8 <- mediation::mediate(med.fit.S8, out.fit.S8, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.S8)
summary(out.fit.S8)
summary(lm(Learn~Gini, data = Shortcuts_science))


#Vocab8
med.fit.V8 <- lm(Learn ~ Gini, data = Shortcuts)
out.fit.V8 <- lm(Vocab8 ~ Learn + Gini, data = Shortcuts)
med.out.V8 <- mediation::mediate(med.fit.V8, out.fit.V8, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.V8)
summary(out.fit.V8)
summary(lm(Learn~Gini, data = Shortcuts))


#Reading8
med.fit.R8 <- lm(Learn ~ Gini, data = Shortcuts)
out.fit.R8 <- lm(Reading8 ~ Learn + Gini, data = Shortcuts)
med.out.R8 <- mediation::mediate(med.fit.R8, out.fit.R8, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.R8)
summary(out.fit.R8)

#Math8
med.fit.M8 <- lm(Learn ~ Gini, data = Shortcuts)
out.fit.M8 <- lm(Math8 ~ Learn + Gini, data = Shortcuts)
med.out.M8 <- mediation::mediate(med.fit.M8, out.fit.M8, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.M8)
summary(out.fit.M8)

#Learning words

#Less9th
med.fit.9thLearn <- lm(Learn ~ Gini, data = Shortcuts)
out.fit.9thLearn <- lm(Less9th ~ Learn+ Gini, data = Shortcuts)
med.out.9thLearn <- mediation::mediate(med.fit.9thLearn, out.fit.9thLearn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.9thLearn)

#HS
med.fit.HSgradLearn <- lm(Learn~ Gini , data = Shortcuts)
out.fit.HSgradLearn <- lm(Hsgradormore ~ Learn+ Gini, data = Shortcuts)
med.out.HSgradLearn <- mediation::mediate(med.fit.HSgradLearn, out.fit.HSgradLearn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.HSgradLearn)

#BA
med.fit.BALearn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.BALearn <- lm(BachDegormore ~ Learn+ Gini, data = Shortcuts)
med.out.BALearn <- mediation::mediate(med.fit.BALearn, out.fit.BALearn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.BALearn)


#MA
med.fit.MALearn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.MALearn <- lm(GradDeg ~ Learn+ Gini, data = Shortcuts)
med.out.MALearn <- mediation::mediate(med.fit.MALearn, out.fit.MALearn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.MALearn)

#Math4
med.fit.R8Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.R8Learn <- lm(Math4 ~ Learn+ Gini, data = Shortcuts)
med.out.R8Learn <- mediation::mediate(med.fit.R8Learn, out.fit.R8Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.R8Learn)

#Math8
med.fit.M8Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.M8Learn <- lm(Math4 ~ Learn+ Gini, data = Shortcuts)
med.out.M8Learn <- mediation::mediate(med.fit.M8Learn, out.fit.M8Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.M8Learn)


#Achievement4
med.fit.4Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.4Learn <- lm(Achievement4 ~ Learn+ Gini, data = Shortcuts)
med.out.4Learn <- mediation::mediate(med.fit.4Learn, out.fit.4Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.4Learn)

#Achievement8
med.fit.8Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.8Learn <- lm(Achievement8 ~ Learn+ Gini, data = Shortcuts)
med.out.8Learn <- mediation::mediate(med.fit.8Learn, out.fit.8Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.8Learn)

#Additional variables

#Vocab4
med.fit.V4Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.V4Learn <- lm(Vocab4 ~ Learn+ Gini, data = Shortcuts)
med.out.V4Learn <- mediation::mediate(med.fit.V4Learn, out.fit.V4Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.V4Learn)

#Vocab8
med.fit.V8Learn <- lm(Learn~ Gini, data = Shortcuts)
out.fit.V8Learn <- lm(Vocab8 ~ Learn+ Gini, data = Shortcuts)
med.out.V8Learn <- mediation::mediate(med.fit.V8Learn, out.fit.V8Learn, mediator = "Learn", treat = "Gini", boot = FALSE)
summary(med.out.V8Learn)


#correlations
cor(Shortcuts[3, 5:21], use = use="pairwise.complete.obs", method = "kendall")

cor.test(Shortcuts$Gini, Shortcuts$MedIncome)

corsdata <- Shortcuts[,3:21]
corsdata <- dplyr::select(corsdata, - Science4, - Science8, -Vocab4, -Vocab8, - Math4, -Math8, -Reading4, -Reading8)
res <- cor(corsdata)
round(res, 2)

#standardizing the data so they can all be on one plot
Shortcuts_std <- Shortcuts
Shortcuts_std[, 2:22] <- scale(Shortcuts_std[, 2:22])

#reverse coding dropout rates so that they are in the same direction (i.e., high = good)
Shortcuts_std$Less9thr <- (Shortcuts_std$Less9th*(-1))
Shortcuts_std$HSnoDiplomar <- (Shortcuts_std$HSnoDiploma*(-1))

alloutcomes <- ggplot(Shortcuts_std, aes(Gini)) + 
  geom_point(aes(y = Math8, colour = "8th grade Math")) + 
  geom_point(aes(y = Reading8, colour = "8th grade Reading"))+
  geom_point(aes(y = Science8, colour = "8th grade Science"))+
  geom_point(aes(y = Vocab8, colour = "8th grade Vocab"))+
  geom_point(aes(y = Math4, colour = "4th grade Math")) + 
  geom_point(aes(y = Reading4, colour = "4th grade Reading"))+
  geom_point(aes(y = Science4, colour = "4th grade Science"))+
  geom_point(aes(y = Vocab4, colour = "4th grade Vocab"))+
  geom_point(aes(y = Less9thr, colour = "Dropout before 9th (r)"))+
  geom_point(aes(y = HSnoDiplomar, colour = "Dropout before HS diploma (r)"))+
  geom_point(aes(y = Hsgradormore, colour = "HS diploma or more"))+
  geom_point(aes(y = BachDegormore, colour = "BA/BS or more"))+
  geom_point(aes(y = GradDeg, colour = "Graduate degree"))+
  xlab("Gini Index of Inequality") +
  ylab ("Standardized Educational Outcome")

alloutcomes + theme_apa() +
  theme(text = element_text(family = "serif", size = 12))+
  ggtitle("Unequal US States Have Worse Educational Outcomes")+ labs(colour = "Variable")


corsdata <- Shortcuts[,2:21]
corsdata <- dplyr::select(corsdata, - Science4, - Science8, -Vocab4, -Vocab8, - Math4, -Math8, -Reading4, -Reading8)
res <- cor(corsdata, use = "pairwise.complete.obs")
cortable <- round(res, 2)

xtable(cortable)

apa_table.word(x = cortable, caption = "Correlation Matrix of All State-Level Variables", landscape = TRUE)+
  theme(text = element_text(family = "serif", size = 12))

ggplot(Shortcuts_std, mapping = aes(x = Gini, y = Shortcut))+
  geom_text_repel(aes(label=StateAB, family = "serif"), box.padding = .04, point.padding = .098)+
  labs(x = "Gini Index of Inequality", y = "Searches for Academic Shortcuts")+
  theme_apa()  +
  theme(text = element_text(family = "serif", size = 12))+
  ggtitle("People Search More for Academic Shortcuts in Unequal US States")

ggplot(data = Shortcuts, mapping = aes(x = Percapi, y = Gini))+
  geom_text_repel(aes(label=StateAB, family = "serif"), box.padding = .04, point.padding = .01)+
  geom_abline()+
  labs(x = "Per Capita Household Income", y = "Gini Index of Inequality")+
  theme_apa()  +
  theme(text = element_text(family = "serif", size = 12))+
  ggtitle("Inequality in US States is Independent of Per Capita Income")

ggplot(data = Shortcuts, mapping = aes(x = MedIncome, y = Gini))+
  geom_text_repel(aes(label=StateAB, family = "serif"), box.padding = .04, point.padding = .01)+
  geom_abline()+
  labs(x = "Median Household Income", y = "Gini Index of Inequality")+
  theme_apa()  +
  theme(text = element_text(family = "serif", size = 12))+
  ggtitle("Inequality in US States is Independent of Median Income")

#maps!
data <- data.frame(murder = USArrests$Murder,
                   state = tolower(rownames(USArrests)))
map <- map_data("state")
k <- ggplot(data, aes(fill = murder))
k + geom_map(aes(map_id = state), map = map) + expand_limits(x = map$long, y = map$lat), map_id, alpha, color, fill, linetype, size)


