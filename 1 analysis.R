# SHARE C19 (w8, summer 2020) and personality (W7)
#libraries
library(tidyverse)
library(haven) #import data
library(car) #recode
library(ggpubr) # combine plots
library(summarytools) # descriptives
library(jtools) # summarize results
library(sjPlot) # summarize results
library(lme4) # multilevel models
library(viridis) # plots

# read and clean data ----
# data from w7 and w8 already merged by id and saved as "c19perso.dta"
s.d <- read_dta("c19perso.dta") 

clean.d <- s.d %>% zap_labels() %>% 
                    dplyr::select(
                           country,
                           gender,
                           age2020,
                           isced1997_r, # education
                           bfi10_extra, # BFI
                           bfi10_agree, # BFI
                           bfi10_consc, # BFI
                           bfi10_neuro, # BFI
                           bfi10_open,  # BFI
                           cah012_,     # wore a mask
                           cah011_3,    # met 5+ friends
                           cah011_4,    # met other family
                           cah013_,     # kept a distance
                           cah014_,    # washed hands more frequently 
                           cah015_) %>%     # used disinfectant
                           mutate(mask=case_when(cah012_==4 ~ 0, # never wears a mask in public
                                                   cah012_==3 ~ 1, 
                                                   cah012_==2 ~ 1,
                                                   cah012_==1 ~ 1)) %>% 
                           mutate(seesf=case_when(cah011_3>2 | cah011_4>2 ~ 0, # sees friends or family as often as usual
                                                   T ~ 1))  %>%                # missing values are dropped later!
                           mutate(dist=case_when(cah013_==4 ~ 0, # never keeps distance
                                                   cah013_==3 ~ 1, 
                                                   cah013_==2 ~ 1,
                                                   cah013_==1 ~ 1)) %>%
                          mutate(allbeh=case_when(mask==1 & seesf==1 & dist==1 & cah014_==1 & cah015_==1 ~1,
                                                  T ~0)) %>% 
                          mutate(ze=scale(bfi10_extra)) %>% 
                          mutate(za=scale(bfi10_agree )) %>% 
                          mutate(zc=scale(bfi10_consc)) %>% 
                          mutate(zn=scale(bfi10_neuro)) %>% 
                          mutate(zo=scale(bfi10_open)) %>% 
                          mutate(zage=scale(age2020))  %>% 
                          mutate(gender=case_when(gender==1~0,
                                                  gender==2~1))

# recode missing values (negative values into NA)
clean.d$cah012_ <-  car::recode(clean.d$cah012_, "-9=NA; -2=NA; -1=NA") 
clean.d$cah011_3 <- car::recode(clean.d$cah011_3, "-9=NA; -2=NA; -1=NA; 5=NA") 
clean.d$cah011_4 <- car::recode(clean.d$cah011_4, "-9=NA; -2=NA; -1=NA; 5=NA") 
clean.d$cah013_ <-  car::recode(clean.d$cah013_, "-9=NA; -2=NA; -1=NA")
clean.d$cah014_ <-  car::recode(clean.d$cah014_, "-2=NA; -1=NA; 5=0")
clean.d$cah015_ <-  car::recode(clean.d$cah015_, "-2=NA; -1=NA; 5=0")

# alternative educational attainment variable, continuous
#clean.d$contisced1997 <-  car::recode(clean.d$isced1997_r, "-2=NA; -1=NA; 95=NA; 97=NA") 
# recode educational attainment (used in the analysis)
clean.d$isced1997_r <-  car::recode(clean.d$isced1997_r, "-2=NA; -1=NA; 95=NA; 97=NA; 0=1; 2=1; 3=2; 4=2;5=3;6=3") 
# drop if missing on any var
clean.d <- clean.d %>% filter_at(vars(country, gender, age2020, isced1997_r,
                                        bfi10_extra, bfi10_agree, bfi10_consc, bfi10_neuro, bfi10_open,
                                        cah012_, cah011_3, cah011_4, cah013_ , cah014_, cah015_), 
                                        all_vars(!is.na(.)))
# renaming 
clean.d <- rename(clean.d, wash = cah014_, disin = cah015_)
# descriptive statistics ----
view(dfSummary(clean.d))
# few respondents with age under 50 (error?), filter them out
clean.d <- clean.d %>% filter(age2020>49) 

#*********************************************************************************************************************
#  multilevel models -----
#*********************************************************************************************************************
# outcome: WEARS A MASK ----

# unconditional model, no predictors, to calculate ICC
# random-intercept for country: mask wearing can differ between countries
M0m <-glmer(mask~ ( 1 | country), data=clean.d, family = "binomial")
summ(M0m)
# add personality trait and covariates
mem <-glmer(mask~  ze+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mam <-glmer(mask~  za+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mcm <-glmer(mask~  zc+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mnm <-glmer(mask~  zn+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mom <-glmer(mask~  zo+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
#results in a table
tab_model(mom, mcm, mem, mam, mnm, show.intercept=F)

# only personality and country
mem <-glmer(mask~  ze + (1 | country), data = clean.d, family = "binomial")
mam <-glmer(mask~  za + (1 | country), data = clean.d, family = "binomial")
mcm <-glmer(mask~  zc + (1 | country), data = clean.d, family = "binomial")
mnm <-glmer(mask~  zn + (1 | country), data = clean.d, family = "binomial")
mom <-glmer(mask~  zo + (1 | country), data = clean.d, family = "binomial")

# plot results for wearing a mask 
fig1 <- plot_coefs(mom, mcm, mem, mam, mnm,
           coefs = c("zo", "zc", "ze", "za", "zn"),
           exp=T)
fig1 <- fig1+
  scale_x_log10() +
  scale_color_manual(values=c("black","grey60","grey60", "black","black")) +
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
  theme(legend.title = element_blank()) +
  guides(shape =guide_legend(reverse = TRUE), color=F) +
  ggtitle("Wears a mask") +
  xlab("OR") + xlim(0.75, 1.3) +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 11, hjust = 0.5)) 

# *******************************************************
# outcome: SEES FRIENDS OR OTHER FAMILY less often ----

# unconditional model, no predictors, to calculate ICC
M0s <-glmer(seesf~ ( 1 | country), data=clean.d, family = "binomial")
summ(M0s)
# add personality trait and covariates
memf <-glmer(seesf~ ze + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mamf <-glmer(seesf~ za + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mcmf <-glmer(seesf~ zc + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mnmf <-glmer(seesf~ zn + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
momf <-glmer(seesf~ zo + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
#results in a table
tab_model(momf, mcmf, memf, mamf, mnmf, show.intercept=F)
# only personality and country
memf <-glmer(seesf~ ze + (1 | country), data = clean.d, family = "binomial")
mamf <-glmer(seesf~ za + (1 | country), data = clean.d, family = "binomial")
mcmf <-glmer(seesf~ zc + (1 | country), data = clean.d, family = "binomial")
mnmf <-glmer(seesf~ zn + (1 | country), data = clean.d, family = "binomial")
momf <-glmer(seesf~ zo + (1 | country), data = clean.d, family = "binomial")

# plot results for limiting in-person contacts
fig2 <- plot_coefs(momf, mcmf, memf, mamf, mnmf,
                   coefs = c("zo", "zc", "ze", "za", "zn"),
                   exp=T)
fig2 <-fig2 + 
  scale_x_log10() +
  scale_color_manual(values=c("black","black","grey60", "grey60","grey60")) +
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
  theme(legend.title = element_blank()) +
  guides(shape =guide_legend(reverse = TRUE), color=F) +
  ggtitle("Limits in-person contacts") +
  xlab("OR") + xlim(0.75, 1.3) +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 11, hjust = 0.5)) 

# ***************************************************
# outcome: KEEPS DISTANCE ----
M0d <-glmer(dist~ ( 1 | country), data=clean.d, family = "binomial")
summ(M0d)
# add personality trait and covariates
memd <-glmer(dist~ ze + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mamd <-glmer(dist~ za + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mcmd <-glmer(dist~ zc + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mnmd <-glmer(dist~ zn + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
momd <-glmer(dist~ zo + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
#results in a table
tab_model(momd, mcmd, memd, mamd, mnmd, show.intercept=F)

# only personality and country
memd <-glmer(dist~ ze + (1 | country), data = clean.d, family = "binomial")
mamd <-glmer(dist~ za + (1 | country), data = clean.d, family = "binomial")
mcmd <-glmer(dist~ zc + (1 | country), data = clean.d, family = "binomial")
mnmd <-glmer(dist~ zn + (1 | country), data = clean.d, family = "binomial")
momd <-glmer(dist~ zo + (1 | country), data = clean.d, family = "binomial")

fig3 <- plot_coefs(momd, mcmd, memd, mamd, mnmd,
                   coefs = c("zo", "zc", "ze", "za", "zn"),
                   exp=T)
fig3 <-fig3 + 
       scale_x_log10() +
       scale_shape_manual(values=c(21,22,23,24,25)) +
       scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
       theme(legend.title = element_blank()) +
       guides(shape =guide_legend(reverse = TRUE), color=F) +
       ggtitle("Keeps distance") +
       xlab("OR") + xlim(0.75, 1.3) +
       ylab("") +
       theme_bw() +
       theme(plot.title = element_text(size = 11, hjust = 0.5))  

# ***************************************************
# outcome: WASHES HANDS ----
M0w <-glmer(wash~ ( 1 | country), data=clean.d, family = "binomial")
summ(M0w)
# add personality trait and covariates
memw <-glmer(wash~ ze + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mamw <-glmer(wash~ za + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mcmw <-glmer(wash~ zc + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mnmw <-glmer(wash~ zn + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
momw <-glmer(wash~ zo + zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
#results in a table
tab_model(momw, mcmw, memw, mamw, mnmw, show.intercept=F)

# only personality and country
memw <-glmer(wash~ ze + (1 | country), data = clean.d, family = "binomial")
mamw <-glmer(wash~ za + (1 | country), data = clean.d, family = "binomial")
mcmw <-glmer(wash~ zc + (1 | country), data = clean.d, family = "binomial")
mnmw <-glmer(wash~ zn + (1 | country), data = clean.d, family = "binomial")
momw <-glmer(wash~ zo + (1 | country), data = clean.d, family = "binomial")

summ(momw, exp=T, confint=T, digits=3)
fig4 <- plot_coefs(momw, mcmw, memw, mamw, mnmw,
                   coefs = c("zo", "zc", "ze", "za", "zn"),
                   exp=T)
fig4 <-fig4 +
  scale_x_log10() +
  scale_color_manual(values=c("black","grey60","grey60","black","grey60")) 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
  theme(legend.title = element_blank()) +
  guides(shape =guide_legend(reverse = TRUE), color=F) +
  ggtitle("Washes hands") +
  xlab("OR") + xlim(0.75, 1.3) +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 11, hjust = 0.5))  
  
# ***************************************************
# outcome: USES DISINFECTANT ----
M0di <-glmer(disin~ ( 1 | country), data=clean.d, family = "binomial")
summ(M0di)
# add personality trait and covariates
memdi <-glmer(disin~ ze+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mamdi <-glmer(disin~ za+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mcmdi <-glmer(disin~ zc+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
mnmdi <-glmer(disin~ zn+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
momdi <-glmer(disin~ zo+ zage+ gender+ factor(isced1997_r) + (1 | country), data = clean.d, family = "binomial")
#results in a table
tab_model(momdi, mcmdi, memdi, mamdi, mnmdi, show.intercept=F)

# only personality and country
memdi <-glmer(disin~ ze + (1 | country), data = clean.d, family = "binomial")
mamdi <-glmer(disin~ za + (1 | country), data = clean.d, family = "binomial")
mcmdi <-glmer(disin~ zc + (1 | country), data = clean.d, family = "binomial")
mnmdi <-glmer(disin~ zn + (1 | country), data = clean.d, family = "binomial")
momdi <-glmer(disin~ zo + (1 | country), data = clean.d, family = "binomial")

fig5 <- plot_coefs(momdi, mcmdi, memdi, mamdi, mnmdi,
                   coefs = c("zo", "zc", "ze", "za", "zn"),
                   exp=T)
fig5 <-fig5 + 
  scale_x_log10() +
  scale_color_manual(values=c("black","grey60","black","black","black")) +
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
  theme(legend.title = element_blank()) +
  guides(shape =guide_legend(reverse = TRUE), color=F) +
  ggtitle("Uses disinfectant") +
  xlab("OR") + xlim(0.75, 1.3) +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 11, hjust = 0.5))  

# **************************************************************************
# combined figures (FIGURE 1)----
ggarrange(fig1, fig2, fig3, fig4, fig5, ncol=5, legend= "none")

#**************************************************************************
# PROBABILITY PLOTS ----
# wears a mask,  predicted probabilities
zeep <- as.data.frame(get_model_data(mem, type="pred", terms= "ze"))
zaap <- as.data.frame(get_model_data(mam, type="pred", terms= "za"))
zccp <- as.data.frame(get_model_data(mcm, type="pred", terms= "zc"))
znnp <- as.data.frame(get_model_data(mnm, type="pred", terms= "zn"))
zoop <- as.data.frame(get_model_data(mom, type="pred", terms= "zo"))
# variable for personality traits
zeep$p <- c("E")
zaap$p <- c("A")
zccp$p <- c("C")
znnp$p <- c("N")
zoop$p <- c("O")

# limits in-person contacts, predicted probabilities
zeepf <- as.data.frame(get_model_data(memf, type="pred", terms= "ze"))
zaapf <- as.data.frame(get_model_data(mamf, type="pred", terms= "za"))
zccpf <- as.data.frame(get_model_data(mcmf, type="pred", terms= "zc"))
znnpf <- as.data.frame(get_model_data(mnmf, type="pred", terms= "zn"))
zoopf <- as.data.frame(get_model_data(momf, type="pred", terms= "zo"))
# variable for personality traits
zeepf$p <- c("E")
zaapf$p <- c("A")
zccpf$p <- c("C")
znnpf$p <- c("N")
zoopf$p <- c("O")

# keeps distance, predicted probabilities
zeepd <- as.data.frame(get_model_data(memd, type="pred", terms= "ze"))
zaapd <- as.data.frame(get_model_data(mamd, type="pred", terms= "za"))
zccpd <- as.data.frame(get_model_data(mcmd, type="pred", terms= "zc"))
znnpd <- as.data.frame(get_model_data(mnmd, type="pred", terms= "zn"))
zoopd <- as.data.frame(get_model_data(momd, type="pred", terms= "zo"))
# variable for personality traits
zeepd$p <- c("E")
zaapd$p <- c("A")
zccpd$p <- c("C")
znnpd$p <- c("N")
zoopd$p <- c("O")

# washes hands, predicted probabilities
zeepw <- as.data.frame(get_model_data(memw, type="pred", terms= "ze"))
zaapw <- as.data.frame(get_model_data(mamw, type="pred", terms= "za"))
zccpw <- as.data.frame(get_model_data(mcmw, type="pred", terms= "zc"))
znnpw <- as.data.frame(get_model_data(mnmw, type="pred", terms= "zn"))
zoopw <- as.data.frame(get_model_data(momw, type="pred", terms= "zo"))
# variable for personality traits
zeepw$p <- c("E")
zaapw$p <- c("A")
zccpw$p <- c("C")
znnpw$p <- c("N")
zoopw$p <- c("O")

# uses disinfectant, predicted probabilities
zeepdi <- as.data.frame(get_model_data(memdi, type="pred", terms= "ze"))
zaapdi <- as.data.frame(get_model_data(mamdi, type="pred", terms= "za"))
zccpdi <- as.data.frame(get_model_data(mcmdi, type="pred", terms= "zc"))
znnpdi <- as.data.frame(get_model_data(mnmdi, type="pred", terms= "zn"))
zoopdi <- as.data.frame(get_model_data(momdi, type="pred", terms= "zo"))
# variable for personality traits
zeepdi$p <- c("E")
zaapdi$p <- c("A")
zccpdi$p <- c("C")
znnpdi$p <- c("N")
zoopdi$p <- c("O")

##############################################################################
# combine data for all 5 traits, mask
allz <-rbind(zeep, zaap, zccp, znnp, zoop)
allz$p <- factor(allz$p, levels=c("O", "C", "E", "A", "N"))
# combine data for all 5 traits, limits in-person contacts
allzf <-rbind(zeepf, zaapf, zccpf, znnpf, zoopf)
allzf$p <- factor(allzf$p, levels=c("O", "C", "E", "A", "N"))
# combine data for all 5 traits, keeps distance
allzd <-rbind(zeepd, zaapd, zccpd, znnpd, zoopd)
allzd$p <- factor(allzd$p, levels=c("O", "C", "E", "A", "N"))
# combine data for all 5 traits, washes hands
allzw <-rbind(zeepw, zaapw, zccpw, znnpw, zoopw)
allzw$p <- factor(allzw$p, levels=c("O", "C", "E", "A", "N"))
# combine data for all 5 traits, uses disinfectant
allzdi <-rbind(zeepdi, zaapdi, zccpdi, znnpdi, zoopdi)
allzdi$p <- factor(allzdi$p, levels=c("O", "C", "E", "A", "N"))

# figure with probabilities ranging from 0 to 100%
fig2a <- ggplot(allzd, aes(x=x, y=predicted, color=p)) +
  theme_bw() +
  xlim(-2,2) + 
  geom_line(data=allzd, aes(x=allzd$x), size=1.2) + # keeps distance
  geom_line(data=allz,aes(x=allz$x), size=1.2) +    # wears a mask
  geom_line(data=allzf,aes(x=allzf$x), size=1.2) +  # limits in-person contacts
  geom_line(data=allzw,aes(x=allzw$x), size=1.2) +  # washes hands
  geom_line(data=allzdi,aes(x=allzdi$x), size=1.2) +  # uses disinfectant
  scale_y_continuous(labels=c("0%", "20%", "40%","60%", "80%", "100%"), limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme(legend.title = element_blank(), legend.position = "none",
         panel.grid.minor = element_blank()) +
  scale_colour_viridis_d() +
  ylab("Predicted probability") + xlab("Z-score") +
  annotate("rect", xmin = -2, xmax = 2, ymin = 0.75, ymax = 1,
           alpha = .1)

# zoom from 80 to 100%
fig2b <- ggplot(allzd, aes(x=x, y=predicted, color=p)) +
  theme_bw() +
  xlim(-2,2) +
  geom_line(data=allzd, aes(x=allzd$x), size=1.2) +
  geom_line(data=allz,aes(x=allz$x), size=1.2) +
  geom_line(data=allzf,aes(x=allzf$x), size=1.2) +
  geom_line(data=allzw,aes(x=allzw$x), size=1.2) + 
  geom_line(data=allzdi,aes(x=allzdi$x), size=1.2) +  
  scale_y_continuous(labels=c("80%", "100%"), limits=c(0.7,1),
                     breaks=c(0.8,1)) +
  theme(legend.title = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_colour_viridis_d() +
  ylab(" ") + xlab("Z-score") +
  annotate("text", x=0, y=0.95, label= "Wears a mask") +
  annotate("text", x=0, y=0.83, label= "Limits in-person contacts") +
  annotate("text", x=0, y=1, label= "Keeps distance") +
  annotate("text", x=0, y=0.915, label= "Washes hands") +
  annotate("text", x=0, y=0.875, label= "Uses disinfectant") 
fig2b
# *******************************************
# FIGURE 2 ----
ggarrange(fig2a, fig2b, widths = c(0.6,1))

#***************************************************************************************
# SUPPLEMENTARY ANALYSIS: practicing all five precautionary behaviors ----

memall <-glmer(allbeh ~ ze + zage+ gender+ contisced1997 + (1 | country), data = clean.d, family = "binomial")
mamall <-glmer(allbeh ~ za + zage+ gender+ contisced1997 + (1 | country), data = clean.d, family = "binomial")
mcmall <-glmer(allbeh ~ zc + zage+ gender+ contisced1997 + (1 | country), data = clean.d, family = "binomial")
mnmall <-glmer(allbeh ~ zn + zage+ gender+ contisced1997 + (1 | country), data = clean.d, family = "binomial")
momall <-glmer(allbeh ~ zo + zage+ gender+ contisced1997 + (1 | country), data = clean.d, family = "binomial")
# RESULTS as a table ----
tab_model(momall, mcmall, memall, mamall, mnmall, show.intercept=F)

# supplementary FIGURE 1----
figall <- plot_coefs(momall, mcmall, memall, mamall, mnmall,
                   coefs = c("zo", "zc", "ze", "za", "zn"),
                   exp=T)

figall <-figall + 
  scale_x_log10() +
  scale_color_manual(values=c("black","grey60","grey60","black","black")) +
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_y_discrete(labels= c("N", "A", "E", "C", "O")) +
  guides(shape =guide_legend(reverse = TRUE), color=F) +
  ggtitle("Wears a mask, limits in-person contacts,\nkeeps distance to others, washes hands,\nand uses disinfectant") +
  xlab("OR") + xlim(0.75, 1.3) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none") 