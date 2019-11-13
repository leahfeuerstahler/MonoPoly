library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(rpf)

### CFF - results I wonder if we should report have three asterisks: ***
#### Complete Data Results - Read in Data ####

res <- read_table2(file="Results/MissingResults.txt")

## fix up quote problems

colnames(res) <- gsub("\"", "", colnames(res))
res <- res %>% 
  mutate(model = factor(gsub("\"", "", model)),
         select = factor(gsub("\"", "", select)),
         thetacond = factor(gsub("\"", "", thetacond)),
         N = factor(N),
         bad = factor(bad))

## order thetacond factor in an intuitive way
res <- res %>%
  mutate(thetacond = fct_reorder(thetacond, truetheta),
         thetacond = fct_relevel(thetacond, "normal01", after = 0))

## read in true and estimated item parameters

# true item parameters
true_n200N5000bad30 <- read_table2(file = "Results/trueitems_n200N5000bad30")
true_n200N5000bad70 <- read_table2(file = "Results/trueitems_n200N5000bad70")
true_n200N10000bad30 <- read_table2(file = "Results/trueitems_n200N10000bad30")
true_n200N10000bad70 <- read_table2(file = "Results/trueitems_n200N10000bad70")
colnames(true_n200N5000bad30) <- gsub("\"", "", colnames(true_n200N5000bad30))
colnames(true_n200N5000bad70) <- gsub("\"", "", colnames(true_n200N5000bad70))
colnames(true_n200N10000bad30) <- gsub("\"", "", colnames(true_n200N10000bad30))
colnames(true_n200N10000bad70) <- gsub("\"", "", colnames(true_n200N10000bad70))

# add "bad" indicator
true_n200N5000bad30 <- true_n200N5000bad30 %>% mutate(bad = !is.na(p2))
true_n200N5000bad70 <- true_n200N5000bad70 %>% mutate(bad = !is.na(p2))
true_n200N10000bad30 <- true_n200N10000bad30 %>% mutate(bad = !is.na(p2))
true_n200N10000bad70 <- true_n200N10000bad70 %>% mutate(bad = !is.na(p2))

# KS - not present in missing data analysis
#ks_n200N5000bad30 <- read_table2(file = "Results/ksresults_n200N5000bad30")
#ks_n200N5000bad70 <- read_table2(file = "Results/ksresults_n200N5000bad70")
#ks_n200N10000bad30 <- read_table2(file = "Results/ksresults_n200N10000bad30")
#ks_n200N10000bad70 <- read_table2(file = "Results/ksresults_n200N10000bad70")
#colnames(ks_n200N5000bad30) <- gsub("\"", "", colnames(ks_n200N5000bad30))
#colnames(ks_n200N5000bad70) <- gsub("\"", "", colnames(ks_n200N5000bad70))
#colnames(ks_n200N10000bad30) <- gsub("\"", "", colnames(ks_n200N10000bad30))
#colnames(ks_n200N10000bad70) <- gsub("\"", "", colnames(ks_n200N10000bad70))
#ks_n200N5000bad30 <- ks_n200N5000bad30 %>% filter(OCC2 == 1)
#ks_n200N5000bad70 <- ks_n200N5000bad70 %>% filter(OCC2 == 1)
#ks_n200N10000bad30 <- ks_n200N10000bad30 %>% filter(OCC2 == 1)
#ks_n200N10000bad70 <- ks_n200N10000bad70 %>% filter(OCC2 == 1)

# 2PL
mod0_n200N5000bad30 <- as_tibble(t(read_table2(file = "Results/mod0values_n200N5000bad30")))
mod0_n200N5000bad70 <- as_tibble(t(read_table2(file = "Results/mod0values_n200N5000bad70")))
mod0_n200N10000bad30 <- as_tibble(t(read_table2(file = "Results/mod0values_n200N10000bad30")))
mod0_n200N10000bad70 <- as_tibble(t(read_table2(file = "Results/mod0values_n200N10000bad70")))
colnames(mod0_n200N5000bad30) <- colnames(mod0_n200N5000bad70) <- 
  colnames(mod0_n200N10000bad30) <- colnames(mod0_n200N10000bad70) <- c("omega", "xi")

# SA
sa_n200N5000bad30 <- as_tibble(t(read_table2(file = "Results/savalues_n200N5000bad30")))
sa_n200N5000bad70 <- as_tibble(t(read_table2(file = "Results/savalues_n200N5000bad70")))
sa_n200N10000bad30 <- as_tibble(t(read_table2(file = "Results/savalues_n200N10000bad30")))
sa_n200N10000bad70 <- as_tibble(t(read_table2(file = "Results/savalues_n200N10000bad70")))
colnames(sa_n200N5000bad30) <- colnames(sa_n200N5000bad70) <- 
  colnames(sa_n200N10000bad30) <- c("omega", "xi", "alpha1", "tau1", "alpha2", "tau2")
colnames(sa_n200N10000bad70) <- c("omega", "xi", "alpha1", "tau1", "alpha2", "tau2", "alpha3", "tau3")

# save q of SA items and 2PL items (q = 0)
sa_n200N5000bad30 <- sa_n200N5000bad30 %>% mutate(q = as.numeric(!is.na(alpha1)) + as.numeric(!is.na(alpha2)))
sa_n200N5000bad70 <- sa_n200N5000bad70 %>% mutate(q = as.numeric(!is.na(alpha1)) + as.numeric(!is.na(alpha2)))
sa_n200N10000bad30 <- sa_n200N10000bad30 %>% mutate(q = as.numeric(!is.na(alpha1)) + as.numeric(!is.na(alpha2)))
sa_n200N10000bad70 <- sa_n200N10000bad70 %>% mutate(q = as.numeric(!is.na(alpha1)) + as.numeric(!is.na(alpha2)) + as.numeric(!is.na(alpha3)))
mod0_n200N5000bad30$q <- mod0_n200N5000bad70$q <- mod0_n200N10000bad30$q <- mod0_n200N10000bad70$q <- 0

## table the relationship between bad items and SA q
table(true_n200N5000bad30$bad, sa_n200N5000bad30$q)
table(true_n200N5000bad70$bad, sa_n200N5000bad70$q)
table(true_n200N10000bad30$bad, sa_n200N10000bad30$q)
table(true_n200N10000bad70$bad, sa_n200N10000bad70$q)

#### Visualize Biases and Standard Errors ####
res <- res %>%
  mutate(thetadiff = thetaest - truetheta,
         thetadiff2 = thetadiff^2)
recovery <- res %>%
  group_by(N, bad, model, select, thetacond) %>%
  summarize(bias = mean(thetadiff), rmse = sqrt(mean(thetadiff^2)), meanse = mean(se)) %>% # *** LMF - think I caught a typo here with the RMSEs (originally was thetadiff2^2 instead of thetadiff^2)
  mutate(linetype = paste0("N = ", N, ", ", bad, " bad items"))

# for random draws from standard normal
ggplot(recovery %>% filter(thetacond == "normal01"),
       aes(model, rmse, group = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~linetype)

aggregate(rmse ~ model, data=recovery, mean)
aggregate(rmse ~ model + N + bad, data=recovery, mean) # *** CFF - report this in-text? (or a condensed way of summarizing these)

ggplot(recovery %>% filter(thetacond == "normal01"),
       aes(model, bias, group = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + geom_hline(yintercept = 0)

ggplot(recovery %>% filter(thetacond == "normal01"),
       aes(model, meanse, group = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~linetype)

# across the theta continuum
# *** CFF - report this one? (say something about how the selection method did not seem to make a big difference?)
# Do we want to report bias or meanse?
# *** LMF - these are tricky - bias is interesting, but confounded with the use of the EAP estimator
#         - I like the idea of meanse, but we would have to be convinced that meanse reflects true improvements in precision rather than biased estimates of the standard error
# *** CFF - some summary on whether MPWI and FI differs much from KL? Or put MPWI and FI in supplement?
# If motivation is to use MP w/ existing item selection algorithm (likely FI or MPWI for operational testing), then there should be some
# mention of it, even if just to say it does just as well as KL
# Probably omit KLL and KLP, as these only show up in catR (not sure if published) and didn't do great anyway. But, mention in-text or footnote.
# *** LMF - I agree that the rsme plot is probably the best illustration at this point
#         - I also agree with the suggestion to drop the KL results at this point

# reorder linetype order
recovery$linetype <- forcats::lvls_reorder(recovery$linetype, c(3, 4, 1, 2))

jpeg("rmse fig.jpeg", width = 6, height = 6, units = "in", res = 600)
ggplot(recovery %>% filter(select == "KL" & thetacond != "normal01"),
       aes(thetacond, rmse, group = model, color = model, shape = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + 
  theme_minimal() + theme(legend.position = "bottom") + 
  scale_colour_grey(labels = c("2PL", "MP", "true")) + 
  scale_x_discrete(labels = as.character(seq(-2, 2, by = .5)), name = expression(theta))
dev.off()


# *** CFF - report this one? Though I have some trouble making good sense of this one
# *** LMF - maybe skip this one - it mostly shows the bias of the EAP estimator rather than any meaningful group differences
#         - absolute bias (new) is a bit more interpretable if we really want to show bias
ggplot(recovery %>% filter(select == "KL" & thetacond != "normal01"),
       aes(thetacond, bias, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + geom_hline(yintercept = 0)

ggplot(recovery %>% filter(select == "KL" & thetacond != "normal01"),
       aes(thetacond, abs(bias), group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + geom_hline(yintercept = 0)

ggplot(recovery %>% filter(select == "KL" & thetacond != "normal01"),
       aes(thetacond, meanse, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype)

#### Results Relative to True Item Parameters ####
res2 <- res %>% select(ni:truetheta, thetacond) %>% 
  mutate(model = paste0("thetaest_", model)) %>%
  spread(model, thetaest)

res2 <- res2 %>% mutate(thetadiff_k0 = thetaest_k0 - thetaest_true,
                        #thetadiff_KS = thetaest_KS - thetaest_true,
                        thetadiff_SA = thetaest_SA - thetaest_true,
                        thetadiff2_k0 = thetadiff_k0^2,
                        #thetadiff2_KS = thetadiff_KS^2,
                        thetadiff2_SA = thetadiff_SA^2)

res3 <- res2 %>% select(ni:thetaest_true) %>%
  gather(key = model, value = thetaest, -(ni:thetacond)) %>% 
  mutate(model = gsub("thetaest_", "", model))

res4 <- res2 %>% select(ni:thetacond, thetadiff_k0:thetadiff_SA) %>%
  gather(key = model, value = thetadiff, -(ni:thetacond)) %>% 
  mutate(model = gsub("thetadiff_", "", model))

res5 <- res2 %>% select(ni:thetacond, thetadiff2_k0:thetadiff2_SA) %>%
  gather(key = model, value = thetadiff2, -(ni:thetacond)) %>% 
  mutate(model = gsub("thetadiff2_", "", model))

res3 <- full_join(res3, res4)
res3 <- full_join(res3, res5)
rm(res4, res5)

res4 <- res %>% select(ni:thetacond)

res2 <- full_join(res3, res4)
rm(res3, res4)

res2 <- res2 %>% filter(!is.na(thetaest))


## visualize differences
recovery2 <- res2 %>%
  group_by(N, bad, model, select, thetacond) %>%
  summarize(rel_bias = mean(thetadiff), rmsd = sqrt(mean(thetadiff2^2))) %>%
  mutate(linetype = paste0("N = ", N, ", ", bad, " bad items"))

# for random draws from standard normal
# with complete data, there is some note about SA being close to true. I don't see that here, though results in general are difficult to discern
ggplot(recovery2 %>% filter(thetacond == "normal01" & model != "true"),
       aes(model, rmsd, group = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~linetype)

ggplot(recovery2 %>% filter(thetacond == "normal01" & model != "true"),
       aes(model, rel_bias, group = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + geom_hline(yintercept = 0)

# across the theta continuum
ggplot(recovery2 %>% filter(select == "KL" & thetacond != "normal01" & model != "true"),
       aes(thetacond, rmsd, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype)

ggplot(recovery2 %>% filter(select == "KL" & thetacond != "normal01" & model != "true"),
       aes(thetacond, rel_bias, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~linetype) + geom_hline(yintercept = 0)


#### Patterns of Bad Items Administered ####

# CFF - should we report anything from this section?
# *** LMF - could be an interesting argument to add in that SA makes better use of these "bad" items than KS
#         - but if the paper is getting long, this would be a good candidate for something to cut

# add character column of reference item bank
res <- res %>% 
  mutate(itembank = paste0(tolower(model), "_n", ni, "N", N, "bad", bad),
         ref_itembank = paste0("true_n", ni, "N", N, "bad", bad)) %>%
  mutate(itembank = gsub("k0", "mod0", itembank))

# count the number of bad items administered (takes some time to compute)
res$nbad_admin <- apply(res, 1, function(x){
  sum(as.numeric(x[paste0("itemadmin", 1:25)]) %in% which(get(x["ref_itembank"])$bad))
})

# very little association between theta errors and nbad_admin
cor(res$thetadiff, res$nbad_admin)
cor(res$thetadiff2, res$nbad_admin)
cor(res$se, res$nbad_admin)

ggplot(res, aes(factor(nbad_admin), thetadiff, fill = model)) + geom_boxplot(outlier.shape = NA) + 
  geom_hline(yintercept = 0) + lims(y = c(-1.5, 1.5)) + facet_wrap(~model)

# CFF - woah is there an outlier in there somewhere?
# what is the warning message about non-finite values? LMF - I think this warning signals that the ylims are smaller than the outliers (which we aren't plotting anyways)
# LMF - yes, seems to be some strange outlier behavior - not something I'd expect to replicate or be meaniningful
ggplot(res, aes(factor(nbad_admin), thetadiff2, fill = model)) + geom_boxplot(outlier.shape = NA) + 
  lims(y = c(0, 1)) + 
  facet_wrap(~model)

# the more bad items administered, the lower the standard errors?
# CFF -more variation in standard errors?
ggplot(res, aes(factor(nbad_admin), se)) + geom_boxplot()

recovery3 <- res %>%
  group_by(N, bad, model, select, thetacond) %>%
  summarize(bias = mean(thetadiff), rmse = sqrt(mean(thetadiff2^2)), meanse = mean(se), prop_nbad = mean(nbad_admin) / 25) %>%
  mutate(bank = paste0("N = ", N, ", ", bad, " bad items"))


# *** LMF - if we report anything from this section, I'd suggest the following plot or the one after
# Sure, either one looks good to me

# reorder linetype order
recovery3$bank <- forcats::lvls_reorder(recovery3$bank, c(3, 4, 1, 2))

jpeg("prop bad items fig.jpeg", width = 6, height = 6, units = "in", res = 600)
ggplot(recovery3 %>% filter(thetacond == "normal01"),
       aes(model, prop_nbad, group = select, linetype = select, color = select)) + 
  geom_point() + geom_line() + facet_wrap(~bank) + theme_minimal() + 
  labs(y = "proportion of \"bad\" items administered") + theme(legend.position = "bottom") + 
  scale_color_grey() + scale_x_discrete(labels = c("2PL", "MP", "true"))
dev.off()


# bad items are less likely to be administered if item pars are estimated than if true item pars are used
# SA is more likely, in most cases, to make use of bad items

ggplot(recovery3 %>% filter(select == "FI" & thetacond != "normal01"),
       aes(thetacond, prop_nbad, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~bank)

ggplot(recovery3 %>% filter(select == "KL" & thetacond != "normal01"),
       aes(thetacond, prop_nbad, group = model, color = model)) + 
  geom_point() + geom_line() + facet_wrap(~bank)

ggplot(recovery3, aes(meanse, prop_nbad, color = bank)) + geom_point()

#### RIMSEs ####

## functions to compute IRFs and IIFs

trace.cdf<-function(theta, item){
  
  ncat<-item$ncat
  p<-item$p
  mu<-item$mu
  sd<-item$sd
  N<-length(theta)
  
  ## 2 categories
  P<-matrix(0,N,ncat-1)
  
  ## add asymptote if available
  if(!is.null(item$kappa)){
    if(length(item$kappa)>1){
      for(j in 1:length(p)){
        C<-invlogit(item$kappa[j])
        P<-P+p[[j]]*(C+(1-C)*pnorm(theta,mu[[j]],sd[[j]]))
        
      }
    } else {
      C<-invlogit(item$kappa)
      P<-P+C
      for(j in 1:length(p)){
        P<-P+p[[j]]*pnorm(theta,mu[[j]],sd[[j]])
      }   
    }
  } else {
    ## mix CDFs
    for(j in 1:length(p)){
      P<-P+p[[j]]*pnorm(theta,mu[[j]],sd[[j]])
    }
  }
  P<-cbind(1-P,P)
  return(P)
}
info.cdf<-function(theta,item){
  ncat<-item$ncat
  p<-item$p
  mu<-item$mu
  sd<-item$sd
  N<-length(theta)
  
  QP<-trace.cdf(theta,item)
  
  if(!is.null(item$kappa)){
    stop("Lower asymptote not yet supported")
  } else {
    
    ## mix CDFs
    Pprime<-rep(0,N)
    for(j in 1:length(p)){
      
      # first derivative w.r.t. theta
      Pprime<-Pprime+p[[j]]*dnorm((theta-mu[[j]])/sd[[j]])*(1/sd[[j]])
      
    }
  }
  
  # sanity check using numerical derivatives
  #tmp<-grad(deriv.check,theta,item=item)
  #plot(Pprime,tmp)
  
  info<-(Pprime^2)/(QP[,1]*QP[,2])
  return(info)
  
}

# response probabilities for true item banks
trace.ib.cdf <- function(theta, pars){
  pcols <- grep("p", names(pars))
  mucols <- grep("mu", names(pars))
  sdcols <- grep("sd", names(pars))
  sapply(1:nrow(pars), function(i){
    p <- as.numeric(pars[i, pcols])
    mu <- as.numeric(pars[i, mucols])
    sd <- as.numeric(pars[i, sdcols])
    p <- p[!is.na(p)]
    mu <- mu[!is.na(mu)]
    sd <- sd[!is.na(sd)]
    trace.cdf(theta, item = list(ncat = 2, p = p, mu = mu, sd = sd))[, 2]
  })
}

# response probabilities for MP item banks (mod0, SA)
trace.ib.mp <- function(theta, pars){
  cols <- 1: (grep("q", names(pars)) - 1) # select columns up to, but not including q
  sapply(1:nrow(pars), function(i){
    p <- as.numeric(pars[i, cols])
    p <- p[!is.na(p)]
    rpf.prob(rpf.lmp(pars$q[i]), p, theta)[2, ]
  })
}

# information for true item banks
info.ib.cdf <- function(theta, pars){
  pcols <- grep("p", names(pars))
  mucols <- grep("mu", names(pars))
  sdcols <- grep("sd", names(pars))
  sapply(1:nrow(pars), function(i){
    p <- as.numeric(pars[i, pcols])
    mu <- as.numeric(pars[i, mucols])
    sd <- as.numeric(pars[i, sdcols])
    p <- p[!is.na(p)]
    mu <- mu[!is.na(mu)]
    sd <- sd[!is.na(sd)]
    info.cdf(theta, item = list(ncat = 2, p = p, mu = mu, sd = sd))
  })
}

# information for MP item banks (mod0, SA)
info.ib.mp <- function(theta, pars){
  cols <- 1: (grep("q", names(pars)) - 1) # select columns up to, but not including q
  sapply(1:nrow(pars), function(i){
    p <- as.numeric(pars[i, cols])
    p <- p[!is.na(p)]
    sapply(theta, function(t){ # rpf.info only takes scalar theta argument
      rpf.info(rpf.lmp(pars$q[i]), p, t)
    })
  })
}

# RIMSE for response probabilities
rimse.trace <- function(true.ib, mp.ib, qpts, wts){
  # true.ib: true item bank
  # mp.ib: monotonic polynomial item bank
  # qpts: quadrature points (sequence of theta)
  # wts: vector of weights corresponding to the quadrature points
  
  p.true <- trace.ib.cdf(qpts, true.ib)
  p.mp <- trace.ib.mp(qpts, mp.ib)
  
  colSums(diag(wts) %*% (p.true - p.mp)^2) / sum(wts)
}

# RIMSE for response probabilities (for KS) - added by CFF
rimse.kstrace <- function(true.ib, ks.ib){
  # true.ib: true item bank
  # mp.ib: monotonic polynomial item bank
  # qpts: quadrature points (sequence of theta)
  # wts: vector of weights corresponding to the quadrature points
  
  qpts<-as.numeric(ks.ib[1,which(grepl("^evalpoints",colnames(ks.ib)))]) # just pick the first one, hope the rest match
  wts<-dnorm(qpts)
  p.true <- trace.ib.cdf(qpts, true.ib)
  p.ks<-t(ks.ib[,paste0("OCC",4:(length(qpts)+3))])
  #p.mp <- trace.ib.mp(qpts, mp.ib)
  
  colSums(diag(wts) %*% (p.true - p.ks)^2) / sum(wts)
}

# RIMSE for item information
rimse.info <- function(true.ib, mp.ib, qpts, wts){
  # true.ib: true item bank
  # mp.ib: monotonic polynomial item bank
  # qpts: quadrature points (sequence of theta)
  # wts: vector of weights corresponding to the quadrature points
  
  i.true <- info.ib.cdf(qpts, true.ib)
  i.mp <- info.ib.mp(qpts, mp.ib)
  
  colSums(diag(wts) %*% (i.true - i.mp)^2) / sum(wts)
}

# compute both types of RIMSEs for all MP items
# CFF: We could use the same grid points for KS and others, but that would require checking to see what grid points were used by KS
qpts <- seq(-5, 5, by = .1)
wts <- dnorm(qpts)

mod0_n200N5000bad30$RIMSE_p <- rimse.trace(true_n200N5000bad30, mod0_n200N5000bad30, qpts, wts) * 100
mod0_n200N5000bad70$RIMSE_p <- rimse.trace(true_n200N5000bad70, mod0_n200N5000bad70, qpts, wts) * 100
mod0_n200N10000bad30$RIMSE_p <- rimse.trace(true_n200N10000bad30, mod0_n200N10000bad30, qpts, wts) * 100
mod0_n200N10000bad70$RIMSE_p <- rimse.trace(true_n200N10000bad70, mod0_n200N10000bad70, qpts, wts) * 100

sa_n200N5000bad30$RIMSE_p <- rimse.trace(true_n200N5000bad30, sa_n200N5000bad30, qpts, wts) * 100
sa_n200N5000bad70$RIMSE_p <- rimse.trace(true_n200N5000bad70, sa_n200N5000bad70, qpts, wts) * 100
sa_n200N10000bad30$RIMSE_p <- rimse.trace(true_n200N10000bad30, sa_n200N10000bad30, qpts, wts) * 100
sa_n200N10000bad70$RIMSE_p <- rimse.trace(true_n200N10000bad70, sa_n200N10000bad70, qpts, wts) * 100

# RIMSEs * 100
mod0_n200N5000bad30$RIMSE_i <- rimse.info(true_n200N5000bad30, mod0_n200N5000bad30, qpts, wts) * 100
mod0_n200N5000bad70$RIMSE_i <- rimse.info(true_n200N5000bad70, mod0_n200N5000bad70, qpts, wts) * 100
mod0_n200N10000bad30$RIMSE_i <- rimse.info(true_n200N10000bad30, mod0_n200N10000bad30, qpts, wts) * 100
mod0_n200N10000bad70$RIMSE_i <- rimse.info(true_n200N10000bad70, mod0_n200N10000bad70, qpts, wts) * 100

sa_n200N5000bad30$RIMSE_i <- rimse.info(true_n200N5000bad30, sa_n200N5000bad30, qpts, wts) * 100
sa_n200N5000bad70$RIMSE_i <- rimse.info(true_n200N5000bad70, sa_n200N5000bad70, qpts, wts) * 100
sa_n200N10000bad30$RIMSE_i <- rimse.info(true_n200N10000bad30, sa_n200N10000bad30, qpts, wts) * 100
sa_n200N10000bad70$RIMSE_i <- rimse.info(true_n200N10000bad70, sa_n200N10000bad70, qpts, wts) * 100

## CFF: added KS here
#ks_n200N5000bad30$RIMSE_p  <- rimse.kstrace(true_n200N5000bad30, ks_n200N5000bad30)*100
#ks_n200N5000bad70$RIMSE_p  <- rimse.kstrace(true_n200N5000bad70, ks_n200N5000bad70)*100
#ks_n200N10000bad30$RIMSE_p  <- rimse.kstrace(true_n200N10000bad30, ks_n200N10000bad30)*100
#ks_n200N10000bad70$RIMSE_p  <- rimse.kstrace(true_n200N10000bad70, ks_n200N10000bad70)*100

RIMSE_res <- rbind(data.frame(bank = "n200N5000bad30", model = "2PL", bad = true_n200N5000bad30$bad, mod0_n200N5000bad30[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N5000bad70", model = "2PL", bad = true_n200N5000bad70$bad, mod0_n200N5000bad70[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N10000bad30", model = "2PL", bad = true_n200N10000bad30$bad, mod0_n200N10000bad30[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N10000bad70", model = "2PL", bad = true_n200N10000bad70$bad, mod0_n200N10000bad70[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N5000bad30", model = "SA", bad = true_n200N5000bad30$bad, sa_n200N5000bad30[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N5000bad70", model = "SA", bad = true_n200N5000bad70$bad, sa_n200N5000bad70[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N10000bad30", model = "SA", bad = true_n200N10000bad30$bad, sa_n200N10000bad30[, c("q", "RIMSE_p", "RIMSE_i")]),
                   data.frame(bank = "n200N10000bad70", model = "SA", bad = true_n200N10000bad70$bad, sa_n200N10000bad70[, c("q", "RIMSE_p", "RIMSE_i")]))

# relationship between recovery and bad items
ggplot(RIMSE_res, aes(bad, RIMSE_p, fill = model)) + 
  geom_boxplot(outlier.shape = NA) + lims(y = c(0, .35)) + facet_wrap(~bank)

ggplot(RIMSE_res, aes(bad, RIMSE_i, fill = model)) + 
  geom_boxplot(outlier.shape = NA) + lims(y = c(0, .35)) + 
  facet_wrap(~bank)

# relationship between recovery and q
ggplot(RIMSE_res %>% filter(model == "SA"), aes(factor(q), RIMSE_p, fill = factor(q))) +
  geom_boxplot(outlier.shape = NA) + lims(y = c(0, .15)) + facet_wrap(~bank)

ggplot(RIMSE_res %>% filter(model == "SA"), aes(factor(q), RIMSE_i, fill = factor(q))) +
  geom_boxplot(outlier.shape = NA) + lims(y = c(0, 17.5)) + 
  facet_wrap(~bank)

#############################
RIMSE_res2 <- rbind(data.frame(bank = "n200N5000bad30", model = "2PL", bad = true_n200N5000bad30$bad, mod0_n200N5000bad30[, c("RIMSE_p")]),
                    data.frame(bank = "n200N5000bad70", model = "2PL", bad = true_n200N5000bad70$bad, mod0_n200N5000bad70[, c("RIMSE_p")]),
                    data.frame(bank = "n200N10000bad30", model = "2PL", bad = true_n200N10000bad30$bad, mod0_n200N10000bad30[, c("RIMSE_p")]),
                    data.frame(bank = "n200N10000bad70", model = "2PL", bad = true_n200N10000bad70$bad, mod0_n200N10000bad70[, c("RIMSE_p")]),
                    data.frame(bank = "n200N5000bad30", model = "SA", bad = true_n200N5000bad30$bad, sa_n200N5000bad30[, c("RIMSE_p")]),
                    data.frame(bank = "n200N5000bad70", model = "SA", bad = true_n200N5000bad70$bad, sa_n200N5000bad70[, c("RIMSE_p")]),
                    data.frame(bank = "n200N10000bad30", model = "SA", bad = true_n200N10000bad30$bad, sa_n200N10000bad30[, c("RIMSE_p")]),
                    data.frame(bank = "n200N10000bad70", model = "SA", bad = true_n200N10000bad70$bad, sa_n200N10000bad70[, c("RIMSE_p")]))
                    #data.frame(bank = "n200N5000bad30", model = "KS", bad = true_n200N5000bad30$bad, ks_n200N5000bad30[, c("RIMSE_p")]),
                    #data.frame(bank = "n200N5000bad70", model = "KS", bad = true_n200N5000bad70$bad, ks_n200N5000bad70[, c("RIMSE_p")]),
                    #data.frame(bank = "n200N10000bad30", model = "KS", bad = true_n200N10000bad30$bad, ks_n200N10000bad30[, c("RIMSE_p")]),
                    #data.frame(bank = "n200N10000bad70", model = "KS", bad = true_n200N10000bad70$bad, ks_n200N10000bad70[, c("RIMSE_p")]))

# *** CFF Report this one?
# *** LMF - this looks good to me
# *** CFF - looks like SA does well for "bad" items in all but N=5000 bad=30 condition?
#           similar, though slightly worse for standard items
RIMSE_res2 <- RIMSE_res2 %>% 
  mutate(bank = forcats::fct_recode(bank, "N = 5000, 30 bad items" = "n200N5000bad30",
                                    "N = 5000, 70 bad items" = "n200N5000bad70",
                                    "N = 10000, 30 bad items" = "n200N10000bad30",
                                    "N = 10000, 70 bad items" = "n200N10000bad70"))

jpeg("rimse fig.jpeg", width = 6, height = 6, units = "in", res = 600)
ggplot(RIMSE_res2, aes(bad, RIMSE_p, fill = model)) + 
  geom_boxplot(outlier.shape = NA) + lims(y = c(0, .35)) + facet_wrap(~bank) + 
  scale_fill_grey(start = .5, labels = c("2PL", "MP")) + theme_minimal() + 
  labs(x = "", y = "RIMSE") + scale_x_discrete(labels = c("2PL items", "non-2PL items")) 
dev.off()
#############################

## do bad items provide more true information than non-bad items?
#theta <- seq(-3, 3, by = .01)
theta <- qpts #seq(-3, 3, by = .01) # If numerical summary, then same grid as qpts?

testinfo_dat <- data.frame(theta = theta, itembank = "n200N5000bad30", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N5000bad30", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)[, true_n200N5000bad30$bad])))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N5000bad30", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)[, !true_n200N5000bad30$bad])))

testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N5000bad70", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70))))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N5000bad70", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N5000bad70", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70)[, !true_n200N5000bad70$bad])))

testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad30", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30))))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad30", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30)[, true_n200N10000bad30$bad])))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad30", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30)[, !true_n200N10000bad30$bad])))

testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad70", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70))))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad70", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70)[, true_n200N10000bad70$bad])))
testinfo_dat <- rbind(testinfo_dat, data.frame(theta = theta, itembank = "n200N10000bad70", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70)[, !true_n200N10000bad70$bad])))

testinfo_dat <- testinfo_dat %>% mutate(avg_info = info / nit_info, # compute average information
                                        infotype = fct_relevel(infotype, "bad", after = 0)) # reorder to better conform to ggplot's default color scheme

# our bad items tend to have relatively high info, even in 30-item categories
ggplot(testinfo_dat, aes(theta, info, col = infotype)) + facet_wrap(~itembank) + geom_path()

# average item information is almost always higher for bad items than for good items
ggplot(testinfo_dat %>% filter(infotype != "total"), aes(theta, avg_info, col = infotype)) + facet_wrap(~itembank) + geom_path()

# report some numerical summary of the above? i.e., in-text rather than a plot?

#######################
# CFF: What about true information vs estimated information (collapsing across type of item) - can we visualize?
testinfo_dat2 <- data.frame(theta = theta, itembank = "n200N5000bad30", model="true", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="true", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)[, true_n200N5000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="true", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N5000bad30)[, !true_n200N5000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="true", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="true", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="true", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N5000bad70)[, !true_n200N5000bad70$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="true", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="true", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30)[, true_n200N10000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="true", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N10000bad30)[, !true_n200N10000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="true", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="true", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70)[, true_n200N10000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="true", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.cdf(theta, true_n200N10000bad70)[, !true_n200N10000bad70$bad])))

# add SA
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="SA", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, sa_n200N5000bad30))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="SA", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.mp(theta, sa_n200N5000bad30)[, true_n200N5000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="SA", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.mp(theta, sa_n200N5000bad30)[, !true_n200N5000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="SA", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, sa_n200N5000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="SA", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.mp(theta, sa_n200N5000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="SA", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.mp(theta, sa_n200N5000bad70)[, !true_n200N5000bad70$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="SA", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, sa_n200N10000bad30))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="SA", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.mp(theta, sa_n200N10000bad30)[, true_n200N10000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="SA", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.mp(theta, sa_n200N10000bad30)[, !true_n200N10000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="SA", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, sa_n200N10000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="SA", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.mp(theta, sa_n200N10000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="SA", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.mp(theta, sa_n200N10000bad70)[, !true_n200N5000bad70$bad])))


# add mod0
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="mod0", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad30))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="mod0", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad30)[, true_n200N5000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad30",model="mod0", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad30)[, !true_n200N5000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="mod0", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="mod0", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N5000bad70",model="mod0", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.mp(theta, mod0_n200N5000bad70)[, !true_n200N5000bad70$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="mod0", nbad = 30, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad30))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="mod0", nbad = 30, nit_info = 60, infotype = "bad", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad30)[, true_n200N10000bad30$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad30",model="mod0", nbad = 30, nit_info = 140, infotype = "good", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad30)[, !true_n200N10000bad30$bad])))

testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="mod0", nbad = 70, nit_info = 200, infotype = "total", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad70))))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="mod0", nbad = 70, nit_info = 140, infotype = "bad", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad70)[, true_n200N5000bad70$bad])))
testinfo_dat2 <- rbind(testinfo_dat2, data.frame(theta = theta, itembank = "n200N10000bad70",model="mod0", nbad = 70, nit_info = 60, infotype = "good", info = rowSums(info.ib.mp(theta, mod0_n200N10000bad70)[, !true_n200N5000bad70$bad])))

# *** CFF - report a plot of this one?
# Is there an in-text summary we can provide?
# That's odd - what's with the spike in info above 3 or so?

# *** LMF - seems to be an error in true info calculation, see the following summaries:
tapply(testinfo_dat2$info, testinfo_dat2$model, summary)
tapply(testinfo_dat2$info, testinfo_dat2$infotype, summary)
# *** LMF - mot sure what to do about this (other than excluding Inf cases) - seems to trace back to the rpf package


ggplot(testinfo_dat2 %>% filter(infotype == "total" & info < Inf), 
       aes(theta, info, col = model)) + facet_wrap(~itembank) + geom_path() #hmm. I imagine there's a way to compute a discrepancy between true and estimated

# *** LMF - Waller and I (2017) computed a RIMSE-like measure for info recovery - could easily genearlize to test info (perhaps divide by #items)
#           hmm....although this is interesting, I wonder if showing such wonky information functions would
#           be easy for reviewers to criticize (and would warrant a fair amount of extra explanation/justification)

#######################

## look at the variety of items administered for each model condition
true_n200N5000bad30$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 30, model == "true") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
true_n200N5000bad70$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 70, model == "true") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
true_n200N10000bad30$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 30, model == "true") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
true_n200N10000bad70$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 70, model == "true") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()

#ks_n200N5000bad30$nadmin <- res %>% filter(ni == 100, N == 1000, bad == 30, model == "KS") %>% select(itemadmin1:itemadmin25) %>% 
#  map_dfc(function(x) summary(factor(x, levels = 1:100))) %>% rowSums()
#ks_n200N5000bad70$nadmin <- res %>% filter(ni == 100, N == 1000, bad == 70, model == "KS") %>% select(itemadmin1:itemadmin25) %>% 
#  map_dfc(function(x) summary(factor(x, levels = 1:100))) %>% rowSums()
#ks_n200N10000bad30$nadmin <- res %>% filter(ni == 100, N == 3000, bad == 30, model == "KS") %>% select(itemadmin1:itemadmin25) %>% 
#  map_dfc(function(x) summary(factor(x, levels = 1:100))) %>% rowSums()
#ks_n200N10000bad70$nadmin <- res %>% filter(ni == 100, N == 3000, bad == 70, model == "KS") %>% select(itemadmin1:itemadmin25) %>% 
#  map_dfc(function(x) summary(factor(x, levels = 1:100))) %>% rowSums()

mod0_n200N5000bad30$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 30, model == "k0") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
mod0_n200N5000bad70$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 70, model == "k0") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
mod0_n200N10000bad30$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 30, model == "k0") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
mod0_n200N10000bad70$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 70, model == "k0") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()

sa_n200N5000bad30$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 30, model == "SA") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
sa_n200N5000bad70$nadmin <- res %>% filter(ni == 200, N == 5000, bad == 70, model == "SA") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
sa_n200N10000bad30$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 30, model == "SA") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()
sa_n200N10000bad70$nadmin <- res %>% filter(ni == 200, N == 10000, bad == 70, model == "SA") %>% select(itemadmin1:itemadmin25) %>% 
  map_dfc(function(x) summary(factor(x, levels = 1:200))) %>% rowSums()

# count the number of items never administerd
# *** CFF - if space, some in-text description of item exposure?
# *** LMF - this would be easy enough to do - the "problem" is that KS apparently is better at using all items in the item bank than other methods
sum(true_n200N5000bad30$nadmin == 0)
#sum(ks_n200N5000bad30$nadmin == 0)
sum(mod0_n200N5000bad30$nadmin == 0)
sum(sa_n200N5000bad30$nadmin == 0)

sum(true_n200N5000bad70$nadmin == 0)
#sum(ks_n200N5000bad70$nadmin == 0)
sum(mod0_n200N5000bad70$nadmin == 0)
sum(sa_n200N5000bad70$nadmin == 0)

sum(true_n200N10000bad30$nadmin == 0)
#sum(ks_n200N10000bad30$nadmin == 0)
sum(mod0_n200N10000bad30$nadmin == 0)
sum(sa_n200N10000bad30$nadmin == 0)

sum(true_n200N10000bad70$nadmin == 0)
#sum(ks_n200N10000bad70$nadmin == 0)
sum(mod0_n200N10000bad70$nadmin == 0)
sum(sa_n200N10000bad70$nadmin == 0)

## compare which items are rarely administered - probably nothing intersting here
#plot(true_n200N10000bad30$nadmin, ks_n200N5000bad30$nadmin, 
#     col = 3 - true_n200N5000bad30$bad, pch = 16)
#abline(0, 1)

plot(true_n200N10000bad30$nadmin, mod0_n200N5000bad30$nadmin, 
     col = 3 - true_n200N5000bad30$bad, pch = 16)
abline(0, 1)

plot(true_n200N10000bad30$nadmin, sa_n200N5000bad30$nadmin, 
     col = 3 - true_n200N5000bad30$bad, pch = 16)
abline(0, 1)

#plot(true_n200N10000bad70$nadmin, ks_n200N5000bad70$nadmin, 
#     col = 3 - true_n200N5000bad70$bad, pch = 16)
#abline(0, 1)

plot(true_n200N10000bad70$nadmin, mod0_n200N5000bad70$nadmin, 
     col = 3 - true_n200N5000bad70$bad, pch = 16)
abline(0, 1)

plot(true_n200N10000bad70$nadmin, sa_n200N5000bad70$nadmin, 
     col = 3 - true_n200N5000bad70$bad, pch = 16)
abline(0, 1)


## count the number of the truly most informative that are administered for each row
res$nBest<-rep(NA,nrow(res)) # initialize
## also a bit slow to compute
#res$nBest <- 

#res$nBest[1:10]<-sapply(1:10, function(i){
res$nBest<-sapply(1:nrow(res), function(i){
  infos <- info.ib.cdf(theta = res$truetheta[i], pars = get(paste0("true_n200N", res$N[i], "bad", res$bad[i])))
  its <- which(rank(infos) > 175)
  sum(its %in% res[i, grep("itemadmin", colnames(res))])
})

summary(res$nBest)

ggplot(res, aes(N, nBest)) + geom_boxplot()

ggplot(res, aes(bad, nBest)) + geom_boxplot()

ggplot(res, aes(model, nBest)) + geom_boxplot()

aggregate(nBest ~ model, data=res, mean)
# *** CFF - Report this? for normal01? Mention collapsing across conditions?
# *** LMF - Sure! It seems simple enough to explain, so that it won't overly complicate the paper
aggregate(nBest ~ model + thetacond, data=res%>%filter(thetacond=="normal01"), mean) 

ggplot(res, aes(select, nBest)) + geom_boxplot() # MPWI isn't too bad (with complete data); just ok with missing (FI and KL look good)
# *** CFF - numerical summary of this?
# *** LMF - agreed, for same reason as above
aggregate(nBest ~ select, data=res%>%filter(thetacond=="normal01"), mean) # report this. Mention collapsing across conditions?

# Looks like SA is better when there's lots of bad items

ggplot(res %>% filter(select=="MPWI"&thetacond=="normal01"), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~N)
ggplot(res %>% filter(select=="KL"&thetacond=="normal01"), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~N)

## plots at discrete points along theta
ggplot(res %>% filter(select=="MPWI"&thetacond!="normal01"&N==5000), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~thetacond)
ggplot(res %>% filter(select=="MPWI"&thetacond!="normal01"&N==10000), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~thetacond)

# *** CFF - Report these?
# *** LMF - I'm less crazy about this one - a lot more complicated to interpret, and the patterns are difficult to explain (e.g., why does k0's relative performance vary so much? perhaps more noise than signal here)
ggplot(res %>% filter(select=="KL"&thetacond!="normal01"&N==5000), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~thetacond)
ggplot(res %>% filter(select=="KL"&thetacond!="normal01"&N==10000), aes(bad, nBest, color=model))+geom_boxplot()+facet_wrap(~thetacond)
