# Don Boyd
# 5/12/2018

usethis::use_build_ignore(c("data-raw"))
usethis::use_build_ignore(c("docs"))


#****************************************************************************************************
#       Setup ####
#****************************************************************************************************
source("./data-raw/programs/libraries.r")
source("./functions_ersd.r") # expected return and standard deviation
source("./functions_optim_portfolio.r")


#****************************************************************************************************
#       Functions for efficient frontier ####
#****************************************************************************************************
efrontier <- function(ers, ersd, cormat, aa.lb=-1e9, aa.ub=1e9){
  # calculates the minimum-variance efficient frontier over a set of portfolio expected returns, using
  #   ers - vector with the set of expected returns  -- e.g., seq(.05, .12, .005)
  #   ersd - data frme with expected returns and standard deviations by asset class
  #   cormat - correlation matrix for the returns by asset class
  #   aa.lb - vector of asset allocation lower bounds by asset class
  #   aa.ub - vector of asset allocation upper bounds by asset class

  # returns eflist -- an efficient-frontier list

  port <- llply(ers, minvport_bounds, ersd, cormat, aa.lb, aa.ub) # temporary list with results

  ervec <- llply(1:length(port), function(i) return(port[[i]]$er.port)) %>% unlist
  sdvec <- llply(1:length(port), function(i) return(port[[i]]$sd.port)) %>% unlist
  efrontier <- tibble(er=ervec, sd=sdvec)

  eflist <- list()
  eflist$efrontier <- efrontier
  eflist$port <- port

  return(eflist)
}


ef_aa <- function(eflist){
  # get the efficient frontier asset allocations
  # eflist - an efficient-frontier list
  # returns ef.aa - a "long" data frame with the asset allocations for each er and sd combination in eflist

  ef.aa <- ldply(1:nrow(eflist$efrontier), function(i) return(eflist$port[[i]]$portfolio))

  return(ef.aa)
}



#****************************************************************************************************
#                Test out the efficient frontiers ####
#****************************************************************************************************
rfr <- .03
ersd %>%
  mutate(sharpe=(er - rfr) / sd) %>%
  arrange(-sharpe)

ers <- seq(min(ersd$er), max(ersd$er), .0001)
efnobounds <- efrontier(ers, ersd, cormat)
efbounds <- efrontier(ers, ersd, cormat, aa.lb=0, aa.ub=1)

# create data frames, possible round the sd to smooth out oddities
efnbdf <- efnobounds$efrontier %>% mutate(type="nobounds")
efbdf <- efbounds$efrontier %>% mutate(type="bounds")
efdf <- bind_rows(efnbdf, efbdf)

# optional: get rid of oddities
efdf2 <- efdf %>%
  mutate(sd=round(sd, 4)) %>%
  group_by(sd, type) %>%
  summarise(er=max(er))
efdf2 %>% filter(sd<.035) %>% arrange(type, sd)


efdf2 %>%
  ggplot(aes(x=sd, y=er, colour=type)) +
  geom_point(size=.2) +
  geom_line(size=.5) +
  scale_x_continuous(breaks=seq(0, .5, .02), limits=c(.02, .28))

p <- ggplot(data=efdf2) +
  geom_point(aes(x=sd, y=er, colour=type), size=.2) +
  geom_line(aes(x=sd, y=er, colour=type), size=.5) +
  scale_x_continuous(name="Standard deviation", breaks=seq(0, .5, .02), limits=c(0, .26)) +
  scale_y_continuous(name="Expected return", breaks=seq(0, 30, .01), limits=c(0, NA)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept = .075, linetype="dashed") +
  geom_vline(xintercept = .0296, linetype="dashed") +
  annotate("text", x=.14, y=.077, label="7.5% expected return") +
  geom_point(data=ersd, aes(x=sd, y=er)) +
  annotate("text", ersd$sd[1], ersd$er[1],label=ersd$class[1], hjust=.5, vjust=1) +
  annotate("text", ersd$sd[2], ersd$er[2],label=ersd$class[2], hjust=.5, vjust=-.5) +
  annotate("text", ersd$sd[3], ersd$er[3],label=ersd$class[3], hjust=.5, vjust=1) +
  annotate("text", ersd$sd[4], ersd$er[4],label=ersd$class[4], hjust=.5, vjust=1) +
  annotate("text", ersd$sd[5], ersd$er[5],label=ersd$class[5], hjust=.5, vjust=1) +
  annotate("text", ersd$sd[6], ersd$er[6],label=ersd$class[6], hjust=.5, vjust=1) +
  ggtitle("Stalebrink assumptions and calculated Stalebrink efficient frontier")
p
ggsave("./results/stalebrink_efrontier.png", p, width=12, height=8)



pfolio.nobounds <- ef_aa(efnobounds)
pfolio.bounds <- ef_aa(efbounds)

ersd
asset <- "stocks"
asset <- "bonds.dom"
asset <- "bonds.intl"
asset <- "re"
asset <- "alts"
asset <- "cash"

bind_rows(pfolio.nobounds %>% mutate(type="nobounds"),
          pfolio.bounds %>% mutate(type="bounds")) %>%
  ggplot(aes(x=per, y=asset.weight, colour=type)) +
  scale_x_continuous(name="Portfolio expected return", breaks=seq(0, 20, .01)) +
  scale_y_continuous(breaks=seq(-1, 2, .25)) +
  geom_point(size=.2) +
  geom_line(size=.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~class, ncol=3) +
  ggtitle("Asset weights at the efficient frontier, unbounded and bounded (no shorting or leverage)",
          subtitle="Stalebrink capital market assumptions")

bind_rows(pfolio.nobounds %>% mutate(type="nobounds"),
          pfolio.bounds %>% mutate(type="bounds")) %>%
  ggplot(aes(x=psd, y=asset.weight, colour=type)) +
  scale_x_continuous(name="Portfolio standard deviation", breaks=seq(0, 20, .01)) +
  scale_y_continuous(breaks=seq(-1, 2, .25)) +
  geom_point(size=.2) +
  geom_line(size=.8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~class, ncol=3) +
  ggtitle("Asset weights at the efficient frontier, unbounded and bounded (no shorting or leverage)",
          subtitle="Stalebrink capital market assumptions")

# bind_rows(pfolio.nobounds %>% mutate(type="nobounds"),
#           pfolio.bounds %>% mutate(type="bounds")) %>%
#   filter(class==asset) %>%
#   ggplot(aes(x=per, y=asset.weight, colour=type)) +
#   geom_point(size=.2) +
#   geom_line(size=.5) +
#   ggtitle(asset)


# pfolio %>%
#   filter(class==asset) %>%
#   ggplot(aes(per, asset.weight)) +
#   geom_point() +
#   geom_line()


#****************************************************************************************************
#                Get data - create ersd and the correlation matrix cormat ####
#****************************************************************************************************
fn <- "CapitalMarketAssumptions(1).xlsx"
cmas <- c("stalebrink", "rvk", "horizon10year2017")
df <- read_excel(paste0("./data-raw/", fn), sheet=cmas[1], skip=2)
df

ersd <- df %>%
  select(1:3) %>%
  mutate_at(vars(er, sd), funs(as.numeric))
ersd

lowtri <- df %>%
  select(-c(1:4)) %>%
  select(class=1, everything())
lowtri
lowtri.m <- as.matrix(lowtri %>% select(-1))
rownames(lowtri.m) <- lowtri$class
rownames(lowtri.m) == colnames(lowtri.m)
lowtri.m

cormat <- getcormat.lt(lowtri.m)
cormat


# minvport_bounds(.09, ersd, cormat, aa.lb=rep(0, nrow(ersd)), aa.ub=rep(1, nrow(ersd)))
minvport_bounds(.09, ersd, cormat)
minvport_bounds(.09, ersd, cormat, aa.lb=0, aa.ub=1)








#****************************************************************************************************
#                Check data ####
#****************************************************************************************************
covmat <- getcovmat(cormat, ersd$sd)
covmat

# Covariance matrix must be positive definite or else a variance can be negative
# Note that "A correlation matrix is a symmetric positive semidefinite matrix with unit diagonal" per
# Higham 2001 (http://eprints.ma.man.ac.uk/232/01/covered/MIMS_ep2006_70.pdf).

# verify that the covmat is positive definite
# if symmetric PD, then all eigen values are positive
eigen(cormat)$values
eigen(covmat)$values
cormat
covmat
chol(covmat)
diag(chol(covmat))
any(diag(chol(covmat)) <= 0) # if true, covmat is NOT PSD


#****************************************************************************************************
#                Adjust to positive definite ONLY IF NEEDED ####
#****************************************************************************************************
cormatadj <- nearPD(cormat, corr=TRUE)$mat

# verify that the adjusted covmat is positive definite
cormatadj
covmatadj <- getcovmat(cormatadj, ersd$sd)
covmatadj

eigen(cormatadj)$values
eigen(covmatadj)$values

# chol(covmatadj)
# diag(chol(covmatadj))
# any(diag(chol(covmatadj)) <= 0)

# compare the unadjusted and adjusted RVK cormats
cormat
cormatadj %>% round(., 2)
(cormatadj %>% round(., 2)) - cormat

cormat <- cormatadj
covmat <- covmatadj

