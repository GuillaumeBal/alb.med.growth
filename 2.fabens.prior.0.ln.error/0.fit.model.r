# basic set up =====================================================================

rm(list = ls())

wd <- "C:/Users/gbal/Desktop/alb.med.growth/2.fabens.prior.0.ln.error"
setwd(wd)

require(R2jags)
require(openxlsx)

# vector ages for pred
ages.pred <- 0:20

# get data =====================================================================

data.growth <- read.xlsx('age.length.data.comb.xlsx')
head(data.growth)

#expand table based on frequecny
data.growth.exp <- data.growth[rep(row.names(data.growth), data.growth$frequency), c(1, 4)]
dim(data.growth.exp)

#plot data
plot(data.growth.exp$age.abs, data.growth.exp$length, pch = '.',
     xlab = 'Age (year)', ylab = 'Length (cm)',
     ylim = c(0, 100))
lines(lowess(data.growth.exp$age.abs, data.growth.exp$length, f = .1), col = 'red')

# run jags ======================================================================

# data
jags.data <- list('age' = data.growth.exp$age.abs,
                  'length' = data.growth.exp$length,
                  'n.fish' = dim(data.growth.exp)[1],
                  'ages.pred' = ages.pred, 
                  'n.ages.pred' = length(ages.pred),
                  'mu.l0' = 3, 'cv.l0' = .1)

# parameters saved
jags.par <- c('k', 'l.inf', 't0', 'sd.log.length',
              'length.pred')

# model file name
jags.model <- 'model.fabens.simple.txt'

# MCMC settings
mcmc.burn <- as.integer(10000)
mcmc.chainLength <- as.integer(20000)  # burn-in plus post-burn
mcmc.thin = 1
mcmc.chains = 3 # needs to be at least 2 for DIC

### run model
jags.outputs <- jags(jags.data, parameters.to.save = jags.par, model.file = jags.model, 
                     n.chains = mcmc.chains, n.burnin = mcmc.burn, n.thin = mcmc.thin, n.iter = mcmc.chainLength,
                     refresh = mcmc.burn / 20,
                     #digits = 1,
                     #inits = jags.inits,
                     DIC = TRUE) 

# jags outputs =======================================================================

print(jags.outputs)
attach.jags(jags.outputs)

pdf('fit.pdf')

# hist posteriors param
par(mfrow = c(2, 2))
hist(k)
hist(l.inf)
hist(t0)
hist(sd.log.length)

# extract summary info
jags.summary <- jags.outputs$BUGSoutput$summary

# plot adjusted vs observed
length.pred.loc <- which(substring(rownames(jags.summary ), 1, 11) == 'length.pred')
summ.length.pred <- jags.summary[length.pred.loc, 3:7]

par(mfrow = c(1, 1))
plot(data.growth.exp$age.abs, data.growth.exp$length, pch = '.', col = 'red',
     ylim = c(0, max(data.growth.exp$length)),
     xlab = 'Age (year)', ylab = 'Length (cm)')
polygon(c(ages.pred, rev(ages.pred)), c(summ.length.pred[ , 1], rev(summ.length.pred[ , 5])),
        col = 'lightgrey', border = 'lightgrey')
polygon(c(ages.pred, rev(ages.pred)), c(summ.length.pred[ , 2], rev(summ.length.pred[ , 4])),
        col = 'grey', border = 'grey')
lines(ages.pred, summ.length.pred[ , 3], lwd = 2)
points(data.growth.exp$age.abs, data.growth.exp$length, pch = '.', col = 'red', cex = 3)

dev.off()

# save results and diagnostics ==================================================================

# create save folder 
save.folder <- paste0(getwd(), '/jags.results/')
dir.create(save.folder,recursive = TRUE )

# create mcmc chain for each parameter
attach.jags(jags.outputs, overwrite = TRUE)

# save summary of parameters estimates
write.table(jags.outputs$BUGSoutput$summary, file ="0.param.summary.txt")

# save mcmc chains
list.var <- c(dimnames(jags.outputs$BUGSoutput$sims.array)[3])[[1]] #list des var
list.var <- gsub("[^[:alnum:]]", "", list.var)
for (i in 1:dim(jags.outputs$BUGSoutput$sims.array)[3]){
  assign(paste0(list.var[i], "1"), mcmc(jags.outputs$BUGSoutput$sims.array[ , 1, i]))
  assign(paste0(list.var[i], "2"), mcmc(jags.outputs$BUGSoutput$sims.array[ , 2, i]))
  assign(paste0(list.var[i], "3"), mcmc(jags.outputs$BUGSoutput$sims.array[ , 3, i]))
  assign(list.var[i], mcmc.list(list(eval(parse(text = paste0(list.var[i], "1"))), 
                                     eval(parse(text = paste0(list.var[i], "2"))),
                                     eval(parse(text = paste0(list.var[i], "3"))))))
  write.table(eval(parse(text = paste0(list.var[i], "3"))),file = paste0(save.folder, list.var[i], ".txt"))
  #uncomment previous line if you wanna store mcmc chain
}

### trace gelman plot and save in a pdf
pdf(file = paste0(save.folder, "/bgr.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
par(mfrow = c(2, 3))
for (i in 1:length(list.var)){
  gelman.plot(eval(parse(text = list.var[i])), main = list.var[i])
}
dev.off()

### trace autocor plot and save in a pdf
pdf(file= paste0(save.folder, "/ac.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
par(mfrow = c(2, 3))
for (i in 1:length(list.var)){
  autocorr.plot(eval(parse(text = list.var[i])), main = list.var[i])
}
dev.off()

#trace density plot and save in a pdf
pdf(file = paste0(save.folder, "/density.pdf"), onefile=TRUE, height = 8.25, width = 11.6)
par(mfrow = c(2, 3))
for (i in 1:length(list.var)){
  densplot(eval(parse(text = list.var[i])), main = list.var[i])
}
dev.off()

#trace history plot and save in a pdf
pdf(file = paste0(save.folder, "/history.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
par(mfrow = c(2, 3))
for (i in 1:length(list.var)){
  traceplot(eval(parse(text = list.var[i])), main = list.var[i])
}
dev.off()