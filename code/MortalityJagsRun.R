
dir = "C:/Users/dmbell/Box/01. david.bell Workspace/Perret/MortModel"

dat = read.csv(file.path(dir, "dummydata_fordave.csv"), header = TRUE)

##libraries
library("rjags")
library("coda")

##reorganize data
#list of inputs

y = dat$SURV

xx = cbind(intercept = 1,
           PREVDIA = dat$PREVDIA,
           PREVCR = dat$PREV_CR,
           PREVDAM = dat$prev.damage,
           PREVBAH = dat$PREV_BAH,
           FIREPROP = dat$area.fire.prop)

dy = dat$REMPER

ecoregionUniq = unique(dat$ECOSUBCD)
ecoregion = match(dat$ECOSUBCD,ecoregionUniq)

data = list('Y' = y,
            'X' = xx,
            'DY' = dy,
            'ECOREGION' = ecoregion,
            'm' = length(y),
            'k' = ncol(xx),
            'r' = length(ecoregionUniq),
            'priorB' = rep(0,ncol(xx)),
            'v1' = 2, #regression variance prior 1
            'v2' = 2) #regression variance prior 2
            
#list of initial parameter values
init = list('B' = rep(0, ncol(xx)),
            err = rep(0,length(y)))

#jags parameters
nchains = 4
nadapt = 10000
nupdate = 10000
ngibbs = 20000

#point to model
model = file.path(dir, "Mort_Model_20240618.R")

t0 = Sys.time()
#run model
jags <- jags.model(model,                      #name of model file
                   data = data,
                   inits = init,
                   n.chains = nchains,                    #Number of independent chains to run
                   n.adapt = nadapt)                   #Number of iterations for adaptation (i.e., burnin)


t1 = Sys.time()

message(paste0("Initial run: ",t1-t0))

update(jags,nupdate)

t2 = Sys.time()

message(paste0("Update run: ",t2-t1))

#get parameters
Bsamp <- coda.samples(jags,c('B','sigB','sigA','sig','alpha'),n.iter=ngibbs,thin=5)

t3 = Sys.time()

message(paste0("Parameter estimates: ",t3-t2))

#plot the distributions of parameter estimates, including traces for assessing model convergence
pdf(file.path(dir,
              paste0('ParmeterTraces_',"DaveTest",'.pdf')),
    width=8,height=10)
plot(Bsamp)
dev.off()

#excluding the indicators for rubin-gelman diagnostic of convergence as they are sometimes error prone
#values less that ~1.2 indicate convergence
RGsamp = coda.samples(jags,c('B','sigB','sigA','sig','alpha'),n.iter=ngibbs,thin=5)

t4 = Sys.time()

message(paste0("Rubin-Gelman run: ",t4-t3))

pdf(file.path(dir,"output",
              paste0('Gelman-Rubin_',spp,'.pdf')),
    width=8,height=10)
gelman.plot(RGsamp)
dev.off()

message(paste0("All runs: ",t4-t0))


