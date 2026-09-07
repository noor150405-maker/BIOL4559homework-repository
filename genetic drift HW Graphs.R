library(ggplot2)
library(data.table)
library(foreach)
library(doMC)
registerDoMC(2)
nGens <- 100
popSizes <- c(100, 500, 1000, 5000, 10000, 500000, 1000000)
startingAlleleFreq <- 0.5
nReplicates <- 20
tmp <- function(popSize) {
   ng <- numeric(nGens)
  ng[1] <- startingAlleleFreq
  for(i in 2:nGens) {
    ng[i] <- rbinom(1, popSize, ng[i-1]) / popSize
  }
  data.table(
    gen = 1:nGens,
    ng = ng,
    popSize = popSize
  )
}
test <- tmp(100)
summary(test$ng)
results <- foreach(
  popSize.i = popSizes,
  .combine = "rbind"
) %do% {
  out <- data.table()
  for(rep in 1:nReplicates) {
    temp <- tmp(popSize.i)
    temp$replicate <- rep
    out <- rbind(out, temp)
  }
  out
}
nrow(results)
summary(results$ng)
ggplot(
  results,
  aes(
    x = gen,
    y = ng,
    group = interaction(popSize, replicate)
  )
) +
  geom_line() +
  facet_wrap(~popSize) +
  ylim(0,1) +
  labs(
    x = "Generation",
    y = "Allele Frequency"
  )
