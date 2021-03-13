
mammals <- read.delim("mammal_activity.txt", header = T)

for (i in seq_along(mammals)) {
  if (is.character(mammals[[i]])) {
    mammals[[i]] <- as.factor(mammals[[i]])
  } else {
    next
  }
}

table(mammals$species, mammals$forest)

library(overlap)
library(boot)
library(circular)

timeRad <- mammals$time * 2 * pi

levels(mammals$species)

# ================== Dasypus =======================
###########overlap dasypus
dasypus_A <- timeRad[mammals$forest == "andean" & mammals$species == "dasypus"]
dasypus_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "dasypus"]
min(length(dasypus_A), length(dasypus_R))
overlapEst(dasypus_A, dasypus_R)


########plot activity overlap
overlapPlot(dasypus_A, dasypus_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Dasypus novemcinctus")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
dasypus_bootA <- resample(dasypus_A, 10000)
dasypus_bootR <- resample(dasypus_R, 10000)
dim(dasypus_bootA); dim(dasypus_bootR)

####### overlap estimates by boostrap
dasypus_overlapp <- bootEst(dasypus_bootA, dasypus_bootR, adjust = c(1, 1, 1))
dim(dasypus_overlapp)
dasypues_mean <- colMeans(dasypus_overlapp)
dasypues_mean


######## calculation of CI
dasypusCI <- dasypus_overlapp[, 1]
bootCI(overlapEst(dasypus_A, dasypus_R)[1], dasypusCI)

bootCIlogit(overlapEst(dasypus_A, dasypus_R)[1], dasypusCI)


# ================== Dasypus =======================
###########overlap dasypus
dasypus_A <- timeRad[mammals$forest == "andean" & mammals$species == "dasypus"]
dasypus_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "dasypus"]
min(length(dasypus_A), length(dasypus_R))
overlapEst(dasypus_A, dasypus_R)


########plot activity overlap
overlapPlot(dasypus_A, dasypus_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Dasypus novemcinctus")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
dasypus_bootA <- resample(dasypus_A, 10000)
dasypus_bootR <- resample(dasypus_R, 10000)
dim(dasypus_bootA); dim(dasypus_bootR)

####### overlap estimates by boostrap
dasypus_overlapp <- bootEst(dasypus_bootA, dasypus_bootR, adjust = c(1, 1, 1))
dim(dasypus_overlapp)
dasypues_mean <- colMeans(dasypus_overlapp)
dasypues_mean


######## calculation of CI
dasypusCI <- dasypus_overlapp[, 1]
bootCI(overlapEst(dasypus_A, dasypus_R)[1], dasypusCI)

bootCIlogit(overlapEst(dasypus_A, dasypus_R)[1], dasypusCI)
