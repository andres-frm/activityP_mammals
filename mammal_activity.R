 
# =============================================================-#
# Ramírez-Mejíaa AF and Sánchez F (2016)                        #
# Activity patterns and habitat use of mammals in an Andean     #
# forest and a Eucalyptus reforestation in Colombia. Hystrix 27 #
# =============================================================-#
# 
# If you are an RStudio user, press Ctrl + shift + o to display 
# the script outline.

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

################################################################-
# ============== Comparison between forest =====================
################################################################-

# ================== Dasypus novemcinctus =======================
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


# ================== Didelphis pernigra =======================
###########overlap didelphis
didelphis_A <- timeRad[mammals$species == "didelphis"]



########plot activity overlap
densityPlot(didelphis_A, xscale = 24, main = "", rug = T)
title(expression(italic("Didelphis pernigra")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



# ================== Mazama rufina =======================
###########overlap mazama
mazama_A <- timeRad[mammals$forest == "andean" & mammals$species == "mazama"]
mazama_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "mazama"]
min(length(mazama_A), length(mazama_R))
overlapEst(mazama_A, mazama_R)


########plot activity overlap
overlapPlot(mazama_A, mazama_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Mazama rufina")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
mazama_bootA <- resample(mazama_A, 10000)
mazama_bootR <- resample(mazama_R, 10000)
dim(mazama_bootA); dim(mazama_bootR)

####### overlap estimates by boostrap
mazama_overlapp <- bootEst(mazama_bootA, mazama_bootR, adjust = c(1, 1, 1))
dim(mazama_overlapp)
dasypues_mean <- colMeans(mazama_overlapp)
dasypues_mean


######## calculation of CI
mazamaCI <- mazama_overlapp[, 1]
bootCI(overlapEst(mazama_A, mazama_R)[1], mazamaCI)

bootCIlogit(overlapEst(mazama_A, mazama_R)[1], mazamaCI)


# ================== Nasuea nasua =======================
###########overlap nasua
nasua_A <- timeRad[mammals$forest == "andean" & mammals$species == "nasua"]
nasua_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "nasua"]
min(length(nasua_A), length(nasua_R))
overlapEst(nasua_A, nasua_R)


########plot activity overlap
overlapPlot(nasua_A, nasua_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Nasua nasua")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
nasua_bootA <- resample(nasua_A, 10000)
nasua_bootR <- resample(nasua_R, 10000)
dim(nasua_bootA); dim(nasua_bootR)

####### overlap estimates by boostrap
nasua_overlapp <- bootEst(nasua_bootA, nasua_bootR, adjust = c(1, 1, 1))
dim(nasua_overlapp)
dasypues_mean <- colMeans(nasua_overlapp)
dasypues_mean


######## calculation of CI
nasuaCI <- nasua_overlapp[, 1]
bootCI(overlapEst(nasua_A, nasua_R)[1], nasuaCI)

bootCIlogit(overlapEst(nasua_A, nasua_R)[1], nasuaCI)



# ================== Nasuella olivacea =======================
###########overlap nasuella
nasuella_A <- timeRad[mammals$forest == "andean" & mammals$species == "nasuella"]
nasuella_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "nasuella"]
min(length(nasuella_A), length(nasuella_R))
overlapEst(nasuella_A, nasuella_R)


########plot activity overlap
overlapPlot(nasuella_A, nasuella_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Nasuella olivacea")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
nasuella_bootA <- resample(nasuella_A, 10000)
nasuella_bootR <- resample(nasuella_R, 10000)
dim(nasuella_bootA); dim(nasuella_bootR)

####### overlap estimates by boostrap
nasuella_overlapp <- bootEst(nasuella_bootA, nasuella_bootR, adjust = c(1, 1, 1))
dim(nasuella_overlapp)
dasypues_mean <- colMeans(nasuella_overlapp)
dasypues_mean


######## calculation of CI
nasuellaCI <- nasuella_overlapp[, 1]
bootCI(overlapEst(nasuella_A, nasuella_R)[1], nasuellaCI)

bootCIlogit(overlapEst(nasuella_A, nasuella_R)[1], nasuellaCI)


# ================== Sciurus granatensis =======================
###########overlap sciurus
sciurus_A <- timeRad[mammals$forest == "andean" & mammals$species == "sciurus"]
sciurus_R <- timeRad[mammals$forest == "reforestation" & mammals$species == "sciurus"]
min(length(sciurus_A), length(sciurus_R))
overlapEst(sciurus_A, sciurus_R)


########plot activity overlap
overlapPlot(sciurus_A, sciurus_R, xscale = 24, main = "", rug = T)
legend("topright", c("Andean forest", "Reforestation"), lty = c(1,2), col = c(1,4), bty = "n")
title(expression(italic("Sciurus granatensis")))
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

###### bootstrap
sciurus_bootA <- resample(sciurus_A, 10000)
sciurus_bootR <- resample(sciurus_R, 10000)
dim(sciurus_bootA); dim(sciurus_bootR)

####### overlap estimates by boostrap
sciurus_overlapp <- bootEst(sciurus_bootA, sciurus_bootR, adjust = c(1, 1, 1))
dim(sciurus_overlapp)
dasypues_mean <- colMeans(sciurus_overlapp)
dasypues_mean


######## calculation of CI
sciurusCI <- sciurus_overlapp[, 1]
bootCI(overlapEst(sciurus_A, sciurus_R)[1], sciurusCI)

bootCIlogit(overlapEst(sciurus_A, sciurus_R)[1], sciurusCI)



################################################################-
# ============== Species comparison =============================
################################################################-

# ================== N. olivacea - N. nasua, andean=======================
###########overlap 

overlapEst(nasua_A, nasuella_A)

########plot activity overlap
overlapPlot(nasua_A, nasuella_A, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. nasua")),
                     expression(italic("N. olivacea"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Andean forest")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)

####### overlap estimates by boostrap

NasuaNasuella_overlapp_A <- bootEst(nasua_bootA, nasuella_bootA, adjust = c(1, 1, 1))
dim(NasuaNasuella_overlapp_A)
NasuaNasuella_mean_A <- colMeans(NasuaNasuella_overlapp_A)
NasuaNasuella_mean_A


######## calculation of CI
NasuaNasuellaCI_A <- NasuaNasuella_overlapp_A[, 1]
bootCI(overlapEst(nasua_A, nasuella_A)[1], NasuaNasuellaCI_A)
bootCIlogit(overlapEst(nasua_A, nasuella_A)[1], NasuaNasuellaCI_A)

# ================== N. olivacea - N. nasua, reforestation=======================
###########overlap 

overlapEst(nasua_R, nasuella_R)

########plot activity overlap
overlapPlot(nasua_R, nasuella_R, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. nasua")),
                     expression(italic("N. olivacea"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Reforestation")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



####### overlap estimates by boostrap

NasuaNasuella_overlapp_R <- bootEst(nasua_bootR, nasuella_bootR, adjust = c(1, 1, 1))
dim(NasuaNasuella_overlapp_R)
NasuaNasuella_mean_R <- colMeans(NasuaNasuella_overlapp_R)
NasuaNasuella_mean_R


######## calculation of CI
NasuaNasuellaCI_R <- NasuaNasuella_overlapp_R[, 1]
bootCI(overlapEst(nasua_R, nasuella_R)[1], NasuaNasuellaCI_R)
bootCIlogit(overlapEst(nasua_R, nasuella_R)[1], NasuaNasuellaCI_R)


# ================== N. olivacea - D. novemcinctus, reforestation=======================
###########overlap 

overlapEst(nasuella_R, dasypus_R)

########plot activity overlap
overlapPlot(nasuella_R, dasypus_R, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. olivacea")),
                     expression(italic("D. novemcinctus"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Reforestation")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



####### overlap estimates by boostrap

DasypusNasuella_overlapp_R <- bootEst(dasypus_bootR, nasuella_bootR, adjust = c(1, 1, 1))
dim(DasypusNasuella_overlapp_R)
DasypusNasuella_mean_R <- colMeans(DasypusNasuella_overlapp_R)
DasypusNasuella_mean_R


######## calculation of CI
DasypusNasuellaCI_R <- DasypusNasuella_overlapp_R[, 1]
bootCI(overlapEst(dasypus_R, nasuella_R)[1], DasypusNasuellaCI_R)
bootCIlogit(overlapEst(dasypus_R, nasuella_R)[1], DasypusNasuellaCI_R)


# ================== N. olivacea - D. novemcinctus, andean =======================
###########overlap 

overlapEst(nasuella_A, dasypus_A)

########plot activity overlap
overlapPlot(nasuella_A, dasypus_A, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. olivacea")),
                     expression(italic("D. novemcinctus"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Andean Forest")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



####### overlap estimates by boostrap

DasypusNasuella_overlapp_A <- bootEst(dasypus_bootA, nasuella_bootA, adjust = c(1, 1, 1))
dim(DasypusNasuella_overlapp_A)
DasypusNasuella_mean_A <- colMeans(DasypusNasuella_overlapp_A)
DasypusNasuella_mean_A


######## calculation of CI
DasypusNasuellaCI_A <- DasypusNasuella_overlapp_A[, 1]
bootCI(overlapEst(dasypus_A, nasuella_A)[1], DasypusNasuellaCI_A)
bootCIlogit(overlapEst(dasypus_A, nasuella_A)[1], DasypusNasuellaCI_A)



# ================== N. nasua - D. novemcinctus, andean =======================
###########overlap 

overlapEst(nasua_A, dasypus_A)

########plot activity overlap
overlapPlot(nasua_A, dasypus_A, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. olivacea")),
                     expression(italic("D. novemcinctus"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Andean Forest")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



####### overlap estimates by boostrap

DasypusNasua_overlapp_A <- bootEst(dasypus_bootA, nasua_bootA, adjust = c(1, 1, 1))
dim(DasypusNasua_overlapp_A)
DasypusNasua_mean_A <- colMeans(DasypusNasua_overlapp_A)
DasypusNasua_mean_A


######## calculation of CI
DasypusNasuaCI_A <- DasypusNasua_overlapp_A[, 1]
bootCI(overlapEst(dasypus_A, nasua_A)[1], DasypusNasuaCI_A)
bootCIlogit(overlapEst(dasypus_A, nasua_A)[1], DasypusNasuaCI_A)


# ================== N. nasua - D. novemcinctus, reforestation =======================
###########overlap 

overlapEst(nasua_R, dasypus_R)

########plot activity overlap
overlapPlot(nasua_R, dasypus_R, xscale = 24, main = "", rug = T)
legend("topright", c(expression(italic("N. olivacea")),
                     expression(italic("D. novemcinctus"))),
       lty = c(1,2), col = c(1,4), bty = "n")
title("Reforestation")
abline(v = 6, lty = "dashed", col = "gray", lwd = 2)
abline(v = 18, lty = "dashed", col = "gray", lwd = 2)



####### overlap estimates by boostrap

DasypusNasua_overlapp_R <- bootEst(dasypus_bootR, nasua_bootR, adjust = c(1, 1, 1))
dim(DasypusNasua_overlapp_R)
DasypusNasua_mean_R <- colMeans(DasypusNasua_overlapp_R)
DasypusNasua_mean_R


######## calculation of CI
DasypusNasuaCI_R <- DasypusNasua_overlapp_R[, 1]
bootCI(overlapEst(dasypus_R, nasua_R)[1], DasypusNasuaCI_R)
bootCIlogit(overlapEst(dasypus_R, nasua_R)[1], DasypusNasuaCI_R)



