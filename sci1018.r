rm(list = ls())
setwd(dirname("G:/Mon disque/Z/SCI1018 trop de documentation/sci1018_code/"))
library(nortest)

coteT <- function(moy, sd, n, mu){
  (moy - mu)/(sd/sqrt(n))
}


tStudentIC <- function(moy, sd, n, IC) {
  df = n - 1
  p = (100 - IC) / 2 / 100 #haf
  
  t.stat <- qt(p, df) * (-1) #valeur positive
  SE <- sd / sqrt(n) # ecart-type standardisé
  IC.inf <- moy - t.stat * SE
  IC.sup <- moy + t.stat * SE
  indice = NULL
  if(temp[length(temp)]>IC.sup){indice="Hausse"}else if(temp[length(temp)]>IC.inf){indice="Baisse"} else {indice="Neutre"}
  
  out <- data.frame(IC.inf, IC.sup,t.stat,df,indice)
  
  plot(x=0.5,y=moy,main =paste("Moyenne ± IC à 95% de", round(IC.inf), "à",round(IC.sup)),ylim =range(c(IC.inf,IC.sup)),xaxt ="n")
  segments(x0=0.5,x1=0.5,
           y0 =IC.inf,y1=IC.sup)
  arrows(
    x0 = 0.5,
    x1 = 0.5,
    y0 = IC.inf,
    y1 = IC.sup,
    length = 0.05,
    code = 3,
    angle = 90
  )
  return(out)
}

testZ <- function(x, x.moy, n.moy, n.sd) {
  x.sd <- n.sd / sqrt(x) #ecart-type de l'echantillon
  x.z <- (x.moy - n.moy) / x.sd
  x.p <- 1 - pnorm(x.z)
  print(x.p)
}

mesureDispersion <- function(x) {
  sse <- sum((x - mean(x)) ^ 2)
  print(sse)
}

mesureTendanceCentrale <- function(x) {
  x.arith.moy <- sum(x) / length(x)
  x.geo.moy <- prod(x) ^ (1 / length(x))
  x <- data.frame(x.arith.moy, x.geo.moy)
  print(x)
}

tableauFreq <- function(matrix) {
  print(chisq.test(matrix))
  print(
    "on rejette H0 ne change pas La proportion des véhicules est indépendante de l'arrondissement de la ville.
  #parce que p-value < 0.0001"
  )
}

testBilateralUnGroupe <- function(x, mu) {
  x.t <-
    (mean(x) - mu) / (sd(x) / sqrt(length(x))) #(sd(x)/sqrt(length(x)) erreur-type
  x.df <- length(x) - 1
  x.p.d <-  1 - pt(q = x.t, df = x.df)
  x.p.g <-  pt(q = -x.t, df = x.df) #mettre un -
  x.p <- x.p.g + x.p.d
  #probabilité de x.p d'observer une valeur supérieure ou égale à x.t
  print(t.test(x = x, mu = mu))
  print("rejette H0 ne change pas x = x.moy parce que x.p < 0.05")
}

distributionNormale <- function(mu, mu.moy, mu.sd) {
  p <- dnorm(mu, mu.moy, mu.sd) #densité de probabilité
  z <- (mu - mu.moy) / mu.sd #d'écart-type de la moyenne
  pc <-
    pnorm(mu, mu.moy, mu.sd)#probabilité cumulative p.113 pp autrement 1-pc
  prob <- data.frame(p, z, pc)
  colnames(prob) <-
    c("densite de probabilite",
      "ecart type de la moyenne",
      "probabilite cumulative")
  print(prob)
  
  mean = mu.moy
  sd = mu.sd
  lb = 0
  ub = mu
  
  mu <- seq(-3, 3, length = 1000) * sd + mean
  hmu <- dnorm(mu, mean, sd)
  
  plot(
    mu,
    hmu,
    type = "n",
    mulab = "Variables",
    ylab = "Densité",
    main = "Distribution normale",
    amues = TRUE
  )
  
  i <- mu <= ub #mu >= lb &
  lines(mu, hmu)
  abline(v = ub, col = 'red')
}

distributionNormaleCentreeReduite <- function(x, x.moy, x.sd) {
  p <- dnorm(x, x.moy, x.sd) #densité de probabilité
  z <- (x - x.moy) / x.sd #d'écart-type de la moyenne
  pc <-
    pnorm(x, x.moy, x.sd)#probabilité cumulative p.113 pp autrement 1-pc
  prob <- data.frame(p, z, pc)
  colnames(prob) <-
    c("densite de probabilite",
      "ecart type de la moyenne",
      "probabilite cumulative")
  print(prob)
  
  mean = 0
  sd = 1
  lb = 0
  ub = z
  
  x <- seq(-3, 3, length = 1000) * sd + mean
  hx <- dnorm(x, mean, sd)
  
  plot(
    x,
    hx,
    type = "n",
    xlab = "Variables",
    ylab = "Densité",
    main = "Distribution normale centrée réduite N(0,1)",
    axes = TRUE
  )
  
  i <- x <= ub #x >= lb &
  lines(x, hx)
  abline(v = ub, col = 'red')
}

probabiliteCumZ <- function(x.moy, x.sd, v1, v2) {
  mean = x.moy
  sd = x.sd
  lb = v1
  ub = v2
  x <- seq(-4, 4, length = 100) * sd + mean
  pz2 <- pnorm(ub, mean, sd)
  pz1 <- pnorm(lb, mean, sd)
  z2 <- (ub - x.moy) / x.sd
  z1 <- (lb - x.moy) / x.sd
  area <- pz2 - pz1
  result <-
    paste("P(", lb, "< x <", ub, ") =", signif(area, digits = 3))
  res <- data.frame(pz1, pz2, result, z1, z2)
  print(res)
  
  mean = 0
  sd = 1
  lb = z1
  ub = z2
  
  x <- seq(-4, 4, length = 100) * sd + mean
  hx <- dnorm(x, mean, sd)
  
  plot(
    x,
    hx,
    type = "n",
    xlab = "Variables",
    ylab = "Densité",
    axes = TRUE
  )
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb, x[i], ub), c(0, hx[i], 0), col = "red")
}

# x[x$Type == groupe1, data]
# matrix <-
#   matrix(
#     data = c(125, 223, 62, 180, 175, 77, 238, 120),
#     nrow = 2,
#     ncol = 4,
#     byrow = TRUE
#   )
# colnames(matrix) <- c("A", "B", "C", "D")
# rownames(matrix) <- c("Domestique", "Importe")

# mesureTendanceCentrale(c(12.3,4.2,5.9, 9.1,3.3,5.1,7.3,3.8,8.0,6.1)) #x #90
# mesureTendanceCentrale(c(10,1,1000,1,10)) #x #90

# mesureDispersion(c(1.3,4.5,4.1,2.1,5.0,1.9)) #x #95 #sse

# distributionNormale(3.4,4.1,1.5) #x,x.moy,x.sd #17

# distributionNormaleCentreeReduite(22.6,14.2,5.05) ##x,x.moy,x.sd #111

# probabiliteCumZ(170,8,165,180) #113

# testZ(9, 50, 47.5, 12.89) #118 #probabilité que l'échantillon soient plus grand que 50mm

# coteT(2.0687, 2.5876, 721, 2.3) #ref 68 SPSS

temp <- c(44,45,46,46,47,46,48)
i <- c(1:7)
doOne <-tStudentIC(mean(temp), sd(temp), length(temp), 95)
tmp <- data.frame(i,temp)
require(stats)
reg <- lm(tmp$temp ~ tmp$i)
coeff = coefficients(reg)
#Equationdeladroitederegression:
eq = paste0("y=", round(coeff[2], 1), "x+", round(coeff[1], 1))
#Graphes
plot(tmp, main = eq, col = "red")
abline(reg, col = "blue")
segments(x0=tmp$i,y0=tmp$temp,x1=tmp$i,
         y1 =fitted(reg),col="blue")
#
# print(doOne$indice)
# temp <- c(45,44,43,42,44,43,42)
# doTwo<-tStudentIC(mean(temp), sd(temp), length(temp), 95)
# print(doTwo$indice)
# tmp <- data.frame(i,temp)
# require(stats)
# reg <- lm(tmp$temp ~ tmp$i)
# coeff = coefficients(reg)
# #Equationdeladroitederegression:
# eq = paste0("y=", round(coeff[2], 1), "x+", round(coeff[1], 1))
# #Graphes
# plot(tmp, main = eq, col = "red")
# abline(reg, col = "blue")
# segments(x0=tmp$i,y0=tmp$temp,x1=tmp$i,
#          y1 =fitted(reg),col="blue")

# tStudentIC(mean(res),sd(res),length(res),95) #158

# 222 testBilateralDeuxGroupes = ou <> eleves.txt
# t.test(x$Data ~ x$Type, var.equal = TRUE)
# res.aide <- Groupe2 - mean(Groupe2)
# res.temoins <- Groupe1 - mean(Groupe1)
# x$res <- c(res.aide, res.temoins)
# 174 normalite
# library(nortest)
# print(ad.test(x$res))
# qqnorm(x$res)
# #onajouteladroitethéorique
# qqline(x$res)
# boxplot(res ~ Type, data = x)

# 235 testUnilateraleDeuxGroupe < > temps-2.txt x est plus grand que y, changement
# t.test(x = x,y = y,data = data,var.equal = eq,alternative = alternative)

# 243 testUnilateraleUnGroupeApparieDependant x-y est plus petit que zéro, changement
#     t.test(x[[greater]], x[[lower]], paired = TRUE, alternative = hypoth))
#     res <- x$Diff - mean(x$Diff)
#     mean.diff <- mean(x$Diff)
#     sd.diff <- sd(x$Diff)
#     SE.diff <- sd.diff / sqrt(nrow(x))
#     boxplot(x[[greater]],x[[lower]],names=c(greater,lower))

# tableauFreq(matrix) #matrix #260
