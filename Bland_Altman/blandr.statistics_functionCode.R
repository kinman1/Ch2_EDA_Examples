

blandr.statistics
function (method1, method2, sig.level = 0.95, LoA.mode = 1) 
{
  ba.data <- blandr.data.preparation(method1, method2, sig.level)
  means <- (ba.data$method1 + ba.data$method2)/2
  differences <- ba.data$method1 - ba.data$method2
  bias <- mean(differences)
  biasStdDev <- sd(differences)
  alpha <- 1 - sig.level
  sig.level.two.tailed <- 1 - (alpha/2)
  sig.level.convert.to.z <- qnorm(sig.level.two.tailed)
  if (LoA.mode == 2) {
    LoA.multiplier <- 2
  }
  else {
    LoA.multiplier <- 1.96
  }
  upperLOA <- bias + (LoA.multiplier * biasStdDev)
  lowerLOA <- bias - (LoA.multiplier * biasStdDev)
  biasSEM <- sd(differences)/sqrt(length(differences))
  biasCI <- qt(sig.level.two.tailed, df = length(differences) - 
                 1) * biasSEM
  biasUpperCI <- bias + biasCI
  biasLowerCI <- bias - biasCI
  LOAVariance <- ((1/length(differences)) + ((sig.level.convert.to.z^2)/(2 * 
                                                                           (length(differences) - 1)))) * biasStdDev^2
  LOA_SEM <- sqrt(LOAVariance)
  LOA_CI <- qt(sig.level.two.tailed, df = length(differences) - 
                 1) * LOA_SEM
  upperLOA_upperCI <- upperLOA + LOA_CI
  upperLOA_lowerCI <- upperLOA - LOA_CI
  lowerLOA_upperCI <- lowerLOA + LOA_CI
  lowerLOA_lowerCI <- lowerLOA - LOA_CI
  proportion <- differences/means * 100
  no.of.observations <- length(means)
  m <- lm(differences ~ means)
  a <- signif(coef(m)[1], digits = 2)
  b <- signif(coef(m)[2], digits = 2)
  regression.equation <- paste("y(differences) = ", b, " x(means) + ", 
                               a, sep = "")
  return(list(means = means, differences = differences, method1 = method1, 
              method2 = method2, sig.level = sig.level, sig.level.convert.to.z = sig.level.convert.to.z, 
              bias = bias, biasUpperCI = biasUpperCI, biasLowerCI = biasLowerCI, 
              biasStdDev = biasStdDev, biasSEM = biasSEM, LOA_SEM = LOA_SEM, 
              upperLOA = upperLOA, upperLOA_upperCI = upperLOA_upperCI, 
              upperLOA_lowerCI = upperLOA_lowerCI, lowerLOA = lowerLOA, 
              lowerLOA_upperCI = lowerLOA_upperCI, lowerLOA_lowerCI = lowerLOA_lowerCI, 
              proportion = proportion, no.of.observations = no.of.observations, 
              regression.equation = regression.equation, regression.fixed.slope = b, 
              regression.fixed.intercept = a))
}