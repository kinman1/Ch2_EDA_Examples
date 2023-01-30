

bland.altman.stats
function (group1, group2, two = 1.96, mode = 1, conf.int = 0.95) 
{
  if (length(group1) != length(group2)) 
    stop("Error in bland.altman.stats: groups differ in length.")
  if (!is.numeric(group1)) 
    stop("Error in bland.altman.stats: group1 is not numeric.")
  if (!is.numeric(group2)) 
    stop("Error in bland.altman.stats: group2 is not numeric.")
  if (two <= 0) 
    stop("Error in bland.altman.stats: inproper value of two.")
  if (mode != 1 & mode != 2) 
    stop("Error in bland.altman.stats: mode must be either 1 oder 2.")
  dfr <- data.frame(group1 = group1, group2 = group2, check.names = FALSE)
  dfr <- na.omit(dfr)
  called.with <- length(group1)
  based.on <- length(dfr[[1]])
  if (based.on < 2) 
    warning("Warning in bland.altman.stats:less than 2 data pairs after deleting NAs.", 
            call. = FALSE)
  if (mode == 1) 
    diffs <- dfr[[1]] - dfr[[2]]
  if (mode == 2) 
    diffs <- dfr[[2]] - dfr[[1]]
  means <- (dfr[[1]] + dfr[[2]])/2
  critical.diff <- two * sd(diffs)
  mean.diffs <- mean(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  lines <- c(lower.limit = lower.limit, mean.diffs = mean.diffs, 
             upper.limit = upper.limit)
  t1 <- qt((1 - conf.int)/2, df = based.on - 1)
  t2 <- qt((conf.int + 1)/2, df = based.on - 1)
  CI.lines <- c(lower.limit.ci.lower = lower.limit + t1 * sqrt(sd(diffs)^2 * 
                                                                 3/based.on), lower.limit.ci.upper = lower.limit + t2 * 
                  sqrt(sd(diffs)^2 * 3/based.on), mean.diff.ci.lower = mean.diffs + 
                  t1 * sd(diffs)/sqrt(based.on), mean.diff.ci.upper = mean.diffs + 
                  t2 * sd(diffs)/sqrt(based.on), upper.limit.ci.lower = upper.limit + 
                  t1 * sqrt(sd(diffs)^2 * 3/based.on), upper.limit.ci.upper = upper.limit + 
                  t2 * sqrt(sd(diffs)^2 * 3/based.on))
  return(list(means = means, diffs = diffs, groups = dfr, based.on = based.on, 
              lower.limit = lower.limit, mean.diffs = mean.diffs, upper.limit = upper.limit, 
              lines = lines, CI.lines = CI.lines, two = two, critical.diff = critical.diff))
}