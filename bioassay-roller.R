library(plyr)
library(reshape2)
library(drc)
library(ggplot2)
library(sets)

load.raw = function(file) {
  data = read.table(
    file=file,
    sep=';',
    header=F,
    row.names = NULL)
  data = data[data$V1 != '', ]
  data = data[data$V1 != 'A' & data$V1 !='H' & data$V1 != '<>', ]
  data = subset(data, select=-c(V2, V13))
  list.df = split(subset(data[!is.na(data$V3),], select=-c(V1)), (0:(nrow(data[!is.na(data$V3),])-1) %/% 6))  # modulo division
  names(list.df) = as.factor(as.vector(data[seq(1, nrow(data), 7),1]))
  list.df = lapply(list.df, function(df) {rownames(df) <- NULL; df})
  list.df
}

df.to.comparable = function(df, doses) {
  test = df[c(F,T),]
  test = data.frame(t(rbind(doses, test)))
  colnames(test) = c('dose', paste('x', 1:3, sep=''))
  rownames(test) = NULL
  ref = df[c(T,F),ncol(df):1]
  ref = data.frame(t(rbind(doses, ref)))
  colnames(ref) = c('dose', paste('x', 1:3, sep=''))
  rownames(ref) = NULL
  result = list(ref,test)
  names(result) = c('ref', 'test')
  result
}

melt.comparable.df = function(df) {
  result = melt(df, id.vars = c('dose'), variable.name='curve', value.name='response', level='sample')
  result = rename(result, c("Lsample"="sample"))
  result
}

drm.model = function(df) {
  mod.normal = LL.4(fixed = c(NA, NA, NA, NA), names=c("Slope","Lower Limit","Upper Limit", "ED50"))
  ref.df = df[df[,4]=='ref',]
  ref = drm(response~dose, data = ref.df, fct = mod.normal)
  test.df = df[df[,4]=='test',]
  test = drm(response~dose, data = test.df, fct = mod.normal)
  mod.shared = LL.4(fixed = c(coef(ref)[1], coef(ref)[2], coef(ref)[3], NA), names=c("Slope","Lower Limit","Upper Limit", "ED50"))
  test.shared = drm(response~dose, data = test.df, fct = mod.shared)
  result = list(ref, test, test.shared)
  names(result) = c('ref', 'test', 'shared')
  result
}

data.list.from.file = function(file, dose) {
  df.list = load.raw(file)
  df.list = lapply(df.list, df.to.comparable, dose)
  df.list
}

merged.model = function(df) {
  mod.normal = LL.4(fixed = c(NA, NA, NA, NA), names=c("Slope","Lower Limit","Upper Limit", "ED50"))
  drm(response~dose, curveid = sample, data = df, fct = mod.normal)
}

f.parallel.test = function(model.set) {
  testFit = modelFit(model.set$test)
  sharedFit = modelFit(model.set$shared)
  RSS = testFit$RSS[2]
  DF = testFit$ModelDf[2]
  SRSS = sharedFit$RSS[2]
  SDF = sharedFit$ModelDf[2]
  
  f = ((SRSS-RSS)/(SDF-DF))/(RSS/DF)
  p = pf(f, SDF-DF,DF)
  if(p>0.95)
    'FAIL'
  else
    'PASS'
}

eq.parallel.test = function(model.set) {
  ref.ci = confint(model.set$ref)
  test.ci = confint(model.set$test)
  ref.slope = interval(ref.ci[1,1], ref.ci[1,2])
  ref.lower = interval(ref.ci[2,1], ref.ci[2,2])
  ref.upper = interval(ref.ci[3,1], ref.ci[3,2])
  test.slope = interval(test.ci[1,1], test.ci[1,2])
  test.lower = interval(test.ci[2,1], test.ci[2,2])
  test.upper = interval(test.ci[3,1], test.ci[3,2])
  slope = interval_is_subinterval(test.slope, ref.slope)
  upper = interval_is_subinterval(test.upper, ref.upper)
  lower = interval_is_subinterval(test.lower, ref.lower)
  if(!slope & !upper & !lower)
    'PASS'
  else
    'FAIL'
}

point.stat.table = function(dose, df.list) {
  ref.point.mean = lapply(df.list, function(x) apply(x$ref[,2:4], 1, mean))
  ref.point.sd = lapply(df.list, function(x) apply(x$ref[,2:4], 1, sd))
  test.point.mean = lapply(df.list, function(x) apply(x$test[,2:4], 1, mean))
  test.point.sd = lapply(df.list, function(x) apply(x$test[,2:4], 1, sd))
  
  merge.mean.sd = function(mean, sd) {
    res = data.frame(t(rbind(dose, mean, sd, sd/mean)))
    colnames(res) = c('Dose', 'Mean', 'SD', 'RSD')
    res
  }
  ref.points = mapply(merge.mean.sd, ref.point.mean, ref.point.sd, SIMPLIFY=F)
  test.points = mapply(merge.mean.sd, test.point.mean, test.point.sd, SIMPLIFY=F)
  mapply(function(a,b) {list(ref=a, test=b)}, ref.points, test.points, SIMPLIFY=F)
}

board.data = function(rp, test, coef, rfu, ab) {
  list.with.names=function(rp, test, coef, rfu, ab) {
    list(rp = rp, test=test, coef = coef, rfu = rfu, ab=ab)
  }
  mapply(list.with.names, rp, test, coef, rfu, ab, SIMPLIFY=F)
}

board.to.file = function(board, file) {
  sink(file)
  cat('\t')
  write.table(round(as.data.frame(t(board$rp)),2), dec = ",", sep='\t', quote=F, row.names=c('RP'))
  cat('\nReference A/B\t')
  write.table(round(board$ab$ref,2), dec = ",", sep='\t', col.names=F, row.names=F, quote=F)
  cat('Test A/B\t')
  write.table(round(board$ab$test,2), dec = ",", sep='\t', col.names=F, row.names=F, quote=F)
  cat('\n\tTest result\n')
  write.table(data.frame(board$test), dec = ",", sep='\t', quote=F, col.names=F, row.names=T)
  cat('\nReference model params\n')
  cat('Coef\t')
  write.table(round(board$coef$ref,2), dec = ",", sep='\t', quote=F, row.names=T)
  cat('\nTest model params\n')
  cat('Coef\t')
  write.table(round(board$coef$test,2), dec = ",", sep='\t', quote=F, row.names=T)
  cat('\nReference RFU statistics\n')
  write.table(round(board$rfu$ref,2), dec = ",", sep='\t', row.names=F, quote=F)
  cat('\nTest RFU statistics\n')
  write.table(round(board$rfu$test,2), dec = ",", sep='\t', row.names=F, quote=F)
  sink()
}

process = function(input.file, output.dir) {
  dose = rev(c(100, 5, 2, 1, 0.5 , 0.25 , 0.1 , 0.05, 0.01, 0))
  file = input.file
  df.list = data.list.from.file(file, dose)
  melt.df = lapply(df.list, melt.comparable.df)
  models = lapply(melt.df, drm.model)
  ##########################
  ## to board result data
  rp = lapply(lapply(lapply(melt.df, merged.model), SI, c(50,50), display=F), function(x) {
      s = c(x[,1:2], x[,2]/x[,1])
      names(s) = c('Mean', 'SD', 'RSD')
      s
    }
  )
  f.test = lapply(models, f.parallel.test)
  eq.test = lapply(models, eq.parallel.test)
  models.for.plot = lapply(melt.df, merged.model)
  rfu = point.stat.table(dose, df.list)
  test.result = mapply(function(a,b) {
    s = c(a, b)
    names(s) = c('F-test','Equivalence test')
    s
  }, f.test, eq.test, SIMPLIFY=F)
  coef = lapply(models, function(x) {
    s = lapply(lapply(x, summary), function(x) x$coefficients[,1:2])
    s = lapply(s[1:2], function(x) cbind(x, RSD = x[,2]/x[,1]))
    s = lapply(s, function(z) {
      colnames(z) = c('Mean', 'SD', 'RSD')
      z
    })
  }
  )
  ab = lapply(coef, lapply, function(x) {
    s = data.frame(x[1,3]/x[1,2])
    colnames(s) = c('A/B')
    s
  }
  )
  db = board.data(rp, test.result, coef, rfu, ab)
  
  ########
  # OUTPUT
  for( key in names(db) ) {
    board.to.file(db[[key]], paste(output.dir, key, '.tsv', sep=''))
  }

  for( key in names(models.for.plot) ) {
    png(filename=paste(output.dir, key, '.png', sep=''))
    plot(ggplot.magic(models.for.plot[[key]], key))
    graphics.off()
  }
}

ggplot.magic = function(model, name) {
  points = model$origData
  ref.points = model$origData[model$origData$sample == 'ref',c(1,3)]
  test.points = model$origData[model$origData$sample == 'test',c(1,3)]
  cf.ref = coef(model)[c(1,3,5,7)]
  cf.test = coef(model)[c(2,4,6,8)]
  LP.4 = function(x, B, D, A, C) D + (A-D)/(1+(x/C)^B)
  LP.4mod <- function(x, ...) LP.4(10^x,... )  # to achieve propper plot
  dose = unique(points$dose)
  
  mp = ggplot(data.frame(x=dose), aes(x)) + scale_x_log10() + labs (title = name, x='dose', y='response')
  mp = mp + stat_function(fun = LP.4mod, 
                          args = list(A = cf.ref[3], B = cf.ref[1], C = cf.ref[4], D = cf.ref[2]), 
                          color='black') # for ref curve
  mp = mp + stat_function(fun = LP.4mod, 
                          args = list(A = cf.test[3], B = cf.test[1], C = cf.test[4], D = cf.test[2]), 
                          color='red',
                          linetype='dashed') # for test curve
  mp = mp + geom_point(data = model$origData, aes(x=dose, y=response), shape = 1, size = 3) # ref points
  mp = mp + geom_point(data = test.points, aes(x=dose, y=response), shape = 2, size = 3) # test points
  mp
  # add legend
}

args <- commandArgs(trailingOnly = TRUE)
cat('*** Begin\n')
process(args[1], args[2])
cat('*** Done\n')