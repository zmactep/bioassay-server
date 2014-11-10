library(plyr)
library(reshape2)
library(drc)
library(ggplot2)
library(sets)
library(scales)
library(grid)

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
    paste('FAIL', 'p-value', round(p, 2), sep='\t')
  else
    paste('PASS', 'p-value', round(p, 2), sep='\t')
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
    res = data.frame(t(rbind(dose, mean, sd, round(sd/mean*100,0))))
    colnames(res) = c('Dose', 'Mean', 'SD', 'RSD, %')
    res
  }
  ref.points = mapply(merge.mean.sd, ref.point.mean, ref.point.sd, SIMPLIFY=F)
  test.points = mapply(merge.mean.sd, test.point.mean, test.point.sd, SIMPLIFY=F)
  mapply(function(a,b) {list(ref=a, test=b)}, ref.points, test.points, SIMPLIFY=F)
}

board.data = function(rp, test, rcoef, coef, cdif, rfu, ab) {
  list.with.names=function(rp, test, rcoef, coef, cdif, rfu, ab) {
    list(rp = rp, test=test, rcoef = rcoef, coef = coef, cdif = cdif, rfu = rfu, ab=ab)
  }
  mapply(list.with.names, rp, test, rcoef, coef, cdif, rfu, ab, SIMPLIFY=F)
}

board.to.file = function(board, file) {
  sink(file)
  cat('\t')
  write.table(round(as.data.frame(t(board$rp)),2), dec = ",", sep='\t', quote=F, row.names=c('RP'))
  cat('\nReference A/D\t')
  write.table(round(board$ab$ref,2), dec = ",", sep='\t', col.names=F, row.names=F, quote=F)
  cat('Test A/D\t')
  write.table(round(board$ab$test,2), dec = ",", sep='\t', col.names=F, row.names=F, quote=F)
  ###
  cat('\nSlope, ref/test\t')
  cat(round(board$coef$ref[1]/board$coef$test[1],2))
  ###
  cat('\nR-coefficient, ref', round(board$rcoef$ref,2), sep='\t')
  cat('\nR-coefficient, test', round(board$rcoef$test,2), sep='\t')
  ###
  cat('\n\tTest result\n')
  write.table(data.frame(board$test), dec = ",", sep='\t', quote=F, col.names=F, row.names=T)
  cat('\nDelta for model\t')
  write.table(round(board$cdif,2), dec = ",", sep='\t', quote=F, row.names=T)
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
  dose = rev(c(100, 5, 2, 1, 0.5 , 0.25 , 0.1 , 0.05, 0.01, 0.0001))
  file = input.file
  df.list = data.list.from.file(file, dose)
  melt.df = lapply(df.list, melt.comparable.df)
  models = lapply(melt.df, drm.model)
  ##########################
  ## to board result data
  rp = lapply(lapply(lapply(melt.df, merged.model), SI, c(50,50), display=F), function(x) {
      s = c(x[,1:2], round(x[,2]/x[,1]*100, 0))
      names(s) = c('Mean', 'SD', 'RSD, %')
      s
    }
  )
  f.test = lapply(models, f.parallel.test)
  eq.test = lapply(models, eq.parallel.test)
  models.for.plot = lapply(melt.df, merged.model)
  rfu = point.stat.table(dose, df.list)
  test.result = mapply(function(a,b) {
    s = c(a, b)
    names(s) = c('F-test', 'Equivalence test')
    s
  }, f.test, eq.test, SIMPLIFY=F)
  coef = lapply(models, function(x) {
    s = lapply(lapply(x, summary), function(x) x$coefficients[,1:2])
    s = lapply(s[1:2], function(x) cbind(x, RSD = round(x[,2]/x[,1]*100,0)))
    s = lapply(s, function(z) {
      colnames(z) = c('Mean', 'SD', 'RSD, %')
      z
    })
  }
  )
  rcoef = lapply(models.for.plot, function(model) {
    df = cbind(model$data[,c(2,4)], predict(model))
    colnames(df) = c('values','sample','predict')
    rco(df)
  })
  cdif = lapply(coef, function(x) {
    slope = round(abs(x$ref[1] - x$test[1])/mean(x$ref[1],x$test[1]), 2)*100
    lower = round(abs(x$ref[2] - x$test[2])/mean(x$ref[2],x$test[2]), 2)*100
    upper = round(abs(x$ref[3] - x$test[3])/mean(x$ref[3],x$test[3]), 2)*100
    ed = round(abs(x$ref[4] - x$test[4])/mean(x$ref[4],x$test[4]), 2)*100
    res = matrix(nrow=4, c(slope, lower, upper, ed))
    rownames(res) = rownames(x$ref)
    colnames(res) = c('Delta, %')
    res
  })
  ab = lapply(coef, lapply, function(x) {
    s = data.frame(x[3,1]/x[2,1])
    colnames(s) = c('A/D')
    s
  }
  )
  db = board.data(rp, test.result, rcoef, coef, cdif, rfu, ab)
  
  ########
  # OUTPUT
  for( key in names(db) ) {
    board.to.file(db[[key]], paste(output.dir, key, '.tsv', sep=''))
  }

  for( key in names(models.for.plot) ) {
    png(width = 1920, height = 1080, filename=paste(output.dir, key, '.png', sep=''))
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

  mp = ggplot(NULL, aes(x=x)) + 
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(0.0001, 1000)) +
    labs (title=name, x='Log Dose, mug/ml', y='Response, RFU') +
    stat_function(fun = LP.4mod,
                  data = data.frame(x = dose, Sample = factor(1)),
                  args = list(A = cf.ref[3], B = cf.ref[1], C = cf.ref[4], D = cf.ref[2]), 
                  colour = "red") + # for ref curve
    stat_function(fun = LP.4mod,
                  data = data.frame(x = dose, Sample = factor(2)),
                  args = list(A = cf.test[3], B = cf.test[1], C = cf.test[4], D = cf.test[2]), 
                  colour = "deepskyblue3",
                  linetype='dashed') + # for test curve
    geom_point(data = model$origData, aes(x=dose, y=response, color=sample, shape=sample), size=5) +# ref points
    theme(title = element_text(size=30),
          axis.text.x = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=20,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=20,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
          legend.text=element_text(size=25),
          legend.title=element_text(size=25),
          legend.key.size=unit(2, "cm"))
    mp
}

ggplot.magic.box = function(model, name) {
  points = model$origData
  ref.points = model$origData[model$origData$sample == 'ref',c(1,3)]
  test.points = model$origData[model$origData$sample == 'test',c(1,3)]
  cf.ref = coef(model)[c(1,3,5,7)]
  cf.test = coef(model)[c(2,4,6,8)]
  LP.4 = function(x, B, D, A, C) D + (A-D)/(1+(x/C)^B)
  LP.4mod <- function(x, ...) LP.4(10^x,... )  # to achieve propper plot
  dose = unique(points$dose)
  dddd = model$origData
  dddd$dose = as.factor(dddd$dose)
  mp = ggplot(NULL, aes(x=x)) + 
    geom_boxplot(data = dddd, aes(x=dose, y=response, fill=sample)) +# ref points
    theme(title = element_text(size=30),
          axis.text.x = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=20,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=20,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=20,hjust=.5,vjust=.5,face="plain"),
          legend.text=element_text(size=25),
          legend.title=element_text(size=25),
          legend.key.size=unit(2, "cm"))
  mp
}

rco = function(df) {
  nom.ref = sum((df$values[df$sample=='ref']-df$predict[df$sample=='ref'])^2)
  denom.ref = sum((df$values[df$sample=='ref'] - mean(df$values[df$sample=='ref']))^2)
  ref = sqrt(1 - nom.ref/denom.ref)
  nom.test = sum((df$values[df$sample=='test'] - df$predict[df$sample=='test'])^2)
  denom.test = sum((df$values[df$sample=='test'] - mean(df$values[df$sample=='test']))^2)
  test = sqrt(1 - nom.test/denom.test)
  list(ref=ref, test=test)
}

args <- commandArgs(trailingOnly = TRUE)
cat('*** Begin\n')
process(args[1], args[2])
cat('*** Done\n')