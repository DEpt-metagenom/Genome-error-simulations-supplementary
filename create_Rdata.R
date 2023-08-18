
###----- FUNCTIONS -----

rsc = function(x){
  x.rsc = (x-mean(x, na.rm=T))/sd(x, na.rm=T)
  return(x.rsc)
}

tbeta = function(x){
  x.t = (x*(length(x)-1)+0.5)/length(x)
  return(x.t)
}

multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #http://ianmadd.github.io/pages/multiplot.html
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow = TRUE)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###----- DATA -----

vars.to.omit = c("predicted.unique.genes")

evars = c("cov", "err", "odup", "pdup")
evar.labels = c(
  cov = "read depth",
  err = "error rate",
  odup = "optical duplication rate",
  pdup = "PCR duplication rate"
)

bacs = read.csv("table_of_samples.csv", sep = ",")

asq = read.csv("assembly_qualities_session4_V3.csv", sep = ";")
asq = asq[,!(colnames(asq) %in% vars.to.omit)]
for(v in evars){
  asq[,paste(v,"rsc", sep=".")] = rsc(asq[,v])
}

qvar.labels = read.csv("quality_metrics_labels_S4_V3.csv", sep = ";")
qvar.labels = qvar.labels[!(qvar.labels$qvar %in% vars.to.omit),]
qvars = qvar.labels$qvar

gvars = c("size.mb.full", "GC", "complexity")
gvar.labels = c(
  size.mb.full = "Genome size (Mbp)",
  GC = "GC%",
  complexity = "Genome complexity"
)

N.nominal = length(unique(asq$cov)) * length(unique(asq$err)) * length(unique(asq$odup)) * length(unique(asq$pdup)) * length(unique(asq$rep))

vals = data.frame(
  evar = "",
  evar.label = "",
  original.value = 0,
  rescaled.value = 0,
  original.mean = 0,
  original.sd = 0
)[-1,]
for(e in evars){
  for(v in unique(asq[,e])){
    vals[nrow(vals)+1,"evar"] = e
    vals[nrow(vals),"evar.label"] = evar.labels[e]
    vals[nrow(vals),"original.value"] = v
    vals[nrow(vals),"rescaled.value"] = unique(asq[asq[,e]==v,paste0(e,".rsc")])
    vals[nrow(vals),"original.mean"] = mean(asq[,e])
    vals[nrow(vals),"original.sd"] = sd(asq[,e])
  }
}

#--------------------------------------------------------------------------------

save(
  rsc, tbeta, multiplot, 
  evars, evar.labels, bacs, asq, qvar.labels, qvars, gvars, gvar.labels, N.nominal, vals, 
  file = "load_vars.RData"
)
