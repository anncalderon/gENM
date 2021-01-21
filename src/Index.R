comp_2006 <- ((out$`1`$pred + out$`2`$pred + out$`3`$pred))
max_comp <- comp_2006
max_comp <- setValues(max_comp,rep(max(getValues(comp_2006)),ncell(comp_2006)))
comp_2006 <- comp_2006 / max_comp

exc_2006 <- ((out$`1`$pred * out$`2`$pred * out$`3`$pred))
max_exc <- exc_2006
max_exc <- setValues(max_exc,rep(max(getValues(exc_2006)),ncell(exc_2006)))
exc_2006 <- exc_2006 / max_exc

max.enmAll.rel <- enmAll.rel <- out[[1]]$pred
max.enmAll.rel <- setValues(enmAll.rel,rep(max(getValues(enmAll.rel)),ncell(enmAll.rel)))
enmAll.rel <- enmAll.rel / max.enmAll.rel

par(mfrow=c(1,2))
plot(out[[1]]$pred, zlim=c(0,1))
plot(enmAll.rel, zlim=c(0,1))

par(mfrow=c(3,3),oma=rep(0,4),omi=rep(0,4))
plot(out[[2]]$pred, zlim=c(0,1))
plot(out[[3]]$pred, zlim=c(0,1))
plot(out[[4]]$pred, zlim=c(0,1))
plot(enmAll.rel, zlim=c(0,1))
plot(comp_2006, zlim=c(0,1))
plot(exc_2006, zlim=c(0,1))
hist(enmAll.rel-out[[1]]$pred)
hist(enmAll.rel - comp_2006)
hist(enmAll.rel - exc_2006)

##########################################
test <- list()
for (i in 1:length(out)){
  test[[i]] <- (out[[1]]$pred - out[[i]]$pred)
}

csum <- out[[2]]$pred
for (i in 3:length(out)){
  csum <- csum + out[[i]]$pred
}

cprod <- out[[2]]$pred
for (i in 3:length(out)){
  cprod <- cprod * out[[i]]$pred
}


par(mfcol=c(3,3))
plot(out[[1]]$pred);plot(csum);plot(cprod);invisible(lapply(test[-1],plot))
invisible(lapply(out[-1],function(x) plot(x$pred)))

out.stack <- stack(lapply(out,function(x) x$pred))
layerStats(out.stack,stat='pearson')
plot(out[[1]]$pred)

