1: ddg.source("helpers.R")
1: packs <- c("gdistance", "fossil", "igraph", "rgbif", "mapproj", 
1:     "mapdata", "sp", "maptools", "dismo", "rJava", "rgdal", "rgeos", 
1:     "raster", "reshape2", "ggplot2", "FedData")
2: lapply(packs[!(packs %in% installed.packages()[, "Package"])], 
2:     install.packages)
3: all(unlist(lapply(packs, require, character.only = TRUE, quietly = TRUE)))
4: symSum <- function(x = "matrix", zero.diag = TRUE) {
4:     ddg.function()
4:     ddg.eval(c("if (zero.diag == TRUE) {", "    diag(x) <- 0", 
4:         "}"))
4:     ddg.eval("sum.lu <- t(x)[lower.tri(x)] + x[lower.tri(x)]")
4:     ddg.eval("x[lower.tri(x)] <- sum.lu")
4:     ddg.eval("x <- t(x)")
4:     ddg.eval("x[lower.tri(x)] <- sum.lu")
4:     ddg.return.value(if (isSymmetric(x)) {
4:         x
4:     }
4:     else {
4:         warning("Output matrix is not symmetirc.")
4:     })
4: }
5: altDiff <- function(x) {
5:     ddg.function()
5:     ddg.return.value(x[2] - x[1])
5: }
6: m <- function(scd) {
6:     ddg.function()
6:     ddg.return.value((1 - (scd/max(scd))))
6: }
7: gClust <- function(x = "coordinates", vp = "vector predictor", 
7:     N = 1, km = TRUE, kcmax = 10, kcstart = 10, kcthresh = 0.005) {
7:     ddg.function()
7:     ddg.eval("set.seed(1)")
7:     ddg.eval(c("if (!km) {", "    if (!(is.matrix(x))) {", "        x <- as.matrix(x)", 
7:         "    }", "    hd <- transition(vp, altDiff, 8, symm = FALSE)", 
7:         "    slope <- geoCorrection(hd)", "    adj <- adjacent(vp, cells = 1:ncell(vp), pairs = TRUE, directions = 8)", 
7:         "    speed <- slope", "    speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))", 
7:         "    Conductance <- geoCorrection(speed)", "    cd <- costDistance(Conductance, x)", 
7:         "    scd <- symSum(cd)", "    Fst <- 0.2 * (1/(1 + 4 * N * m(scd)))", 
7:         "    diag(Fst) <- 0", "    Fst.g <- 1 - Fst", "    diag(Fst.g) <- 0", 
7:         "    Fst.mg <- dino.mst(Fst.g)", "    Fst.ig <- graph.adjacency(Fst.mg, weighted = TRUE, mode = \"undirected\")", 
7:         "    fg.mP <- fastgreedy.community(Fst.ig)", "    gc <- fg.mP$membership", 
7:         "    names(gc) <- rownames(Fst)", "} else {", "    y <- extract(vp, x)", 
7:         "    kc <- lapply(1:kcmax, kmeans, x = y, nstart = kcstart)", 
7:         "    wss <- unlist(lapply(kc, function(x) x$tot.withinss))", 
7:         "    dw <- abs(diff(wss/max(wss)))", "    if (all(dw > kcthresh)) {", 
7:         "        nc <- kcmax", "    }", "    else {", "        nc <- max((1:(kcmax - 1))[dw >= kcthresh])", 
7:         "    }", "    kc <- kc[[nc]]", "    gc <- kc$cluster", 
7:         "}"))
7:     return(ddg.return.value(gc))
7: }
8: ENM <- function(x = "coordinates", p = "predictors", c.rad = 50000, 
8:     seed = 123, n = 1000) {
8:     ddg.function()
8:     ddg.eval(c("if (class(p) == \"RasterLayer\") {", "    p <- stack(p)", 
8:         "}"))
8:     ddg.eval("set.seed(seed)")
8:     ddg.eval("circ <- circles(x, d = c.rad, lonlat = T)")
8:     ddg.eval("random <- spsample(circ@polygons, n, type = \"random\", iter = 100)")
8:     ddg.eval("gsp_bc <- extract(p, x)")
8:     ddg.eval("gsp_bc <- data.frame(cbind(x, env = gsp_bc))")
8:     ddg.eval("random_bc <- extract(p, random)")
8:     ddg.eval("random <- random@coords")
8:     ddg.eval("colnames(random) <- c(\"lon\", \"lat\")")
8:     ddg.eval("random_bc <- data.frame(cbind(random, env = random_bc))")
8:     ddg.eval("random_bc <- random_bc[!is.na(random_bc[, 3]), ]")
8:     ddg.eval(c("me <- maxent(p, gsp_bc[, c(\"lon\", \"lat\")], random_bc[, c(\"lon\", ", 
8:         "    \"lat\")])"))
8:     ddg.eval(c("e <- evaluate(gsp_bc[, c(\"lon\", \"lat\")], random_bc[, c(\"lon\", ", 
8:         "    \"lat\")], me, p)"))
8:     ddg.eval("pred_me <- predict(me, p)")
8:     ddg.eval("out <- list(eval = e, pred = pred_me, model = me)")
8:     return(ddg.return.value(out))
8: }
9: gENM <- function(x = "coordinates", clust = "gen clusters", p = "Environmental") {
9:     ddg.function()
9:     ddg.eval("df.gsp <- data.frame(x)")
9:     ddg.eval("groups <- split(df.gsp, clust)")
9:     ddg.eval("analysis <- (lapply(groups, ENM, p))")
9:     ddg.eval("enm.all <- list(ENM(do.call(rbind, groups), p))")
9:     ddg.return.value(out <- append(enm.all, analysis))
9: }
10: gAnalysis <- function(x = "gENM output", filename = "../results/gENM.jpeg", 
10:     mfrow = c(3, 3), ext = extent(-73.70833, -66.95833, 41, 47.45833), 
10:     open.file = TRUE) {
10:     ddg.function()
10:     ddg.eval(c("jpeg(filename = filename, width = 1700, height = 1700, units = \"px\", ", 
10:         "    pointsize = 35, quality = 90, bg = \"white\")"))
10:     ddg.eval(c("par(mfrow = mfrow, oma = rep(0, 4), omi = rep(0, 4), bty = \"n\", ", 
10:         "    mar = rep(0.01, 4), mai = rep(0, 4))"))
10:     ddg.eval(c("for (i in 1:length(x)) {", "    zoom(x[[i]]$pred, ext = ext, xaxt = \"n\", yaxt = \"n\", new = FALSE, ", 
10:         "        asp = 1)", "}"))
10:     ddg.eval("dev.off()")
10:     ddg.eval("auc <- unlist(lapply(x, function(x) x$eval@auc))")
10:     ddg.eval("cor <- unlist(lapply(x, function(x) x$eval@cor))")
10:     ddg.eval("out <- data.frame(auc, cor)")
10:     return(ddg.return.value(out))
10:     ddg.return.value(if (open.file) {
10:         system(paste("open", filename))
10:     })
10: }
11: gDensCurv <- function(x = "coordinates", p = "mintemp.2006", 
11:     pr = "temp for range", gc = "clusters") {
11:     ddg.function()
11:     ddg.eval("mt.2006 <- extract(p, x)")
11:     ddg.eval("mt.c <- split(mt.2006, gc)")
11:     ddg.eval("mt.r <- extract(pr, x)")
11:     ddg.eval("r.c <- split(mt.r, gc)")
11:     ddg.eval("mt.range <- do.call(rbind, lapply(r.c, range))")
11:     ddg.eval("mt.range <- melt(mt.range)")
11:     ddg.eval("mt.range <- data.frame(mt.range)")
11:     ddg.eval("mt.range[, 1:2] <- apply(mt.range[, 1:2], 2, factor)")
11:     ddg.eval("mt.mc <- melt(mt.c)")
11:     ddg.eval("colnames(mt.mc) <- c(\"MinTemp\", \"Cluster\")")
11:     ddg.eval(c("mt.mcplot <- ggplot(mt.mc, aes(MinTemp, colour = Cluster, fill = Cluster)) + ", 
11:         "    geom_density(alpha = 0.7) + scale_x_continuous(breaks = seq(250.8, ", 
11:         "    275.9, 4)) + scale_color_manual(values = c(\"greenyellow\", ", 
11:         "    \"pink2\", \"lightskyblue1\")) + scale_fill_manual(values = c(\"chartreuse\", ", 
11:         "    \"lightcoral\", \"steelblue1\")) + theme(panel.background = element_rect(fill = \"gray8\"), ", 
11:         "    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ", 
11:         "    legend.position = \"none\", axis.line = element_line(colour = \"white\"), ", 
11:         "    axis.title = element_text(color = \"white\"), axis.ticks = element_line(size = 2, ", 
11:         "        colour = \"black\"), axis.text = element_text(colour = \"white\"), ", 
11:         "    axis.text.x = element_text(colour = \"black\", size = 15), ", 
11:         "    axis.text.y = element_text(colour = \"black\", size = 15))"))
11:     ddg.return.value(mt.mcplot + geom_vline(data = mt.range, 
11:         aes(xintercept = value, colour = Var1)))
11: }
2: mintemp.2006 <- raster("../data/01_01_2006.tiff")
3: mintemp.2050 <- raster("../data/01_01_2050.tiff")
4: mintemp.2099 <- raster("../data/01_01_2099.tiff")
5: gsp <- read.csv("../data/RICTMEdukesnantucket.csv")
6: if (is.matrix(gsp) == FALSE) {
6:     gsp <- data.matrix(gsp)
6: }
7: clust <- gClust(x = gsp, vp = mintemp.2006)
8: out <- gENM(x = gsp, clust = clust, p = mintemp.2006)
9: gAnalysis(x = out)
10: gDensCurv(x = gsp, p = mintemp.2006, pr = mintemp.2006, gc)
