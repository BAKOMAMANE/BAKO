## ---- echo=FALSE, include=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.width = 5, fig.height = 5, fig.cap = '', collapse = TRUE)
library(terra)
#library(dismo)
#library(XML)


## ---- maps_1-------------------------------------------------------------
library(terra)
p <- vect(system.file("exdata/lux.shp", package="terra"))
plot(p)


## ---- maps_2-------------------------------------------------------------
n <- size(p)
plot(p, col=rainbow(n))


## ---- maps_3-------------------------------------------------------------
u <- unique(p$NAME_1)
u
m <- match(p$NAME_1, u)
plot(p, col=rainbow(n)[m])
#text(p, 'NAME_2', cex=.75, halo=TRUE)


## ---- maps_4-------------------------------------------------------------
library(raster)
spplot(p, "AREA")


## ------------------------------------------------------------------------
library(terra)
b <- rast(system.file("exdata/logo.grd", package="terra"))

r <- rast(p, res=0.01 )
values(r) <- 1:ncell(r)
r <- mask(r, p)


## ---- maps_5-------------------------------------------------------------
plot(r)
plot(p, add=TRUE)


## ---- maps_6-------------------------------------------------------------
image(r)
plot(p, add=TRUE)


## ---- raster-20a, fig.width=7--------------------------------------------
plot(b)


## ---- raster-20b---------------------------------------------------------
plotRGB(b, r=1, g=2, b=3)


## ---- maps_7-------------------------------------------------------------
bounds <- list("sp.polygons", p)
spplot(r, sp.layout=bounds)


## ---- raster-20c, fig.width=9, fig.height=3------------------------------
spplot(b, layout=c(3,1))
