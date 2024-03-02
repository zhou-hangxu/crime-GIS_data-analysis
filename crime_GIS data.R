setwd("/Users/ZHOUHANGXU/Desktop/1/data")

###[2] moran I
library(sf)
library(spdep)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(spatialreg)
library(CARBayes)
library(RColorBrewer)

##データの準備
pref <- read_sf('airbnb_Chicago 2015.shp')
pref <- subset(pref, select=-c(AREAID, response_r, room_type, num_spots, crowded, dependency, without_hs, unemployed, harship_in, num_theft, population))
#error: pref <- select(pref, -c(AREAID, response_r, room_type, num_spots, crowded, dependency, without_hs, unemployed, harship_in, num_theft, population)) 
pref <- na.omit(pref)  #損失値を除く

##近接行列を作る
nb1 <- poly2nb(pref) #クイ-ン形近接行列
nb1

#近接行列をplotする
#coords <- st_coordinates(st_centroid(pref))
#plot(st_geometry(pref), col='white', border='grey')
#plot(nb1, coords, add=TRUE, col='red', cex=0.01, lwd=1.5)

#行列形式を交換する、listwに.
w1 <- nb2listw(nb1)


##moran's I 
accept_r1 <- pref$accept_r
moran <- moran.test(accept_r1, listw=w1)
moran

price_pp1 <- pref$price_pp
moran <- moran.test(price_pp1, listw=w1)
moran

rev_rating1 <- pref$rev_rating
moran <- moran.test(rev_rating1, listw=w1)
moran

income_pc1 <- pref$income_pc
moran <- moran.test(income_pc1, listw=w1)
moran

poverty1 <- pref$poverty
moran <- moran.test(poverty1, listw=w1)
moran

num_crimes1 <- pref$num_crimes
moran <- moran.test(num_crimes1, listw=w1)
moran

##誤差項のmoran's Iの計算
formula <- accept_r ~ log(price_pp) + rev_rating + log(income_pc) + poverty + num_crimes
lmod <- lm(formula, data=pref) #回帰
summary(lmod)
lm.morantest(lmod, w1)

###[3] model 
##model1: SLX model
slx <- spatialreg::lmSLX(formula, data=pref, listw=w1)
summary(slx)
spatialreg::impacts(slx, listw=w1) #直接・間接効果

##model2: 
w2 <- nb2mat(nb1, style='B') #行基準化なしの隣接行列

#leroux model
leroux <- S.CARleroux(formula, data=pref, family='gaussian', W=w2, burnin=5000, n.sample=20000)
leroux

#ICAR model
icar <- S.CARleroux(formula, data=pref, rho=1, family='gaussian', W=w2, burnin=5000, n.sample=20000)
icar

###[4] leroux modelによって、図を作成する
#事前の設定
nc <- 10
breaks <- seq(60, 100, len=nc+1)
pal <- rev(brewer.pal(nc, 'RdYlBu'))

##被説明変数
plot(pref[,'accept_r'], pal=pal, breaks=breaks, axes=TRUE, lwd=0.1)

##空間成分
pref$accept_r1  <- leroux$fitted.values
breaks <- seq(80, 105, len=nc+1)
plot(pref[,'accept_r1'], pal=pal, breaks=breaks, axes=TRUE, lwd=0.1)

##ノイズ
breaks <- seq(-15, 15, len=nc+1)
pref$noise <- pref$accept_r-pref$accept_r1
plot(pref[,'noise'], pal=pal, breaks=breaks, axes=TRUE, lwd=0.1)


