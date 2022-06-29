

library(data.table)
library(magrittr)
library(rmarkdown)
library(hht)
library(EMD)
library(Rlibeemd)
library(ggplot2)
library(xts)
library(seewave)
library(forecast)

df.raw <- fread("./data/2021-05-19.drop-nospace.tsv", skip = 3)

df <- df.raw[,.(t = Time, x = Drp, g = Gravity)]

df.n <- df[, .(t , g = g - mean(df$g), n = .I/12 )]

ggplot(df.n,aes(n,g, group = 1 )) + geom_line() + ggtitle("The signal")

df.imf.1 <- extractimf(df.n$g, df.n$n, check=FALSE)

df.n[,imf1 := df.imf.1$imf]

#ggplot(df.n) + geom_line(aes(x = n, y = imf1), color= 'blue') + geom_line(aes(x = n, y = g), color= 'red')


df.imf.x <- EMD::emd(df.n$g, df.n$n,  plot.imf = FALSE, max.sift = 500
                     , max.imf = 10, tol=0.00000001, boundary = "wave"
                     , stoprule = "type1" )

plot(df.imf.x$residue, main = "Residue", type = "l")

df.imf <- df.imf.x$imf %>% data.table()
# 
# df.imf[,n :=  .I]
# 
# setcolorder(df.imf, c("n", names(df.imf)[1:(length(names(df.imf)) - 1 )]) )

imfs <- data.table(g = df.n$g, df.imf) 

plot(as.ts(imfs[,1:10]), main = 'IMFs')

par(mfrow = c(1,1))

##3 check additivity = residue
#3 plot( imfs$g  - imfs[,-c("g"), with = FALSE] %>% rowSums() )


(test1 <- hilbertspec(df.imf.x$imf))

plot(test1$energy, type = 'b')

#spectrogram(test1$amplitude[,1],test1$instantfreq[,1])

test1.amp <- data.table(n = df.n$n, test1$amplitude)  %>%
  melt(id.vars = "n", value.name = 'amplitude', variable.name = "imf")


test1.freq.0 <- data.table(n= df.n$n, test1$instantfreq )
names(test1.freq.0) <- c("n", paste0("V", 1:(length(names(test1.freq.0)) - 1 ) ) )

test1.freq <- test1.freq.0 %>% 
  melt(id.vars = "n", value.name = 'freq', variable.name = "imf")

test1.spectrum <- merge(test1.freq, test1.amp, by = c("n","imf"))

# 
# ggplot(test1.spectrum[freq > 0 ], aes(x= n, y = amplitude, color = imf, group = imf )) +   geom_line() +
#   facet_wrap(~ imf) + ggtitle("Ampitude by time")
# 


ggplot(test1.spectrum[freq > 0 ], aes(x= n, y = freq, color = imf, group = imf )) +  geom_point() + geom_line() +
  facet_wrap(~ imf) + ggtitle("Freq by time")



ggplot(test1.spectrum[freq > 0  ], aes(x = freq)) +  
  geom_histogram(bins=40, alpha = 0.6, aes( fill = imf)) + 
  geom_density(color = 'grey50', size = 1, adjust = .5 ) + 
  facet_wrap(~ imf, nrow = 5, scale='free_y') + theme(legend.position = "none") +
  ggtitle("Frequency histograms ")




Acf(df.n$g)


par(mfrow = c(2,5))
Acf(df.n$g,lag.max = 50)
Acf(df.imf$V1,lag.max = 50)
Acf(df.imf$V2,lag.max = 50)
Acf(df.imf$V3,lag.max = 50)
Acf(df.imf$V4,lag.max = 50)
Acf(df.imf$V5,lag.max = 50)
Acf(df.imf$V6,lag.max = 50)
Acf(df.imf$V7,lag.max = 50)
Acf(df.imf$V8,lag.max = 50)
Acf(df.imf$V9,lag.max = 50)
par(mfrow = c(1,1))


Acf(1:100,lag.max =100 )

Acf(0.1* sin(25*(1:10000)) + .5*rnorm(10000) ,lag.max = 150 )


tmpinterm <- EMD::extrema(imfs[, V1])

zerocross <- as.numeric(round(apply(tmpinterm$cross, 1, mean), 2) )

hist(diff(df.n$n[zerocross[seq(1, length(zerocross), by=1)]]), freq=FALSE, xlab="", main="")


################ Noise

#data(UKgas, package = "datasets")

eemd <-  ceemdan(df.n$g, ensemble_size = 5000, num_imfs = 10, noise_strength = .2
                 , num_siftings = 0,  S_number = 20, threads = 6, rng_seed = 10)

#3plot(eemd)

data.table(eemd)[,-c("Residual"), with = FALSE]  -> eemd.dt

test2 <- hilbertspec(eemd) 

test2$energy

# plot(test2$energy, type = 'b')


test2.amp <- data.table(n = df.n$n, test2$amplitude)  %>%
  melt(id.vars = "n", value.name = 'amplitude', variable.name = "imf")


test2.freq.0 <- data.table(n= df.n$n, test2$instantfreq )
names(test2.freq.0) <- c("n", paste0("V", 1:(length(names(test2.freq.0)) - 1 ) ) )


test2.freq <- test2.freq.0 %>% 
  melt(id.vars = "n", value.name = 'freq', variable.name = "imf")

test2.spectrum <- merge(test2.freq, test2.amp, by = c("n","imf"))

ggplot(test2.spectrum[freq > 0 ], aes(x= n, y = amplitude, color = imf, group = imf )) +   geom_line() +
  facet_wrap(~ imf,nrow = 5) + ggtitle("Amplitude by time") + theme(legend.position = 'none')


ggplot(test2.spectrum[freq > 0 ], aes(x= n, y = freq, color = imf, group = imf )) +  geom_point() + geom_line() +
  facet_wrap(~ imf) + ggtitle("Freq by time, EEMD algo")



ggplot(test2.spectrum[freq > 0  ], aes(x = freq)) +  
  geom_histogram(bins=40, alpha = 0.6, aes( fill = imf)) + 
  geom_density(color = 'grey50', size = 1, adjust = .5 ) + 
  facet_wrap(~ imf, nrow = 3, scale='free_y') + 
  theme(legend.position = 'bottom') + ggtitle("Freq histograms, EEMD algo")




#Acf(eemd.dt,type = "corr")




###########

emd.result <- Sig2IMF(df.n$g, df.n$n)


PlotIMFs(emd.result)



trials <- 1000 #This is going to take a while!
nimf <- 10
trials.dir <- "stokes_eemd"
noise.amp <- 0.1
dfreq <- 0.01

EEMD(df.n$g, df.n$n, noise.amp, trials, nimf,  trials.dir = trials.dir)


#Compile EEMD runs
eemd.result <- EEMDCompile(trials.dir, trials, nimf)

time.span <- c(0, 10)

hgram <-HHRender(eemd.result, dt = 0.1,dfreq = 0.01, freq.span =  c(0,0.5))

HHGramImage(hgram)


eemd.result$hinstfreq[,,1] %>% data.table() %>% ts() %>% plot()

###############
# 
# 
# bsm <- tsSmooth(StructTS(df.n$g))
# 
# plot(bsm, main = "Local linear trend and seasonal components by StructTS")


### KALMAN FILTER 

library(FKF)


## <--------------------------------------------------------------------------->
## Example: Local level model for the Nile's annual flow.
## <--------------------------------------------------------------------------->
## Transition equation:
## alpha[t+1] = alpha[t] + eta[t], eta[t] ~ N(0, HHt)          
## Measurement equation:
## y[t] = alpha[t] + eps[t], eps[t] ~  N(0, GGt)

y <- df.n$g

## Set constant parameters:
dt <- ct <- matrix(0) 
Zt <- Tt <- matrix(1)
a0 <- y[1]            # Estimation of the first year flow 
P0 <- matrix(100)     # Variance of 'a0'

## Estimate parameters:
fit.fkf <- optim(c(HHt = var(y, na.rm = TRUE) *.002,
                   GGt = var(y, na.rm = TRUE) * 1.0 ),
                 fn = function(par, ...)
                   -fkf(HHt = matrix(par[1]), GGt = matrix(par[2]), ...)$logLik,
                 yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
                 Zt = Zt, Tt = Tt)

## Filter Nile data with estimated parameters:
fkf.obj <- fkf(a0, P0, dt, ct, Tt, Zt, HHt = matrix(fit.fkf$par[1]),
               GGt = matrix(fit.fkf$par[2]), yt = rbind(y))

## Compare with the stats' structural time series implementation:
fit.stats <- StructTS(y, type = "level")

fit.fkf$par
fit.stats$coef


plot(fkf.obj, type = "resid.qq")
abline(0,1, col="red")

plot(fkf.obj, type = "qqchisq")
abline(0,1, col="red")

plot(fkf.obj, type = "state", main="FKF state")


plot(fkf.obj$at[1,], type = 'l', main="FKF state and its variance")
lines(fkf.obj$Ptt[1,,], col= 'blue')


ts(fkf.obj$att[1, ]) -> kalman.g


plot(y, main = "Data and FLF level", type='p', cex=0.5)
lines(fitted(fit.stats), col = "green")
lines(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
legend("top", c("Tectonics data", "Local level (StructTS)", "Local level (fkf)"),
       col = c("black", "green", "blue"), lty = 1)


df.imf.2 <- extractimf(kalman.g, df.n$n, check=FALSE)


df.imf.x <- EMD::emd(kalman.g, df.n$n,  plot.imf = FALSE, max.sift = 1500
                     , max.imf = 10, tol=0.00000001, boundary = "wave"
                     , stoprule = "type2" )

plot(df.imf.x$residue, main = "Residue", type = "l")

df.imf.2$niter

# 
eemd <-  ceemdan(kalman.g, ensemble_size = 1000, num_imfs = 8, noise_strength = .2
                 , num_siftings = 150,  S_number = 20, threads = 6, rng_seed = 10)

# plot(eemd)

data.table(eemd)[,-c("Residual"), with = FALSE]  -> eemd.dt

test2 <- hilbertspec(eemd[,(1:(dim(eemd)[2] - 1))] )

test2$energy

plot(test2$energy , type = 'b')



imfs.eemd <- data.table(g = df.n$g, k = kalman.g, eemd[,(1:(dim(eemd)[2] - 1))]) 

plot(as.ts(imfs.eemd), main = 'IMFs of FKF data (EEMD)')

par(mfrow = c(1,1))

plot( imfs.eemd$`IMF 1`, type= 'l')

fft(imfs.eemd$`IMF 1` )  %>% Arg() %>% diff()  %>% plot


df.imf <- df.imf.x$imf %>% data.table()
# 
# df.imf[,n :=  .I]
# 
# setcolorder(df.imf, c("n", names(df.imf)[1:(length(names(df.imf)) - 1 )]) )

imfs.emd <- data.table(g = df.n$g, k = kalman.g, df.imf) 

plot(as.ts(imfs.emd[,1:10]), main = 'IMFs of FKF data (EMD)')

par(mfrow = c(1,1))

plot(x = 1:nrow(imfs.emd[,3]), y = t(imfs.emd[,3]), type= 'l')

fft(imfs.emd$`V1` )  %>% Arg()   %>% plot

(test1 <- hilbertspec(df.imf.x$imf))

plot(test1$energy, type = 'b')

#spectrogram(test1$amplitude[,1],test1$instantfreq[,1])

test1.amp <- data.table(n = df.n$n, test1$amplitude)  %>%
  melt(id.vars = "n", value.name = 'amplitude', variable.name = "imf")


test1.freq.0 <- data.table(n= df.n$n, test1$instantfreq )
names(test1.freq.0) <- c("n", paste0("V", 1:(length(names(test1.freq.0)) - 1 ) ) )

test1.freq <- test1.freq.0 %>% 
  melt(id.vars = "n", value.name = 'freq', variable.name = "imf")

test1.spectrum <- merge(test1.freq, test1.amp, by = c("n","imf"))

 
ggplot(test1.spectrum, aes(x= n, y = amplitude, color = imf, group = imf )) +   geom_line() +
   facet_wrap(~ imf) + ggtitle("Ampitude by time") +  theme(legend.position = 'none')
 
ggplot(test1.spectrum, aes(x= n, y = freq, color = imf, group = imf )) +  
  geom_point() + geom_line() +
  facet_wrap(~ imf) + ggtitle("Freq by time") +  theme(legend.position = 'none')



ggplot(test1.spectrum[freq > 0  ], aes(x = freq)) +  
  geom_histogram(bins=40, alpha = 0.6, aes( fill = imf)) + 
  geom_density(color = 'grey50', size = 1, adjust = .5 ) + 
  facet_wrap(~ imf, nrow = 3, scale='free_y') + 
  theme(legend.position = 'bottom') + ggtitle("Freq histograms, EMD algorithm")




########



eemd <-  ceemdan(kalman.g, ensemble_size = 5000, num_imfs = 8, noise_strength = .1
                 , num_siftings = 50,  S_number = 5, threads = 6, rng_seed = 10)

#3plot(eemd)

data.table(eemd)[,-c("Residual"), with = FALSE]  -> eemd.dt

test2 <- hilbertspec(eemd) 

test2$energy

# plot(test2$energy, type = 'b')


test2.amp <- data.table(n = df.n$n, test2$amplitude)  %>%
  melt(id.vars = "n", value.name = 'amplitude', variable.name = "imf")


test2.freq.0 <- data.table(n= df.n$n, test2$instantfreq )
names(test2.freq.0) <- c("n", paste0("V", 1:(length(names(test2.freq.0)) - 1 ) ) )


test2.freq <- test2.freq.0 %>% 
  melt(id.vars = "n", value.name = 'freq', variable.name = "imf")

test2.spectrum <- merge(test2.freq, test2.amp, by = c("n","imf"))

ggplot(test2.spectrum[freq > 0 ], aes(x= n, y = amplitude, color = imf, group = imf )) +   geom_line() +
  facet_wrap(~ imf,nrow = 2) + ggtitle("Amplitude by time") + theme(legend.position = 'none')


ggplot(test2.spectrum[freq > 0 ], aes(x= n, y = freq, color = imf, group = imf )) +  geom_point() + geom_line() +
  facet_wrap(~ imf) + ggtitle("Freq by time, EEMD algo")



ggplot(test2.spectrum[freq > 0  ], aes(x = freq)) +  
  geom_histogram(bins=40, alpha = 0.6, aes( fill = imf)) + 
  geom_density(color = 'grey50', size = 1, adjust = .5 ) + 
  facet_wrap(~ imf, nrow = 3, scale='free_y') + 
  theme(legend.position = 'bottom') + ggtitle("Freq histograms, EEMD algorithm")


plot(fks(fkf.obj) ) 


library(GENEAread)

GENEAread::stft(eemd.dt$`IMF 3`,type = 'svm') %>% plot

seewave::hilbert(ts(df.n$g)) %>% Arg() %>% diff() %>% 
  
  
seewave::hilbert(ts(imfs.emd$`V1`) ) %>% Arg() %>% diff() %>% plot

seewave::hilbert(ts(eemd.dt$`IMF 1`) ) %>% Arg() %>% diff() %>% plot

seewave::hilbert( kalman.g[1:300], f = 1 ) %>% Arg() %>% diff() %>% plot



fft(imfs.emd$`V1` )  %>% Arg() %>% diff()  %>% plot

fft(eemd.dt$`IMF 1` )  %>% Arg() %>% diff()  %>% plot



n <- 10000
sig  <-   rnorm(n+1, 1,0.2) *  sin(2* pi * 0.3 *  (0:n)) +  rnorm(n+1, 1,0.4)  * sin(2* pi *.2 * (0:n))  +
  .2*rnorm(n+1, 0,1) %>% ts
  
#  plot(sig, type='l')

#Acf(sig ,lag.max = 500 )


#seewave::hilbert(ts(sig)) %>% Arg() %>% atan %>% plot(type = 'l')

sig.stft <- stft(sig, type = 'sum',plot.it = TRUE
                 , calc.null = TRUE, win = 9, reassign = TRUE
                 , wtype = 'uniform.window'
                 ) 


dd <- seewave::hilbert(sig) %>% Arg() %>% diff()

plot(1000* dd/(2*pi), main= 'Frequency, mHz', ylab = 'mHz')





