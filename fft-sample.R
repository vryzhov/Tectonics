

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


theme_set(theme_minimal())


## https://labrtorian.com/tag/fast-fourier-transform/

f1 <- function(t, l) {
  if (t < -l | t > l) {
    res <- 0
  } else {
    res <- sin(2*pi * 1 *t) + 2*sin(2*pi* 3 *t + 3.14/7)
  }
  return(res)
}
f1v <- Vectorize(f1)


library(gridExtra)
N <- 1000
df <- NULL
a <- 0
b <- 25
t <- seq(a, b, length.out = N)



Signal <- (df.signal$Gravity - mean(df.signal$Gravity)) ##%>% stats::filter( rep(1,10),circular = TRUE)
df <- NULL 
a <- 1
b <- 2*length(Signal)
N <- 60*length(Signal)
t <- seq(a, b, length.out = N)




tmp <- data.frame(t = t, y =  rep(NA,N), z = rep(NA,N), absz = rep(NA,N), f = rep(NA,N), nT = rep(NA,N))

df <- NULL
for(i in 1:30) {
#  s <- c(Signal, - rev(Signal) )
  tmp <- tmp %>%
     mutate(y = f1v(t, 2 * i)) %>%
     mutate(y =  c( rep( c(Signal,  -rev(Signal) ), i)   , rep(0.0, N - i*b ) )  ) %>%
    mutate(z = fft(y)) %>%
    mutate(absz = abs(z)) %>%
    mutate(deltaf = 1 / (N * ((b - a) / N))) %>%
    mutate(f = (1:N - 1) * deltaf) %>%
    mutate(nT = factor(paste(2 * i, "periods"), levels = paste(2 * i, "periods")))
  df <- bind_rows(df, tmp)
}

df <- data.table(df)


df %>%
  group_by(nT) %>%
  ggplot(aes(x = f, y = y)) +
  geom_line() +
  facet_wrap(~nT)
 

df[nT == '60 periods'] %>% 
  ggplot(aes(x = f, y = absz)) +
  geom_line() +
  xlim(c(0,15))

df %>%
  group_by(nT) %>%
  ggplot(aes(x = f, y = absz)) +
  geom_line() +
  xlim(c(0,N/(2*b))) +
  facet_wrap(~nT)



############


as.ts(Signal) %>% StructTS(type = "trend") -> kf

kf.smooth <- tsSmooth(kf) 
kf.smooth[,1] %>% plot(main = 'Smoothed signal')

# 
# kf.imf.x <- EMD::emd(as.numeric(kf.smooth[,1]), 1:300,  plot.imf = FALSE, max.sift = 1500
#                      , max.imf = 10, tol=0.000001, boundary = "wave"
#                      , stoprule = "type2" )

(StructTS(Signal,type = "level") %>% tsSmooth() -> kf.level )%>% plot(main = 'Signal level')

kf.detrended <- kf.smooth[,1] - kf.level[,1]

kf.detrended %>% plot(main = "Detrended smoothed signal")

#######


#######

kf.imf.x <- ceemdan(as.numeric(kf.detrended[25:300]) 
        , ensemble_size = 2000, num_imfs = 9, noise_strength = .2
        , num_siftings = 150,  S_number = 20, threads = 6, rng_seed = 10)


plot(kf.imf.x[,"Residual"], main = "Residual", type = "l")


kf.imf <- data.table(t=25:300, Signal = as.numeric(kf.detrended[25:300])
                     , kf.imf.x)

melt(kf.imf, id.vars = "t", variable.name = 'IMF') %>% 
  ggplot(aes(x = t, y = value)) + geom_line() + facet_wrap(~IMF, scales = 'free_y')
 

kf.imf.hilbert <- hilbertspec(kf.imf.x[,(1:(dim(kf.imf.x)[2] ))] )

kf.imf.hilbert$energy

plot(kf.imf.hilbert$energy, type = 'b')

kf.freq.0 <- data.table(t = 25:300, kf.imf.hilbert$instantfreq )

kf.amp.0 <- data.table(t = 25:300, kf.imf.hilbert$amplitude )

kf.amp.0[,Amp0 := rowSums(kf.amp.0)]

kf.amp.0[, Amp := Amp0 - t]
kf.amp.0[, Amp0 := NULL]

names(kf.freq.0) <- c("t", paste0("IMF_", 1:(length(names(kf.freq.0)) - 1 ) ) )
names(kf.amp.0) <- c("t", paste0("IMF_", 1:(length(names(kf.amp.0)) - 2 )), "Amp" ) 


kf.freq <- kf.freq.0 %>% 
  melt(id.vars = "t", value.name = 'freq', variable.name = "imf")
kf.amp <- kf.amp.0 %>% 
  melt(id.vars = c("t", "Amp"), value.name = 'amp', variable.name = "imf")


ggplot(kf.freq, aes(x= t, y = freq, color = imf, group = imf )) +  
  geom_point(size = .2) + geom_line() +
  facet_wrap(~ imf) + ggtitle("Freq by time") +  theme(legend.position = 'none')


ggplot(kf.amp, aes(x= t, y = amp, color = imf, group = imf )) +  
  geom_point(size = .2) + geom_line() +
  facet_wrap(~ imf) + ggtitle("Amplitude by time") +  theme(legend.position = 'none')


ggplot(kf.freq, aes(x= t, y = freq, color = imf, group = imf )) +  
  geom_point(size = 0.2) + geom_line() +
  facet_wrap(~ imf, scale = 'free_y') + ggtitle("Freq by time") +  theme(legend.position = 'none')




ggplot(kf.freq, aes(x = freq)) +  
  geom_histogram(bins=60, alpha = 0.6, aes( fill = imf)) + 
  geom_density(color = 'grey50', size = 1, adjust = .3 ) + 
  facet_wrap(~ imf, nrow = 3, scale='free_y') + 
  theme(legend.position = 'none') + 
  ggtitle("Freq histograms for KF smooth, EEMD algorithm")


kf.spectrum <- merge(kf.freq, kf.amp, by = c("t","imf"))



ggplot(kf.spectrum[imf %in% paste0("IMF_", 4:9)], aes(x= t, y = freq, color = imf, group = imf )) +  
  geom_point(size = 0.2) + geom_line(aes(size = exp(amp)) ) +
   ggtitle("Freq by time") +  theme(legend.position = 'bottom') + scale_y_sqrt()



# 
# 
# ggplot(kf.spectrum[imf %in% paste0("IMF_", 8:9) & t < 299], aes(x= t, y = freq, color = imf, group = imf )) +  
#   geom_point(size = 0.2) + geom_line(aes(size = exp(amp)) ) +
#   ggtitle("Freq by time") +  theme(legend.position = 'bottom') 




ggplot(kf.spectrum[imf %in% paste0("IMF_", 5:9) ]) + 
 stat_density_2d_filled(aes(x=t,y=freq/300, alpha = amp), n = 300, h = c(5, 0.0005))  +
  #geom_point(aes(x=t,y=freq,alpha = amp)) +
  scale_y_sqrt( ) + 
    ylab("Hz") + theme(legend.position = 'bottom')
  




ggplot(kf.spectrum[imf %in% paste0("IMF_", 7)]) + 
  stat_density_2d_filled(aes(x=t,y=freq/300, alpha = amp), n = c(300,400), h = c(.001,.001))  + 
  #geom_point(aes(x=t,y=freq,alpha = amp)) +
  scale_y_sqrt() 



########

a <-  sin(2*pi* 3*(1:(300*300))/(300*300 ) )


# 3 periods for 300*300 = 90000 seconds 
# => 3/90000 = 3.333333e-05 Hz = 
# (.) * 1000 * 1000 = 33.3 mcHz

plot(a)

hht <- ifreq(a,f = 1)

length(a)* hht$f[,2]* 1000 # freq in Hz by 5 min interval


length(a) * hht$f[,2] * 1000/3600 # freq in Hz


(length(a) * hht$f[,2] * 1000/3600 ) * 1000 * 1000 # freq in mcHz



######

  ######################### FFT Periodogram

TimeSeq <- 1:300

FreqSeq <- 0:(length(TimeSeq)-1)/length(TimeSeq)

# toFFT <- kf.smooth[TimeSeq,1]

#toFFT <- Signal

toFFT <- kf.detrended[TimeSeq]

# toFFT <- kf.level[TimeSeq,1]

#toFFT <- sin(2*pi* 3*(1:300)/300 )

#toFFT <- Signal[TimeSeq]

ModFft <- Mod(2*fft(toFFT)/length(TimeSeq) )
# 
# plot(FreqSeq, ModFft^2 , type = 'h', xlim=c(0,.06), lwd = 3
#      , main="Periodogram of smoothed signal")

#  Y <- cbind(Id = 1:length(TimeSeq), Freq = FreqSeq, ModFft^2) 
#  
#  Y[order(Y[,3]),] %>% data.table() %>% `[`(Id < 120) %>%tail(8) -> PerMax
# 
# PerMax[, T_hours:= (300/Freq)/3600]

Periodogram <- data.table(Frequency = FreqSeq, Pgram = (as.numeric(ModFft) )^2) 

PgramSum <- (as.numeric(ModFft) )^2 %>% sum

Periodogram[,Periodogram_total := Pgram/PgramSum]

Periodogram[,Periodogram := Periodogram_total * 2]

Periodogram[, `:=`(T_hours = 300/(Frequency*3600), F_mcHz = 1000000*(Frequency/300) )  ]


AmpMax <- 0.01
FreqMax <- 0.08
Periodogram[, lab := NULL]
Periodogram[ Periodogram  > AmpMax ,lab := paste0("T = ", round(T_hours,2), ' hrs')]

ggplot(Periodogram[Frequency < min(FreqMax, 0.5) & Frequency > 0], aes(x=F_mcHz, xend=F_mcHz, y=0, yend=Periodogram) ) +
  geom_point(aes(y = Periodogram, color = Periodogram  < AmpMax)) + ylab("Periodogram: Energy %") + xlab("Frequency, mcHz") +
  geom_segment(color = 'grey40', size =.5) + theme(legend.position = 'none') + 
  ggrepel::geom_text_repel(mapping = aes(y = Periodogram, label = lab)
                           , nudge_x = 0.0, nudge_y = 0.015,check_overlap = T
                           , max.overlaps = 10) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Periodogram of the filtered signal")




seewave::spectro(kf.smooth[TimeSeq,1] , f = 1/300, wl=20, ovlp = 5
                 , fftw = TRUE, wn = 'rectangle', noisereduction = NULL, zp = 0, osc = TRUE)


seewave::spectro(Signal, f = 1/300, wl=20, ovlp = 5, fftw = TRUE, wn = 'rectangle')


seewave::spectro(kf.detrended[TimeSeq], f = 1/300, wl=10, ovlp = 5, fftw = TRUE, wn = 'rectangle')



phonTools::spectrogram(kf.smooth[TimeSeq,1], fs = 1, 
                       windowlength=80,
                       timestep=1000,dynamicrange  = 50,
                       maxfreq=1/2,window="hamming")

#################


library(Rwave)


tmp <- cwtTh(kf.smooth[TimeSeq,1], noctave = 7.5, nvoice = 150 , moments = 70)


tmp <- cwtTh(Signal, noctave = 2 , nvoice = 200 , moments = 100)


x1 <-  sin(2*pi* 3*0:299/300 - 20 ) 
x2 <- sin(2*pi* 5*0:299/300  )
x3  <- sin(2*pi* 10*0:299/300 + 8 ) 

testSignal <- 3* x1 + 2 *x2 +  1.2 *x3 + #1 * x1* (1:300 - 1)/300  + 
      3 * sin(2*pi* (1 + 15*(1-0:299)/300) *0:299/300  )
 
plot(testSignal, type = 'l')

# plot(testSignal, type = 'l')

tmp <- cwtTh(testSignal, noctave = 7.5, nvoice = 150 , moments = 70)

tmp <- cgt(testSignal, nvoice = 150 , scale = 0.5)


smoothts(testSignal, windowsize = 1) %>% plot

tfmean(cgt(kf.smooth[TimeSeq,1], nvoice = 150 , scale = 0.5))

plot(testSignal)

###########

seewave::acoustat(kf.smooth[TimeSeq,1], f = 1/300, wl=20, ovlp = 90
                  , fftw = TRUE) %>% plot



