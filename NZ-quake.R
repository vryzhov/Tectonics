
library(data.table)
library(magrittr)
library(ggplot2)
library(ggplot2)
library(forecast)
library(seewave)
library(viridis)

library(hht)
library(EMD)

library(fasttime)

library(Rlibeemd)

library(plotly)

library(hrbrthemes)

library(tuneR)

##library(Hmisc) # to discretize

files = c(  ## NZ
  
  "../../tectonics/data/fdsnws-dataselect-2016-11-01.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-02.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-03.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-04.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-05.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-06.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-07.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-08.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-09.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-10.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-11.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-12.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-13.csv",
  "../../tectonics/data/fdsnws-dataselect-2016-11-14.csv"
)  




files = c(  ## Mexico
  
 # "../../tectonics/data/fdsnws-dataselect-2021-08-15.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-16.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-17.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-18.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-19.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-20.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-21.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-22.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-23.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-24.csv",
 # "../../tectonics/data/fdsnws-dataselect-2021-08-25.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-26.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-27.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-28.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-29.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-30.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-08-31.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-01.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-02.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-03.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-04.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-05.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-06.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-07.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-09-08.csv"
)





files <- c(     # Flores sea
  
  "../../tectonics/data/fdsnws-dataselect-2021-12-01.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-02.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-03.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-04.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-05.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-06.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-07.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-12-08.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-09.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-10.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-11.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-12.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-13.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-12-14.csv"

) 



files <- c( # SSB, 7.5  2021/11/28 10:52:13   -4.490   -76.846  123.0  NORTHERN PERU
  
  "../../tectonics/data/fdsnws-dataselect-2021-11-15.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-16.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-17.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-18.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-19.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-20.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-21.csv",  
  "../../tectonics/data/fdsnws-dataselect-2021-11-22.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-23.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-24.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-25.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-26.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-27.csv",
  "../../tectonics/data/fdsnws-dataselect-2021-11-28.csv"
  
) 

files <- c( #  MIDW 7.5  2021/11/28 10:52:13   -4.490   -76.846  123.0  NORTHERN PERU
  
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-15.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-16.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-17.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-18.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-19.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-20.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-21.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-22.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-23.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-24.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-25.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-26.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-27.csv",
 "../../tectonics/data/fdsnws-dataselect-MIDW-2021-11-28.csv"

)




# 
# 
# files <- c(  # SSB  accelerometer
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-15.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-16.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-17.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-18.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-19.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-20.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-21.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-22.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-23.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-24.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-25.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-26.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-27.csv",
#      "../../tectonics/data/fdsnws-dataselect-SSB-HNZ-2021-11-28.csv"
# )





files <- c(  # PFO  
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-15.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-16.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-17.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-18.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-19.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-20.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-21.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-22.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-23.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-24.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-25.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-26.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-27.csv",
  "../../tectonics/data/fdsnws-dataselect-PFO-LHZ-2021-11-28.csv"
)




files <- c(  # MIDW, VHZ
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-15.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-16.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-17.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-18.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-19.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-20.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-21.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-22.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-23.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-24.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-25.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-26.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-27.csv",
  "../../tectonics/data/fdsnws-dataselect-MIDW-VHZ-2021-11-28.csv"
)




files <- c(  # MIDW, LGZ
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-15.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-16.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-17.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-18.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-19.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-20.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-21.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-22.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-23.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-24.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-25.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-26.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-27.csv",
  "../../tectonics/data/fdsnws-dataselect-ST-LGZ-2021-11-28.csv"
)





## PERU ??? 
files <- c(    # MEMB VGZ http://ds.iris.edu/mda/SG/MEMB/--/VGZ/?starttime=2005-02-15&endtime=2599-12-31
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-15.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-16.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-17.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-18.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-19.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-20.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-21.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-22.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-23.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-24.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-25.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-26.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-27.csv",
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-11-28.csv"

)


## see  http://www.seismologie.be/en/gravimetry/observations/real-time-g
## for Station of Membach (MEMB)
## Province of Liège




# 6.0  2021/09/27 06:17:22  +35.252   +25.260    6.0  CRETE, GREECE
# 5.8  2021/09/26 17:12:07  +13.927  +120.542   82.0  MINDORO, PHILIPPINES
# 6.5  2021/09/22 09:57:08  +12.160   -87.854   27.0  NEAR COAST OF NICARAGUA
# 5.9  2021/09/21 23:15:53  -37.488  +146.364   17.0  NEAR S.E. COAST OF AUSTRALIA
# 6.5  2021/09/21 13:14:30  -36.785   -74.032   16.0  OFF COAST OF CENTRAL CHILE
# 6.1  2021/09/20 20:25:25  +46.406  +152.411   39.0  KURIL ISLANDS
# 5.8  2021/09/20 18:34:48  -21.086  -173.778   22.0  TONGA ISLANDS
# 5.7  2021/09/16 17:51:55  +55.029  -156.490   23.0  SOUTH OF ALASKA

### CRETE
files <- c( ##  6.0  2021/09/27 06:17:22  +35.252   +25.260    6.0  CRETE, GREECE
            ## startdate="2021-09-13"
            ## enddate="2021-09-30"
 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-15.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-16.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-17.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-18.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-19.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-20.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-21.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-22.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-23.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-24.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-25.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-26.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-27.csv", 
  "../../tectonics/data/fdsnws-dataselect-MEMB-VGZ-2021-09-28.csv" 
 
)

## first_date <- '2021-11-15 00:00:00'

first_date <- '2021-09-15 00:00:00'


lapply(files,fread) %>% rbindlist -> tectonics.data


(Freq_Hz <- nrow(tectonics.data)/(length(files)*24*60*60) ) # 0.1 Hz

tectonics.data[, z := V1 -  mean(tectonics.data$V1)]

tectonics.data[, time := as.POSIXct(first_date,tz = 'UTC') + (.I - 1)/Freq_Hz ]

#tectonics.data[, T := lubridate::floor_date(time, "second")]

#setkey(tectonics.data, T)

#tectonics.data.avg <- tectonics.data[,.(Z = mean(z)), by = T]

#setkey(tectonics.data.avg, T)
# 
# g <- tectonics.data[ ,.(T = time, Z =z)] %>% ggplot(aes(x = T, y = Z)) + geom_line()
# ggplotly(g)


g <- tectonics.data[ ,.(T = time, Z = z)] %>% ggplot(aes(x = T, y = Z)) + geom_line()
ggplotly(g)



#fwrite(tectonics.data[,.(T = time, Z = V1)], "data/Peru-MEMB-VGZ.csv")


# https://math.mcmaster.ca/~bolker/eeid/2010/Ecology/Spectral.pdf

del <- 1/Freq_Hz # sampling interval = 10 seconds
x.spec <- spectrum(tectonics.data$z,log = 'yes', span=400,plot=FALSE)
spx <- (x.spec$freq/del) * 1e6 
spy <- 2*x.spec$spec

g2 <- ggplot(data.table(cbind(spy,spx)),aes(x = spx, y = spy) )+ geom_line() +  
  xlab("frequency, mcHz") + ylab("spectral density") + scale_y_sqrt()
ggplotly(g2)

par(mfrow = c(1,1))

seewave::spec(tectonics.data$z, f = Freq_Hz , plot = TRUE,  ## f = 0.1 Hz
     fftw = TRUE, wl = 2, wn = "rectangle") %>% 
  data.table -> sig.freq 
sig.freq[, Freq_mcHz := 1000 * x * 1e6] # from kHz to mcHz


sig.freq[,low.band := ifelse(Freq_mcHz > 1 & Freq_mcHz < 50, '0 - 50 mcHz', '50 - 46000 mcHz ')]

g3 <- ggplot(sig.freq[Freq_mcHz < 60000 & Freq_mcHz > 1, .(Freq_mcHz,Amplitude = y, low.band)], 
       aes(x = Freq_mcHz, y = Amplitude)  ) + geom_line() +
  #  geom_bar(stat = 'identity' ,  fill='grey40', position = 'dodge', color= 'grey50')  + 
  # geom_smooth(se = FALSE, span = 0.08,  formula = 'y ~ x', method = 'loess', size = 2, color="blue") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  #scale_x_log10() +
  facet_wrap(~low.band, nrow = 2, scales = 'free') + ggtitle("Low band and High band Spectra or Peruvian signal")
ggplotly(g3)





### Smoother
tectonics.data.filtered <- tectonics.data$z %>% 
  stats::filter(x = . , filter = c(1:5, 6, 5:1)/sum(c(1:5, 6, 5:1))
                , method = 'convolution', sides = 2, init = tectonics.data$z[1:16] %>% rev, circular = FALSE)

tectonics.data.smooth <- (tectonics.data$z -  tectonics.data.filtered)

tectonics.data.smooth[1:100000] %>% plot(type = 'l',cex = 0.2)

seewave::spec(tectonics.data.smooth[16:20000] , f = Freq_Hz , plot = FALSE,  ## f = 0.1 Hz
     fftw = FALSE, wl = 50) %>% 
  data.table -> smooth.freq 
smooth.freq[, Freq_mcHz := 1000 * x * 1e6] # from kHz to mcHz


smooth.freq[,low.band := ifelse(Freq_mcHz > 1 & Freq_mcHz < 50, '0 - 50 mcHz', '50 - 46000 mcHz ')]

g31 <- ggplot(smooth.freq[Freq_mcHz < 60000 & Freq_mcHz > 1, .(Freq_mcHz,Amplitude = y, low.band)], 
             aes(x = Freq_mcHz, y = Amplitude)  ) + geom_line() +
  #  geom_bar(stat = 'identity' ,  fill='grey40', position = 'dodge', color= 'grey50')  + 
  # geom_smooth(se = FALSE, span = 0.08,  formula = 'y ~ x', method = 'loess', size = 2, color="blue") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  #scale_x_log10() +
  facet_wrap(~low.band, nrow = 2, scales = 'free') + ggtitle("Low band and High band Spectra or Peruvian signal")
ggplotly(g31)




par(mfrow = c(1,1))

spctr <- spectro(tectonics.data[,z], f = Freq_Hz, ovlp = 60, zp = 200
                 ,wl = 80, norm = TRUE, wn = 'bartlett',plot = TRUE, flog = TRUE
                 ,palette = function(n) viridis(n), scale = FALSE)


filled.contour(x=spctr[[1]],y=spctr[[2]], z=t(spctr[[3]]), 
               color.palette =  function(n) viridis(n) , #function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
               main= "Spectrogram of the unfiltered signal", xlab = 'Time', ylab = 'Frequency', nlevels = 6) 


spctr.trunc <- spectro(tectonics.data[1:50000,z], f = Freq_Hz, ovlp = 10, zp = 100
                 ,wl = 20, norm = TRUE, wn = 'hamming',plot = TRUE, flog = TRUE
                 ,palette = function(n) viridis(n), scale = FALSE)

filled.contour(x=spctr.trunc[[1]],y=spctr.trunc[[2]], z=t(spctr.trunc[[3]]), 
               color.palette =  function(n) viridis(n) , #function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
               main= "Spectrogram of the unfiltered signal", xlab = 'Time', ylab = 'Frequency', nlevels = 10) 






seewave::ffilter(tectonics.data$z , f = Freq_Hz
                 , from  = 1.2e-3  # cut 24 hours
                 , wl = 200, bandpass = TRUE, ovlp = 20, wn = "hanning")  %>% 
  data.table() %>% setnames("z") -> filtered01


seewave::ffilter(filtered01$z , f = Freq_Hz
                 , from  = 1.2e-3  # cut 24 hours
                 , wl = 200, bandpass = TRUE, ovlp = 20, wn = "hanning")  %>% 
  data.table() %>% setnames("z") -> filtered0

# 
seewave::bwfilter(ftectonics.data$z , f = Freq_Hz, n = 3
                 , from =  1/(300*60)  # 300 min = 60 mcHz
                 , to = 1/(60*60)     # 60 min = 280 mcHz
                 ,  bandpass = TRUE)  %>%
  data.table() %>% setnames("z")   -> filtered1


seewave::bwfilter(filtered1$z , f = Freq_Hz, n = 2
                  , from =  1/(300*60)  # 300 min = 60 mcHz
                  , to = 1/(60*60)     # 60 min = 280 mcHz
                  ,  bandpass = TRUE)  %>%
  data.table() %>% setnames("z")   -> filtered2


seewave::bwfilter(filtered2$z , f = Freq_Hz, n = 2
                  , from =  1/(300*60)  # 300 min = 60 mcHz
                  , to = 1/(60*60)     # 60 min = 280 mcHz
                  ,  bandpass = TRUE)  %>%
  data.table() %>% setnames("z")   -> filtered3


seewave::bwfilter(filtered3$z , f = Freq_Hz, n = 2
                  , from =  1/(300*60)  # 300 min = 60 mcHz
                  , to = 1/(60*60)     # 60 min = 280 mcHz
                  ,  bandpass = TRUE)  %>%
  data.table() %>% setnames("z")   -> filtered4



seewave::bwfilter(filtered4$z , f = Freq_Hz, n = 3
                  , from =  1/(300*60)  # 300 min = 60 mcHz
                  , to = 1/(60*60)     # 60 min = 280 mcHz
                  ,  bandpass = TRUE)  %>%
  data.table() %>% setnames("z")   -> filtered

# 
# seewave::ffilter(filtered3$z , f = Freq_Hz, n = 2
#                  , from =  1/(300*60)  # 300 min = 60 mcHz
#                  , to = 1/(60*60)     # 60 min = 280 mcHz
#                  , wl = 200, bandpass = TRUE, ovlp = 50, wn = "rectangle")  %>%
#   data.table() %>% setnames("z")   -> filtered

# seewave::ffilter(filtered2$z , f = Freq_Hz
#                  , from =  1/(300*60)  # 300 min = 60 mcHz
#                  , to = 1/(60*60)     # 60 min = 280 mcHz
#                  , wl = 200, bandpass = TRUE, ovlp = 50, wn = "rectangle")  %>% 
#   data.table() %>% setnames("z")   -> filtered3
# 
# seewave::ffilter(filtered3$z , f = Freq_Hz
#                  , from  = 1/(300*60)  # 300 min = 55 mcHz
#                  , to = 1/(60*60)     # 60 min = 277 mcHz
#                  , wl = 200, bandpass = TRUE, ovlp = 50, wn = "rectangle")  %>%
#   data.table() %>% setnames("z")   -> filtered4
# 
# seewave::ffilter(filtered4$z , f = Freq_Hz
#                  , from  = 1/(300*60)  # 300 min = 55 mcHz
#                  , to = 1/(60*60)     # 60 min = 277 mcHz
#                  , wl = 200, bandpass = TRUE, ovlp = 50, wn = "rectangle")  %>%
#   data.table() %>% setnames("z")   -> filtered
# 



tectonics.data[ ,.(T = .I, z)] %>% ggplot(aes(x = T, y = z)) + geom_line()


filtered[,.(T = .I, z)] %>% ggplot(aes(x = T, y = z)) + geom_line() + ggtitle("Filtered signal") + 
  theme_minimal()



x.spec <- spectrum(tectonics.data$z,log = 'yes', span=20,plot=FALSE)
spx <- (x.spec$freq/del) * 1e6 
spy <- 2*x.spec$spec

dt.original <- data.table(spy = as.numeric(spy),spx = as.numeric(spx), signal = 'original')

x.spec <- spectrum(filtered$z,log = 'yes', span=20,plot=FALSE)
spx <- (x.spec$freq/del) * 1e6 
spy <- 2*x.spec$spec

dt.filtered <- data.table(spy = as.numeric(spy),spx = as.numeric(spx), signal = 'filtered')


dt.spectrum <- funion(dt.original, dt.filtered)

dt.spectrum[,signal := factor(signal, levels = c("original","filtered"))]


g4 <-  ggplot(dt.spectrum[spy > 0 & spx < 2600],aes(x = spx, y = spy) ) + geom_line() +  
  xlab("frequency, mcHz") + ylab("spectral density") + facet_wrap(~signal, nrow = 2, scales = 'free_y') + 
  scale_y_log10() + ggtitle("Spectra of Original and Filtered signals") + theme_minimal()

ggplotly(g4)



# 
# ggplot(sig.freq[Freq_mcHz < 146000, .(Freq_mcHz,Amplitude = y, low.band)], 
#        aes(x = Freq_mcHz, y = Amplitude)  ) + geom_line() +
#   #  geom_bar(stat = 'identity' ,  fill='grey40', position = 'dodge', color= 'grey50')  + 
#   # geom_smooth(se = FALSE, span = 0.08,  formula = 'y ~ x', method = 'loess', size = 2, color="blue") + 
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
#   facet_wrap(~low.band, nrow = 2, scales = 'free')
# 




spctr <- spectro(filtered[,z], f = Freq_Hz, ovlp = 70, zp = 180
                 ,wl = 50*512,  norm = TRUE, wn = 'hamming',plot = TRUE, flog = TRUE
                 ,palette = function(n) viridis(n), scale = TRUE)



filled.contour(x=spctr[[1]],y=spctr[[2]] * 1000* 1000 * 1000, z=t(spctr[[3]]),   nlevels = 10,
               color.palette =  function(n) viridis(n) , #function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
               main= "Spectrogram of the filtered signal", xlab = 'Time', ylab = 'Frequency, mcHz') 


signal.wave <- Wave(left = tectonics.data[,z],  samp.rate = Freq_Hz, bit =16)

filtered.wave <- Wave(left = filtered[,z], samp.rate = Freq_Hz, bit =16)

ggspectro(filtered.wave, ovlp = 60, zp = 180, wl =  7*512, wn = 'hamming', fftw  = TRUE) +
      geom_tile(aes(fill = amplitude) ) + stat_contour(color = 'grey20', size = 0.2) +
  ylim(c(0,5e-7))

## 300 mcHz = 0.3 mHz = 3e-4 Hz = 0.000 000 03 Khz = 3e-7 kHz

# F = 800 mcHz => T =  20 min


Signal.freq.phase <- ifreq(filtered.wave,f = Freq_Hz, plot = TRUE)


Freq.dt <- data.table(Signal.freq.phase$f)
Freq.dt[, f_mcHz := ifreq*1000*1000*1000]


ggplot(Freq.dt[f_mcHz < 500], aes(x= time, y = f_mcHz) ) + # geom_jitter(width = 0.5, height = 0.5) + 
  geom_point(size = 0.5, alpha = .1, color = 'blue') 

 #+ #+ ylim(c(1, 1000) )
   #geom_smooth(se = FALSE, span = .1)



Freq.500.dt <- Freq.dt[f_mcHz < 500]

# 
# Freq.500.dt$time.cuts <- cut2(Freq.500.dt$time, g = 10)
# Freq.500.dt$freq.cuts <- cut2(Freq.500.dt$f_mcHz, g = 10)



Freq.500.dt$time.cuts <- cut(Freq.500.dt$time, breaks = 120)
Freq.500.dt$freq.cuts <- cut(Freq.500.dt$f_mcHz, breaks = 140)


Freq.500.dt[, z := .N, by = .(time.cuts, freq.cuts)]

Freq.500.dt[,time.tick := mean(time), by = time.cuts]

Freq.500.dt[,freq.tick := mean(f_mcHz), by = freq.cuts]


setkey(Freq.500.dt, time.tick, freq.tick)
       

Freq.500.dt[,.(time.cuts, freq.cuts, z)] %>% unique() %>% 
  ggplot() +
  geom_tile(aes(x = time.cuts, y = freq.cuts, fill = log(z)) ) + 
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()

  # 
  # 
  # filled.contour(x=.$time.tick,y=.$freq.tick, z=t(.$z)
  #                ,   nlevels = 5,
  #                color.palette =  function(n) viridis(n) , #function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
  #                main= "Spectrogram of the filtered signal", xlab = 'Time', ylab = 'Frequency, kHz') 
  # 
  # 

#resamp(tectonics.data$z, f=.1, g = .05) %>% data.table -> Signal

# 
# tectonics.data -> Signal
# 
# Signal[, z := V1 -mean(Signal$V1)]
# 
# 
# Signal[abs(z) < 100000,.(T = .I,z)] %>% ggplot(aes(x = T, y= z)) + geom_line()
# 
# # https://math.mcmaster.ca/~bolker/eeid/2010/Ecology/Spectral.pdf
# 
# del <- 10 # sampling interval = 10 seconds
# x.spec <- spectrum(Signal$z,log = 'yes', span=400,plot=FALSE)
# spx <- (x.spec$freq/del) * 1e6 
# spy <- 2*x.spec$spec
# 
# plot(spy~spx,xlab="frequency, mcHz",ylab="spectral density",type="l")
# 
# 
# plot(spy~spx,xlab="frequency, mcHz",ylab="spectral density",type="l", xlim= c(18000, 45000), ylim = c(0,2e10))
# 
# # 
# spectrum(Signal$z, spans = 100) 

# par(mfrow=c(1,1))
# Acf(tectonics.data$z,lag.max = 10000)
par(mfrow=c(1,1))

#plot(ts(tectonics.data$z))

#tectonics.data$z %>% #filter( sides=2, filter=rep(1,50)) 
  # seewave::ffilter(ts(tectonics.data$z) , f = .1, from  = 0.002, to = 0.1, wl = 512)  %>% 
  # data.table -> filtered

  
  seewave::ffilter(Signal$z , f = .10
                   , from  = (1/300)/60  # 300 min 
                   , to = (1/20)/60      # 20 min 
                   , wl = 200, bandpass = TRUE)  %>% 
    data.table -> filtered0
  
  seewave::ffilter(filtered0$V1 , f = .10
                   , from  = (1/300)/60  # 300 min 
                   , to = (1/20)/60      # 20 min 
                   , wl = 200, bandpass = TRUE)  %>% 
    data.table -> filtered
  
  
summary(filtered)  

x.spec <- spectrum(filtered$V1, span= 2, plot = TRUE, log='yes')

spx <- (x.spec$freq/del) * 1e6 
spy <- 2*x.spec$spec

plot(spy~spx,xlab="frequency, mcHz",ylab="spectral density",type="l" , xlim=c(1, 50))






capped <- filtered[,.(T = .I, z = V1 ) ][ , .(T, zf =  ifelse(abs(z) < 8000, z, NA ) )]

summary(capped$zf)

capped %>%
  ggplot(aes(x= T, y = zf)) + geom_line() -> gg

ggplotly(gg)

# 
# spctr <- seewave::spectro( toFFT 
#                            , f = 1/300, wl=24, ovlp = 80
#                            , fftw = FALSE, wn = 'rectangle',   cont = TRUE, zp = 1000, scale = FALSE)

spctr <- spectro(capped[!is.na(zf),zf], f = 1, ovlp = 80, zp = 30
                 ,wl = 100, norm = TRUE, wn = 'bartlett',plot = TRUE, flog = TRUE
                 ,palette = function(n) viridis(n), scale = FALSE)

spctr[[2]] %>% length


filled.contour(x=spctr[[1]],y=spctr[[2]], z=t(spctr[[3]]), 
               color.palette =  function(n) viridis(n) , #function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
               main= "Spectrogram of filtered signal", xlab = 'Time', ylab = 'Frequency') 



########

# Signal <- tectonics.data$z


Signal.freq.phase <- ifreq(Signal,f = Freq_Hz, plot = TRUE)


Freq.dt <- data.table(Signal.freq.phase$f)
Freq.dt[, f_mcHz := ifreq*1000*1000*1000]

ggplot(Freq.dt, aes(x= time, y = f_mcHz) ) + geom_line() + ylim(c(1, 1000) )




eemd <-  ceemdan(resamp(Signal, f = Freq_Hz, g = .05)[1:25000], ensemble_size = 50, num_imfs = 30, noise_strength = 0.2
                 , num_siftings = 0,  S_number = 30, threads = 6, rng_seed = 10)


data.table(eemd)[,-c("Residual"), with = FALSE]  -> eemd.dt

test2 <- hilbertspec(eemd) 

test2$energy %>% plot(type = 'b',cex = .2)

plot(eemd[,16:25] ) 

IMF_NO <- 12

# bartlett, blackman, flattop, hamming, hanning, or rectangle
imf.spctr <- seewave::spectro(eemd[1:600,IMF_NO] 
                              , f = Freq_Hz, wl=6, ovlp = 30
                              , fftw = FALSE, wn = 'blackman', cont = TRUE, zp = 40, scale = FALSE)




