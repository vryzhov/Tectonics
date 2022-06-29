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



## see  http://www.seismologie.be/en/gravimetry/observations/real-time-g
## for Station of Membach (MEMB)
## Province of Liège


##http://www.seismologie.be/en/gravimetry/observations/real-time-g
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

first_date <- '2021-09-15 00:00:00'

lapply(files,fread) %>% rbindlist -> tectonics.data

(Freq_Hz <- nrow(tectonics.data)/(length(files)*24*60*60) ) # 0.1 Hz

tectonics.data[, z := V1 -  mean(tectonics.data$V1)]

tectonics.data[, time := as.POSIXct(first_date,tz = 'UTC') + (.I - 1)/Freq_Hz ]

g <- tectonics.data[ ,.(T = time, Z = z)] %>% ggplot(aes(x = T, y = Z)) + geom_line()
ggplotly(g)



### Save data
#fwrite(tectonics.data[,.(T = time, Z = V1)], "data/Crete-MEMB-VGZ.csv")



### Remove long period oscillations

emd.data <- tectonics.data[,.(idx = .I, z) ]

eemd <-  ceemdan(emd.data$z, ensemble_size = 200L, num_imfs = 10, noise_strength = .1
                 , num_siftings = 50,  S_number = 5, threads = 6, rng_seed = 10)

plot(eemd)

tectonics.data[,residue :=  as.integer(eemd[, dim(eemd)[2]] ) ]
tectonics.data[,signal := z -  residue] # IMF 1 

g.res <- tectonics.data[ ,.(T = time,  residue)] %>% ggplot(aes(x = T, y = residue)) + geom_line()
ggplotly(g.res)

g.sig <- tectonics.data[ ,.(T = time, Z = signal)] %>% ggplot(aes(x = T, y = Z)) + geom_line()
ggplotly(g.sig)


### Select some time period before the earthquake

tectonics.sample <- tectonics.data[time >= as.POSIXct('2021-09-25 08:00:00', tz = 'UTC') & 
               time <= as.POSIXct('2021-09-27 06:00:00', tz = 'UTC') ] 

g.sample <- tectonics.sample[,.(T = time, Z = signal)] %>% ggplot(aes(x = T, y = Z)) + geom_line()
ggplotly(g.sample)


#### Spectro


signal.wave <- Wave(left = tectonics.sample[,signal],  samp.rate = Freq_Hz, bit =16)


spectro(signal.wave, ovlp = 60, zp = 80, wl =  1*512, wn = 'hanning', fftw  = TRUE, osc = TRUE)


seewave::ggspectro(signal.wave, ovlp = 60, zp = 80, wl =  1*512#, dB = 'D'
                , wn = 'hanning', fftw  = TRUE, osc = TRUE) +
  geom_tile(aes(fill = amplitude) ) + stat_contour(color = 'grey20', size = 0.2)# + ylim(c(0,3e-6))



seewave::ggspectro(signal.wave, ovlp = 40, zp = 100, wl =2*512, dB = 'D'
                   , wn = 'hanning', fftw  = TRUE, osc = TRUE, flim=c(0,5e-6)) +
  geom_tile(aes(fill = amplitude) ) + stat_contour(color = 'grey20', size = 0.2)# + ylim(c(0,3e-6))



seewave::spectro(signal.wave, ovlp = 80, zp = 100, dB = 'D'
                 , wl =  3*512, wn = 'hanning', fftw  = TRUE
                 , osc = TRUE
                 , flim =c(0, 3e-6) )



### Filter  higher freqs

filtered.wave <- seewave::ffilter(signal.wave , f = Freq_Hz
                 , from  = 1e-9  # cut 24 hours
                 , to = 1e-6
                 , wl = 20, bandpass = TRUE, ovlp = 20, wn = "hanning") %>%
  Wave(left = .,  samp.rate = Freq_Hz, bit =16)



filtered.wave <- seewave::bwfilter(signal.wave , f = Freq_Hz
                                  , from  = 2e-3 # Hz
                                  , to = 0.02 # Hz
                                  , n = 1, bandpass = TRUE, output="Wave")


seewave::spectro(filtered.wave, ovlp = 80, zp = 100
                 , wl =  3*512, wn = 'hamming', fftw  = TRUE
                 , osc = TRUE
                 , flim =c(0, 3e-6) ,  dB = 'D')





