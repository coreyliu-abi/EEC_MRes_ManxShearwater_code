# set working directory
setwd("C:/Users/corey/OneDrive - Imperial College London/IC/PG/Summer Project/Code/R_code")


require(tidyr)
require(dplyr)
require(warbleR)
require(soundgen)
require(tuneR)
require(audio)
require(sound)
require(seewave)




# ---------- 1. Data downloading ----------------------------------------------------------

# import the data frame of list of recordings downloaded using XC_download_from_API.sh and bulk_download_XC.py
xc_manx_list <- read.csv('xc-manx.csv')
str(xc_manx_list)

# check the types of recordings 
unique(xc_manx_list$type)

# check range of recording duration
range(xc_manx_list$length)


# remove recordings that do not contain calls or songs
xc_manx_list <- filter(xc_manx_list, type != 'wing beats')

# get the list of recordings containing only Manx shearwater calls 
xc_manx_list_only <- filter(xc_manx_list, also == '[]')
xc_manx_list_mix <- filter(xc_manx_list, also != '[]')


# download all Manx shearwater recordings from Xeno-Canto - to do so un-comment the following code
# query_xc(qword = 'Puffinus puffinus', download = T, X = xc_manx_list_only, file.name = NULL,
#         parallel = 1, path = '../R_code/XC_download', pb = TRUE)

# query_xc(qword = 'Puffinus puffinus', download = T, X = xc_manx_list_mix, file.name = NULL,
#          parallel = 1, path = '../R_code/XC_download_multisp', pb = TRUE)


# ---------- 2. Recording format conversion ----------------------------------------------------------
# Add the path where SoX is located so that the following code can run - to do so uncomment the following code
new_path <- "C:/Program Files (x86)/sox-14-4-2"
current_path <- Sys.getenv("PATH")
new_path <- paste(new_path, current_path, sep = .Platform$path.sep)
Sys.setenv(PATH = new_path)

# convert mp3 files into wav files - to do so uncomment the following code
# mp32wav(path = '../R_code/XC_download', dest.path = '../R_code/XC_wav')

# mp32wav(path = '../R_code/XC_download_multisp', dest.path = '../R_code/XC_wav_multisp')
 
# mp32wav(path = '../R_code/MC_mp3', dest.path = '../R_code/MC_wav')


# ---------- 3. Split audios ----------------------------------------------------------
# split the wav files into 5-second clips
#split_sound_files(path = '../R_code/XC_wav', sgmt.dur = 5)
#split_sound_files(path = '../R_code/XC_wav_multisp', sgmt.dur = 5)

split_sound_files(path = '../R_code/XC_wav', sgmt.dur = 3)
split_sound_files(path = '../R_code/XC_wav_multisp', sgmt.dur = 3)
split_sound_files(path = '../R_code/MC_wav', sgmt.dur = 3)



# manually save the clips into a separate folder

##segment('../R_code/XC_wav/318179.wav', shortestSyl = 1200, shortestPause = 1000, SNR = 0.1, windowLength = 150, plot = T, maxDur = 100, saveAudio = '../R_code/XC_call_seg')


##require(ohun)
##detection <- energy_detector(files = "simulated_2.wav", threshold = 50, path = tempdir())

##require(gibbonR)
##DetectBLED(input = 'C:/Users/corey/Downloads', max.freq = 2300, output.dir = 'C:/Users/corey/Downloads')




# -------------------------- barolo audios --------------------------------------

## xc_barolo_list <- read.csv('xc-barolo.csv')
## xc_barolo_list_only <- filter(xc_barolo_list, also == '[]' | also == "['Puffinus puffinus']")
## colnames(xc_barolo_list_only)[1] <- 'Recording_ID'

## query_xc(qword = 'Puffinus baroli', download = T, X = xc_barolo_list_only, file.name = NULL,
##                  parallel = 1, path = '../R_code/XC_barolo', pb = TRUE)

## mp32wav(path = '../R_code/XC_barolo', dest.path = '../R_code/XC_wav_barolo')


# ---------------------------- 3.1 extract clips with Manx calls ------------------------------
## =============================== manx shearwater only recordings ==============================
# select audios with Manx shearwater calls and put them into one folder 

birdnet_manx_list <- list.files(path = '../R_code/BirdNET_manx_3s', full.names = T)
manx_list_full <- list.files(path = '../R_code/XC_3s_seg', pattern = '.wav', full.names = T)
manx_list <- list.files(path = '../R_code/XC_3s_seg', pattern = '.wav')
manx_df <- data.frame(file_name = manx_list, file_path = manx_list_full)


# make sure the recordings are listed in the same order 
birdnet_manx_list <- sort(birdnet_manx_list)
manx_df <- arrange(manx_df, file_name)

# create empty data frame to hold the info of recordings with manx calls
with_manx <- manx_df[0, ]
for (i in 1:length(birdnet_manx_list)) {
  results <- read.csv(birdnet_manx_list[i])
  if(nrow(results) > 0){
    with_manx <- rbind(with_manx, manx_df[i, ])
  }
}




## ========================== multi-species recordings ========================================
# select audios with Manx shearwater calls and put them into one folder 

birdnet_msp_list <- list.files(path = '../R_code/BirdNET_multisp_3s', full.names = T)
msp_list_full <- list.files(path = '../R_code/XC_3s_multisp', pattern = '.wav', full.names = T)
msp_list <- list.files(path = '../R_code/XC_3s_multisp', pattern = '.wav')
msp_df <- data.frame(file_name = msp_list, file_path = msp_list_full)


# make sure the recordings are listed in the same order 
birdnet_msp_list <- sort(birdnet_msp_list)
msp_df <- arrange(msp_df, file_name)

# create empty data frame to hold the info of recordings with manx calls
with_manx_multisp <- msp_df[0, ]
for (i in 1:length(birdnet_msp_list)) {
  results <- read.csv(birdnet_msp_list[i])
  if(nrow(results) > 0){
    with_manx_multisp <- rbind(with_manx_multisp, msp_df[i, ])
  }
}







## ========================== Macauley recordings ========================================
# select audios with Manx shearwater calls and put them into one folder 

birdnet_mc_list <- list.files(path = '../R_code/BirdNET_mc', full.names = T)
mc_list_full <- list.files(path = '../R_code/MC_3s', pattern = '.wav', full.names = T)
mc_list <- list.files(path = '../R_code/MC_3s', pattern = '.wav')
mc_df <- data.frame(file_name = mc_list, file_path = mc_list_full)


# make sure the recordings are listed in the same order 
birdnet_mc_list <- sort(birdnet_mc_list)
mc_df <- arrange(mc_df, file_name)

# create empty data frame to hold the info of recordings with manx calls
with_manx_mc <- mc_df[0, ]
for (i in 1:length(birdnet_mc_list)) {
  results <- read.csv(birdnet_mc_list[i])
  if(nrow(results) > 0){
    with_manx_mc <- rbind(with_manx_mc, mc_df[i, ])
  }
}



# put all recordings in one folder manually








# --------------------- 4. down-sampling audio sources to 22.05 kHz

# barolo_44khz <- list.files(path = '../R_code/XC_barolo_clip', pattern = ".wav", full.names = T)
# barolo_44khz_nameonly <- list.files(path = '../R_code/XC_barolo_clip', pattern = ".wav")



for (i in 1:nrow(with_manx)) {
  audio <- readWave(with_manx$file_path[i])
  audio <- resamp(audio, g = 22050, output = 'Wave')
  writeWave(audio, filename = file.path('../R_code/XC_manx_22050', with_manx$file_name[i]), extensible = F)
}

for (i in 1:nrow(with_manx_multisp)) {
  audio <- readWave(with_manx_multisp$file_path[i])
  audio <- resamp(audio, g = 22050, output = 'Wave')
  writeWave(audio, filename = file.path('../R_code/XC_multisp_22050', with_manx_multisp$file_name[i]), extensible = F)
}

for (i in 1:nrow(with_manx_mc)) {
  audio <- readWave(with_manx_mc$file_path[i])
  audio <- resamp(audio, g = 22050, output = 'Wave')
  if(max(audio@left) > 32767 | min(audio@left) < -32767){
    audio <- tuneR::normalize(audio, unit = '16')
  }
  writeWave(audio, filename = file.path('../R_code/MC_22050', with_manx_mc$file_name[i]), extensible = F)
}


#for (i in 1:length(barolo_44khz)) {
#  audio <- readWave(barolo_44khz[i])
#  audio <- resamp(audio, g = 22050, output = 'Wave')
#  writeWave(audio, filename = file.path('../R_code/XC_barolo_22050', barolo_44khz_nameonly[i]), extensible = F)
#}




# remove frequencies below 200 Hz and above 6 kHz
bandpass('../R_code/3s_22050', samplingRate = 22050, 
         lwr = 500, upr = 6000, action = 'pass', saveAudio = '../R_code/3s_22050_f', cores = 4)



#for (i in 1:nrow(audio_names_df)) {
#  audio <- readWave(audio_names_df$file_path[i])
#  filtered_audio <- bandpass(audio, samplingRate = audio@samp.rate, 
#                             lwr = 200, upr = 8000, action = 'pass', output = 'Wave')
#  if(max(filtered_audio@left) > 32767 | min(filtered_audio@left) < -32767){
#    filtered_audio <- tuneR::normalize(filtered_audio, unit = '16')
#  }
#  writeWave(filtered_audio, filename = file.path('../R_code/3s_22050_f', audio_names_df$file_name[i]), extensible = F)
#}




# ------------------------------ 5. Audio normalization -------------------------
manx_list_full1 <- list.files(path = '../R_code/3s_22050_f', pattern = ".wav", full.names = T)
manx_list1 <- list.files(path = '../R_code/3s_22050_f', pattern = ".wav")
audio_names_df <- data.frame(file_name = manx_list1, file_path = manx_list_full1)

audio_names_df$max_amp <- NA
audio_names_df$rms_amp <- NA

for (i in 1:nrow(audio_names_df)) {
  audio <- readWave(audio_names_df$file_path[i])
  audio_names_df$max_amp[i] <- max(abs(audio@left))
  audio_names_df$rms_amp[i] <- rms(audio@left)
  #scaling_factor <- runif(1, min = 0.75, max = 1)
  #audio_names_df$random_gain[i] <- scaling_factor
  #normalized_audio <- audio * scaling_factor
  #writeWave(normalized_audio, filename = file.path('../R_code/3s_rangain', audio_names_df$file_name[i]), extensible = F)
}


normalizeFolder('../R_code/3s_22050_f', type = 'rms', maxAmp = -1, windowLength = 23, killDC = T, saveAudio = '../R_code/3s_norm')

audio_names_df_final <- audio_names_df
audio_names_df_final$file_path <- gsub('3s_22050_f', '3s_norm', audio_names_df_final$file_path)

for (i in 1:nrow(audio_names_df_final)) {
  audio <- readWave(audio_names_df_final$file_path[i])
  #audio_names_df_final$norm_max_amp[i] <- max(abs(audio@left))
  #audio_names_df_final$norm_rms_amp[i] <- rms(audio@left)
  #scaling_factor <- runif(1, min = 0.75, max = 1)
  #audio_names_df$random_gain[i] <- scaling_factor
  #normalized_audio <- audio * scaling_factor
  writeWave(audio, filename = audio_names_df_final$file_path[i], extensible = F)
  }

##normalizeFolder(myfolder = '../R_code/augmentation_test', type = 'peak', maxAmp = 0, saveAudio = '../R_code/augmentation_test/normalized')


# generate source number place-holder recordings
for (i in 1:nrow(audio_names_df)) {
  audio <- readWave(audio_names_df$file_path[i])
  scaling_factor <- 0.05
  normalized_audio <- audio * scaling_factor
  writeWave(normalized_audio, filename = file.path('../R_code/3s_rangain_ph', audio_names_df$file_name[i]), extensible = F)
}



# randomly assign number of sources to each audio
audio_list_full <- list.files(path = '../R_code/3s_rangain_ph', pattern = ".wav", full.names = T)
audio_list <- list.files(path = '../R_code/3s_rangain_ph', pattern = ".wav")
audio_ph_df <- data.frame(file_name = audio_list, file_path = audio_list_full)


audio_2source <- sample_frac(audio_names_df, size = 0.67)
colnames(audio_2source)[2] <- 'file_path_3'

audio_3source <- sample_frac(audio_2source, size = 0.33)
colnames(audio_2source)[2] <- 'file_path_2'


audio_names_df_final <- left_join(audio_names_df, audio_2source, by = c('file_name', 'random_gain'))
audio_names_df_final <- left_join(audio_names_df_final, audio_3source, by = c('file_name', 'random_gain'))

audio_names_df_final <- relocate(audio_names_df_final, random_gain, .after = file_path_3)




# --------------------- 6. compile augmented data information
#audio_names_df_5 <- data.frame(file_name = file_list_nameonly_3, file_path = file_list_3)
#audio_names_df_5$Recording_ID <- sub(".wav", "", audio_names_df_5$file_name)

#audio_names_df_5 <- left_join(audio_names_df_5, audio_mix_noise[c(1, 4:7)], by = 'file_name')
#audio_names_df_5 <- left_join(audio_names_df_5, audio_mix_barolo[c(1, 4:7)], by = 'file_name')
#audio_names_df_5 <- left_join(audio_names_df_5, audio_names_df_2[c(1, 4)], by = 'file_name')
#audio_names_df_5 <- left_join(audio_names_df_5, audio_names_df_3[c(1, 4:6)], by = 'file_name')
#audio_names_df_5 <- left_join(audio_names_df_5, audio_names_df_4[c(1, 4)], by = 'file_name')
#audio_names_df_5$file_path <- gsub('5_XC_randur', '6_XC_downsample', audio_names_df_5$file_path)



# ------------------------ 7. split training dataset into 80:20 training:validation dataset

train_df <- slice_sample(audio_names_df_final, prop = 0.7)
# train_df <- filter(audio_names_df_5, subset == 'train')
train_df$subset <- rep('train', nrow(train_df))
train_df$final_file_path <- train_df$file_path
train_df$final_file_path <- gsub('3s_norm', 'train', train_df$final_file_path)
train_df$file_path <- gsub('3s_norm', 'train', train_df$file_path)
train_df$file_path_2 <- gsub('3s_22050_f', '3s_rangain_ph', train_df$file_path_2)
train_df$file_path_3 <- gsub('3s_22050_f', '3s_rangain_ph', train_df$file_path_3)

#train_df$file_path <- gsub('6_XC_downsample', 'train', train_df$file_path)

valid_df <- anti_join(audio_names_df_final, train_df, by = "file_name")
# valid_df <- filter(audio_names_df_5, subset == 'validation')
valid_df$subset <- rep('validation', nrow(valid_df))
valid_df$final_file_path <- valid_df$file_path
valid_df$final_file_path <- gsub('3s_norm', 'validation', valid_df$final_file_path)
valid_df$file_path <- gsub('3s_norm', 'validation', valid_df$file_path)
valid_df$file_path_2 <- gsub('3s_22050_f', '3s_rangain_ph', valid_df$file_path_2)
valid_df$file_path_3 <- gsub('3s_22050_f', '3s_rangain_ph', valid_df$file_path_3)

#valid_df$file_path <- gsub('6_XC_downsample', 'validation', valid_df$file_path)


audio_names_df_tv <- rbind(train_df, valid_df)
audio_names_df_tv <- arrange(audio_names_df_tv, file_name)
audio_names_df_tv <- relocate(audio_names_df_tv, final_file_path, .before = file_path)
audio_names_df_tv$file_path_2 <- gsub('3s_22050_f', '3s_rangain_ph', audio_names_df_tv$file_path_2)
audio_names_df_tv$file_path_3 <- gsub('3s_22050_f', '3s_rangain_ph', audio_names_df_tv$file_path_3)

write.csv(audio_names_df_tv, 'augmented_data_info_73.csv', row.names = F)
audio_names_df_tv82 <- read.csv('augmented_data_info_82.csv')
train_df <- filter(audio_names_df_tv82, subset =='train')
valid_df <- filter(audio_names_df_tv82, subset =='validation')


#audio_names_df_5 <- left_join(audio_names_df_5[-2], rbind(train_df[c(1:2, 11)], valid_df[c(1:2, 11)]), by = 'file_name')
#audio_names_df_5 <- relocate(audio_names_df_5, file_path, .after = file_name)

# write.csv(audio_names_df_5, 'augmented_data_info.csv', row.names = F)
# audio_names_df_5 <- read.csv('augmented_data_info.csv')


train_list <- select(train_df, final_file_path, file_path)
valid_list <- select(valid_df, final_file_path, file_path)


#train_list <- select(train_df, file_path, noise_file_path, barolo_file_path)
#train_list$original_file_path <- gsub('train', 'XC_5s_seg', train_list$file_path)
#train_list <- relocate(train_list, original_file_path, .after = file_path)


#valid_list <- select(valid_df, file_path, noise_file_path, barolo_file_path)
#valid_list$original_file_path <- gsub('validation', 'XC_5s_seg', valid_list$file_path)
#valid_list <- relocate(valid_list, original_file_path, .after = file_path)


# change the file path for hpc

train_list_hpc <- train_list
train_list_hpc$file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                 train_list_hpc$file_path)
train_list_hpc$final_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                       train_list_hpc$final_file_path)
train_list_hpc$file_path_2 <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                   train_list_hpc$file_path_2)
train_list_hpc$file_path_3 <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                   train_list_hpc$file_path_3)

#train_list_hpc$original_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", train_list_hpc$original_file_path)
#train_list_hpc$noise_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", train_list_hpc$noise_file_path)

write.table(train_list_hpc, file = 'train_example_list_82.txt', sep = '\t', row.names = F, quote = F)

valid_list_hpc <- valid_list
valid_list_hpc$file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                 valid_list_hpc$file_path)
valid_list_hpc$final_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                       valid_list_hpc$final_file_path)
valid_list_hpc$file_path_2 <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                   valid_list_hpc$file_path_2)
valid_list_hpc$file_path_3 <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", 
                                   valid_list_hpc$file_path_3)

#valid_list_hpc$original_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", valid_list_hpc$original_file_path)
#valid_list_hpc$noise_file_path <- gsub("^.{0,9}", "/rds/general/user/jl819/home/MixIT/dataset/XC_manx_data", valid_list_hpc$noise_file_path)

write.table(valid_list_hpc, file = 'validation_example_list_82.txt', sep = '\t', row.names = F, quote = F)


# transfer the files into corresponding folders
#old_train_path <- gsub('train', '6_XC_downsample', train_list)
#file.copy(old_train_path, train_list)

train_list$old_train_path <- gsub('train', '3s_rangain', train_list$final_file_path)
file.copy(from = train_list$old_train_path, to = train_list$final_file_path)

#old_valid_path <- gsub('validation', '6_XC_downsample', valid_list)
#file.copy(old_valid_path, valid_list)

valid_list$old_train_path <- gsub('validation', '3s_rangain', valid_list$final_file_path)
file.copy(from = valid_list$old_train_path, to = valid_list$final_file_path)
































# ========== 5.2 mix Barolo shearwater calls with 20% of clips

# barolo call clips were manually selected using Raven Pro and stored in ../R_code/XC_barolo_clip
barolo_list <- list.files(path = '../R_code/XC_barolo_22050', pattern = ".wav", full.names = T)
barolo_list_nameonly <- list.files(path = '../R_code/XC_barolo_22050', pattern = ".wav")
barolo_names_df <- data.frame(barolo_file_name = barolo_list_nameonly, barolo_file_path = barolo_list)
barolo_names_df$barolo_file_name_only <- sub(".wav", "", barolo_names_df$barolo_file_name)


audio_list_1 <- list.files(path = '../R_code/1_XC_withnoise', pattern = ".wav", full.names = T)
audio_list_nameonly_1 <- list.files(path = '../R_code/1_XC_withnoise', pattern = ".wav")

audio_list_1 <- list.files(path = '../R_code/XC_rangain_1', pattern = ".wav", full.names = T)
audio_list_nameonly_1 <- list.files(path = '../R_code/XC_rangain_1', pattern = ".wav")

audio_names_df_1 <- data.frame(file_name = audio_list_nameonly_1, file_path = audio_list_1)
audio_names_df_1$file_name_only <- sub(".wav", "", audio_names_df_1$file_name)
audio_names_df_1$random_gain <- audio_names_df_2$random_gain

audio_mix_barolo <- audio_names_df_1 %>% sample_frac(0.20)
audio_mix_barolo$barolo_SNR <- NA

audio_mix_barolo <- cbind(audio_mix_barolo, barolo_names_df[1:519,])


# mix barolo calls with recordings
for (i in 1:nrow(audio_mix_barolo)) {
  audio <- readWave(audio_mix_barolo$file_path[i])
  barolo <- readWave(audio_mix_barolo$barolo_file_path[i])
  
  # Silence padding to barolo clips to make them to 5 seconds 
  at <- sample(c('start', 'end'), size = 1)
  if(at == 'start'){
    barolo <- addsilw(barolo, d = 5, output = 'Wave', at = at)
    barolo_dur <- seewave::duration(barolo)
    barolo <- extractWave(barolo, from = barolo_dur-5, to = barolo_dur, xunit = 'time')
    barolo <- extractWave(barolo, from = 0, to = 5, xunit = 'time')
    
  }
  if(at == 'end'){
    barolo <- addsilw(barolo, d = 5, output = 'Wave', at = at)
    barolo <- extractWave(barolo, from = 0, to = 5, xunit = 'time')
    barolo <- extractWave(barolo, from = 0, to = 5, xunit = 'time')
  }
  
  # Specify the desired SNR in dB
  snr <- runif(1, min = 0, max = 40)
  audio_mix_barolo$barolo_SNR[i] <- snr
  
  # Calculate the root mean square values
  rms_audio <- rms(audio@left)
  rms_barolo <- rms(barolo@left)
  
  # Calculate the decibel value
  dB <- 20 * log10(rms_barolo / rms_audio)
  
  # Calculate the gain factor to adjust the amplitude
  gain_factor <- 10^(((-snr)-dB) / 20)
  
  # Change the decibel value of the noise recording
  adjusted_barolo <- barolo * gain_factor
  seewave::duration(adjusted_barolo)

  mixed_audio <- audio + adjusted_barolo
  if(max(mixed_audio@left) > 32767 | min(mixed_audio@left) < -32767){
    mixed_audio <- tuneR::normalize(mixed_audio, unit = '16')
  }
  if(max(adjusted_barolo@left) > 32767 | min(adjusted_barolo@left) < -32767){
    adjusted_barolo <- tuneR::normalize(adjusted_barolo, unit = '16')
  }
  writeWave(mixed_audio, filename = file.path('../R_code/XC_withbarolo_2', audio_mix_barolo$file_name[i]), extensible = F)
  writeWave(adjusted_barolo, filename = file.path('../R_code/XC_barolo_22050/XC_barolo_ransnr', audio_mix_barolo$barolo_file_name[i]), extensible = F)
}


# write.csv(audio_mix_barolo, 'audio_barolo_mixed.csv', row.names = F)



# move files without noise into 1_XC_withnoise
audio_no_barolo <- anti_join(audio_names_df_1, audio_mix_barolo, by = "file_name")

source_folder <- '../R_code/XC_rangain_1'
destination_folder <- '../R_code/XC_withbarolo_2'
destination_folder <- '../R_code/temp'

for (file_name in audio_no_barolo$file_name) {
  source_path <- file.path(source_folder, file_name)
  destination_path <- file.path(destination_folder, file_name)
  file.copy(source_path, destination_path)
}

audio_no_barolo$barolo_file_name <- NA
audio_no_barolo$barolo_file_path <- NA
audio_no_barolo$barolo_file_name_only <- NA
audio_no_barolo$barolo_SNR <- NA


audio_names_df_5 <- rbind(audio_mix_barolo, audio_no_barolo)
audio_names_df_5 <- arrange(audio_names_df_5, file_name)


# compile augmented data info
audio_names_df_5$final_file_path <- audio_names_df_5$file_path 
audio_names_df_5$final_file_path <- gsub('XC_rangain_1', 'XC_withbarolo_2', audio_names_df_5$final_file_path )
audio_names_df_5$barolo_file_path <- gsub('XC_barolo_22050', 'XC_barolo_22050/XC_barolo_ransnr', audio_names_df_5$barolo_file_path )




## extra augmentations - optional; not used for now
# ========== 5.3 with a 75% chance mix with the noise recordings from ff1010bird
# get a list of all the clips in the folder
audio_list <- list.files(path = '../R_code/XC_5s_seg', pattern = ".wav", full.names = T)
audio_list_nameonly <- list.files(path = '../R_code/XC_5s_seg', pattern = ".wav")
length(audio_list_nameonly) # 2596
2596*0.75/2 # 974 10-second noise recordings required




# randomly select 75% of the recordings
audio_names_df <- data.frame(file_name = audio_list_nameonly, file_path = audio_list)
audio_names_df$file_name_only <- sub(".wav", "", audio_names_df$file_name)

## audio_mix_noise <- sample_frac(audio_names_df, size = 0.75)
## audio_mix_noise$noise_SNR <- NA
## nrow(audio_mix_noise)
## write.csv(audio_mix_noise, 'audio_noise_augmented.csv', row.names = F)



# get background noise by removing recordings with birds
noise_list <- list.files(path = '../R_code/background_noise/ff1010bird_wav/wav', pattern = ".wav", full.names = T)
noise_list_nameonly <- list.files(path = '../R_code/background_noise/ff1010bird_wav/wav', pattern = ".wav")

noise_names_df <- data.frame(file_name = noise_list_nameonly, file_path=noise_list)
noise_names_df$file_name_only <- sub(".wav", "", noise_names_df$file_name)

noise_meta <- read.csv('../R_code/background_noise/ff1010bird_metadata.csv')
noise_meta$itemid <- as.character(noise_meta$itemid)
noise_names_df <- left_join(noise_names_df, noise_meta, by = join_by(file_name_only == itemid))
noise_names_df <- filter(noise_names_df, hasbird == 0)

source_folder <- '../R_code/background_noise/ff1010bird_wav/wav'
destination_folder <- '../R_code/background_noise/ff1010bird_wav/noise'
for (file_name in noise_names_df$file_name) {
  source_path <- file.path(source_folder, file_name)
  destination_path <- file.path(destination_folder, file_name)
  file.copy(source_path, destination_path)
}



noise_mix_audio <- sample_n(noise_names_df, 974)
source_folder <- '../R_code/background_noise/ff1010bird_wav/noise'
destination_folder <- '../R_code/background_noise/ff1010bird_wav/noise_selected'
for (file_name in noise_mix_audio$file_name) {
  source_path <- file.path(source_folder, file_name)
  destination_path <- file.path(destination_folder, file_name)
  file.copy(source_path, destination_path)
}



# split noise recordings into 5-second clips
split_sound_files(path = '../R_code/background_noise/ff1010bird_wav/noise_selected', sgmt.dur = 5)


noise_5s_list <- list.files(path = '../R_code/background_noise/ff1010bird_wav/noise_5s', pattern = ".wav", full.names = T)
noise_5s_list_nameonly <- list.files(path = '../R_code/background_noise/ff1010bird_wav/noise_5s', pattern = ".wav")

noise_5s_names_df <- data.frame(noise_file_name = noise_5s_list_nameonly, noise_file_path = noise_5s_list)
noise_5s_names_df$noise_file_name_only <- sub(".wav", "", noise_5s_names_df$noise_file_name)

noise_5s_names_df <- noise_5s_names_df[-1948, ]
audio_mix_noise <- cbind(audio_mix_noise, noise_5s_names_df)
str(audio_mix_noise)


# mix noise with recordings
for (i in 1:nrow(audio_mix_noise)) {
  audio <- readWave(audio_mix_noise$file_path[i])
  sr_audio <- audio@samp.rate
  noise <- readWave(noise_5s_names_df$file_path[i])
  if(noise@samp.rate < sr_audio){
    audio <- resamp(audio, g = noise@samp.rate, output = 'Wave')
  }
  if(noise@samp.rate > sr_audio){
    noise <- resamp(noise, g = sr_audio, output = 'Wave')
  }
  
  # Specify the desired SNR in dB
  snr <- runif(1, min = 0, max = 40)
  audio_mix_noise$SNR[i] <- snr
  
  # Calculate the root mean square values
  rms_audio <- rms(audio@left)
  rms_noise <- rms(noise@left)
  
  # Calculate the decibel value
  dB <- 20 * log10(rms_noise / rms_audio)
  
  # Calculate the gain factor to adjust the amplitude
  gain_factor <- 10^(((-snr)-dB) / 20)
  
  # Change the decibel value of the noise recording
  adjusted_noise <- noise * gain_factor
  dB_new <- 20 * log10(rms(adjusted_noise@left) / rms_audio)
  print(dB_new == (-snr))
  
  mixed_audio <- audio + adjusted_noise
  if(max(mixed_audio@left) > 32767 | min(mixed_audio@left) < -32767){
    mixed_audio <- tuneR::normalize(mixed_audio, unit = '16')
  }
  writeWave(mixed_audio, filename = file.path('../R_code/1_XC_withnoise', audio_mix_noise$file_name[i]), extensible = F)
}


#write.csv(audio_mix_noise, 'audio_noise_augmented.csv', row.names = F)


# move files without noise into 1_XC_withnoise
audio_no_noise <- anti_join(audio_names_df, audio_mix_noise, by = "file_name")

source_folder <- '../R_code/XC_5s_seg'
destination_folder <- '../R_code/1_XC_withnoise'
for (file_name in audio_no_noise$file_name) {
  source_path <- file.path(source_folder, file_name)
  destination_path <- file.path(destination_folder, file_name)
  file.copy(source_path, destination_path)
}






# ========== 5.4 random pass filter
file_list_1 <- list.files(path = '../R_code/3_XC_normalized', pattern = ".wav", full.names = T)
file_list_nameonly_1 <- list.files(path = '../R_code/3_XC_normalized', pattern = ".wav")
audio_names_df_3 <- data.frame(file_name = file_list_nameonly_1, file_path = file_list_1)
audio_names_df_3$file_name_only <- sub(".wav", "", audio_names_df_3$file_name)
audio_names_df_3$lower_filter <- NA
audio_names_df_3$higher_filter <- NA
audio_names_df_3$filter_type <- NA



for (i in 1:length(file_list_1)) {
  audio <- readWave(file_list_1[i])
  # high-pass filter all recordings at 200 Hz
  audio <- bwfilter(audio, f = audio@samp.rate, n = 8, from = 200, output = 'Wave', bandpass = T)
  low <- runif(1, min = 200, max = 2000)
  low_list <- c(as.integer(low), NA)
  low_selected <- sample(low_list, size = 1)
  audio_names_df_3$lower_filter[i] <- low_selected
  if(!is.na(low_selected)){
    high <- runif(1, min = low + 100, max = low + 1000)
    high_list <- c(NA, as.integer(high))
    high_selected <- sample(high_list, size = 1)
  } else{
    high_selected <- as.integer(runif(1, min = low + 1, max = low + 1000))
  }
  audio_names_df_3$higher_filter[i] <- high_selected
  #low pass filter
  if(is.na(low_selected) & !is.na(high_selected)){
    filtered_audio <- bwfilter(audio, f = audio@samp.rate, n = sample(1:5, size = 1), 
             to = high_selected, output = 'Wave')
    audio_names_df_3$filter_type[i] <- 'low-pass'
  }
  # high pass filter
  if(!is.na(low_selected) & is.na(high_selected)){
    filtered_audio <- bwfilter(audio, f = audio@samp.rate, n = sample(1:5, size = 1), 
             from = low_selected, output = 'Wave')
    audio_names_df_3$filter_type[i] <- 'high-pass'
  }
  # band pass filter
  if(!is.na(low_selected) & !is.na(high_selected)){
      filtered_audio <- bwfilter(audio, f = audio@samp.rate, n = sample(1:5, size = 1), 
             from = low_selected, to = high_selected, bandpass = F, output = 'Wave')
      audio_names_df_3$filter_type[i] <- 'band-stop'
  }
  if(max(filtered_audio@left) > 32767 | min(filtered_audio@left) < -32767){
    filtered_audio <- tuneR::normalize(filtered_audio, unit = '16')
  }
  writeWave(filtered_audio, filename = file.path('../R_code/4_XC_filtered', file_list_nameonly_1[i]))
}





# ========== 4.5 select a random 3- to 4.5-second window within the 5-second clip
file_list_2 <- list.files(path = '../R_code/4_XC_filtered', pattern = ".wav", full.names = T)
file_list_nameonly_2 <- list.files(path = '../R_code/4_XC_filtered', pattern = ".wav")
audio_names_df_4 <- data.frame(file_name = file_list_nameonly_2, file_path = file_list_2)
audio_names_df_4$file_name_only <- sub(".wav", "", audio_names_df_4$file_name)
audio_names_df_4$duration <- NA

min_duration <- 3
max_duration <- 5

for (i in 1:length(file_list_2)) {
  audio <- readWave(file_list_2[i])
  duration <- seewave::duration(audio)
  duration_aug <- runif(1, min_duration, max_duration)
  start_time <- runif(1, 0, duration - duration_aug)
  end_time <- start_time + duration_aug
  window <- extractWave(audio, from = start_time, to = end_time, xunit = 'time')
  audio_names_df_4$duration[i] <- seewave::duration(window)
  writeWave(window, filename = file.path('../R_code/5_XC_randur', file_list_nameonly_2[i]), extensible = F)
}







