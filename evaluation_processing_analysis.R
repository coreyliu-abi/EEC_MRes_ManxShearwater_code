# set working directory
setwd("C:/Users/corey/OneDrive - Imperial College London/IC/PG/Summer Project/Data/Evaluation")


require(tidyr)
require(dplyr)
require(warbleR)
require(soundgen)
require(tuneR)
require(audio)

require(sound)
require(seewave)
require(stringr)
require(caret)
unsep_manx_presence <- read.csv('unsep_birdnet_eval_3.csv')
bird_mixit_manx_presence <- read.csv('bird_mixit_birdnet_results.csv')
clip_sep_summary <- read.csv('clip_sep_summary.csv')
bird_mixit_total <- read.csv('mixit_unsep_birdnet_eval.csv')
indiv_sep_results <- read.csv('indiv_sep_results.csv')

# ------------------------------- 1. descriptive statistics for unseparated results
# number of clips with certain number of individuals 
unsep_manx_presence %>% distinct(recording_name, .keep_all = T) %>% group_by(indiv_num) %>% summarise(count = length(unique(recording_name)))

#   indiv_num count
#1         1    55
#2         2    61
#3         3     4

# number of each overlap type
unsep_manx_presence %>% distinct(recording_name, .keep_all = T) %>% group_by(time_overlap, freq_overlap) %>% summarise(count = n())
#   time_overlap freq_overlap count
#1            0            0    59
#2            0            1     9
#3            1            0    10
#4            1            1    42
multi_indiv <- filter(unsep_manx_presence, indiv_num > 1)


# audibility score 
unsep_manx_presence %>% group_by(total_quality) %>% summarise(count = n())
#   total_quality count
#1             1    34
#2             2    93
#3             3    35
#4             4    14
#5             5     8
#6             6     5


# vocalization completeness
unsep_manx_presence %>% group_by(comp_syll, comp_motif) %>% summarise(count = n())
#   comp_syll comp_motif count
#1         0          0    82
#2         1          0    65
#3         1          1    42


# classification accuracy 
unsep_manx_presence_dist <- distinct(unsep_manx_presence, recording_name, .keep_all = T)
sum(unsep_manx_presence_dist$birdnet_manx_present)/nrow(unsep_manx_presence_dist)
# 88 / 120 = 73.3%




# ------------------------------ 2. descriptive statistics for separated results

sum(indiv_sep_results$success_sep)
# 159 individuals were successfully separated

nrow(filter(indiv_sep_results, over_sep == 0, under_sep == 0))
# 43 individuals were cleanly separated


nrow(filter(bird_mixit_total, main_bg == 1))
# number of main background noise output = 120
nrow(filter(bird_mixit_total, main_bg == 1& manx_present.x ==1))
# number of main background noise output with bird call = 53

# take non-main background noise outputs
non_main_bg <- filter(bird_mixit_total, main_bg == 0)
sum(non_main_bg$manx_present.x) 
# 291 outputs contains manx calls

# classification accuracy = 0.4707904
sum(non_main_bg$birdnet_manx_present.x)/sum(non_main_bg$manx_present.x) 

# over- and under-separation frequency
over_sep <- non_main_bg %>% filter(over_sep > 0)
nrow(over_sep)
# 214 cases of over-separation
over_sep %>% filter(freq_sep == 1) %>% nrow()
# 85 caused by different frequency
over_sep %>% filter(syll_sep == 1) %>% nrow()
# 67 caused by different syllables
over_sep %>% filter(amp_sep == 1) %>% nrow()
# 50 caused by different amplitudes

under_sep <- non_main_bg %>% filter(under_sep > 0)
nrow(under_sep)
# 103 cases of under-separation
under_sep %>% filter(freq_overlap.x == 1) %>% nrow()
# 85 caused by different frequency
under_sep %>% filter(time_overlap.x == 1) %>% nrow()
# 67 caused by different syllables



non_main_bg %>% filter(under_sep > 0) %>% nrow()
# 103 cases of under-separation


sum(non_main_bg$birdnet_manx_present.x)/sum(non_main_bg$manx_present.x)


manx_call_nonoise <- bird_mixit_total %>% filter(indiv_num.x != 0, noise == 0, main_bg == 0) # one channel with only 
nrow(manx_call_nonoise)
# number of output with bird call only and no noise = 135


manx_call_noise <- bird_mixit_total %>% filter(indiv_num.x != 0, noise == 1, main_bg == 0) # one channel with only 
nrow(manx_call_noise)
# number of output with bird call and noise = 151

# proportion of manx call outputs with noise
151/291 # = 0.5189003


# number of noise-only output (background noise excluded) = 52
noise <- bird_mixit_total %>% filter(indiv_num.x == 0, noise == 1, main_bg == 0) # one channel with only 
nrow(noise)


blank <- filter(bird_mixit_total, blank ==1)
nrow(blank)
length(unique(blank$recording_name))
# number of blank output = 22, number of recordings that has blank outputs = 21


# overall summary
output_type_summary <- bird_mixit_total %>% group_by(blank, noise, main_bg, indiv_num.x) %>% summarise(clip_count = n())
output_type_summary
sum(output_type_summary$clip_count)
#    blank noise main_bg indiv_num.x clip_count
#    <int> <int>   <int>       <int>      <int>
#1     0     0       0           1        126
#2     0     0       0           2          9
#3     0     1       0           0         52
#4     0     1       0           1        102
#5     0     1       0           2         48
#6     0     1       0           3          1
#7     0     1       1           0         68
#8     0     1       1           1         36
#9     0     1       1           2         16
#10    1     0       0           0         22

# number of main background noise clips = 16+36+68 =120
# number of noise_only clips = 52
# number of bird call clips with considerable noise = 102+48+1 = 151
# number of blank output = 22
# number of bird call clips without noise = 126+9 = 135
120+151+52+22+135


# completeness of separation
clip_sep_summary %>% group_by(full_clean_sep, full_partial_sep, half_clean_sep, half_partial_sep) %>% summarise(count = n())
#   full_clean_sep full_partial_sep half_clean_sep half_partial_sep count
#1              0                0              0                0     8
#2              0                0              0                1    16
#3              0                1              0                0    75
#4              1                0              0                0    21

75+21 # = 96
8 + 16 # 24



# separation of overlapping clips 
overlap <- filter(clip_sep_summary, time_overlap == 1 | freq_overlap == 1)
overlap %>% group_by(full_clean_sep, full_partial_sep, half_clean_sep, half_partial_sep) %>% summarise(count = n())
#   full_clean_sep full_partial_sep half_clean_sep half_partial_sep count
#1              0                0              0                0     5
#2              0                0              0                1    16
#3              0                1              0                0    38
#4              1                0              0                0     2

# fully separated = 40
40/nrow(overlap)


# by audiomoths
indiv_separated$AM_id <- gsub(".*OL(.+)_2.*", "\\1", indiv_separated$recording_name)
indiv_separated %>% group_by(AM_id) %>% summarise(count = n())

indiv_non_separated <- filter(df_summary_2, num_indiv_present > num_main_indiv)
# 23 clips have all the individuals separated





# over-separation cause summary
over_sep_summary <- bird_mixit_total %>% group_by(freq_sep, syll_sep, motif_sep, timb_sep, amp_sep) %>% summarise(count = n())
sum(bird_mixit_total$freq_sep) # 85
sum(bird_mixit_total$syll_sep) # 48
sum(bird_mixit_total$motif_sep) # 13
sum(bird_mixit_total$timb_sep) # 23
sum(bird_mixit_total$amp_sep) # 48

# number of outputs over-separated
bird_mixit_total %>% group_by(over_sep, under_sep) %>% summarise(count = n())
bird_mixit_total %>% group_by(over_sep, under_sep) %>% summarise(count = length(unique(recording_name)))


# under separation summary
overlap_summary <- filter(df_summary_2, time_overlap == 1|freq_overlap == 1)
over <- filter(bird_mixit_total, main_indiv_chan == 1, time_overlap == 1|freq_overlap == 1)
time_freq_overlap <- filter(df_summary_2, time_overlap == 1, freq_overlap == 1)
time_freq_overlap_sep <- filter(time_freq_overlap, num_indiv_present == num_main_indiv)

time_overlap <- filter(df_summary_2, time_overlap == 1, freq_overlap == 0)
time_overlap_sep <- filter(time_overlap, num_indiv_present == num_main_indiv)

freq_overlap <- filter(df_summary_2, time_overlap == 0, freq_overlap == 1)
freq_overlap_sep <- filter(freq_overlap, num_indiv_present == num_main_indiv)

bird_mixit_total %>% group_by(time_overlap.x, freq_overlap.x) %>% summarise(count = n())

prop.test(x = c(27, 6), n = c(42, 9))
prop.test(x = c(27, 8), n = c(42, 10))
prop.test(x = c(8, 6), n = c(10, 9))

fisher.test(data.frame(sep = c(27, 6),total = c(42, 9)))
fisher.test(data.frame(sep = c(27, 8),total = c(42, 10)))
fisher.test(data.frame(sep = c(8, 6),total = c(10, 9)))

(27+8+6)/(42+10+9)


# 21 recordings have clear separation for all outputs 
bird_mixit_total %>% group_by(recording_name) %>% summarise(over_sep_score = sum(over_sep), under_sep_score = sum(under_sep)) %>% filter(over_sep_score == 0 & under_sep_score == 0) %>% nrow()


# focus on only main individual channel
main_indiv <- filter(bird_mixit_total, main_indiv_chan == 1) # 160
nrow(filter(main_indiv, over_sep == 0 & under_sep == 0))
length(unique(main_indiv$recording_name))
sum(main_indiv$under_sep) # 103
sum(main_indiv$over_sep) # 146
160-103 = 57

over_sep_summary1 <- manx_present %>% group_by(freq_sep, syll_sep, motif_sep, timb_sep, amp_sep) %>% summarise(count = n())
over_sep_summary1
sum(main_indiv$freq_sep) # 85
sum(main_indiv$syll_sep) # 48
sum(main_indiv$motif_sep) # 13
sum(main_indiv$timb_sep) # 23
sum(main_indiv$amp_sep) # 48

sum(main_indiv$time_overlap.x) # 31
sum(main_indiv$freq_overlap.x) # 45
76/
  
  
  
  # focus on manx present output that is not main background noise
  manx_present <- filter(bird_mixit_total, manx_present.x == 1, main_bg == 0) # 291
nrow(filter(manx_present, over_sep != 0)) # 213
nrow(filter(manx_present, under_sep != 0)) # 103

over_sep_summary1 <- manx_present %>% group_by(freq_sep, syll_sep, motif_sep, timb_sep, amp_sep) %>% summarise(count = n())
over_sep_summary1
sum(manx_present$freq_sep) # 85
sum(manx_present$syll_sep) # 48
sum(manx_present$motif_sep) # 13
sum(manx_present$timb_sep) # 23
sum(manx_present$amp_sep) # 48


sum(manx_present$time_overlap.x) # 53
sum(manx_present$freq_overlap.x) # 76
53/103
76/103

nrow(filter(manx_present, noise == 1))
155/291







# ------------------------------- 3. statistical tests -------------------------------

# effect of type of overlap on separation success
overlap_indiv <- indiv_sep_results %>% filter(time_overlap.y == 1|freq_overlap.y == 1)
overlap %>% group_by(time_overlap, freq_overlap, full_sep) %>% summarise(count = n())
#   time_overlap.y freq_overlap.y success_sep count
#1              0            1        0     3
#2              0            1        1     6
#3              1            0        0     2
#4              1            0        1     8
#5              1            1        0    16
#6              1            1        1    26


# pairwise comparison between different overlap type
# time-freq vs freq
fisher.test(data.frame(sep = c(26, 6),total = c(42, 9)))
# time_freq vs time
fisher.test(data.frame(sep = c(26, 8),total = c(42, 10)))
# time vs freq
fisher.test(data.frame(sep = c(8, 6),total = c(10, 9)))



# audibility effect on separation success
audibility_model <- glm(success_sep ~ total_quality, data =indiv_sep_results, family = 'binomial', na.action = 'na.omit')
summary(audibility_model)




# compare partially separated clips 
half_sep_clips <- filter(clip_sep_summary, full_sep ==1|half_sep == 1)
# accuracy before separation
sum(half_sep_clips$birdnet_manx_present)/nrow(half_sep_clips) # 73.2%
# accuracy after separation
sum(half_sep_clips$birdnet_after_sep)/nrow(half_sep_clips) # 54.5%


half_sep_df <- half_sep_clips %>% group_by(birdnet_manx_present, birdnet_after_sep) %>% summarise(count = n())
half_sep_df
half_sep_table <- matrix(c(61, 21, 0, 30), nrow = 2,
                         dimnames = list("Separated" = c("Present", "Absent"), 
                                         "Unseparated" = c("Present", "Absent")))
half_sep_table
mcnemar.test(half_sep_table, y = NULL, correct = TRUE)
# significantly lower accuracy


# full separation
full_sep_clips <- filter(clip_sep_summary, full_sep ==1)
# accuracy before separation
sum(full_sep_clips$birdnet_manx_present)/nrow(full_sep_clips) # 69.8%
# accuracy after separation
sum(full_sep_clips$birdnet_after_sep)/nrow(full_sep_clips) # 50%


full_sep_df <- full_sep_clips %>% group_by(birdnet_manx_present, birdnet_after_sep) %>% summarise(count = n())
full_sep_df
full_sep_table <- matrix(c(50, 17, 0, 29), nrow = 2,
                         dimnames = list("Separated" = c("Present", "Absent"), 
                                         "Unseparated" = c("Present", "Absent")))
full_sep_table
mcnemar.test(full_sep_table, y = NULL, correct = TRUE)
# significantly lower accuracy




# all individuals separated cleanly 
full_clean_sep_clips <- filter(clip_sep_summary, full_sep ==1, full_clean_sep == 1)
# accuracy before separation
sum(full_clean_sep_clips$birdnet_manx_present)/nrow(full_clean_sep_clips) # 33.3%
# accuracy after separation
sum(full_clean_sep_clips$birdnet_after_sep)/nrow(full_clean_sep_clips) # 23.8%


full_clean_sep_df <- full_clean_sep_clips %>% group_by(birdnet_manx_present, birdnet_after_sep) %>% summarise(count = n())
full_clean_sep_df
full_clean_sep_table <- matrix(c(5, 2, 0, 14), nrow = 2,
                               dimnames = list("Separated" = c("Present", "Absent"), 
                                               "Unseparated" = c("Present", "Absent")))
full_clean_sep_table
mcnemar.test(full_clean_sep_table, y = NULL, correct = TRUE)
fisher.test(data.frame(sep = c(7, 5),total = c(21, 21)))
# accuracy did not change







# ------------------------------ 4. plots -----------------------------------
require(ggplot2)


# number of clips with a certain number individuals and the number fully and partially separated
# bar plot - number of clips ~ number of individuals present (num_indiv_present) 

clip_sep_summary$sep_type <- NA
for (i in 1:nrow(clip_sep_summary)) {
  if(clip_sep_summary$full_clean_sep[i] ==1){
    clip_sep_summary$sep_type[i] <- 'Full + Clean'
  }
  if(clip_sep_summary$full_partial_sep[i] ==1){
    clip_sep_summary$sep_type[i] <- 'Full + Partial'
  }
  if(clip_sep_summary$half_clean_sep[i] ==1){
    clip_sep_summary$sep_type[i] <- 'Some + Clean'
  }
  if(clip_sep_summary$half_partial_sep[i] ==1){
  clip_sep_summary$sep_type[i] <- 'Some + Partial'
  }
  if(clip_sep_summary$half_partial_sep[i] ==0&clip_sep_summary$full_clean_sep[i] ==0&clip_sep_summary$full_partial_sep[i] ==0&clip_sep_summary$half_clean_sep[i] ==0){
    clip_sep_summary$sep_type[i] <- 'None'
  }
}



clip_sep_type_summary <- clip_sep_summary %>% group_by(num_indiv_present, sep_type) %>% summarise(count = n())

clip_sep_type_summary$count_normal <- c('17', '35', '3', '4', '39', '5', '13', '', '3')


group.colors <- c('Full + Clean' = "forestgreen", 'Full + Partial' = "lightgreen", 'Some + Partial' ="goldenrod1", "None" = "firebrick1")

sep_type_plot <- ggplot(data = clip_sep_type_summary, aes(x = num_indiv_present, y = count, 
                                         fill = factor(sep_type, c('Full + Clean', 'Full + Partial', 'Some + Partial', 'None')))) + 
  geom_bar( position = 'stack', stat = 'identity', width = 0.8) + 
  labs(y = 'Number of Evaluation Clips', x= 'Number of Manx Shearwaters in the Clip', size = 20)+
  scale_fill_manual(values = group.colors, name = 'Separation Results')+ 
  geom_text(size = 7, aes(label = count_normal), position = position_stack(vjust = 0.5)) + 
  annotate(geom ='text', x=3, y=5.5, label="1", size = 7) + annotate(geom ='text', x=3, y=8.5, label="n = 4", size = 7) + 
  annotate(geom ='text', x=1, y=57, label="n = 55", size = 7) + annotate(geom ='text', x=2, y=63, label="n = 61", size = 7) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
                     axis.text=element_text(size=17),
                     axis.title.x=element_text(size=20, vjust = -1), 
                     axis.title.y=element_text(size=20, vjust = 1.5), 
                     legend.key.size = unit(1, 'cm'), 
                     legend.title = element_text(size=13), 
                     legend.text = element_text(size=11), 
                     plot.margin = margin(t = 10, r = 0, b = 20, l = 20, unit = "pt"))

sep_type_plot

jpeg("Figure5a.jpg", width = 4000, height = 3500, units = 'px', res = 500)
sep_type_plot
dev.off()



# number of overlapping clips and the separation results
# bar plot - number of clips ~ overlap type (num_indiv_present)
clip_sep_summary$overlap_type <- NA
for (i in 1:nrow(clip_sep_summary)) {
  if(clip_sep_summary$freq_overlap[i] ==1&clip_sep_summary$time_overlap[i] ==0){
    clip_sep_summary$overlap_type[i] <- 'Frequency'
  }
  if(clip_sep_summary$freq_overlap[i] ==0&clip_sep_summary$time_overlap[i] ==1){
    clip_sep_summary$overlap_type[i] <- 'Time'
  }
  if(clip_sep_summary$freq_overlap[i] ==1&clip_sep_summary$time_overlap[i] ==1){
    clip_sep_summary$overlap_type[i] <- 'Frequency + Time'
  }
  if(clip_sep_summary$freq_overlap[i] ==0&clip_sep_summary$time_overlap[i] ==0&clip_sep_summary$num_indiv_present[i] > 1){
    clip_sep_summary$overlap_type[i] <- 'Non-Overlap'
  }
}

overlap <- filter(clip_sep_summary, !is.na(overlap_type))
overlap_sep_type_summary <- overlap %>% group_by(overlap_type, sep_type) %>% summarise(count = n())
overlap_sep_type_summary$count_normal <- c('6', '', '2', '', '25', '4', '12', '2', '2', '', '7', '2')
overlap_sep_type_summary$overlap_type <- factor(overlap_sep_type_summary$overlap_type, c('Frequency + Time', 'Frequency', 'Time', 'Non-Overlap'))

overlap_type_plot <- ggplot(data = overlap_sep_type_summary, aes(x = overlap_type, y = count, 
                                                          fill = factor(sep_type, c('Full + Clean', 'Full + Partial', 'Some + Partial', 'None')))) + 
  geom_bar(position = 'stack', stat = 'identity', width = 0.8) + 
  labs(y = 'Number of Evaluation Clips', x= 'Type of Vocalization Overlap', size = 20)+
  scale_x_discrete(labels = c('Frequency\n+ Time', 'Frequency', 'Time', 'Non-Overlap'))+
  scale_fill_manual(values = group.colors, name = 'Separation Results')+ 
  geom_text(size = 8, aes(label = count_normal), position = position_stack(vjust = 0.5)) + annotate(geom ='text', x=2, y=-1, label="1", size = 8) +
  annotate(geom ='text', x=1, y=43, label="1", size = 8) + annotate(geom ='text', x=3, y=13, label="n = 10", size = 8)+ 
  annotate(geom ='text', x=1, y=45, label="n = 42", size = 8)+ annotate(geom ='text', x=2, y=11, label="n = 9", size = 8)+
  annotate(geom ='text', x=3, y=11, label="1", size = 8)+ annotate(geom ='text', x=4, y=6, label="n = 4", size =8)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
                     axis.text=element_text(size=17),
                     axis.title.x=element_text(size=20, vjust = -1), 
                     axis.title.y=element_text(size=20, vjust = 1.5), 
                     legend.key.size = unit(1, 'cm'), 
                     legend.title = element_text(size=13), 
                     legend.text = element_text(size=11), 
                     plot.margin = margin(t = 10, r = 0, b = 20, l = 20, unit = "pt"))

overlap_type_plot

jpeg("Figure5b.jpg", width = 4000, height = 3800, units = 'px', res = 500)
overlap_type_plot
dev.off()





# contingency tables 
# some + partial separation
require(ggmosaic)
half_sep_df <- as.data.frame(half_sep_df)
half_sep_df[4, ] <- c(0, 1, 0)
half_sep_df$birdnet_manx_present <- as.character(half_sep_df$birdnet_manx_present)
half_sep_df$birdnet_after_sep <- as.character(half_sep_df$birdnet_after_sep)
half_sep_df$birdnet_manx_present1[half_sep_df$birdnet_manx_present == '0'] <- 'Negative'
half_sep_df$birdnet_manx_present1[half_sep_df$birdnet_manx_present == '1'] <- 'Positive'
half_sep_df$birdnet_after_sep1[half_sep_df$birdnet_after_sep == '0'] <- 'Negative'
half_sep_df$birdnet_after_sep1[half_sep_df$birdnet_after_sep == '1'] <- 'Positive'

half_contingency <- 
  ggplot(half_sep_df) +
  geom_mosaic(aes(weight = count, x = product(birdnet_after_sep1), 
                  fill = birdnet_manx_present1), na.rm = T) +
  annotate(geom ='text', x=0.72, y=0.5, label="61", size = 7) + annotate(geom ='text', x=0.22, y=0.8, label="21", size = 7) + 
  annotate(geom ='text', x=0.22, y=0.3, label="30", size = 7) + 
  labs(y = 'Classification Results Before Separation', x= 'Classification Results After Separation', size = 20)+
  scale_fill_manual(values = c('lightgreen', 'pink'), 
                    breaks=c('Positive', 'Negative'), labels =c('Positive', 'Negative'), 
                    name = 'Classification Results\nBefore Separation') + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text.y =element_text(size=13, angle = 90, hjust = 0.5), 
          axis.text.x=element_text(size=13),
          axis.title.x=element_text(size=14, vjust = -1), 
          axis.title.y=element_text(size=14, vjust = 1.5), 
          legend.key.size = unit(1, 'cm'), 
          legend.title = element_text(size=11, face = 'bold'), 
          legend.text = element_text(size=10))
half_contingency

jpeg("Figure6a.jpg", width = 3000, height = 2100, units = 'px', res = 500)
half_contingency
dev.off()


# full + partial separation
full_sep_df <- as.data.frame(full_sep_df)
full_sep_df[4, ] <- c(0, 1, 0)
full_sep_df$birdnet_manx_present <- as.character(full_sep_df$birdnet_manx_present)
full_sep_df$birdnet_after_sep <- as.character(full_sep_df$birdnet_after_sep)
full_sep_df$birdnet_manx_present1[full_sep_df$birdnet_manx_present == '0'] <- 'Negative'
full_sep_df$birdnet_manx_present1[full_sep_df$birdnet_manx_present == '1'] <- 'Positive'
full_sep_df$birdnet_after_sep1[full_sep_df$birdnet_after_sep == '0'] <- 'Negative'
full_sep_df$birdnet_after_sep1[full_sep_df$birdnet_after_sep == '1'] <- 'Positive'

full_contingency <- 
  ggplot(full_sep_df) +
  geom_mosaic(aes(weight = count, x = product(birdnet_after_sep1), 
                  fill = birdnet_manx_present1), na.rm = T) +
  annotate(geom ='text', x=0.72, y=0.5, label="50", size = 7) + annotate(geom ='text', x=0.23, y=0.82, label="17", size = 7) + 
  annotate(geom ='text', x=0.23, y=0.31, label="29", size = 7) + 
  labs(y = 'Classification Results Before Separation', x= 'Classification Results After Separation', size = 20)+
  scale_fill_manual(values = c('lightgreen', 'pink'), 
                    breaks=c('Positive', 'Negative'), labels =c('Positive', 'Negative'), 
                    name = 'Classification Results\nBefore Separation') + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text.y =element_text(size=13, angle = 90, hjust = 0.5), 
        axis.text.x=element_text(size=13),
        axis.title.x=element_text(size=14, vjust = -1), 
        axis.title.y=element_text(size=14, vjust = 1.5), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=11, face = 'bold'), 
        legend.text = element_text(size=10))
full_contingency

jpeg("Figure6b.jpg", width = 3000, height = 2100, units = 'px', res = 500)
full_contingency
dev.off()



# full + clean separation
full_clean_sep_df <- as.data.frame(full_clean_sep_df)
full_clean_sep_df[4, ] <- c(0, 1, 0)
full_clean_sep_df$birdnet_manx_present <- as.character(full_clean_sep_df$birdnet_manx_present)
full_clean_sep_df$birdnet_after_sep <- as.character(full_clean_sep_df$birdnet_after_sep)
full_clean_sep_df$birdnet_manx_present1[full_clean_sep_df$birdnet_manx_present == '0'] <- 'Negative'
full_clean_sep_df$birdnet_manx_present1[full_clean_sep_df$birdnet_manx_present == '1'] <- 'Positive'
full_clean_sep_df$birdnet_after_sep1[full_clean_sep_df$birdnet_after_sep == '0'] <- 'Negative'
full_clean_sep_df$birdnet_after_sep1[full_clean_sep_df$birdnet_after_sep == '1'] <- 'Positive'

full_clean_contingency <- 
  ggplot(full_clean_sep_df) +
  geom_mosaic(aes(weight = count, x = product(birdnet_after_sep1), 
                  fill = birdnet_manx_present1), na.rm = T) +
  annotate(geom ='text', x=0.88, y=0.5, label="5", size = 7) + annotate(geom ='text', x=0.39, y=0.94, label="2", size = 7) + 
  annotate(geom ='text', x=0.39, y=0.43, label="14", size = 7) + 
  labs(y = 'Classification Results Before Separation', x= 'Classification Results After Separation', size = 20)+
  scale_fill_manual(values = c('lightgreen', 'pink'), 
                    breaks=c('Positive', 'Negative'), labels =c('Positive', 'Negative'), 
                    name = 'Classification Results\nBefore Separation') + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text.y =element_text(size=13, angle = 90, hjust = 0.5), 
        axis.text.x=element_text(size=13),
        axis.title.x=element_text(size=14, vjust = -1), 
        axis.title.y=element_text(size=14, vjust = 1.5), 
        legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=11, face = 'bold'), 
        legend.text = element_text(size=10))
full_clean_contingency

jpeg("Figure6c.jpg", width = 3000, height = 2100, units = 'px', res = 500)
full_clean_contingency
dev.off()













# -------------------------- 1. classification accuracy before separation


unsep_manx_presence_dist <- distinct(unsep_manx_presence, recording_name, .keep_all = T)
#manx_presence_dist1 <- distinct(manx_presence1, recording_name, .keep_all = T)

# classification accuracy before separation
unsep_accuracy <- sum(unsep_manx_presence_dist$birdnet_manx_present)/sum(unsep_manx_presence_dist$manx_present) 
unsep_accuracy # = 0.733
unsep_confmat <- confusionMatrix(data = as.factor(unsep_manx_presence_dist$birdnet_manx_present), reference = as.factor(unsep_manx_presence_dist$manx_present))
unsep_confmat



# Descriptive stat for evaluation data


# vocalization audibility - 189 vocalizations
low_quality <- filter(unsep_manx_presence, total_quality == 1|total_quality == 2)
# 127 individual vocalizations are of low quality
med_quality <- filter(unsep_manx_presence, total_quality == 3|total_quality == 4)
# 49 individual vocalizations are of medium quality
high_quality <- filter(unsep_manx_presence, total_quality == 5|total_quality == 6)
# 13 individual vocalizations are of high quality

medhigh_quality <- filter(unsep_manx_presence, total_quality > 2)
# 62 vocalizations 



# vocalization completeness
full_motif <- filter(unsep_manx_presence, comp_motif ==1, comp_syll ==1)
full_motif_medqua <- filter(unsep_manx_presence, comp_motif ==1, comp_syll ==1, total_quality > 2)
# 41 calls with complete motif, 23 with audio quality higher than 2
full_syllable <- filter(unsep_manx_presence, comp_motif ==0, comp_syll ==1)
full_syllable_medqua <- filter(unsep_manx_presence, comp_motif ==0, comp_syll ==1, total_quality > 2)
# 65 calls with complete syllable only, 25 with audio quality higher than 2 
incomplete_syllable <- filter(unsep_manx_presence, comp_motif ==0, comp_syll ==0)
incomplete_syllable_medqua <- filter(unsep_manx_presence, comp_motif ==0, comp_syll ==0, total_quality > 2)
# 82 calls with incomplete syllable, 13 calls with total quality score higher than 2
23+25+13





# confidence score distribution
unsep_manx_presence_dist %>% filter(confidence_before > 0.1) %>% nrow() # 28
unsep_manx_presence_dist %>% filter(confidence_before > 0.5) %>% nrow() # 6
positive_classification <- unsep_manx_presence_dist %>% filter(!is.na(confidence_before))
mean(positive_classification$confidence_before) # 0.1188523
sqrt(var(positive_classification$confidence_before)) # 0.1700115
max(positive_classification$confidence_before) # 0.8596
min(positive_classification$confidence_before) # 0.0102


# --------------------- 2. classification accuracy after separation by bird mixit

# classification accuracy for all output
bird_mixit_accuracy <- sum(bird_mixit_total$birdnet_manx_present.x)/sum(bird_mixit_total$manx_present.x) 
bird_mixit_accuracy # 0.4796512

bird_mixit_confmat <- confusionMatrix(data = as.factor(bird_mixit_total$birdnet_manx_present.x), 
                                      reference = as.factor(bird_mixit_total$manx_present.x), 
                                      positive = '1')
bird_mixit_confmat
#           Reference
# Prediction   0   1
#          0 121 194
#          1  15 150
# Accuracy : 0.5646          
# 95% CI : (0.5189, 0.6095)
# No Information Rate : 0.7167          
# P-Value [Acc > NIR] : 1

# The accuracy is significantly lower after separation when taking outputs without bird call into account
prop.test(x = c(271, 88), n = c(480, 120))
# X-squared = 10.684, df = 1, p-value = 0.001081
#    prop 1    prop 2 
#   0.5645833 0.7333333

# The accuracy is significantly lower after separation when not taking outputs without bird call into account
prop.test(x = c(150, 88), n = c(344, 120))
#    prop 1    prop 2 
#  0.4360465 0.7333333 
# X-squared = 30.293, df = 1, p-value = 3.714e-08



# classification accuracy for non-background noise channels (n = 360)
bird_mixit_nomainbg <- filter(bird_mixit_total, main_bg == 0)
bird_mixit_nomainbg_accuracy <- sum(bird_mixit_nomainbg$birdnet_manx_present.x)/sum(bird_mixit_nomainbg$manx_present.x) 
bird_mixit_nomainbg_accuracy # 0.4707904

bird_mixit_confmat_nomainbg <- confusionMatrix(data = as.factor(bird_mixit_nomainbg$birdnet_manx_present.x), 
                                      reference = as.factor(bird_mixit_nomainbg$manx_present.x), 
                                      positive = '1')
bird_mixit_confmat_nomainbg
#           Reference
#Prediction   0   1
#          0  63 160
#          1   6 131

#Accuracy : 0.5389          
#95% CI : (0.4859, 0.5913)
#No Information Rate : 0.8083          
#P-Value [Acc > NIR] : 1  

prop.test(x = c(131, 88), n = c(291, 120))
# X-squared = 26.242, df = 1, p-value = 3.012e-07
#    prop 1    prop 2 
#    0.4501718 0.7333333


#### classification accuracy for main individual channel
bird_mixit_mainindiv <- filter(bird_mixit_total, main_indiv_chan == 1)
# 160 vocalization out of 189 can be separated into a main individual channel
bird_mixit_mainindiv_accuracy_after <- sum(bird_mixit_mainindiv$birdnet_manx_present.x)/sum(bird_mixit_mainindiv$manx_present.x)
bird_mixit_mainindiv_accuracy_before <- sum(bird_mixit_mainindiv$birdnet_manx_present.y)/sum(bird_mixit_mainindiv$manx_present.y)

bird_mixit_mainindiv_accuracy # 0.375


# McNemar’s Test to test whether the proportion of vocalizations identified as Manx shearwaters changed after separation
bird_mixit_mainindiv %>% group_by(birdnet_manx_present.x, birdnet_manx_present.y) %>% summarise(count = n())
#birdnet_manx_present.x birdnet_manx_present.y count
#                 <dbl>                  <dbl> <int>
#1                   0                      0    25
#2                   0                      1    75
#3                   1                      0    13
#4                   1                      1    47


mainindiv_table <- matrix(c(47, 75, 13, 25), nrow = 2,
               dimnames = list("Separated" = c("Present", "Absent"), 
                               "Unseparated" = c("Present", "Absent")))
mainindiv_table
mcnemar.test(mainindiv_table, y = NULL, correct = TRUE)
# classification accuracy is significantly lower after separation


bird_mixit_mainindiv_pp <- filter(bird_mixit_mainindiv, birdnet_manx_present.y == 1, birdnet_manx_present.x == 1)
wilcox.test(bird_mixit_mainindiv_pp$confidence_after, bird_mixit_mainindiv_pp$confidence_before, alternative = 'two.sided', paired = T)
# confidence score became significantly lower




#### classification accuracy for clear separation results (no over- and under-separation )
bird_mixit_mainindiv_clear <- filter(bird_mixit_mainindiv, over_sep == 0, under_sep == 0)
bird_mixit_mainindiv_clear_accuracy_after <- sum(bird_mixit_mainindiv_clear$birdnet_manx_present.x)/sum(bird_mixit_mainindiv_clear$manx_present.x)
bird_mixit_mainindiv_clear_accuracy_before <- sum(bird_mixit_mainindiv_clear$birdnet_manx_present.y)/sum(bird_mixit_mainindiv_clear$manx_present.y)

bird_mixit_mainindiv_clear_accuracy
# 43 vocalizations are separated without over and under- separation
bird_mixit_mainindiv_clear %>% group_by(birdnet_manx_present.x, birdnet_manx_present.y) %>% summarise(count = n())
#birdnet_manx_present.x birdnet_manx_present.y count
#                    <dbl>                  <int> <int>
#1                      0                      0    12
#2                      0                      1    15
#3                      1                      0     6
#4                      1                      1    10
mainindiv_clear_table <- matrix(c(10, 15, 6, 12), nrow = 2,
                          dimnames = list("Separated" = c("Present", "Absent"), 
                                          "Unseparated" = c("Present", "Absent")))
mainindiv_clear_table
mcnemar.test(mainindiv_clear_table, y = NULL, correct = TRUE)
# still significantly lower 


# prop.test(x = c(sum(bird_mixit_mainindiv$birdnet_manx_present), 88), n = c(sum(bird_mixit_mainindiv$manx_present), 120))
# X-squared = 19.566, df = 1, p-value = 9.717e-06
#    prop 1    prop 2 
#    0.4629630 0.7333333



bird_mixit_mainindiv_clear_pp <- filter(bird_mixit_mainindiv_clear, birdnet_manx_present.y == 1, birdnet_manx_present.x == 1)

wilcox.test(bird_mixit_mainindiv_clear_pp$confidence_after, bird_mixit_mainindiv_clear_pp$confidence_before, alternative = 'two.sided', paired = T)
# confidence score became significantly lower 


#### classification accuracy for clear individual calls with medium-high quality and at least full syllables
bird_mixit_mainindiv_good <- filter(bird_mixit_mainindiv, total_quality > 2, comp_syll == 1)
# 45 vocalizations
bird_mixit_mainindiv_good %>% group_by(birdnet_manx_present.x, birdnet_manx_present.y) %>% summarise(count = n())
mainindiv_good_table <- matrix(c(28, 13, 0, 4), nrow = 2,
                                dimnames = list("Separated" = c("Present", "Absent"), 
                                                "Unseparated" = c("Present", "Absent")))
mainindiv_good_table
mcnemar.test(mainindiv_good_table, y = NULL, correct = TRUE)
# still significantly lower 

wilcox.test(bird_mixit_mainindiv_good$confidence_after, bird_mixit_mainindiv_good$confidence_before, alternative = 'less', paired = T)
# confidence score became significantly lower 



##### stricter criteria - complete motif vocalizations
bird_mixit_mainindiv_good1 <- filter(bird_mixit_mainindiv, total_quality > 2, comp_motif == 1)
# 25 vocalizations are separated without over and under- separation
bird_mixit_mainindiv_good1 %>% group_by(birdnet_manx_present.x, birdnet_manx_present.y) %>% summarise(count = n())
mainindiv_good_table1 <- matrix(c(17, 7, 0, 1), nrow = 2,
                               dimnames = list("Separated" = c("Present", "Absent"), 
                                               "Unseparated" = c("Present", "Absent")))
mainindiv_good_table1
mcnemar.test(mainindiv_good_table1, y = NULL, correct = TRUE)
# still significantly lower 

wilcox.test(bird_mixit_mainindiv_good1$confidence_after, bird_mixit_mainindiv_good1$confidence_before, alternative = 'less', paired = T)
# confidence score became significantly lower 


##### stricter criteria - complete motif vocalizations and higher audio quality
bird_mixit_mainindiv_good2 <- filter(bird_mixit_mainindiv, total_quality > 3, comp_motif == 1)
# 16 vocalizations
bird_mixit_mainindiv_good2 %>% group_by(birdnet_manx_present.x, birdnet_manx_present.y) %>% summarise(count = n())
mainindiv_good_table2 <- matrix(c(11, 4, 0, 1), nrow = 2,
                                dimnames = list("Separated" = c("Present", "Absent"), 
                                                "Unseparated" = c("Present", "Absent")))
mainindiv_good_table2
mcnemar.test(mainindiv_good_table2, y = NULL, correct = TRUE)
# no difference now, similar classification accuracy

wilcox.test(bird_mixit_mainindiv_good2$confidence_after, bird_mixit_mainindiv_good2$confidence_before, alternative = 'less', paired = T)
# confidence score became significantly lower



# effect of audibility on classification results



#audibility_model1 <- glm(birdnet_manx_present.x ~ max_audibility, data =  distinct(individual_sep_results, recording_name, .keep_all = T), family = 'binomial', na.action = 'na.omit')
#summary(audibility_model1)

#audibility_model2 <- glm(birdnet_manx_present.y ~ total_quality, data = individual_sep_results, family = 'binomial', na.action = 'na.omit')
#summary(audibility_model2)

#individual_sep_results$success_sep <- as.factor(individual_sep_results$success_sep)
#m1<-wilcox.test(total_quality ~ success_sep, data=individual_sep_results, na.rm=TRUE, paired=FALSE, exact = F)
#m1

median(indiv_sep_results$total_quality)
mean(indiv_sep_results$total_quality)


# classification accuracy when using clips as unit












# ----------------------- 3. descriptive stat for bird MixIT separation results



# ------------------------ 3. Manx MixIT results
manx_mixit_lab <- read.csv('manx_mixit_label.csv')
