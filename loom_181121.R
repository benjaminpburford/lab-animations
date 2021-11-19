library(tidyverse)

# build metadata -------------------------------------------------------------------
# contains condition, condition_num, start_dist_cm, dot_speed_cms, angle_rel_fish, dot_rad_cm
loom_181121_meta <- expand.grid(start_dist_cm = 60, # specify starting distance (same for all)
                                dot_speed_cms = c(32,16,4), # dot speed in cm/s
                                angle_rel_fish = c(-90,90), # dot approach angle (from left or right essentially)
                                dot_rad_cm = c(0.2,0.5,1,2,4,8)) %>% # dot radius in cm
  mutate(start_dist_cm = start_dist_cm + dot_rad_cm, # add dot radius to starting position so that larger dots dont appear closer
         condition = str_c("loom","_",angle_rel_fish,"_",dot_speed_cms,"_",dot_rad_cm), # condition specs
         condition_num = as.integer(as.character(factor(condition,labels = seq(1,length(unique(condition)),1))))) %>% # condition dummy number
  relocate(condition,condition_num) %>%
  arrange(condition_num)

# export as tsv
write.table(loom_181121_meta,"loom_181121_meta.tsv",row.names = F,sep = "\t")

# function for loom -------------------------------------------------------------------
# dot will always be traveling in straight line to the fish

# from real time tracking, it takes in frame and fish body length and x,y position in pixels
# we must specify condition number (cond, in reference to meta file), pixels to cm conversion (pixcm), and frame rate (fps)
# meta file will be filtered by condition to get starting position, size, and speed of stimulus

# outputs dot x,y position and radius in pixels (in that order)
loom_fun <- function(cond,fish_len_pix,pixcm,frame,fps,fish_x,fish_y) {
  
  temp <- filter(loom_181121_meta,condition_num==cond)
  
  tot_frames <- (temp$start_dist_cm)/((temp$dot_speed_cms)/fps)
  
  if (frame > tot_frames) {
    stop("total frames reached, trial stop") # stops function once dot has traveled the start distance (60 cm)
    
  } else {
    start_dist_pix <- temp$start_dist_cm*pixcm # converts all to pixels
    dot_speed_pixs <- temp$dot_speed_cms*pixcm
    dot_rad_pix <- temp$dot_rad_cm*pixcm
    
    rad_rel_fish <- temp$angle_rel_fish*(pi/180) # converts start angle to radians
    radius <- start_dist_pix-((frame-1)*(1/fps)*dot_speed_pixs) # determines radius, or the distance from the fish
    
    dot_x <- radius*cos(rad_rel_fish) # converts to cartesian
    dot_y <- radius*sin(rad_rel_fish)
    dot_x <- dot_x + fish_x # adds fish position to dot position to maintain dot position relative to fish
    dot_y <- dot_y + fish_y
    
    c(dot_x,dot_y,dot_rad_pix) # the output
  }
  
}

# example -------------------------------------------------------------------
loom_fun(cond = 1, # we define
         fish_len_pix = 32, # from calibration
         pixcm = 8, # we define
         frame = 1, # we define, but probably built into shell
         fps = 45, # we define
         fish_x = 0, # from tracking
         fish_y = 0) # from tracking

