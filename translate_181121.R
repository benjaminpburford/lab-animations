library(tidyverse)

# build metadata -------------------------------------------------------------------
# contains condition, condition_num, start_dist_cm, dot_speed_cms, angle_rel_fish, dot_rad_cm, dot_dir
translate_181121_meta <- expand.grid(start_dist_cm = 15, # specify starting distance, the distance from fish that dot will circle fish 
                                dot_speed_cms = c(32,16,4), # dot speed in cm/s
                                angle_rel_fish = c(180), # dot start angle (dot starts from behind, same for all)
                                dot_rad_cm = c(0.2,0.5,1,2,4,8), # dot radius in cm
                                dot_dir = c("clockwise","anticlockwise")) %>% # circling direction for dot
  mutate(start_dist_cm = start_dist_cm + dot_rad_cm, # add dot radius to starting position so that larger dots dont appear closer
         condition = str_c("translate","_",dot_dir,"_",dot_speed_cms,"_",dot_rad_cm), # condition specs
         condition_num = as.integer(as.character(factor(condition,labels = seq(1,length(unique(condition)),1))))) %>% # condition dummy number
  relocate(condition,condition_num) %>%
  arrange(condition_num)

# export as tsv
write.table(translate_181121_meta,"translate_181121_meta.tsv",row.names = F,sep = "\t")

# function for translate -------------------------------------------------------------------
# dot will always be traveling around the fish maintaining a consistent distance from fish

# from real time tracking, it takes in frame and fish body length and x,y position in pixels
# we must specify condition number (cond, in reference to meta file), pixels to cm conversion (pixcm), and frame rate (fps)
# meta file will be filtered by condition to get starting position, rotation direction, size, and speed of stimulus

# outputs dot x,y position and radius in pixels (in that order)
translate_fun <- function(cond,fish_len_pix,pixcm,frame,fps,fish_x,fish_y) {
  
  temp <- filter(translate_181121_meta,condition_num==cond)
  
  travel_dist <- 2*pi*temp$start_dist_cm # determine circumference of travel path for total frame calc
  
  tot_frames <- (travel_dist)/((temp$dot_speed_cms)/fps)
  
  if (frame > tot_frames) {
    stop("total frames reached, trial stop") # stops function once dot has traveled the circumference
    
  } else {
    start_dist_pix <- temp$start_dist_cm*pixcm # converts all to pixels
    dot_speed_pixs <- temp$dot_speed_cms*pixcm
    dot_rad_pix <- temp$dot_rad_cm*pixcm
    dot_dir <- temp$dot_dir # get dot direction
    
    rad_rel_fish <- temp$angle_rel_fish*(pi/180) # converts start angle to radians
    
    second <- (frame-1)/fps # get seconds for time step
    radius <-  start_dist_pix # radius always the same
    dot_speed_rad = dot_speed_pixs/radius # get angular velocity of dot in radians from linear velocity
    dot_rad = case_when(frame==1~rad_rel_fish, # now determine the radian values for each time step, starting with initial position
                        frame>1&dot_dir=="clockwise"~rad_rel_fish-(dot_speed_rad*second),
                        frame>1&dot_dir=="anticlockwise"~rad_rel_fish+(dot_speed_rad*second))
    
    dot_x = radius*cos(dot_rad) # convert radians to cartesian coordinates
    dot_y = radius*sin(dot_rad)
    
    dot_x <- dot_x + fish_x # adds fish position to dot position to maintain dot position relative to fish
    dot_y <- dot_y + fish_y
    
    c(dot_x,dot_y,dot_rad_pix) # the output
  }
  
}

# example -------------------------------------------------------------------
translate_fun(cond = 5, # we define
              fish_len_pix = 32, # from calibration
              pixcm = 8, # we define
              frame = 1, # we define, but probably built into shell
              fps = 45, # we define
              fish_x = 0, # from tracking
              fish_y = 0) # from tracking

