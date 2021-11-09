library(tidyverse)
library(gganimate)
library(gifski)

# fish and tank starting conditions
fish_len = 4 # fish body length should be ~ 4 cm
fish_head_x = 0 # defining the head as the vertex
fish_head_y = 0
tank_diam = 100 # diameter of arena
fps = 60 # frames per second
start_dist = (tank_diam/2)+(tank_diam/10) # stimuli starts beyond edge of arena by 10 cm (radius)
sweep_dist = fish_len*2 # point through which stimuli will pass 

# stimuli approach angle
dot_angle <- tibble(angle_rel_fish = c(-90,90)) %>% # starting angles relative to fish head
  mutate(rad_rel_fish = angle_rel_fish*(pi/180)) # covert to radians

# stimuli size
dot_size = tibble(dot_size_rel_bl = c(0.1,0.25,0.5,1,2,4)) %>% # size relative to body length
  mutate(dot_size_cm = (fish_len*dot_size_rel_bl)/2) # in cm, needs to be provided as a radius (so /2)

# stimuli speed
dot_speed = c(8,4,1) # speeds in bodylengths per second


# buid dataframe
approach <- expand.grid(rad_rel_fish = dot_angle$rad_rel_fish, # make all unique combinations of approach angle, size, and speed
                        dot_size_cm = dot_size$dot_size_cm,
                        dot_speed_blps = dot_speed) %>%
  mutate(dot_numframes = round((start_dist*2)/((fish_len*dot_speed_blps)/fps)), # determine number of frames required for animation (depends on speed)
         dot_start_x = sweep_dist+(start_dist*cos(rad_rel_fish)), # convert stimuli approach angle to cartesian
         dot_start_y = start_dist*sin(rad_rel_fish)) %>%
  uncount(dot_numframes) %>% # repeat rows by the number of frames required for each animation
  left_join(dot_angle) # regain degree angles - easier to visualize

# plot to check starting positions relative to fish head
approach %>% 
  group_by(dot_start_x,dot_start_y) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(dot_start_x,dot_start_y)) +
  geom_point() +
  geom_point(aes(fish_head_x,fish_head_y),col="red") +
  xlim(c(-start_dist,start_dist)) +
  ylim(c(-start_dist,start_dist))

# continue building dataframe
approach <- approach %>%
  group_by(angle_rel_fish,dot_size_cm,dot_speed_blps) %>%
  mutate(angle_size_speed = str_c(angle_rel_fish,"_",dot_size_cm,"_",dot_speed_blps), # dummy variable to be converted later
         frame = seq(1:length(dot_speed_blps)), # frames number for animation
         second = frame/fps) %>% # add frame numbers for each animation
  ungroup() %>%
  mutate(radius = case_when(frame==1~start_dist,
                            frame>1~start_dist-(dot_speed_blps*fish_len*second)),
         dot_x = sweep_dist+(radius*cos(rad_rel_fish)), # convert stimuli approach angle and radius to cartesian coordinates
         dot_y = radius*sin(rad_rel_fish),
         condition = as.integer(as.character(factor(angle_size_speed,labels = seq(1,length(unique(angle_size_speed)),1)))),
         fish_head_x = fish_head_x,
         fish_head_y = fish_head_y) %>%
  select(c(condition,angle_rel_fish,dot_size_cm,dot_speed_blps,frame,second,fish_head_x,fish_head_y,dot_x,dot_y))

# plot to check
approach %>%
  filter(condition==28) %>% # examine any of the conditions
  ggplot(aes(dot_x,dot_y,size=dot_size_cm)) +
  geom_point(aes(fish_head_x,fish_head_y),size=4,col="red") +
  geom_point() +
  xlim(c(-start_dist,start_dist)) +
  ylim(c(-start_dist,start_dist)) +
  transition_time(second)


# create and write metadata
valence_sweep_031121_meta <- approach %>%
  group_by(condition) %>%
  mutate(total_time_s = max(second)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-c(frame,second,fish_head_x,fish_head_y,dot_x,dot_y))

write.csv(valence_sweep_031121_meta,"valence_sweep_031121_meta.csv",row.names = F)

# select relevant columns and write data
valence_sweep_031121_matrix <- approach %>%
  select(-c(angle_rel_fish,dot_speed_blps)) %>%
  arrange(condition,frame)

write.csv(valence_sweep_031121_matrix,"valence_sweep_031121_matrix.csv",row.names = F)

