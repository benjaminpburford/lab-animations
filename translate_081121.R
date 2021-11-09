library(tidyverse)
library(gganimate)
library(gifski)

# fish and tank starting conditions
fish_len = 4 # fish body length should be ~ 4 cm
fish_head_x = 0 # defining the head as the vertex
fish_head_y = 0
fps = 60 # frames per second
start_dist = fish_len*3 # stimuli will be 3 body lengths from fish (radius)
travel_dist = 2*pi*start_dist # stimuli will travel one full rotation around fish (circumference)

# stimuli approach angle
dot_angle <- tibble(angle_rel_fish = 180) %>% # starts behind fish head
  mutate(rad_rel_fish = angle_rel_fish*(pi/180)) # covert to radians

# stimuli size
dot_size = tibble(dot_size_rel_bl = c(0.1,0.25,0.5,1,2,4)) %>% # size relative to body length
  mutate(dot_size_cm = (fish_len*dot_size_rel_bl)/2) # in cm, needs to be provided as a radius (so /2)

# stimuli speed
dot_speed = c(8,4,1) # speeds in bodylengths per second

# stimuli direction
dot_dir = c("clockwise","anticlockwise") # will rotate either direction


# build dataframe
approach <- expand.grid(rad_rel_fish = dot_angle$rad_rel_fish, # make all unique combinations of approach angle, size, speed, direction
                        dot_size_cm = dot_size$dot_size_cm,
                        dot_speed_blps = dot_speed,
                        dot_dir = dot_dir) %>%
  mutate(dot_numframes = round(travel_dist/((fish_len*dot_speed_blps)/fps)), # determine number of frames required for animation (depends on speed)
         dot_start_x = start_dist*cos(rad_rel_fish), # convert stimuli approach angle to cartesian
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
  group_by(angle_rel_fish,dot_size_cm,dot_speed_blps,dot_dir) %>%
  mutate(angle_size_speed = str_c(angle_rel_fish,"_",dot_size_cm,"_",dot_speed_blps,"_",dot_dir), # dummy variable to be converted later
         frame = seq(1:length(dot_speed_blps)), # frames number for animation
         second = frame/fps) %>% # add frame numbers for each animation
  ungroup() %>%
  mutate(radius = start_dist,
         dot_speed_rad = (dot_speed_blps*fish_len)/radius, # get angular velocity of dot in radians from linear velocity
         dot_rad = case_when(frame==1~rad_rel_fish, # now determine the radian values for each time step, starting with initial position
                             frame>1&dot_dir=="clockwise"~rad_rel_fish-(dot_speed_rad*second),
                             frame>1&dot_dir=="anticlockwise"~rad_rel_fish+(dot_speed_rad*second)),
         dot_x = radius*cos(dot_rad), # convert radians to cartesian coordinates
         dot_y = radius*sin(dot_rad),
         condition = as.integer(as.character(factor(angle_size_speed,labels = seq(1,length(unique(angle_size_speed)),1)))),
         fish_head_x = fish_head_x,
         fish_head_y = fish_head_y) %>%
  select(c(condition,angle_rel_fish,dot_size_cm,dot_speed_blps,dot_dir,frame,second,fish_head_x,fish_head_y,dot_x,dot_y))

# plot to check
approach %>%
  filter(condition==28) %>% # examine any of the conditions
  ggplot(aes(dot_x,dot_y,size=dot_size_cm)) +
  geom_point(aes(fish_head_x,fish_head_y),size=4,col="red") +
  geom_point() +
  xlim(c(-start_dist,start_dist)) +
  ylim(c(-start_dist,start_dist)) +
  transition_time(second)


# select relevant rows, columns and write metadata
translate_081121_meta <- approach %>%
  group_by(condition) %>%
  mutate(total_time_s = max(second)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-c(frame,second,fish_head_x,fish_head_y,dot_x,dot_y))

write.csv(translate_081121_meta,"translate_081121_meta.csv",row.names = F)

# select relevant columns and write data
translate_081121_matrix <- approach %>%
  select(-c(angle_rel_fish,dot_dir,dot_speed_blps)) %>%
  arrange(condition,frame)

write.csv(translate_081121_matrix,"translate_081121_matrix.csv",row.names = F)



