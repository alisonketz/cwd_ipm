####################################################################
###
### Loading home range sections based on the section which contain 
### the most number of GPS fixes for the GPS collared deer
###
####################################################################


home_section <- read_csv("~/Documents/Movement/Collared_deer_section/home_section.csv")

df_cap$home_section <- c()
for(i in 1:nrow(home_section)){
  df_cap$home_section[df_cap$lowtag %in% home_section$lowtag[i]] <-  home_section$sect[i]
}


####################################################################
###
### Home range sections for fawns/deer w/o GPS fix location section
### based on the mortality and censor sheet GPS locations
###
####################################################################

###
### no homerange sections for these deer,
### lets check see if they are ever CWD +
###

df_cap[is.na(df_cap$home_section),1]

#extracting the lowtags and GPS fixes at mortality
# of all deer without GPS fix homerange sections
df_mort_loc <- d_mort[d_mort$lowtag %in% df_cap$lowtag[is.na(df_cap$home_section)],c(2,19:20)]

#all but 5 of these deer without GPS fix home range sections 
#were captured as 0 age fawns. 
table(df_cap$ageclass_cap[df_cap$lowtag %in% df_mort_loc$lowtag])
df_cap[df_cap$lowtag %in% df_mort_loc$lowtag & df_cap$ageclass_cap != "Fawn",]

#there are 16 deer for which we don't have a home range section from mortality data
#several of these overlap with the 
sum(is.na(df_mort_loc$long))

low_home_tofind <-  df_mort_loc$lowtag[is.na(df_mort_loc$long)]

#comparing these 16 deer without GPS fix homerange sections
sort(df_cap$lowtag[df_cap$lowtag %in% df_mort_loc$lowtag & df_cap$ageclass_cap != "Fawn"])
sort(low_home_tofind)

df_cap$lowtag[df_cap$lowtag %in% df_mort_loc$lowtag & df_cap$ageclass_cap != "Fawn"] %in% low_home_tofind
sum(df_cap$lowtag[df_cap$lowtag %in% df_mort_loc$lowtag & df_cap$ageclass_cap != "Fawn"] %in% low_home_tofind)
#7 of these overlap

#removing deer from the data frame for determining home range section
#for which there is no fix location at mortality
df_mort_loc <- df_mort_loc[!is.na(df_mort_loc$long),]
df_mort_loc <- df_mort_loc[!is.na(df_mort_loc$lat),]
df_mort_loc[df_mort_loc$lowtag==6222,2] <- substr(df_mort_loc[df_mort_loc$lowtag==6222,2],1,8)
class(df_mort_loc$lat) <- "numeric"

points <- st_as_sf(df_mort_loc,
            coords = c("long","lat"),
            crs = 4326,
            agr = "constant"
            )
points_proj <- points %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# set CRS for points to be NAD83, same as study section polygon
st_crs(points_proj) <-  st_crs(study_df)

points_poly_joined <- sf::st_join(points_proj, study_df) #%>%  # spatial join to get intersection of points and poly
            # filter(!is.na(dsection)) #filtering only for points that fall within the study area

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_proj,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of mortalities for deer w/o homerange sections")
outplot

ggsave(paste0("figures/mortalities_gps_sections_nohome.png"),outplot, height = 6,width = 6)


##########################################################################
###
### Notice in that plot there are 7 individuals 
### whose mortalities occured outside of the study area
###
##########################################################################

###filtering out those deer, we must then also look at whether 

low_home_tofind_mortwide <- points_poly_joined %>% filter(is.na(dsection)) %>% pull(lowtag)

#extracting the points for all the individuals with mortalities within the study area
points_poly_joined <- sf::st_join(points_proj, study_df) %>%  # spatial join to get intersection of points and poly
            filter(!is.na(dsection)) #filtering only for points that fall within the study area

nrow(points_poly_joined)

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_poly_joined,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of mortalities for deer without homerange sections")
outplot
ggsave(paste0("figures/mortalities_gps_sections_nohome_clean.png"),outplot, height = 2.5,width = 6)

#assigning each of these mortality sections to be the home range section

for(i in 1:nrow(points_poly_joined)){
  df_cap$home_section[df_cap$lowtag %in% points_poly_joined$lowtag[i]] <- points_poly_joined$dsection[i]
}

#################################################################################
###
### For the deer remaining without a home range section
### use the censor sheet to find a home range section
###
#################################################################################

#how many? 179
sum(is.na(df_cap$home_section))
table(df_cap$ageclass_cap[is.na(df_cap$home_section)])
#what is the censor sheet gps location for the deer without a homerange?
df_cens_loc <- d_cens[d_cens$lowtag %in% df_cap$lowtag[is.na(df_cap$home_section)],c(1,11:12)]
nrow(df_cens_loc)
df_cens_loc <- df_cens_loc[!is.na(df_cens_loc$latitude),]
nrow(df_cens_loc)
head(df_cens_loc)
points <- st_as_sf(df_cens_loc,
            coords = c("longitude","latitude"),
            crs = 4326,
            agr = "constant"
            )
points_proj <- points %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# set CRS for points to be NAD83, same as study section polygon
st_crs(points_proj) <-  st_crs(study_df)

points_poly_joined <- sf::st_join(points_proj, study_df) #%>%  # spatial join to get intersection of points and poly
            # filter(!is.na(dsection)) #filtering only for points that fall within the study area

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_proj,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of censored deer without homerange sections")
outplot

ggsave(paste0("figures/censor_gps_sections_nomort_nohome.png"),outplot, height = 2.5,width = 6)


###filtering out those deer, we must then also look at whether 
low_home_tofind_censorwide <- points_poly_joined %>% filter(is.na(dsection)) %>% pull(lowtag)

#extracting the points for all the individuals with mortalities within the study area
points_poly_joined <- sf::st_join(points_proj, study_df) %>%  # spatial join to get intersection of points and poly
            filter(!is.na(dsection)) #filtering only for points that fall within the study area

nrow(points_poly_joined)

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_poly_joined,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of censors for deer without homerange sections")
outplot
ggsave(paste0("figures/censor_gps_sections_nomort_nohome_clean.png"),outplot, height = 2.5,width = 6)

#assigning each of these censor sections to be the home range section

for(i in 1:nrow(points_poly_joined)){
  df_cap$home_section[df_cap$lowtag %in% points_poly_joined$lowtag[i]] <- points_poly_joined$dsection[i]
}

####################################################################
###
### Home range sections for deer still missing? 
### GPS location fixes from collars, mortalities, or censor events
###
####################################################################

#there's still 96 deer without home range sections
sum(is.na(df_cap$home_section))
table(df_cap$ageclass_cap[is.na(df_cap$home_section)])

#filling in the missing long/lat in the fawn capture data
d_fawncap$long[is.na(d_fawncap$long)] <- 89.50818
d_fawncap$lat[is.na(d_fawncap$lat)] <- 43.02643
d_fawncap$long <- d_fawncap$long * -1

low_fawn_tohome <- df_cap$lowtag[is.na(df_cap$home_section) & df_cap$ageclass_cap == "Fawn"]

names(d_fawncap)
fawncap_home <- d_fawncap[d_fawncap$lowtag %in% low_fawn_tohome,c(2,8:9)]
fawncap_home

points <- st_as_sf(fawncap_home,
            coords = c("long","lat"),
            crs = 4326,
            agr = "constant"
            )
points_proj <- points %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# set CRS for points to be NAD83, same as study section polygon
st_crs(points_proj) <-  st_crs(study_df)

points_poly_joined <- sf::st_join(points_proj, study_df) #%>%  # spatial join to get intersection of points and poly
            # filter(!is.na(dsection)) #filtering only for points that fall within the study area

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_proj,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of fawncaptures w/o homerange sections")
outplot

ggsave(paste0("figures/fawncap_gps_sections_nomort_nohome.png"),outplot, height = 2.5,width = 6)

#this fawn was captured outside the study area
# d_fawncap[d_fawncap$lowtag==5764,]
# points_poly_joined[points_poly_joined$lowtag==5764,]
# d_cens[d_cens$lowtag==5764,]
# d_surv[d_surv$lowtag==5764,]

#extracting the points for all the individuals with fawncaptures within the study area
points_poly_joined <- sf::st_join(points_proj, study_df) %>%  # spatial join to get intersection of points and poly
            filter(!is.na(dsection)) #filtering only for points that fall within the study area

nrow(points_poly_joined)

outplot <- ggplot() + geom_sf(data = study_df) + 
          geom_sf(data = points_poly_joined,aes(color = lowtag)) + 
          ggtitle ("GPS fix locations of fawncaptures w/o homerange sections")
outplot
ggsave(paste0("figures/fawncap_gps_sections_nomort_nohome_clean.png"),outplot, height = 2.5,width = 6)


#assigning each of these censor sections to be the home range section

for(i in 1:nrow(points_poly_joined)){
  df_cap$home_section[df_cap$lowtag %in% points_poly_joined$lowtag[i]] <- points_poly_joined$dsection[i]
}


####################################################################
###
### Home range sections for fawns are based on 
### GPS location / section of fawn capture
###
####################################################################


# points <- st_as_sf(d_fawncap,
#             coords = c("long","lat"),
#             agr = "constant"
#             )
# st_crs(points) <- 4326
# points_proj <- points %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# # set CRS for points to be NAD83, same as study section polygon
# st_crs(points_proj) <-  st_crs(study_df)

# points_poly_joined <- sf::st_join(points_proj, study_df) #%>%  # spatial join to get intersection of points and poly
#             # filter(!is.na(dsection)) #filtering only for points that fall within the study area

# outplot <- ggplot() + geom_sf(data = study_df) + 
#           geom_sf(data = points_proj,aes(color = lowtag)) + 
#           ggtitle ("GPS fix locations of fawn captures")
# outplot

# ggsave(paste0("figures/fawn_capture_gps_sections.png"),outplot, height = 2.5,width = 6)


####################################################################
###
### Home range sections for remaining deer without 
### home range sections?
###
####################################################################

#there's 96 that we don't have home range section references for
sum(is.na(df_cap$home_section))
table(df_cap$ageclass_cap[is.na(df_cap$home_section)])
#retaining the one that was a recapture

low_check_nohome <- df_cap$lowtag[is.na(df_cap$home_section)]
#6876 is 

#These 3 deer are totally suspect, so just removing them. 
df_cap <- df_cap[!(df_cap$lowtag %in% low_check_nohome),]

#only retaining this one, will get GPS points from Marie next week probably
#continuing with recap location
low_check_nohome <- low_check_nohome[1]

#the fawn is the one captured and collared outside of the study area
#the >2yrs deer is
#one of these was recapture
# d_cap[d_cap$lowtag %in% low_check_nohome,]
# d_cap$capturestatus[d_cap$lowtag %in% low_check_nohome]
# d_cap$collarid[d_cap$lowtag %in% low_check_nohome]
# d_cap$observationsnotes[d_cap$lowtag %in% low_check_nohome]
# d_tooth[d_tooth$lowtag %in% low_check_nohome,]

# #looks like one of these was assigned a new lowtag?!?
# d_cwd[d_cwd$lowtag %in% low_check_nohome,]
# d_post_cwd[d_post_cwd$lowtag %in% low_check_nohome,]
# df_cap[df_cap$lowtag %in% low_check_nohome,]

#but can't find any deer with the lowtag mentioned in the season3 cwd test
# d_fawncap[d_fawncap$lowtag==6027,]
# d_cap[d_cap$lowtag==6027,]
# d_cwd[d_cwd$lowtag==6027,]
# d_mort[d_mort$lowtag==6027,]
# d_cens[d_cens$lowtag==6027,]

# miss_sect<-d_surv[d_surv$lowtag %in% df_cap$lowtag[is.na(df_cap$home_section)],]
# miss_sect[miss_sect$cwd_cap==1,]
# miss_sect[miss_sect$cwd_cap==0 & miss_sect$cwd_mort==1,]


# d_cap[d_cap$lowtag %in% 6876,]
# d_cap$capturestatus[d_cap$lowtag %in% 6876]
# d_cap$collarid[d_cap$lowtag %in% 6876]
# d_cap$observationsnotes[d_cap$lowtag %in% 6876]
# d_tooth[d_tooth$lowtag %in% 6876,]


#for the 3 that are not fawns, we could use the capture lat/long for now

# d_cap_loc_temp <- d_cap[d_cap$lowtag %in% low_check_nohome,c(4,10:11)]
# d_cap_loc_recap <- d_cap[d_cap$lowtag %in% low_check_nohome[1],c(4,62:63)]
# d_cap_loc <- d_cap_loc_temp
# d_cap_loc[2,] <- d_cap_loc_recap
# d_cap_loc$long <- d_cap_loc$long * -1

# points <- st_as_sf(d_cap_loc,
#             coords = c("long","lat"),
#             crs = 4326,
#             agr = "constant"
#             )
# points_proj <- points %>% st_transform("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# # set CRS for points to be NAD83, same as study section polygon
# st_crs(points_proj) <-  st_crs(study_df)

# points_poly_joined <- sf::st_join(points_proj, study_df) %>%  # spatial join to get intersection of points and poly
#             filter(!is.na(dsection)) #filtering only for points that fall within the study area

# points_poly_joined$lowtag <- as.factor(points_poly_joined$lowtag)
# outplot <- ggplot() + geom_sf(data = study_df) + 
#           geom_sf(data = points_poly_joined,aes(color = lowtag)) + 
#           ggtitle ("GPS fix locations of 1 deer w/o homerange sections")
# outplot

# ggsave(paste0("figures/cap_gps_sections_miss.png"),outplot, height = 2.5,width = 6)


# #assigning each of these censor sections to be the home range section
# for(i in 1:nrow(points_poly_joined)){
#   df_cap$home_section[df_cap$lowtag %in% points_poly_joined$lowtag[i]] <- points_poly_joined$dsection[i]
# }

# #removing the one fawn that was captured outside of the study area
# #and removing the one adult that is NA for home_section... unsure of why 
# df_cap <- df_cap[!is.na(df_cap$home_section),]
df_cap$lowtag[is.na(df_cap$home_section)]


n_cap <- nrow(df_cap)
n_mort <- nrow(d_mort)
n_cens <- nrow(d_cens)
n_cap
n_mort
n_cens
