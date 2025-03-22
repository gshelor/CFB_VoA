library(pacman)
pacman::p_load(tidyverse, gt, cfbfastR, here, RColorBrewer, gtExtras, cfbplotR, ggpubr, webshot2, parallel, sf, terra, geosphere)

StateBoundaries <- read_sf(here("Data", "StateBoundaries", "cb_2018_us_state_500k.shp")) |>
  filter(as.numeric(STATEFP) < 60 & as.numeric(STATEFP) != 2) |>
  st_transform(4326)

StateBoundaries_NoHI <- StateBoundaries |>
  filter(NAME != "Hawaii")

teams <- cfbfastR::cfbd_team_info(year = 2013) |>
  filter(school != "Georgia State" & school != "South Alabama") |>
  select(school, mascot, abbreviation, conference, division, venue_name, city, state, longitude, latitude, elevation, capacity, year_constructed, grass, dome)
teams$elevation <- as.numeric(teams$elevation)

teams_sf <- st_as_sf(teams, coords = c("longitude", "latitude"), crs = 4326)

teams_sf_nohawaii <- teams_sf |>
  filter(school != "Hawai'i")

teams_sf_3Indies <- teams_sf |>
  filter(school != "Hawai'i" & school != "Army" & school != "Navy")

teams_sf_4Indies <- teams_sf |>
  filter(school != "Hawai'i" & school != "Army" & school != "Navy" & school != "Notre Dame")

teams_sf_4Indies_HIAcad <- teams_sf |>
  filter(school != "Hawai'i" & school != "Army" & school != "Navy" & school != "Air Force")

teams_sf_2Indies <- teams_sf |>
  filter(school != "Army" & school != "Navy")

### if I include Hawaii, and add Independents as a separate "conference" of geographically clustered schools
nk = 12 # number of clusters to test
nkm <- data.frame(number = numeric(nk), sse = numeric(nk)) # empty df to hold results
set.seed(802)
for(j in 1:nk){ ## loop through each cluster number
  nkm$number[j] <- j ## cluster number
  nkm$sse[j] <- kmeans(st_coordinates(teams_sf), centers = j)$tot.withinss ## record error
}
plot(nkm$number, nkm$sse, xlab = 'Number of Clusters', ylab = 'SSE', main = 'K-Means Error Evaluation Plot')
lines(nkm$number, nkm$sse)

set.seed(802)
kmeans_HI <- kmeans(st_coordinates(teams_sf), centers = 12)
plot(st_geometry(teams_sf), col=factor(kmeans_HI$cluster), main = 'Assigned Clusters, with Hawaii')

teams_sf$twelveConfCluster <- kmeans_HI$cluster


### still include Hawaii, but don't use treat indies as a conference
set.seed(802)
kmeans_HI_noIndy <- kmeans(st_coordinates(teams_sf), centers = 11)
plot(st_geometry(teams_sf), col=factor(kmeans_HI_noIndy$cluster), main = 'Assigned Clusters, with Hawaii')

teams_sf$elevenConfCluster <- kmeans_HI_noIndy$cluster


teams_sf$twelveConfCluster_centroid_dist <- -999
teams_sf$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf)){
  temp_clust <- teams_sf |>
    filter(twelveConfCluster == teams_sf$twelveConfCluster[i])
  teams_sf$twelveConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust))))))
  
  temp_clust2 <- teams_sf |>
    filter(elevenConfCluster == teams_sf$elevenConfCluster[i])
  teams_sf$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}

for (i in 1:12){
  temp_conf <- teams_sf |>
    filter(twelveConfCluster == i)
  if (nrow(temp_conf) >= 15){
    print(paste("conference:", i))
  }
}

### filtering out 12 conferences by conference cluster number
teams_sf_12Conf1 <- teams_sf |>
  filter(twelveConfCluster == 1)
teams_sf_12Conf2 <- teams_sf |>
  filter(twelveConfCluster == 2)
teams_sf_12Conf3 <- teams_sf |>
  filter(twelveConfCluster == 3)
teams_sf_12Conf4 <- teams_sf |>
  filter(twelveConfCluster == 4)
teams_sf_12Conf5 <- teams_sf |>
  filter(twelveConfCluster == 5)
teams_sf_12Conf6 <- teams_sf |>
  filter(twelveConfCluster == 6)
teams_sf_12Conf7 <- teams_sf |>
  filter(twelveConfCluster == 7)
teams_sf_12Conf8 <- teams_sf |>
  filter(twelveConfCluster == 8)
teams_sf_12Conf9 <- teams_sf |>
  filter(twelveConfCluster == 9)
teams_sf_12Conf10 <- teams_sf |>
  filter(twelveConfCluster == 10)
teams_sf_12Conf11 <- teams_sf |>
  filter(twelveConfCluster == 11)
teams_sf_12Conf12 <- teams_sf |>
  filter(twelveConfCluster == 12)

### filtering out 11 conferences based on cluster number
teams_sf_11Conf1 <- teams_sf |>
  filter(elevenConfCluster == 1)
teams_sf_11Conf2 <- teams_sf |>
  filter(elevenConfCluster == 2)
teams_sf_11Conf3 <- teams_sf |>
  filter(elevenConfCluster == 3)
teams_sf_11Conf4 <- teams_sf |>
  filter(elevenConfCluster == 4)
teams_sf_11Conf5 <- teams_sf |>
  filter(elevenConfCluster == 5)
teams_sf_11Conf6 <- teams_sf |>
  filter(elevenConfCluster == 6)
teams_sf_11Conf7 <- teams_sf |>
  filter(elevenConfCluster == 7)
teams_sf_11Conf8 <- teams_sf |>
  filter(elevenConfCluster == 8)
teams_sf_11Conf9 <- teams_sf |>
  filter(elevenConfCluster == 9)
teams_sf_11Conf10 <- teams_sf |>
  filter(elevenConfCluster == 10)
teams_sf_11Conf11 <- teams_sf |>
  filter(elevenConfCluster == 11)


### no hawaii
nk = 12 # number of clusters to test
nkm_noHI <- data.frame(number = numeric(nk), sse = numeric(nk)) # empty df to hold results
set.seed(802)
for(j in 1:nk){ ## loop through each cluster number
  nkm_noHI$number[j] <- j ## cluster number
  nkm_noHI$sse[j] <- kmeans(st_coordinates(teams_sf_nohawaii), centers = j)$tot.withinss ## record error
}
plot(nkm_noHI$number, nkm_noHI$sse, xlab = 'Number of Clusters', ylab = 'SSE', main = 'K-Means Error Evaluation Plot')
lines(nkm_noHI$number, nkm_noHI$sse)

set.seed(802)
kmeans_noHI <- kmeans(st_coordinates(teams_sf_nohawaii), centers = 11)
plot(st_geometry(teams_sf_nohawaii), col=factor(kmeans_noHI$cluster), main = 'Assigned Clusters, Hawaii as only Indy')

### adding cluster number as conference
teams_sf_nohawaii$elevenConfCluster <- kmeans_noHI$cluster

teams_sf_nohawaii$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf_nohawaii)){
  temp_clust2 <- teams_sf_nohawaii |>
    filter(elevenConfCluster == teams_sf_nohawaii$elevenConfCluster[i])
  teams_sf_nohawaii$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf_nohawaii[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}


### looking at conferences separately
teams_sf_noHI_Conf1 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 1)
teams_sf_noHI_Conf2 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 2)
teams_sf_noHI_Conf3 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 3)
teams_sf_noHI_Conf4 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 4)
teams_sf_noHI_Conf5 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 5)
teams_sf_noHI_Conf6 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 6)
teams_sf_noHI_Conf7 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 7)
teams_sf_noHI_Conf8 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 8)
teams_sf_noHI_Conf9 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 9)
teams_sf_noHI_Conf10 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 10)
teams_sf_noHI_Conf11 <- teams_sf_nohawaii |>
  filter(elevenConfCluster == 11)

for (i in 1:12){
  temp_conf <- teams_sf_nohawaii |>
    filter(elevenConfCluster == i)
  if (nrow(temp_conf) >= 15){
    print(paste("conference:", i))
  }
}

### Hawaii and Army/Navy as Indies
# nk = 12 # number of clusters to test
nkm_3Indies <- data.frame(number = numeric(nk), sse = numeric(nk)) # empty df to hold results
set.seed(802)
for(j in 1:nk){ ## loop through each cluster number
  nkm_3Indies$number[j] <- j ## cluster number
  nkm_3Indies$sse[j] <- kmeans(st_coordinates(teams_sf_3Indies), centers = j)$tot.withinss ## record error
}
plot(nkm_3Indies$number, nkm_3Indies$sse, xlab = 'Number of Clusters', ylab = 'SSE', main = 'K-Means Error Evaluation Plot')
lines(nkm_3Indies$number, nkm_3Indies$sse)

set.seed(802)
kmeans_3Indies <- kmeans(st_coordinates(teams_sf_3Indies), centers = 11)
plot(st_geometry(StateBoundaries_NoHI))
plot(st_geometry(teams_sf_3Indies), col=factor(kmeans_3Indies$cluster), main = 'Assigned Clusters, Hawaii and Army/Navy as Indies', add = T)

### adding cluster number as conference
teams_sf_3Indies$elevenConfCluster <- kmeans_3Indies$cluster


teams_sf_3Indies$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf_3Indies)){
  temp_clust2 <- teams_sf_3Indies |>
    filter(elevenConfCluster == teams_sf_3Indies$elevenConfCluster[i])
  teams_sf_3Indies$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf_3Indies[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}


### looking at conferences separately
teams_sf_3Indies_Conf1 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 1)
teams_sf_3Indies_Conf2 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 2)
teams_sf_3Indies_Conf3 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 3)
teams_sf_3Indies_Conf4 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 4)
teams_sf_3Indies_Conf5 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 5)
teams_sf_3Indies_Conf6 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 6)
teams_sf_3Indies_Conf7 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 7)
teams_sf_3Indies_Conf8 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 8)
teams_sf_3Indies_Conf9 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 9)
teams_sf_3Indies_Conf10 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 10)
teams_sf_3Indies_Conf11 <- teams_sf_3Indies |>
  filter(elevenConfCluster == 11)

for (i in 1:12){
  temp_conf <- teams_sf_3Indies |>
    filter(elevenConfCluster == i)
  if (nrow(temp_conf) >= 15){
    print(paste("conference:", i))
  }
}

### Just Army/Navy as Indies
# nk = 12 # number of clusters to test
nkm_2Indies <- data.frame(number = numeric(nk), sse = numeric(nk)) # empty df to hold results
set.seed(802)
for(j in 1:nk){ ## loop through each cluster number
  nkm_2Indies$number[j] <- j ## cluster number
  nkm_2Indies$sse[j] <- kmeans(st_coordinates(teams_sf_2Indies), centers = j)$tot.withinss ## record error
}
plot(nkm_2Indies$number, nkm_2Indies$sse, xlab = 'Number of Clusters', ylab = 'SSE', main = 'K-Means Error Evaluation Plot')
lines(nkm_2Indies$number, nkm_2Indies$sse)

set.seed(802)
kmeans_2Indies <- kmeans(st_coordinates(teams_sf_2Indies), centers = 11)
plot(st_geometry(StateBoundaries))
plot(st_geometry(teams_sf_2Indies), col=factor(kmeans_2Indies$cluster), main = 'Assigned Clusters, Hawaii and Army/Navy as Indies', add = T)

### adding cluster number as conference
teams_sf_2Indies$elevenConfCluster <- kmeans_2Indies$cluster


teams_sf_2Indies$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf_2Indies)){
  temp_clust2 <- teams_sf_2Indies |>
    filter(elevenConfCluster == teams_sf_2Indies$elevenConfCluster[i])
  teams_sf_2Indies$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf_2Indies[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}

### looking at conferences separately
teams_sf_2Indies_Conf1 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 1)
teams_sf_2Indies_Conf2 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 2)
teams_sf_2Indies_Conf3 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 3)
teams_sf_2Indies_Conf4 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 4)
teams_sf_2Indies_Conf5 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 5)
teams_sf_2Indies_Conf6 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 6)
teams_sf_2Indies_Conf7 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 7)
teams_sf_2Indies_Conf8 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 8)
teams_sf_2Indies_Conf9 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 9)
teams_sf_2Indies_Conf10 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 10)
teams_sf_2Indies_Conf11 <- teams_sf_2Indies |>
  filter(elevenConfCluster == 11)

for (i in 1:12){
  temp_conf <- teams_sf_2Indies |>
    filter(elevenConfCluster == i)
  if (nrow(temp_conf) >= 15){
    print(paste("conference:", i))
  }
}




### Army/Navy, Notre Dame, and Hawaii as Indies
# nk = 12 # number of clusters to test
nkm_4Indies <- data.frame(number = numeric(nk), sse = numeric(nk)) # empty df to hold results
set.seed(802)
for(j in 1:nk){ ## loop through each cluster number
  nkm_4Indies$number[j] <- j ## cluster number
  nkm_4Indies$sse[j] <- kmeans(st_coordinates(teams_sf_4Indies), centers = j)$tot.withinss ## record error
}
plot(nkm_4Indies$number, nkm_4Indies$sse, xlab = 'Number of Clusters', ylab = 'SSE', main = 'K-Means Error Evaluation Plot')
lines(nkm_4Indies$number, nkm_4Indies$sse)

set.seed(802)
kmeans_4Indies <- kmeans(st_coordinates(teams_sf_4Indies), centers = 11)
plot(st_geometry(StateBoundaries_NoHI))
plot(st_geometry(teams_sf_4Indies), col=factor(kmeans_4Indies$cluster), main = 'Assigned Clusters, Hawaii and Army/Navy as Indies', add = T)

### adding cluster number as conference
teams_sf_4Indies$elevenConfCluster <- kmeans_4Indies$cluster


teams_sf_4Indies$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf_4Indies)){
  temp_clust2 <- teams_sf_4Indies |>
    filter(elevenConfCluster == teams_sf_4Indies$elevenConfCluster[i])
  teams_sf_4Indies$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf_4Indies[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}


### looking at conferences separately
teams_sf_4Indies_Conf1 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 1)
teams_sf_4Indies_Conf2 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 2)
teams_sf_4Indies_Conf3 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 3)
teams_sf_4Indies_Conf4 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 4)
teams_sf_4Indies_Conf5 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 5)
teams_sf_4Indies_Conf6 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 6)
teams_sf_4Indies_Conf7 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 7)
teams_sf_4Indies_Conf8 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 8)
teams_sf_4Indies_Conf9 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 9)
teams_sf_4Indies_Conf10 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 10)
teams_sf_4Indies_Conf11 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 11)


for (i in 1:11){
  temp_conf <- teams_sf_4Indies |>
    filter(elevenConfCluster == i)
  if (nrow(temp_conf) >= 15){
    print(paste("conference:", i, ", # of teams:", nrow(temp_conf)))
    set.seed(802)
    temp_extra_teams <- sample(temp_conf$school, size = nrow(temp_conf) - 14, replace = FALSE)
    print(temp_extra_teams)
  } else if (nrow(temp_conf) < 6){
    print(paste("conference that needs teams:", i))
  }
  
  ### forming divisions if conference size is >= 12
  if (nrow(temp_conf) >= 12){
    temp_divisions <- kmeans(st_coordinates(temp_conf), centers = 2)
    temp_conf$new_division <- temp_divisions$cluster
    print(paste(unique(temp_conf$elevenConfCluster), "division 1:"))
    print(temp_conf$school[temp_conf$new_division == 1])
    print(paste(unique(temp_conf$elevenConfCluster), "division 2:"))
    print(temp_conf$school[temp_conf$new_division == 2])
  }
}

### moving teams from conferences with too many to conferences with fewer
teams_sf_4Indies$elevenConfCluster[teams_sf_4Indies$school == "Louisville"] = 10
teams_sf_4Indies$elevenConfCluster[teams_sf_4Indies$school == "Purdue"] = 10
teams_sf_4Indies$elevenConfCluster[teams_sf_4Indies$school == "Indiana"] = 10


### rerunning code from above to calculate distances from cluster centroids and create divisions
teams_sf_4Indies$elevenConfCluster_centroid_dist <- -999
for (i in 1:nrow(teams_sf_4Indies)){
  temp_clust2 <- teams_sf_4Indies |>
    filter(elevenConfCluster == teams_sf_4Indies$elevenConfCluster[i])
  teams_sf_4Indies$elevenConfCluster_centroid_dist[i] = distGeo(st_coordinates(teams_sf_4Indies[i,]), st_coordinates(st_centroid(st_as_sf(convHull(vect(temp_clust2))))))
}


### looking at conferences separately
teams_sf_4Indies_Conf1 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 1)
teams_sf_4Indies_Conf2 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 2)
teams_sf_4Indies_Conf3 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 3)
teams_sf_4Indies_Conf4 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 4)
teams_sf_4Indies_Conf5 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 5)
teams_sf_4Indies_Conf6 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 6)
teams_sf_4Indies_Conf7 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 7)
teams_sf_4Indies_Conf8 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 8)
teams_sf_4Indies_Conf9 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 9)
teams_sf_4Indies_Conf10 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 10)
teams_sf_4Indies_Conf11 <- teams_sf_4Indies |>
  filter(elevenConfCluster == 11)


for (i in 1:11){
  temp_conf <- teams_sf_4Indies |>
    filter(elevenConfCluster == i)
  if (nrow(temp_conf) >= 17){
    print(paste("conference:", i, ", # of teams:", nrow(temp_conf)))
    set.seed(802)
    temp_extra_teams <- sample(temp_conf$school, size = nrow(temp_conf) - 16, replace = FALSE)
    print(temp_extra_teams)
  } else if (nrow(temp_conf) < 6){
    print(paste("conference that needs teams: Cluster", i))
  }
  
  ### forming divisions if conference size is >= 12
  if (nrow(temp_conf) >= 12){
    temp_divisions <- kmeans(st_coordinates(temp_conf), centers = 2)
    temp_conf$new_division <- temp_divisions$cluster
    print(paste(unique(temp_conf$elevenConfCluster), "division 1:"))
    print(temp_conf$school[temp_conf$new_division == 1])
    print(paste(unique(temp_conf$elevenConfCluster), "division 2:"))
    print(temp_conf$school[temp_conf$new_division == 2])
  }
}
