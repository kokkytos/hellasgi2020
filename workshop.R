library(raster) 
library(rasterVis)
library(sf)
library(stringr)
library(furrr)
library(tibble)
library(dplyr)
library(ggplot2)
library(zoo)


####====== Some settings ==================== ####
raster::rasterOptions(chunksize=2e+09,
			maxmemory=8e+09,
			progress="text")
####========================================= ####


dnb_files <- list.files('geotiffs', pattern = "^SVDNB_npp_*.*tif$", full.names = T)

# extract dates
date_str <- str_extract(dnb_files, "[0-9]{8}")
dates <- as.Date(date_str, "%Y%m%d")


# stack images
s <- raster::stack(dnb_files) %>%
  setNames(dates) %>%
  setZ(dates)


s <-subset(s, which(getZ(s)>="2018-01-01"))


# read kallikratis file
kallikratis <- sf::read_sf("vector/kallikratis.gpkg") %>% 
  st_transform(4326)


#filter some polygons
kallikratis <- kallikratis %>% dplyr::filter(substr(KALCODE, 1, 2) %in% c("37", "39","40","41","42","43","44"))
#todo again show a filtered plot of kallikratis 

# extract extent of kallikratis
ext <- raster::extent(kallikratis)

# crop stack to raster
s_crop <- crop(s, ext)

#todo remove some noise HERE
s_crop[s_crop<=5] <- NA # δεν είναι memory safe
s_crop <- raster::calc(s_crop, fun=function(x){x[x<=5]<-NA; return(x)}) 
s_crop <- raster::reclassify(s_crop, c(-Inf,5,NA))

s_crop <- s_crop %>% setNames(names(s)) %>% setZ(getZ(s))


# preview some raster
levelplot(s_crop[[1:2]])




#todo show a plot of kallikratis 


### Calculate monthly SoL for each year
### todo να διαβάσω λίγο για purrr map
future::plan(multisession, workers = availableCores() ) #the maximum number of parallel processes running will be decided by availableCores()

df <-
  furrr:::future_map_dfr(seq_along(1:raster::nlayers(s_crop)), ~ {
    return(
      data.frame(
        SoL = exactextractr::exact_extract(s_crop[[.x]], kallikratis, "sum"),
        #layer = names(s_crop[[.x]]),
        date = getZ(s_crop[[.x]]),
        year = lubridate::year(getZ(s_crop[[.x]])),
        month = lubridate::month(getZ(s_crop[[.x]]),abbr=TRUE,label=TRUE),
        KALCODE = kallikratis$KALCODE,
        name = kallikratis$LEKTIKO,
        stringsAsFactors = TRUE
      )
    )
  }, .progress = TRUE) %>% as_tibble()


# Stop clusters
future:::ClusterRegistry("stop")



# Find month with Max SoL per year, View the result
df %>% dplyr::group_by(year, name) %>% 
  filter(SoL == max(SoL)) %>%
  arrange(name, year)

View(df)


# USE group=1
# https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
# επίσης κάνω filter μονο για την ΠΕ Αχαϊας
g <- ggplot(data = df %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39)), aes(x=month, y=SoL,group = 1)) + 
  geom_line() + 
  geom_point(size=0.5) +
  facet_grid(year~name)+
  xlab("Μήνας")+
  ylab("SoL (nanoWatt)")+ #todo add units
  ggtitle("Sum of Lights ανά δήμο και έτος")+
  theme(axis.text.x = element_text(angle=90, vjust=.5))
(g)


#mapping SoL

df2 <- kallikratis %>% dplyr::left_join(df, by='KALCODE')

ggplot(df2 %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39))) +  
  geom_sf(aes(fill = SoL)) + 
  facet_grid(year~month)+
  theme(axis.text.x = element_text(angle=90, vjust=.5))+
  ggtitle("Sum of Lights ανά δήμο και έτος")


# zapply ανά τρίμηνο
raster::getZ(s_crop)
quarter_stack <- raster::zApply(s_crop, by=zoo::as.yearqtr, fun=mean, name="Quarter")

levelplot(quarter_stack)

future::plan(multisession, workers = availableCores())
df_quarter <-
  furrr:::future_map_dfr(seq_along(1:raster::nlayers(quarter_stack)), ~ {
    return(
      data.frame(
        SoL = exactextractr::exact_extract(quarter_stack[[.x]], kallikratis, "sum"),
        #layer = names(s_crop[[.x]]),
        date = as.Date(getZ(quarter_stack[[.x]])),
        year = lubridate::year(as.Date(getZ(quarter_stack[[.x]]))),
        month = lubridate::month(as.Date(getZ(quarter_stack[[.x]])),abbr=TRUE,label=TRUE),
        KALCODE = kallikratis$KALCODE,
        name = kallikratis$LEKTIKO,
        stringsAsFactors = TRUE
      )
    )
  }, .progress = TRUE) %>% as_tibble()

# Stop clusters
future:::ClusterRegistry("stop")

g <- ggplot(data = df_quarter %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39)), aes(x=as.yearqtr(date), y=SoL,group = 1)) + 
  geom_line() + 
  geom_point(size=0.5) +
  xlab("Τρίμηνο")+
  ylab("SoL (nanoWatt)")+ #todo add units
  ggtitle("Sum of Lights ανά δήμο και τρίμηνο")+
  theme(axis.text.x = element_text(angle=90, vjust=.5))+
  scale_x_yearqtr(breaks = seq(from = min(as.yearqtr(df_quarter$date)), to =max(as.yearqtr(df_quarter$date)), by = 0.25),
                  format = "%YQ%q")

(g1 <-g+  facet_grid(.~name))

(g2 <-g+  facet_grid(name ~ .))

(g2 <-g+  facet_grid(name ~ ., scales="free"))

