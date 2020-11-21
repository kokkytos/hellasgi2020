library(raster) 
library(rasterVis)
library(sf)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)


# ορισμός rasterOptions -----------------------------------------------------------

raster::rasterOptions(chunksize=2e+09,
			maxmemory=8e+09,
			progress="text")


# λίστα αρχείων tiff στο geotiffs directory -----------------------------------

(dnb_files <- list.files('geotiffs', pattern = "^SVDNB_npp_*.*tif$", full.names = T))


# εξαγωγή ημερομηνιών από τα filenames --------------------------------------------

(date_str <- str_extract(dnb_files, "[0-9]{8}"))
(dates <- as.Date(date_str, "%Y%m%d"))


#  δημιουργία raster stack από τα geotiffs  -----------------------------------------------------------

dnb_stack <- raster::stack(dnb_files) %>%
  setNames(dates) %>%
  setZ(dates)


# φιλτράρισμα της χρονοσειράς. Κρατάμε μόνο 2018 & 2019 ---------------------------------------------------

dnb_stack <-subset(dnb_stack, which(getZ(dnb_stack)>="2018-01-01"))



# ανάγνωση αρχείου διανυσματικών δεδομένων των δήμων κατά Καλλικράτη ---------------------------------

kallikratis_full <- sf::read_sf("vector/kallikratis.gpkg") %>% 
  st_transform(4326)


# Φιλτράρισμα δεδομένων. Κρατάμε μόνο τα πολύγωνα με τους κωδικούς της Πελοπονήσου. Φίλτρο από τα πρώτα δύο ψηφία του KALCODE

kallikratis <- kallikratis_full %>% dplyr::filter(substr(KALCODE, 1, 2) %in% c("37", "39","40","41","42","43","44"))

# εξαγωγή του extent του object των δήμων που φιλτράραμε ---------------------------------

ext <- raster::extent(kallikratis)


# Σύντομη οπτικοποίηση σε χάρτη ---------------------------------

ggplot() +
  ggtitle("Δήμοι Καλλικράτη") +
  geom_sf(data = kallikratis_full, aes(fill = "Ελλάδας"), colour="gray25") +
  geom_sf(data = kallikratis,
          aes(fill = "Πελοποννήσου"), colour="black") +
  geom_sf(
    data = ext %>% sf::st_bbox() %>% sf::st_as_sfc() %>% st_set_crs("EPSG:4326"),
    colour = "blue",
    fill = NA)+
  scale_fill_manual("Δήμοι",values = c("Ελλάδας" = "gray", "Πελοποννήσου"="red"))+
  theme_minimal()


# αποκοπή του raster με βάση το extent object των δήμων ---------------------------------

dnb_stack_crop <- raster::crop(dnb_stack, ext)


# αφαίρεση θορύβου. τρεις επιλογές ---------------------------------

dnb_stack_crop[dnb_stack_crop<=5] <- NA # δεν είναι memory safe, χάνονται τα τα properties names και Z

dnb_stack_crop <- raster::calc(dnb_stack_crop, fun=function(x){x[x<=5]<-NA; return(x)}) # χάνονται τα τα properties names και Z

dnb_stack_crop <- raster::reclassify(dnb_stack_crop, c(-Inf,5,NA)) # δεν διατηρούνται οι τιμές Z


# αν δεν διατηρούνται τα properties names και Z κατά το crop. Ξανά θέτουμε τιμές

dnb_stack_crop <- dnb_stack_crop %>% setNames(names(dnb_stack)) %>% setZ(getZ(dnb_stack))


# preview some rasters  ---------------------------------
# 
levelplot(dnb_stack_crop[[1:2]])


### Υπολογισμός μηνιαίου SoL (Sum of Lights) για όλα τα έτη  και τους δήμους---------------------------------

tbl <-
  purrr::map_dfr(seq_along(1:raster::nlayers(dnb_stack_crop)), ~ {
    df <- data.frame(
      SoL = exactextractr::exact_extract(dnb_stack_crop[[.x]], kallikratis, "sum"),
      date = getZ(dnb_stack_crop[[.x]]),
      year = lubridate::year(getZ(dnb_stack_crop[[.x]])),
      month = lubridate::month(getZ(dnb_stack_crop[[.x]]), abbr = TRUE, label =
                                 TRUE),
      KALCODE = kallikratis$KALCODE,
      name = kallikratis$LEKTIKO,
      stringsAsFactors = TRUE
    )
    
    return(df)
    
  }) %>% as_tibble()





# Βρες τον μήνα με τον μέγιστο SoL ανά δήμο και για κάθε έτος, Προβολή του αποτελέσματος ---------------------------------

tbl_max  <- tbl %>% dplyr::group_by(year, name) %>% 
  filter(SoL == max(SoL)) %>%
  arrange(name, year)

View(tbl_max)


# οπτικοποίηση δεδομένων. Φίλτράρισμα μόνο για  ΠΕ Ηλείας -----------------
# 
g <- ggplot(data = tbl %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39)), aes(x=month, y=SoL,group=1)) + 
  geom_line() + 
  geom_point(size=0.5) +
  facet_grid(year~name)+
  xlab("Μήνας")+
  ylab("SoL (nanoWatts/cm2/sr)")+ 
  ggtitle("Sum of Lights ανά δήμο και έτος")+
  theme(axis.text.x = element_text(angle=90, vjust=.5))

g



# Χαρτογραφική απόδοση του SoL --------------------------------------------
# 
tbl2 <- kallikratis %>% dplyr::left_join(tbl, by='KALCODE')

ggplot(tbl2 %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39))) +  
  geom_sf(aes(fill = SoL)) + 
  facet_grid(year~month)+
  theme(axis.text.x = element_text(angle=90, vjust=.5))+
  ggtitle("Sum of Lights ανά δήμο και έτος")



# Μέσο SoL ανά τρίμηνο για το raster stack με zapply ----------------------
# 
raster::getZ(dnb_stack_crop)
quarter_stack <- raster::zApply(dnb_stack_crop, by=zoo::as.yearqtr, fun=mean, name="Quarter")

levelplot(quarter_stack)

# ανά τρίμηνο και δήμο

tbl_quarter <-
  purrr::map_dfr(seq_along(1:raster::nlayers(quarter_stack)), ~ {
    
    df <- data.frame(
      SoL = exactextractr::exact_extract(quarter_stack[[.x]], kallikratis, "sum"),
      date = as.Date(getZ(quarter_stack[[.x]])),
      KALCODE = kallikratis$KALCODE,
      name = kallikratis$LEKTIKO,
      quarter =  as.numeric(getZ(quarter_stack[[.x]]))
    )
    
    return(df)
    
  }) %>% as_tibble() %>% mutate(quarter = as.yearqtr(quarter)) 



g <- ggplot(data = tbl_quarter %>% dplyr::filter(substr(KALCODE,1,2) %in% c(39)), aes(x=quarter, y=SoL,group = 1)) + 
  geom_line() + 
  geom_point(size=0.5) +
  xlab("Τρίμηνο")+
  ylab("SoL (nanoWatts/cm2/sr)")+ 
  ggtitle("Sum of Lights ανά δήμο και τρίμηνο")+
  theme(axis.text.x = element_text(angle=90, vjust=.5))+
  scale_x_yearqtr(breaks = seq(from = min(as.yearqtr(tbl_quarter$date)), to =max(as.yearqtr(tbl_quarter$date)), by = 0.25),
                  format = "%YQ%q")

(g1 <-g+  facet_grid(.~name))

(g2 <-g+  facet_grid(name ~ .))

(g2 <-g+  facet_grid(name ~ ., scales="free"))


# Αποθήκευση ως αρχείο PNG
# 
ggsave(
    "sol_q_g1.png",
    plot = g1,
    device = "png",
    path = "plots",
    width = 40,
    height = 20,
    units = c("cm"),
    dpi = 72,
)
