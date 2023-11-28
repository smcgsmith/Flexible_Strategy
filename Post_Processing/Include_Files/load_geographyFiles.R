## Set up geography files ##
county.summary <- county.fips
county.summary[which(county.summary$fips==46113),1]<- 46102 # South Dakota county that changed name/number 
county.summary[nrow(county.summary)+1,] <- c(51550, "virginia,chesapeake") # Adding in a county missing from county.fips
county.summary$fips<-as.numeric(county.summary$fips)
county.summary <- separate(county.summary, polyname, into = c("county", "extra"), sep = ":", fill = "right")
county.summary$county[county.summary$county == 'montana,park'] <- "montana,yellowstone national"
county.summary <- county.summary %>%
  select(fips, county) %>%
  dplyr::rename('polyname' = county) %>%
  distinct()

county.summary$polyname=as.factor(county.summary$polyname)