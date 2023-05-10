path_trans <- "/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/flexibleStrategy/Sensitivity/Files_To_Process/"
path_output <- "/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/flexibleStrategy/Sensitivity_Results/"
path0 <- "/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/flexibleStrategy/Sensitivity/"

farmsover<- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Sensitivity/farmsOver1000MinFlaps.txt", header = TRUE)

flaps1 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0001.txt")
flaps2 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0002.txt")
flaps3 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0003.txt")
flaps4 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0004.txt")
flaps5 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0005.txt")
flaps6 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0006.txt")
flaps7 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0007.txt")
flaps8 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0008.txt")
flaps9 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0009.txt")
flaps10 <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/FLAPS/QuarterlyFLAPS/FLAPS12_Quarterly_USDOS_format_0010.txt")


summary.fnames <- list.files(path = path_trans, recursive = TRUE, pattern = "_summary.txt", full.names = FALSE)

###################################################################################################
#### Vaccine Sensitivity

#set up summary files for merge
summary.fname <- summary.fnames[1]
summary.file <- fread(paste0(path_trans, summary.fname), header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Num_Inf", "nAffCounties", "Duration", "RunTimeSec"))
summary.file$Type <- unlist(strsplit(summary.fname, "_2022Nov"))[1]
merger <- merge(summary.file, farmsover, by.x = "Seed_FIPS", by.y = "FIPS", all.x = TRUE)
summary.file$LargeFarms <- merger$noLargeFarms
summary.file$par_set <- paste0(unlist(strsplit(summary.file$Type, "_"))[2:6], collapse = "_")
summary.file$FLAPS <- paste0(unlist(strsplit(summary.file$Type, "_"))[10:14], collapse = "_")

#First
flex.file.name <- paste0(summary.file$par_set[1],"_flex.txt")
flex.file <- fread(paste0(paste0(path0,"/Flex_Files/"),flex.file.name), header = FALSE)
summary.file$trigger <- flex.file[2,]$V1
summary.file$threshold <- flex.file[2,]$V2
summary.file$action <- flex.file[2,]$V3
summary.file$target <- flex.file[2,]$V4
summary.file$priority <- flex.file[2,]$V5

#Second
summary.file$trigger2 <- flex.file[3,]$V1
summary.file$threshold2 <- flex.file[3,]$V2
summary.file$action2 <- flex.file[3,]$V3
summary.file$target2 <- flex.file[3,]$V4
summary.file$priority2 <- flex.file[3,]$V5

#MERGE SUMMARY AND FLAPS FILES--CHECK THAT THESE ARE THE CORRECT VARIABLE NAMES
summary.file <- if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0001"){merge(summary.file, flaps1, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
  if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0002"){merge(summary.file, flaps2, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
    if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0003"){merge(summary.file, flaps3, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
      if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0004"){merge(summary.file, flaps4, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
        if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0005"){merge(summary.file, flaps5, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
          if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0006"){merge(summary.file, flaps6, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
            if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0007"){merge(summary.file, flaps7, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
              if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0008"){merge(summary.file, flaps8, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
                if(summary.file$FLAPS == "FLAPS12_Quarterly_USDOS_format_0009"){merge(summary.file, flaps9, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
                {merge(summary.file, flaps10, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))} 

#check b_Q1 and d_Q1 match what's in the file
summary.file$b_Q3 <- as.numeric(as.character(summary.file$b_Q3))
summary.file$d_Q3 <- as.numeric(as.character(summary.file$d_Q3))
summary.file$Farm.Size <- summary.file$b_Q3 + summary.file$d_Q3

#THIS SECTION TAKES SEVERAL MINUTES TO RUN
#WARNING MESSAGES EXPECTED
#merge above file with flaps files
for(i in 2:length(summary.fnames)){
  summary.fname <- summary.fnames[i]
  summary.res <- fread(paste0(path_trans, summary.fname), header = TRUE, select = c("Rep", "Seed_Farms", "Seed_FIPS", "Num_Inf", "nAffCounties", "Duration", "RunTimeSec"))
  if(dim(summary.res) == 0){
    print("Skipped")
    next
  }
  summary.res$Type <- unlist(strsplit(summary.fname, "_2020Nov"))[1]
  merger <- merge(summary.res, farmsover, by.x = "Seed_FIPS", by.y = "FIPS", all.x = TRUE)
  summary.res$LargeFarms <- merger$noLargeFarms
  summary.res$par_set <- paste0(unlist(strsplit(summary.res$Type, "_"))[2:6], collapse = "_")
  summary.res$FLAPS <- paste0(unlist(strsplit(summary.res$Type, "_"))[10:14], collapse = "_")
  
  #First step
  flex.file.name <- paste0(summary.res$par_set[1],"_flex.txt")
  flex.file <- fread(paste0(paste0(path0,"/Flex_Files/"),flex.file.name), header = FALSE)
  summary.res$trigger <- flex.file[2,]$V1
  summary.res$threshold <- flex.file[2,]$V2
  summary.res$action <- flex.file[2,]$V3
  summary.res$target <- flex.file[2,]$V4
  summary.res$priority <- flex.file[2,]$V5
  
  #Second step
  summary.res$trigger2 <- flex.file[3,]$V1
  summary.res$threshold2 <- flex.file[3,]$V2
  summary.res$action2 <- flex.file[3,]$V3
  summary.res$target2 <- flex.file[3,]$V4
  summary.res$priority2 <- flex.file[3,]$V5
  
  #MERGE SUMMARY AND FLAPS FILES--CHECK THAT THESE ARE THE CORRECT VARIABLE NAMES
  summary.res <- if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0001"){merge(summary.res, flaps1, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
    if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0002"){merge(summary.res, flaps2, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
      if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0003"){merge(summary.res, flaps3, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
        if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0004"){merge(summary.res, flaps4, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
          if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0005"){merge(summary.res, flaps5, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
            if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0006"){merge(summary.res, flaps6, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
              if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0007"){merge(summary.res, flaps7, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
                if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0008"){merge(summary.res, flaps8, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
                  if(summary.res$FLAPS == "FLAPS12_Quarterly_USDOS_format_0009"){merge(summary.res, flaps9, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))}else 
                  {merge(summary.res, flaps10, by.x = c("Seed_Farms", "Seed_FIPS"), by.y = c("Id", "County_fips"))} 
  
  #check b_Q1 and d_Q1 match what's in the file
  summary.res$b_Q3 <- as.numeric(as.character(summary.res$b_Q3))
  summary.res$d_Q3 <- as.numeric(as.character(summary.res$d_Q3))
  summary.res$Farm.Size <- summary.res$b_Q3 + summary.res$d_Q3
  
  summary.file <- rbind(summary.file, summary.res)
  print(i)
}

setwd(path0)
write.csv(summary.file, paste0(paste0("SensitivityData_",Sys.Date()),".csv"), col.names = TRUE, row.names = FALSE)

############################################################################################################
##
## Density, clustering, and shipment data for the counties
##
############################################################################################################

#Farm density
farm.density = data.frame(table(flaps1$County_fips))
colnames(farm.density) = c("FIPS", "Farms")

#read in the county information and merge:
county.info = read.csv("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Sensitivity/Data/county_data.csv")
farm.info = merge(farm.density, county.info[,c("FIPS", "AREA_M2")], by="FIPS")
farm.info$AREA_KM2 = farm.info$AREA_M2 / (1000^2)
farm.info$Density = farm.info$Farms / farm.info$AREA_KM2

#Movements (2009 CVI data)
#Hi, low, and medium number of shipments in
#Hi, low, and medium number of shipments out

cvi = read.csv("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Sensitivity/Data/CVIData_Complete_10percent_vr3.csv")
Out_summary = data.frame(table(cvi$O_FIPS))
colnames(Out_summary) = c("FIPS", "Out_shipments")
Out_summary = merge(county.info[,c("FIPS", "STATE_NAME")], Out_summary, by="FIPS", all=TRUE)
Out_summary$Out_shipments[is.na(Out_summary$Out_shipments)] = 0
Out_summary = Out_summary[,!colnames(Out_summary) %in% "STATE_NAME"]
head(Out_summary)

In_summary = data.frame(table(cvi$D_FIPS))
colnames(In_summary) = c("FIPS", "In_shipments")
In_summary = merge(county.info[,c("FIPS", "STATE_NAME")], In_summary, by="FIPS", all=TRUE)
In_summary$In_shipments[is.na(In_summary$In_shipments)] = 0
In_summary = In_summary[,!colnames(In_summary) %in% "STATE_NAME"]
head(In_summary)

# Merge shipment data and ensure there are no NAs (Brandon I don't think you're using shipment data)
County.cvi = merge(Out_summary, In_summary, by="FIPS", all= TRUE)
County.cvi = County.cvi[!is.na(County.cvi$Out_shipments),]
head(County.cvi)
County.cvi[is.na(County.cvi$Out_shipments),]

#Clustering Data:
clustering = read.csv("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Sensitivity/Data/clustering_500m_update.csv")
colnames(clustering) = c("FIPS", "clustering.prop", "run")
clustering = clustering[,c("FIPS", "clustering.prop")]
head(clustering)

#Merge all of the data:
county.data = merge(farm.info[,c("FIPS", "Density")], County.cvi, by="FIPS")
county.data = merge(county.data, clustering, by="FIPS", all.x=TRUE)
head(county.data)

##################################################################################
##
## Add in demography
##
##################################################################################

#usdos <- fread("Sensitivity_Data.csv", header = TRUE)
usdos <- summary.file

str(usdos)
str(county.data)
county.data$FIPS <- as.numeric(as.character(county.data$FIPS))

#CHECK VARIABLE NAMES ARE CORRECT
usdos = merge(usdos, county.data, by.x="Seed_FIPS", by.y="FIPS", all.x=TRUE)
colnames(usdos)[colnames(usdos) == "Out_shipments"] = "out.shipments"
colnames(usdos)[colnames(usdos) == "In_shipments"] = "in.shipments"
colnames(usdos)[colnames(usdos) == "clustering.prop"] = "clustering"
colnames(usdos)[colnames(usdos) == "Density"] = "density"
colnames(usdos)[colnames(usdos) == "Farm.Size"] = "premises.size"
colnames(usdos)[colnames(usdos) == "LargeFarms"] = "num.large.prems"

#Scale all of the coefficients:
#change parameter names as needed
usdos_scaled <-as.data.frame(usdos %>% mutate_at(vars("premises.size", "density", "out.shipments", "in.shipments", "clustering", "num.large.prems"),
                                                 funs(scale(as.vector(.)))))

usdos_scaled$premises.size <- as.numeric(usdos_scaled$premises.size)
usdos_scaled$num.large.prems <- as.numeric(usdos_scaled$num.large.prems)
usdos_scaled$density <- as.numeric(usdos_scaled$density)
usdos_scaled$out.shipments <- as.numeric(usdos_scaled$out.shipments)
usdos_scaled$in.shipments <- as.numeric(usdos_scaled$in.shipments)
usdos_scaled$clustering <- as.numeric(usdos_scaled$clustering)
usdos_scaled$threshold <- as.numeric(usdos_scaled$threshold)
usdos_scaled$threshold2 <- as.numeric(usdos_scaled$threshold2)
usdos_scaled$target <- as.factor(usdos_scaled$target)
usdos_scaled$target2 <- as.factor(usdos_scaled$target2)

setwd(path0)
write.csv(usdos_scaled, paste0(paste0("SensitivityData_Scaled_",Sys.Date()),".csv"), col.names = TRUE) 
