  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  source("postProcessing_USDOSv2.1.R")
  
  processUSDOS(export.datafiles = 3,
               summaryTable = F,
               duration = T,
               premInf = T,
               premReport = T,
               epidemicExtent = T,
               movementBan = F,
               premisesCulled = F,
               premisesVax = F,
               diagnosticTests = F,
               animalsInfected = F,
               countyRisk = F,
               localSpread = F,
               plot_color = "color_blue",
               map_color = "color_red",
               ls_match = TRUE,
               controlValue = F,
               animalsControlled = F,
               maps = TRUE,
               dataExist = FALSE,
               verbose = 1,
               flexibleControl = TRUE)
  
  path0 <- dirname(rstudioapi::getActiveDocumentContext()$path)
  path_output <- file.path(path0, "Output_Files/")
  dependencies <- file.path(path0,"Dependencies/")
  data_output <- file.path(path0, "Data/")
  # source(paste0(dependencies,"loadDependencies_postProcessing.R"))
  # source(paste0(dependencies,"map_by_fips_standalone.R"))
  
  # import data if you don't want to use all of post-processing
  # source(paste0(dependencies,"import_USDOS_Runs.R"))
  # use postprocessing
  
  setwd(path0)
  Dur.long <- read.csv("Data/Duration_long.csv")
  EpidExt.long <- read.csv("Data/EpidemicExtent_long.csv")
  PremInf.long <- read.csv("Data/PremisesInfected_long.csv")
  
  Dur.long <- Dur.long %>% mutate(cType = str_extract(type, "^[^_]+"))
  Dur <- Dur.long %>%
    mutate(trigger = str_extract(type, "^[^_]+"),
           metric = "Duration (days)",
          delay = case_when(str_detect(type, "static_newPremReportsOverX") ~ "0",
                            str_detect(type, "flex_1Day") ~ "1",
                            str_detect(type, "flex_2Days") ~ "2",
                            str_detect(type, "flex_percentIncrease") ~ "3",
                            str_detect(type, "noControl_noDiagnostics") ~ "No Control"),
          cType = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                            str_detect(cType, "flex") ~ "State-dependent control",
                            str_detect(cType, "static") ~ "Static control"),
          type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
                           str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
                           str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
                           str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax"))

  
  Dur.long$metric <- "Duration (days)"
  EpidExt.long$metric <-  "Number of infected counties"
  PremInf.long$metric <-  "Number of infected premises"
  
  list <- list(Dur.long, 
               EpidExt.long, 
               PremInf.long)
  
  total.all <- do.call("rbind", list)

# Create a new column "Delay" using case_when function from dplyr
# Remove "1Day_" or "2Days_" prefix from the "type" column using separate function from tidyr
total.all <- total.all %>% mutate(type = str_replace(type, ".*?_", "")) %>%
  mutate(delay = case_when(
    str_detect(type, "^1Day") ~ "1",
    str_detect(type, "^2Days") ~ "2",
    TRUE ~ "0"
  )) %>%
  mutate(cType = case_when(
    str_detect(type, "noControl_noDiagnostics") ~ "No control",
    str_detect(cType, "flex") ~ "State-dependent control",
    str_detect(cType, "static") ~ "Static control")) %>%
  mutate(type = str_replace(type, "^(1|2)(Day|Days)_", "")) %>%
  mutate(trigger = str_extract(type, "^[^_]+")) %>%
  mutate(type = str_replace(type, "^[^_]+_[^_]+_[^_]+_[^_]+_", "")) %>%
  filter(!(type == "cull_cull_3000_10000_earliest_earliest" | type == "cull_cull_0_10000_earliest_earliest"))
  
  setwd(data_output)
  write.csv(total.all, file = "Total.long.csv")

jpeg("OutbreakMetrics_High_Panel.jpeg", width = 1800, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value > PremInf_cutoff,
                   metric == "Number of infected counties" ~ Value > EpidExt_cutoff,
                   metric == "Duration (days)" ~ Value > Dur_cutoff)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                   type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                   type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                   type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax")) %>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = "Control Strategy", fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(metric~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]),
                    labels = c("State-dependent", "Static") )+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]),
                     labels = c("State-dependent", "Static"))+
  theme_bw()+
  theme(legend.position = "none",
    strip.text.x = element_text(size = 20, face = "bold"), 
    strip.text.y = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size =20),
    axis.title.x.bottom = element_text(size = 24, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    strip.background.y = element_blank(),
    strip.placement = "outside"
  )
dev.off()

jpeg("OutbreakMetrics_High_Legend_Panel.jpeg", width = 2000, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value > PremInf_cutoff,
                   metric == "Number of infected counties" ~ Value > EpidExt_cutoff,
                   metric == "Duration (days)" ~ Value > Dur_cutoff)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = "Control Strategy", fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(metric~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]),
                    labels = c("State-dependent", "Static") )+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]),
                     labels = c("State-dependent", "Static"))+
  theme_bw()+
  theme(
    strip.text.x = element_text(size = 20, face = "bold"), 
    strip.text.y = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size =20),
    axis.title.x.bottom = element_text(size = 24, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    strip.background.y = element_blank(),
    strip.placement = "outside"
    )
dev.off()

jpeg("OutbreakMetrics_Low_Legend_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value <= PremInf_cutoff,
                   metric == "Number of infected counties" ~ Value <= EpidExt_cutoff,
                   metric == "Duration (days)" ~ Value <= Dur_cutoff)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = "Control Strategy", fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(metric~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]),
                    labels = c("State-dependent", "Static") )+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]),
                     labels = c("State-dependent", "Static"))+
  theme_bw()+
  theme(
    strip.text.x = element_text(size = 20, face = "bold"), 
    strip.text.y = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size =20),
    axis.title.x.bottom = element_text(size = 24, face = "bold"),
    axis.title.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    strip.background.y = element_blank(),
    strip.placement = "outside"
  )
dev.off()

jpeg("OutbreakMetrics_Low_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value <= PremInf_cutoff,
                   metric == "Number of infected counties" ~ Value <= EpidExt_cutoff,
                   metric == "Duration (days)" ~ Value <= Dur_cutoff)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  facet_grid(metric~type, switch = "y", scales = "free_y",
             labeller = labeller(metric = label_wrap_gen())) +
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 17, face = "bold"), 
        strip.text.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size =18),
        axis.text.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside")
dev.off()

jpeg("FRSES_Low_Legend_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value <= PremInf_cutoff,
                   metric == "Number of infected counties" ~ Value <= EpidExt_cutoff,
                   metric == "Duration (days)" ~ Value <= Dur_cutoff)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  facet_grid(metric~type, switch = "y", scales = "free_y",
             labeller = labeller(metric = label_wrap_gen())) +
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 17, face = "bold"), 
        strip.text.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size =18),
        axis.text.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        strip.background.y = element_blank(),
        strip.placement = "outside")
dev.off()
  
jpeg("OutbreakMetrics_overMin_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value > PremInf_min,
                   metric == "Number of infected counties" ~ Value > EpidExt_min,
                   metric == "Duration (days)" ~ Value > Dur_min)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  facet_grid(metric~type, scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 17, face = "bold"), 
        strip.text.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size =18),
        axis.text.x.bottom = element_blank(),
        axis.title.y.left = element_blank())
dev.off()

jpeg("OutbreakMetrics_overMin_Legend_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
total.all %>%
  filter(case_when(metric == "Number of infected premises" ~ Value > PremInf_min,
                   metric == "Number of infected counties" ~ Value > EpidExt_min,
                   metric == "Duration (days)" ~ Value > Dur_min)) %>%
  filter(!(type == "noControl_noDiagnostics"| trigger == "availability" | trigger == "decrease")) %>%
  mutate(type = case_when(type == "cull_vax_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Vax",
                          type == "cull_cull_0_-1_earliest_earliest" ~ "MB, IP Cull, DC Cull",
                          type == "cull_vax_0_3000_earliest_earliest" ~ "MB, IP Cull, 3km Vax",
                          type == "cull_vax_3000_10000_earliest_earliest" ~ "MB, IP Cull, 3km Cull, 10km Vax"))%>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  facet_grid(metric~type, scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 17, face = "bold"), 
        strip.text.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size =18),
        axis.text.x.bottom = element_blank(),
        axis.title.y.left = element_blank())
dev.off()

#===============================================================================
#
#Cost analysis
#
#===============================================================================
setwd(path0)
Anim.long <- read.csv("Output_Files/Cull_Vax_030723.csv")
Dur.long <- read.csv("Output_Files/Duration_long.csv")
library(tidyr)
library(dplyr)

gc()

Dur.long <- data.table(Dur.long) %>%
  mutate(controlStatus = "Duration (days)",
         delay = case_when(str_detect(type, "static_newPremReportsOverX") ~ "0",
                           str_detect(type, "flex_1Day") ~ "1",
                           str_detect(type, "flex_2Days") ~ "2",
                           str_detect(type, "flex_percentIncrease") ~ "3",
                           str_detect(type, "noControl_noDiagnostics") ~ "No Control"),
         cType = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(type, "flex") ~ "State-dependent control",
                           str_detect(type, "static") ~ "Static control"),
         trigger = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                          str_detect(type,"percentIncrease") ~ "percentIncrease",
                          str_detect(type,"newPremReportsOverX") ~ "newPremReportsOverX",
                          str_detect(type,"availability") ~ "availability",
                          str_detect(type,"decrease") ~ "decrease"),
         type = case_when(cType == "State-dependent control" ~ str_extract(type, "\\d+_\\d+.*"),
                          cType == "Static control" ~ str_extract(type, "\\d+_\\d+.*"),
                          str_detect(trigger,"No control") ~ "No control")) %>%
  mutate(Value = as.numeric(Value),
         delay = as.factor(delay)) %>%
  select(-fips & -polyname)
# %>%
#   pivot_wider(names_from = controlStatus, values_from = Value, values_fn = NULL) 
# %>%
#   mutate_if(is.numeric, ~replace(., is.na(.), 0))
         # type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
         #                  str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
         #                  str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
         #                  str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
         #                  str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax"))

gc()


Anim.long <- data.table(Anim.long) %>% 
  mutate(Value = as.numeric(anim),
         delay = as.factor(delay)) %>%
  select(-id & -Rep &  -anim) %>%
  bind_rows(Dur.long)

gc()

#===============================================================================
#
# Checking calculations
#
#===============================================================================

Dur.long %>%
  filter(Value > Dur_cutoff) %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest")) %>%
  filter(trigger == "percentIncrease" | trigger == "newPremReportsOverX") %>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = "Control Strategy", fill = "Control Strategy", color = "Control Strategy")+
  theme_bw()

Anim.long %>%
  filter(Value > 15000000 & controlStatus == "effective.cull") %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest")) %>%
  filter(trigger == "percentIncrease" | trigger == "newPremReportsOverX") %>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = "Control Strategy", fill = "Control Strategy", color = "Control Strategy")+
  theme_bw()

summaryTable <- Anim.long %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest")  | str_detect(type, "No control")) %>%
  filter(trigger == "percentIncrease" | trigger == "newPremReportsOverX" | trigger == "No control") %>%
  group_by(controlStatus,cType,type,delay)%>%
  summarise(
    mean = mean(Value),
    median = median(Value),
    min = min(Value),
    max = max(Value)
  )


# Pivot Anim.long to wide data.frame to calculate a cost column
wideAnim <- Anim.long %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = controlStatus, values_from = Value, values_fn = NULL) %>%
  mutate_if(is.numeric, ~replace(.,is.na(.),0))


# wideAnim <- Dur.long %>%
#   mutate(delay = as.numeric(delay))%>%
#   left_join(Anim.long %>%
#               filter(delay != "No control") %>%
#               mutate(delay = as.numeric(delay)), 
#             by = c("fips", "polyname", "type", "delay", "cType","trigger")) %>%
#   mutate_if(is.numeric, ~replace(., is.na(.), 0))

wideAnim$cost <- 1090*wideAnim$effective.cull+22*wideAnim$effective.vax + 2160000*wideAnim$`Duration (days)`
wideAnim$cost <- 563000000*wideAnim$effective.cull+247000000*wideAnim$effective.vax + 2160000*wideAnim$`Duration (days)`
#wideAnim$cost_noDuration <- 1090*wideAnim$effective.cull+22*wideAnim$effective.vax

cost_cutoff = quantile(wideAnim$cost, 0.975)
cost_cutoff = 15000000000
cost_min = min(wideAnim$cost)

wideAnim <- wideAnim %>% 
  mutate(level = case_when(cost > cost_cutoff ~ "High",
                           cost <= cost_cutoff ~ "Low",
                           cost > cost_min ~ "OverMin"),
         level = case_when(cost > cost_cutoff ~ "Top 2.5% of outbreaks",
                           cost <= cost_cutoff ~ "97.5% of outbreaks")) %>% 
  filter(trigger != "availability" & trigger != "decrease" & trigger != "No control") %>% #& !is.na(type) & trigger != "No control"
  filter(`Duration (days)` < 364)
  
  

setwd(path_output)
jpeg("Cost_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
wideAnim %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest") | str_detect(type, "No control")) %>%
  ggplot(aes(x =cType, y = cost, col = cType, fill = cType)) + 
  geom_violin()+
  labs(x = "Control Strategy", y = "Cost (USD)")+
  facet_grid(~level, scales = "free", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 22, face = "bold"), 
        strip.text.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 24),
        axis.title.y.left = element_text(size = 24),
        axis.title.x.bottom = element_text(size = 24))
dev.off()

jpeg("Cost_Upper.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
wideAnim %>% 
  mutate(level = case_when(cost > cost_cutoff ~ "Top 2.5% of outbreaks",
                           cost <= cost_cutoff ~ "97.5% of outbreaks")) %>%
  filter(!is.na(type) & trigger != "availability" & trigger != "decrease" & cost > cost_cutoff) %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest") | str_detect(type, "No control")) %>%
  ggplot(aes(x = cType, y = cost, col = cType, fill = cType)) + 
  geom_violin()+
  labs(x = "Control Strategy", y = "Cost (USD)", colour = "Control Strategy", fill = "Control Strategy")+
  scale_fill_manual(values = c("yellow3",cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c("black","black","black"))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 22, face = "bold"), 
        strip.text.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 24),
        axis.title.y.left = element_text(size = 24),
        axis.title.x.bottom = element_text(size = 24))
dev.off()

jpeg("Cost_Lower.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
wideAnim %>% 
  mutate(level = case_when(cost > cost_cutoff ~ "Top 2.5% of outbreaks",
                           cost <= cost_cutoff ~ "97.5% of outbreaks")) %>%
  filter(!is.na(type) & trigger != "availability" & trigger != "decrease" & cost <= cost_cutoff) %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest") | str_detect(type, "No control")) %>%
  ggplot(aes(x = cType, y = cost, col = cType, fill = cType)) + 
  geom_violin()+
  labs(x = "Control Strategy", y = "Cost (USD)", colour = "Control Strategy", fill = "Control Strategy")+
  scale_fill_manual(values = c("yellow3",cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c("black","black","black"))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 22, face = "bold"), 
        strip.text.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 24),
        axis.title.y.left = element_text(size = 24),
        axis.title.x.bottom = element_text(size = 24))
dev.off()

#===============================================================================
#
#Cost analysis - no duration
#
#===============================================================================

wideAnim <- data.table(Anim.long) %>% 
  mutate(fips = FIPS,
         Value = anim) %>%
  select(-id & -Rep & -FIPS & -anim) %>%
  left_join(county.summary, by = "fips")  %>%
  mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = controlStatus, values_from = Value, values_fn = median) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


wideAnim$cost <- 1090*wideAnim$effective.cull+22*wideAnim$effective.vax

hist(wideAnim$cost[wideAnim$cost>620000000])

cost_cutoff = quantile(wideAnim$cost, 0.975)
cost_cutoff = 1000000000
cost_cutoff = 620000000
cost_min = min(wideAnim$cost)

wideAnim <- wideAnim %>% 
  mutate(level = case_when(cost > cost_cutoff ~ "High",
                           cost <= cost_cutoff ~ "Low",
                           cost > cost_min ~ "OverMin"))

foo <- wideAnim %>% 
  mutate(level = case_when(cost > cost_cutoff ~ "Top 2.5% of outbreaks",
                           cost <= cost_cutoff ~ "97.5% of outbreaks")) %>%
  filter(!is.na(type)) %>%
  filter(str_detect(type, "cull_vax_0_-1_earliest_earliest"), 
         (trigger == "percentIncrease" | trigger == "newPremReportsOverX"),
         (delay == 3 | delay == 0), 
         (cType == "State-dependent control" | cType == "Static control"))


jpeg("Cost_Panel.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
foo %>%
  ggplot(aes(x = cType, y = cost, col = cType, fill = cType)) + 
  geom_violin()+
  labs(x = "Control Strategy", y = "Cost (USD)")+
  facet_grid(~level, scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(cbPalette[1],cbPalette[12]))+
  scale_color_manual(values = c(cbPalette[1],cbPalette[12]))+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 22, face = "bold"), 
        strip.text.y = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 24),
        axis.title.y.left = element_text(size = 24),
        axis.title.x.bottom = element_text(size = 24))
dev.off()

#===============================================================================
#
# Decision delay
#
#===============================================================================
setwd(data_output)
Dur.long <- read.csv("Duration_long.csv")
EpidExt.long <- read.csv("EpidemicExtent_long.csv")
PremInf.long <- read.csv("PremisesInfected_long.csv")

Dur.long <- Dur.long %>% mutate(cType = str_extract(type, "^[^_]+"))
Dur <- Dur.long %>%
  mutate(trigger = str_extract(type, "^[^_]+"),
         metric = "Duration (days)",
         delay = case_when(str_detect(type, "static_newPremReportsOverX") ~ "0",
                           str_detect(type, "flex_1Day") ~ "1",
                           str_detect(type, "flex_2Days") ~ "2",
                           str_detect(type, "flex_percentIncrease") ~ "3",
                           str_detect(type, "noControl_noDiagnostics") ~ "No Control"),
         cType = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(cType, "flex") ~ "State-dependent control",
                           str_detect(cType, "static") ~ "Static control"),
         type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                          str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
                          str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
                          str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax"))

EpidExt <- EpidExt.long %>%
  mutate(trigger = str_extract(type, "^[^_]+"),
         metric = "Duration (days)",
         delay = case_when(str_detect(type, "static_newPremReportsOverX") ~ "0",
                           str_detect(type, "flex_1Day") ~ "1",
                           str_detect(type, "flex_2Days") ~ "2",
                           str_detect(type, "flex_percentIncrease") ~ "3",
                           str_detect(type, "noControl_noDiagnostics") ~ "No Control"),
         cType = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(cType, "flex") ~ "State-dependent control",
                           str_detect(cType, "static") ~ "Static control"),
         type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                          str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
                          str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
                          str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax"))

PremInf <- PremInf.long %>%
  mutate(trigger = str_extract(type, "^[^_]+"),
         metric = "Duration (days)",
         delay = case_when(str_detect(type, "static_newPremReportsOverX") ~ "0",
                           str_detect(type, "flex_1Day") ~ "1",
                           str_detect(type, "flex_2Days") ~ "2",
                           str_detect(type, "flex_percentIncrease") ~ "3",
                           str_detect(type, "noControl_noDiagnostics") ~ "No Control"),
         cType = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(cType, "flex") ~ "State-dependent control",
                           str_detect(cType, "static") ~ "Static control"),
         type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                          str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
                          str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
                          str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax"))


Dur <- Dur %>%
  filter(Value > Dur_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na()

EpidExt <- EpidExt %>%
  filter(Value > EpidExt_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na()

PremInf <- PremInf %>%
  filter(Value > PremInf_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na()

setwd(plot_output)
jpeg("Delay_PremInfHigh.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
PremInf %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected premises", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_EpidExtHigh.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
EpidExt %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_DurationHigh.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
Dur %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

#===============================================================================
#
# Lower
#
#===============================================================================
setwd(plot_output)
jpeg("Delay_PremInf_Low.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
PremInf %>%
  filter(Value < EpidExt_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected premises", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_EpidExt_Low.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
EpidExt %>%
  filter(Value < EpidExt_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_Duration_Low.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
Dur %>%
  filter(Value < Dur_cutoff &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

#===============================================================================
#
# Over min
#
#===============================================================================

setwd(plot_output)
jpeg("Delay_PremInf_OverMin.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
PremInf %>%
  filter(Value > PremInf_min &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected premises", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_EpidExt_OverMin.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
EpidExt %>%
  filter(Value > EpidExt_min &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

jpeg("Delay_Duration_OverMin.jpeg", width = 1500, height = 1300, units = 'px', res = 100)
Dur %>%
  filter(Value > Dur_min &
           (type == "MB, IP Cull, DC Vax" | type == "No control")) %>%
  drop_na() %>%
  ggplot(aes(x = delay, y = Value, fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c("black","yellow3",cbPalette[1],cbPalette[12],"grey"))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(y = "Number of infected counties", x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") + 
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()


#===============================================================================
#
# Prioritization
#
#===============================================================================
setwd(data_output)
pattern <- 'smallest|largest|closest|farthest|cull_vax_0_-1_earliest_earliest_3_3|cull_vax_0_-1_earliest_earliest_0_0|cull_vax_0_3000_earliest_earliest_0_0|cull_vax_0_3000_earliest_earliest_3_3'
PremInf.long <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Post_Processing_Rework/Data/PremisesInfected_long.csv")
# Filter rows based on the 'type' column
PremInf_filtered <- PremInf.long[grep(pattern, type, ignore.case = TRUE), ]

PremInf <- PremInf_filtered %>%
  mutate(metric = "Number of infected premises",
         control_type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(type, "flex") ~ "State-dependent control",
                           str_detect(type, "static") ~ "Static control"),
         priority = case_when(str_detect(type,"largest") ~ "Largest",
                              str_detect(type,"smallest") ~ "Smallest",
                              str_detect(type,"earliest") ~ "Earliest",
                              str_detect(type,"closest") ~ "Closest",
                              str_detect(type,"farthest") ~ "Farthest"),
         type = case_when(str_detect(type,"cull_vax_0_-1") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_vax_0_3000") ~ "MB, IP Cull, 3km Vax"))

PremInf <- PremInf %>%
  filter(Value > PremInf_cutoff) %>%
  drop_na()

setwd(plot_output)
jpeg("Priortization_PremInfHigh.jpeg", width = 1500, height = 1800, units = 'px', res = 100)
PremInf %>%
  ggplot(aes(x = type, y = Value, fill = priority)) +
  geom_violin()+
  theme_bw() +
  labs(y = "Number of infected premises", x = "Control strategy", colour = "Priority", fill = "Priority") + 
  theme(axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32),
        strip.text.x.top = element_text(size = 28),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.position = c(0.9,0.9)) +
  facet_grid(~control_type)
dev.off()
