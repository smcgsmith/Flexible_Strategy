#===============================================================================
#
# 1) Set working directory to current file location
# 2) Load dependencies necessary for sensitivity analyses and plotting
# 3) Import and process sensitivity runs
# 4) Load function necessary to run PRCC and linear models
# 5) Specify file locations: path0 is where this file exists, and path_output is directory where figures/tables are saved to
# 6) Ensure that sensitivity data are loaded. If they are not, load them and add Over500 (takeoff) and Fade.Out columns
#
#===============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("Dependencies/loadDependencies_postProcessing.R")
# Uncomment import_SensitivityRuns.R to load sensitivity runs located in the Files_To_Process directory
# that should be located in the Sensitivity directory. 
source("Dependencies/import_SensitivityRuns.R")
source("Dependencies/run_sensitivityModels.R")
path0 <- dirname(rstudioapi::getActiveDocumentContext()$path)
path_output <- file.path(path0, "Output_Files/")

if(!("usdos_scaled" %in% ls())){
  usdos_scaled <- read.csv("SensitivityData_Scaled_2023-07-29.csv") #if imported today, will need to change to today's date
  # usdos_scaled <- read.csv("SensitivityData_Scaled_2023-03-27.csv")
  #1 assigned to take off & Fade out, respectively for analyses
  usdos_scaled$Over5000 = usdos_scaled$Num_Inf > 5000
  usdos_scaled$Over5000 = ifelse(usdos_scaled$Over5000, 1, 0)
  
  usdos_scaled$Fade.Out = usdos_scaled$Num_Inf > 1 & usdos_scaled$Num_Inf < 5000 & usdos_scaled$Duration < 365
  usdos_scaled$Fade.Out = ifelse(usdos_scaled$Fade.Out, 1, 0)

  } else{
  
  #1 assigned to take off & Fade out, respectively for analyses
  usdos_scaled$Over5000 = usdos_scaled$Num_Inf > 5000
  usdos_scaled$Over5000 = ifelse(usdos_scaled$Over5000, 1, 0)
  
  usdos_scaled$Fade.Out = usdos_scaled$Num_Inf > 1 & usdos_scaled$Num_Inf < 5000 & usdos_scaled$Duration < 365
  usdos_scaled$Fade.Out = ifelse(usdos_scaled$Fade.Out, 1, 0)
}

#===============================================================================
#
# For each PRCC model: 1. create model; 2. run model, 3. save model output
# 1) PRCC across all outbreaks and covariates
# 2) PRCC only on outbreaks that takeoff with all coviarates
# 3) PRCC only on outbreaks that fadeout with all coviarates
# PRCC analysis complete complete
#
#===============================================================================

#===============================================================================
#
# PRCC: All outbreaks and covariates
#
#===============================================================================

#Subset to appropriate group
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & priority != "closest" & priority != "farthest")

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2", "target2", 
                    "decision.time","decision.time2",
                    "days.to.report", "days.to.index.report", "days.to.DC.report",
                    "out.shipments", "in.shipments",  "clustering", "density", "premises.size", "num.large.prems")

prcc.trans = .prcc(prcc.parameters = prccParameters, data = usdos_scaled_subset)

#Setting the order of the parameter
#need prcc.trans dataframe to order regression
prcc.trans1 <- prcc.trans %>% #add parameter column
  arrange(NumInf.est) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(c("NumInf.est", "parameters", "Epi", "Dur", "Over","Out")) %>% #Only take the columns we want #Rename parameter column
  mutate(parameters = factor(parameters, levels = parameters)) #Necessary or order ggplot sensitivity estimates


prcc.trans.long <- prcc.trans1 %>% 
  pivot_longer(!parameters, names_to = "variable", values_to = "value") %>%
  mutate(variable = case_when(variable == "NumInf.est" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)")) %>% #Rename facet labels
  mutate(color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF"),
         variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

color_vector <- prcc.trans.long %>% filter(variable == "No. Premises Infected") %>% pull(color)

prcc_percentIncrease <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = prcc.trans.long, aes(y = value, x = parameters, fill=blue))+
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "PRCC (Sensitivity)", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "PRCC_AllOutbreaks_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
prcc_percentIncrease
dev.off()

#===============================================================================
#
# PRCC: Takeoff only and covariates
#
#===============================================================================

usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2", "target2", 
                    "decision.time", "decision.time2", "days.to.report", "days.to.index.report", "days.to.DC.report", 
                    "out.shipments", "in.shipments",  "clustering", "density", "premises.size", "num.large.prems")

prcc.trans = .prcc(prcc.parameters = prccParameters, data = usdos_scaled_subset)

#Setting the order of the parameter
#need prcc.trans dataframe to order regression
prcc.trans.long <- prcc.trans %>% #add parameter column
  select(c("NumInf.est", "parameters", "Epi", "Dur", "Over","Out")) %>% #Only take the columns we want #Rename parameter column
  arrange(match(parameters,prcc.trans1$parameters)) %>%
  mutate(parameters = factor(parameters, levels = parameters)) %>% #Necessary or order ggplot sensitivity estimates 
  pivot_longer(!parameters, names_to = "variable", values_to = "value") %>%
  filter(variable != "Over" & variable != "Out") %>%
  mutate(variable = case_when(variable == "NumInf.est" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection")) %>% #Rename facet labels
  mutate(color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF"),
         variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

color_vector <- prcc.trans.long %>% filter(variable == "No. Premises Infected") %>% pull(color)

prcc_percentIncrease_takeoff <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = prcc.trans.long, aes(y = value, x = parameters, fill=blue))+
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "PRCC (Sensitivity)", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "PRCC_TakeOff_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
prcc_percentIncrease_takeoff
dev.off()

#===============================================================================
#
# PRCC: Fadeout only and covariates
#
#===============================================================================

usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2", "target2", 
                    "decision.time", "decision.time2", "days.to.report", "days.to.index.report", "days.to.DC.report", 
                    "out.shipments", "in.shipments",  "clustering", "density", "premises.size", "num.large.prems")

prcc.trans = .prcc(prcc.parameters = prccParameters, data = usdos_scaled_subset)

#Setting the order of the parameter
#need prcc.trans dataframe to order regression
prcc.trans.long <- prcc.trans %>% #add parameter column
  select(c("NumInf.est", "parameters", "Epi", "Dur", "Over","Out")) %>% #Only take the columns we want #Rename parameter column
  arrange(match(parameters,prcc.trans1$parameters)) %>%
  mutate(parameters = factor(parameters, levels = parameters)) %>% #Necessary or order ggplot sensitivity estimates 
  pivot_longer(!parameters, names_to = "variable", values_to = "value") %>%
  filter(variable != "Over" & variable != "Out") %>%
  mutate(variable = case_when(variable == "NumInf.est" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection")) %>% #Rename facet labels
  mutate(color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF"),
         variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

color_vector <- prcc.trans.long %>% filter(variable == "No. Premises Infected") %>%  pull(color)

prcc_percentIncrease_fadeout <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = prcc.trans.long, aes(y = value, x = parameters, fill=blue))+
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "PRCC (Sensitivity)", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color =color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "PRCC_Fadeout_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
prcc_percentIncrease_fadeout
dev.off()

#===============================================================================
##
## Model: All outbreaks and covariates
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease")


single.model = formula(RESPONSE ~ priority + priority2 + threshold + target + threshold2 + target2 + 
                         decision.time + decision.time2 + days.to.report + days.to.index.report + days.to.DC.report +
                         out.shipments + in.shipments + clustering + density + premises.size + num.large.prems)

lmScale <- .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged <- lmScale %>%
  select("NumInf.values","Dur.values","Epi.values","Over.values","Out.values") %>%
  set_names("NumInf","Dur","Epi","Over","Out") %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  # filter(variable == "NumInf") %>%
  arrange(NumInf) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale %>%
  select("NumInf.values","Dur.values","Epi.values","Over.values","Out.values") %>%
  set_names("NumInf","Dur","Epi","Over","Out") %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF"))

color_vector <- lmScale.long %>% filter(variable == "No. Premises Infected") %>%  pull(color)

SingleModel_PercentIncrease <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease
dev.off()

#===============================================================================
##
## Model: All covariates, Takeoff == 1
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

single.model = formula(RESPONSE ~ priority + priority2 + threshold + target + threshold2 + target2 + 
                         decision.time + decision.time2 + days.to.report + days.to.index.report + days.to.DC.report +
                         out.shipments + in.shipments + clustering + density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  select("NumInf.values","Dur.values","Epi.values","Over.values","Out.values") %>%
  set_names("NumInf","Dur","Epi","Over","Out") %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
        filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")


color_vector <- lmScale.long %>% filter(variable == "No. Premises Infected") %>%  pull(color)
  
SingleModel_PercentIncrease_takeoff <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_TakeOff_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease_takeoff
dev.off()

#===============================================================================
##
## Model: All covariates, Fadeout == 1
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

single.model = formula(RESPONSE ~ priority + priority2 + threshold + target + threshold2 + target2 + 
                         decision.time + decision.time2 + days.to.report + days.to.index.report + days.to.DC.report +
                         out.shipments + in.shipments + clustering + density + premises.size + num.large.prems) 

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  select("NumInf.values","Dur.values","Epi.values","Over.values","Out.values") %>%
  set_names("NumInf","Dur","Epi","Over","Out") %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

color_vector <- lmScale.long %>% filter(variable == "No. Premises Infected") %>%  pull(color)

SingleModel_PercentIncrease_fadeout <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector)) + 
  theme(strip.text = element_text(size = 16))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_FadeOut_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease_fadeout
dev.off()

#===============================================================================
##
## Full model: All outbreaks and covariates
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease")

combinations <- combn(prccParameters, 2, simplify = FALSE)

# Create the formula string
formula_str <- "RESPONSE ~ "

# Generate the terms for each combination
for (i in 1:length(combinations)) {
  formula_str <- paste0(formula_str, paste(combinations[[i]], collapse = " * "), " + ")
}

# Remove the trailing " + " from the formula string
formula_str <- substring(formula_str, 1, nchar(formula_str) - 3)

# Create the formula object
full.model <- as.formula(formula_str)

lmScale = .linearModel(model = full.model, data = usdos_scaled_subset)

setwd(path0)
write.csv(lmScale, file = "full_model_results_wide_final.csv")

lmScale <- #read.csv("full_model_results_wid_new07202023.csv") %>%
  read.csv("full_model_results_wide_noFarthest.csv") %>%
  filter((Out.p.value <= 0.001 & round(Out.values, 1) > 0) | 
         (Over.p.value <= 0.001 & round(Over.values, 1) > 0) | 
         (NumInf.p.value <= 0.001 & round(NumInf.values, 1) > 0) | 
         (Dur.p.value <= 0.001 & round(Dur.values, 1) > 0) | 
         (Epi.p.value <= 0.001 & round(Epi.values, 1) > 0)) %>%
  #filter(if_any(ends_with("p.value"), ~. < 0.05)) %>%
  #filter(if_all(round(ends_with(".values"),0), ~. > 0)) %>%
  dplyr::rename(parameters = X) #Rename parameter column

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  select("parameters","NumInf.values") %>%
  arrange(NumInf.values) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale  %>%
  arrange(match(parameters,lmScale_arranged_full$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(parameters, contains(".values")) %>%
  pivot_longer(cols = contains(".values"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           str_detect(parameters, "time|days.to") ~ "black",
                           TRUE ~ "#CA4A68FF"))

color_vector <- lmScale.long %>% filter(variable == "No. Premises Infected") %>%  pull(color)

FullModel_PercentIncrease <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22, color = color_vector#, margin = margin(t = 10, r = 0, b = 10, l = 0, unit = "pt")
)) + 
  theme(strip.text = element_text(size = 20))

setwd(path_output)
jpeg(filename = "supplement_FullModel_AllParameters_PercentIncrease.jpeg", width = 2400, height = 2200, units = 'px', res = 100)
FullModel_PercentIncrease
dev.off()

#===============================================================================
## Main text sensitivity figure
## Full model: All covariates, Takeoff == 1
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

lmScale = .linearModel(model = full.model, data = usdos_scaled_subset)

setwd(path0)
write.csv(lmScale, file = "takeoff_model_results_wide_final.csv")

lmScale_takeoff <- read.csv("takeoff_model_results_wide_new07202023.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  filter((NumInf.p.value <= 0.05 & round(NumInf.values, 1) > 0) | 
           (Dur.p.value <= 0.05 & round(Dur.values, 1) > 0) | 
           (Epi.p.value <= 0.05 & round(Epi.values, 1) > 0)) %>%
  dplyr::rename(parameters = X)

lmScale_fadeout <- read.csv("fadeout_model_results_wide_new07202023.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  filter((NumInf.p.value <= 0.05 & round(NumInf.values, 1) > 0) | 
           (Dur.p.value <= 0.05 & round(Dur.values, 1) > 0) | 
           (Epi.p.value <= 0.05 & round(Epi.values, 1) > 0)) %>%
  dplyr::rename(parameters = X)

 takeoff_fadeout_union <- union(lmScale_takeoff$parameters, lmScale_fadeout$parameters)

lmScale <- #read.csv("takeoff_model_results_wide_new07202023.csv") %>%
  read.csv("takeoff_model_results_wide_new07202023.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  dplyr::rename(parameters = X) %>%
  filter(parameters %in% takeoff_fadeout_union)
  

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  select("parameters","NumInf.values") %>%
  arrange(NumInf.values) #Order the df based on Number of infected premises (smallest to largest)

#ordering values for plot
lmScale.long_takeoff <- lmScale %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(parameters, contains(".values")) %>%
  pivot_longer(cols =contains(".values"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           str_detect(parameters, "time|days.to") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)") %>%
  mutate(scenario = "> 5000 IPs")

color_vector <- lmScale.long_takeoff %>% filter(variable == "No. Premises Infected") %>%  pull(color)

# Add stars for significant p-values
significant_lmScale.long_takeoff <- lmScale %>%
  select(parameters, contains("p.value")) %>%
  pivot_longer(cols =contains("p.value"), names_to = "variable", values_to = "p.value") %>%
  filter(p.value <= 0.001) %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  left_join(lmScale.long_takeoff, by = c("parameters", "variable")) %>%
  mutate(scenario = "> 5000 IPs")

FullModel_PercentIncrease_takeoff <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long_takeoff, aes(y = value, x = parameters, fill = blue)) +
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 18, color = color_vector)) + #, margin = margin(t = 10, b =10))) + 
  theme(strip.text = element_text(size = 20)) + 
  geom_text(data = significant_lmScale.long_takeoff, aes(x = parameters, y = value, label ="***"),
            size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_takeoff.jpeg", width = 2400, height = 2000, units = 'px', res = 100)
FullModel_PercentIncrease_takeoff
dev.off()

#===============================================================================
##
## Full model: All covariates, Fadeout == 1
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

lmScale = .linearModel(model = full.model, data = usdos_scaled_subset)

setwd(path0)
write.csv(lmScale, file = "fadeout_model_results_wide_final.csv")

lmScale <- #read.csv("fadeout_model_results_wide_new07202023.csv") %>%
  read.csv("fadeout_model_results_wide_noFarther.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  dplyr::rename(parameters = X) %>%
  filter(parameters %in% takeoff_fadeout_union)

#ordering values for plot
lmScale.long_fadeout <- lmScale %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  #arrange(NumInf.values) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(parameters, contains(".values")) %>%
  pivot_longer(cols =contains(".values"), names_to = "variable", values_to = "value") %>%
  mutate(scenario = "1 < IPs < 5000 ") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           str_detect(parameters, "time|days.to") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

color_vector <- lmScale.long_fadeout %>% filter(variable == "No. Premises Infected") %>%  pull(color)

# Add stars for significant p-values
significant_lmScale.long_fadeout <- lmScale %>%
  select(parameters, contains("p.value")) %>%
  pivot_longer(cols =contains("p.value"), names_to = "variable", values_to = "p.value") %>%
  filter(p.value <= 0.001) %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  left_join(lmScale.long_fadeout, by = c("parameters", "variable")) %>%
  mutate(scenario = "1 < IPs < 5000 ")


FullModel_PercentIncrease_fadeout <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long_fadeout, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 18, color = color_vector)) + 
  theme(strip.text = element_text(size = 16)) + 
  geom_text(data = significant_lmScale.long_fadeout, aes(x = parameters, y = value, label ="***"),
            size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_fadeout.jpeg", width = 2400, height = 2000, units = 'px', res = 100)
FullModel_PercentIncrease_fadeout
dev.off()

lmScale_all <- bind_rows(lmScale.long_takeoff,lmScale.long_fadeout) %>%
  mutate(bar_color = case_when(str_detect(scenario, "1 < IP") ~ "#5791C0",
                      TRUE ~ "#DE7431"))
signif_all <- bind_rows(significant_lmScale.long_takeoff,significant_lmScale.long_fadeout)
color_vector_all <- lmScale_all %>% filter(variable == "No. Premises Infected") %>%  pull(color)
bar_color_vector <- lmScale_all %>%  pull(bar_color)

maintext_takeoff_fadeout_facet <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale_all, aes(y = value, x = parameters, fill = scenario)) + 
  scale_y_discrete(limits=c(-1.5,-1, -.5, 0, .5, 1, 1.5)) +
  labs(y = "Proportional Effect", x = NULL, fill = "Outbreak size") + 
  coord_flip() + 
  facet_wrap(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = unique(bar_color_vector)) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 22,color = color_vector_all),
        legend.position = c(0.93,0.85),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20)) + 
  theme(strip.text = element_text(size = 22)) +
  geom_hline(data = lmScale_all, 
             aes(yintercept = 0), color = "black", size = 0.25)
#+ 
  # geom_text(data = signif_all, aes(x = parameters, y = value, label ="***"),
  #           size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "maintext_sensitivity_panel.jpeg", width = 2400, height = 2000, units = 'px', res = 100)
maintext_takeoff_fadeout_facet
dev.off()

#===============================================================================
##
## Supplement figure: includes all significant predictors of outbreak metrics & 
## predictors with large effect sizes
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

# lmScale = .linearModel(model = full.model, data = usdos_scaled_subset)
# 
# setwd(path0)
# write.csv(lmScale, file = "takeoff_model_results_wide_subset.csv")
setwd(path0)
lmScale_takeoff <- read.csv("takeoff_model_results_wide.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  filter((NumInf.p.value <= 0.05 | round(NumInf.values, 1) > 0) | 
           (Dur.p.value <= 0.05 | round(Dur.values, 1) > 0) | 
           (Epi.p.value <= 0.05 | round(Epi.values, 1) > 0)) %>%
  dplyr::rename(parameters = X)

lmScale_fadeout <- read.csv("fadeout_model_results_wide.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  filter((NumInf.p.value <= 0.05 | round(NumInf.values, 1) > 0) | 
           (Dur.p.value <= 0.05 | round(Dur.values, 1) > 0) | 
           (Epi.p.value <= 0.05 | round(Epi.values, 1) > 0)) %>%
  dplyr::rename(parameters = X)

takeoff_fadeout_union <- union(lmScale_takeoff$parameters, lmScale_fadeout$parameters)

lmScale <- read.csv("takeoff_model_results_wide.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  dplyr::rename(parameters = X) %>%
  filter(parameters %in% takeoff_fadeout_union)


#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  select("parameters","NumInf.values") %>%
  arrange(NumInf.values) #Order the df based on Number of infected premises (smallest to largest)

#ordering values for plot
lmScale.long_takeoff <- lmScale %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(parameters, contains(".values")) %>%
  pivot_longer(cols =contains(".values"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)") %>%
  mutate(scenario = "> 5000 IPs")

color_vector <- lmScale.long_takeoff %>% filter(variable == "No. Premises Infected") %>%  pull(color)

# Add stars for significant p-values
significant_lmScale.long_takeoff <- lmScale %>%
  select(parameters, contains("p.value")) %>%
  pivot_longer(cols =contains("p.value"), names_to = "variable", values_to = "p.value") %>%
  filter(p.value <= 0.05) %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  left_join(lmScale.long_takeoff, by = c("parameters", "variable")) %>%
  mutate(scenario = "> 5000 IPs")

FullModel_PercentIncrease_takeoff <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long_takeoff, aes(y = value, x = parameters, fill = blue)) +
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 18, color = color_vector)) + #, margin = margin(t = 10, b =10))) + 
  theme(strip.text = element_text(size = 20)) + 
  geom_text(data = significant_lmScale.long_takeoff, aes(x = parameters, y = value, label ="***"),
            size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_takeoff.jpeg", width = 2400, height = 2000, units = 'px', res = 100)
FullModel_PercentIncrease_takeoff
dev.off()

#===============================================================================
##
## Full model: All covariates, Fadeout == 1
##
#===============================================================================
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

# lmScale = .linearModel(model = full.model, data = usdos_scaled_subset)
# 
# setwd(path0)
# write.csv(lmScale, file = "fadeout_model_results_wide_subset.csv")

lmScale <- read.csv("fadeout_model_results_wide.csv") %>%
  select(-contains("Out"), -contains("Over")) %>%
  dplyr::rename(parameters = X) %>%
  filter(parameters %in% takeoff_fadeout_union)

# #ordering values for plot
# lmScale_arranged_full <- lmScale %>%
#   select("parameters","NumInf.values") %>%
#   arrange(NumInf.values) #Order the df based on Number of infected premises (smallest to largest)

#ordering values for plot
lmScale.long_fadeout <- lmScale %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  #arrange(NumInf.values) %>% #Order the df based on Number of infected premises (smallest to largest)
  select(parameters, contains(".values")) %>%
  pivot_longer(cols =contains(".values"), names_to = "variable", values_to = "value") %>%
  mutate(scenario = "1 < IPs < 5000 ") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")),
         color = case_when(str_detect(parameters, "out.shipments|in.shipments|clustering|density|premises.size|num.large.prems") ~ "black",
                           TRUE ~ "#CA4A68FF")) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

color_vector <- lmScale.long_fadeout %>% filter(variable == "No. Premises Infected") %>%  pull(color)

# Add stars for significant p-values
significant_lmScale.long_fadeout <- lmScale %>%
  select(parameters, contains("p.value")) %>%
  pivot_longer(cols =contains("p.value"), names_to = "variable", values_to = "p.value") %>%
  filter(p.value <= 0.05) %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters) ,
         variable = case_when(str_detect(variable,"NumInf") ~ "No. Premises Infected",
                              str_detect(variable,"Epi") ~ "No. Counties Infected",
                              str_detect(variable,"Dur") ~ "Duration of Infection" ,
                              str_detect(variable,"Over") ~ "P(Outbreak Take-off)",
                              str_detect(variable,"Out") ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  left_join(lmScale.long_fadeout, by = c("parameters", "variable")) %>%
  mutate(scenario = "1 < IPs < 5000 ")


FullModel_PercentIncrease_fadeout <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale.long_fadeout, aes(y = value, x = parameters, fill = blue)) + 
  scale_y_discrete(limits=c(-1, -.5, 0, .5, 1)) +
  labs(y = "Proportional Effect", x = NULL) + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = blue) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 18, color = color_vector)) + 
  theme(strip.text = element_text(size = 16)) + 
  geom_text(data = significant_lmScale.long_fadeout, aes(x = parameters, y = value, label ="***"),
            size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_fadeout.jpeg", width = 2000, height = 1800, units = 'px', res = 100)
FullModel_PercentIncrease_fadeout
dev.off()

lmScale_all <- bind_rows(lmScale.long_takeoff,lmScale.long_fadeout) %>%
  mutate(bar_color = case_when(str_detect(scenario, "1 < IP") ~ "#5791C0",
                               TRUE ~ "#DE7431"))
signif_all <- bind_rows(significant_lmScale.long_takeoff,significant_lmScale.long_fadeout)
color_vector_all <- lmScale_all %>% filter(variable == "No. Premises Infected") %>%  pull(color)
bar_color_vector <- lmScale_all %>%  pull(bar_color)

supplement_takeoff_fadeout_facet <- ggplot() + 
  theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(size = 22),  axis.title.x = element_text(size = 22)) +
  geom_col(data = lmScale_all, aes(y = value, x = parameters, fill = scenario)) + 
  scale_y_discrete(limits=c(-1.5,-1, -.5, 0, .5, 1, 1.5)) +
  labs(y = "Proportional Effect", x = NULL, fill = "Outbreak size") + 
  coord_flip() + 
  facet_grid(~variable, labeller= label_wrap_gen()) +
  scale_fill_manual (values = unique(bar_color_vector)) + 
  theme(panel.spacing.y = unit(1.5, "lines"),
        axis.text.x = element_text(size = 16),
        axis.text.y.left = element_text(size = 15,color = color_vector_all),
        legend.position = c(0.93,0.85),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20)) + 
  theme(strip.text = element_text(size = 22)) + 
  geom_text(data = signif_all, aes(x = parameters, y = value, label ="***"),
            size = 9, color = "black", vjust = -1, hjust = -0.7)

setwd(path_output)
jpeg(filename = "supplement_sensitivity_panel.jpeg", width = 2400, height = 2400, units = 'px', res = 100)
supplement_takeoff_fadeout_facet
dev.off()