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
# source("Dependencies/import_SensitivityRuns.R")
source("Dependencies/run_sensitivityModels.R")
path0 <- dirname(rstudioapi::getActiveDocumentContext()$path)
path_output <- file.path(path0, "Output_Files/")

if(!("usdos_scaled" %in% ls())){
  usdos_scaled <- read.csv("SensitivityData_Scaled_2023-03-27.csv") #if imported today, will need to change to today's date
  
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
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease")

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                     "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))

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
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                    "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))

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
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                    "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "PRCC_TakeOff_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
prcc_percentIncrease_fadeout
dev.off()

##################################################################################
##
## Model: All outbreaks and covariates
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease")


single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale <- .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(NumInf) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

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
        axis.text.y.left = element_text(size = 22, color = colors.long)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease
dev.off()

##################################################################################
##
## Model: All covariates, Takeoff == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_TakeOff_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease_takeoff
dev.off()

##################################################################################
##
## Model: All covariates, Fadeout == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems) 

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

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
        axis.text.y.left = element_text(size = 22, color = colors.long)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_FadeOut_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
SingleModel_PercentIncrease_fadeout
dev.off()

##################################################################################
##
## Full model: All outbreaks and covariates
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease")


single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments * target + 
                         out.shipments * target2 + 
                         out.shipments * threshold + 
                         out.shipments * threshold2 + 
                         out.shipments * priority + 
                         out.shipments * priority2 + 
                         out.shipments * in.shipments +
                         out.shipments * clustering + 
                         out.shipments * density + 
                         out.shipments * premises.size + 
                         out.shipments * num.large.prems +
                         in.shipments * target + 
                         in.shipments * target2 + 
                         in.shipments * threshold + 
                         in.shipments * threshold2 + 
                         in.shipments * priority + 
                         in.shipments * priority2 + 
                         in.shipments * clustering +
                         in.shipments * density + 
                         in.shipments * premises.size + 
                         in.shipments * num.large.prems + 
                         clustering * target + 
                         clustering * target2 + 
                         clustering * threshold + 
                         clustering * threshold2 + 
                         clustering * priority + 
                         clustering * priority2 + 
                         clustering * density +
                         clustering * premises.size +
                         clustering * num.large.prems +
                         density * target + 
                         density * target2 + 
                         density * threshold + 
                         density * threshold2 + 
                         density * priority + 
                         density * priority2 + 
                         density * premises.size +
                         density * num.large.prems +
                         premises.size * target + 
                         premises.size * target2 + 
                         premises.size * threshold + 
                         premises.size * threshold2 + 
                         premises.size * priority + 
                         premises.size * priority2 +
                         premises.size * num.large.prems +
                         num.large.prems * target + 
                         num.large.prems * target2 + 
                         num.large.prems * threshold + 
                         num.large.prems * threshold2 + 
                         num.large.prems * priority + 
                         num.large.prems * priority2)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(NumInf) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged_full %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

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
        axis.text.y.left = element_text(size = 8)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease.jpeg", width = 1440, height = 840, units = 'px', res = 100)
FullModel_PercentIncrease
dev.off()

##################################################################################
##
## Full model: All covariates, Takeoff == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Over5000 == 1)

single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged_full %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

FullModel_PercentIncrease_takeoff <- ggplot() + 
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
        axis.text.y.left = element_text(size = 22)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_takeoff.jpeg", width = 1440, height = 840, units = 'px', res = 100)
FullModel_PercentIncrease_takeoff
dev.off()
##################################################################################
##
## Full model: All covariates, Fadeout == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "percentIncrease" & Fade.Out == 1)

single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

FullModel_PercentIncrease_fadeout <- ggplot() + 
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
        axis.text.y.left = element_text(size = 22)) + 
  theme(strip.text = element_text(size = 17))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_PercentIncrease_fadeout.jpeg", width = 1440, height = 840, units = 'px', res = 100)
FullModel_PercentIncrease_fadeout
dev.off()


##################################################################################
##
## Availability parameter sets
##
##################################################################################


#===============================================================================
#
# PRCC: All outbreaks and covariates
#
#===============================================================================

#Subset to appropriate group
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability")

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                    "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

setwd(path_output)
jpeg(filename = "PRCC_AllOutbreaks_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

#===============================================================================
#
# PRCC: Takeoff only and covariates
#
#===============================================================================

usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Over5000 == 1)

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                    "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

setwd(path_output)
jpeg(filename = "PRCC_TakeOff_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

#===============================================================================
#
# PRCC: Fadeout only and covariates
#
#===============================================================================

usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Fade.Out == 1)

#Plots
prccParameters <- c("priority", "priority2", "threshold","target", "threshold2",  "target2", "out.shipments", "in.shipments",  "clustering", 
                    "density", "premises.size", "num.large.prems")

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
  mutate(variable = factor(variable, levels = c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) #Ensures the ordering of ggplot facets are correct

setwd(path_output)
jpeg(filename = "PRCC_TakeOff_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.short)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

##################################################################################
##
## Model: All outbreaks and covariates
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability")


single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale <- .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(NumInf) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.long)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

##################################################################################
##
## Model: All covariates, Takeoff == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Over5000 == 1)

single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_TakeOff_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.long)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

##################################################################################
##
## Model: All covariates, Fadeout == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Fade.Out == 1)

single.model = formula(RESPONSE ~ priority + priority2 + 
                         threshold + target + 
                         threshold2 + target2 + 
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems) 

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale.long <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged$parameters)) %>% #Order the df based on Number of infected premises (smallest to largest)
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)"))) %>%
  filter(variable != "P(Outbreak Take-off)", variable != "P(Outbreak Fade-out)")

setwd(path_output)
jpeg(filename = "SingleModel_AllParameters_FadeOut_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22, color = colors.long)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

##################################################################################
##
## Full model: All outbreaks and covariates
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability")


single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(NumInf) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged_full %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22)) + 
  theme(strip.text = element_text(size = 17))
dev.off()

##################################################################################
##
## Full model: All covariates, Takeoff == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Over5000 == 1)

single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale_arranged_full <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale_arranged_full %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22)) + 
  theme(strip.text = element_text(size = 17))
dev.off()
##################################################################################
##
## Full model: All covariates, Fadeout == 1
##
##################################################################################
usdos_scaled_subset <- subset(usdos_scaled, trigger == "availability" & Fade.Out == 1)

single.model = formula(RESPONSE ~ priority * priority2 + 
                         priority * threshold +
                         priority * threshold2 +
                         priority * target +
                         priority * target2 +
                         priority2 * threshold +
                         priority2 * threshold2 +
                         priority2 * target +
                         priority2 * target2 +
                         threshold * threshold2 +
                         threshold * target +
                         threshold * target2 +
                         threshold2 * target +
                         threshold2 * target2 +
                         out.shipments + in.shipments + clustering + 
                         density + premises.size + num.large.prems)

lmScale = .linearModel(model = single.model, data = usdos_scaled_subset)

#ordering values for plot
lmScale <- lmScale %>%
  rownames_to_column(var = "parameters") %>% #Rename parameter column
  arrange(match(parameters,lmScale_arranged_full$parameters)) #Order the df based on Number of infected premises (smallest to largest)

lmScale.long <- lmScale %>%
  pivot_longer(cols = c("NumInf","Dur","Epi","Over","Out"), names_to = "variable", values_to = "value") %>%
  mutate(parameters = factor(parameters, levels = lmScale_arranged_full$parameters),
         variable = case_when(variable == "NumInf" ~ "No. Premises Infected",
                              variable == "Epi" ~ "No. Counties Infected",
                              variable == "Dur" ~ "Duration of Infection" ,
                              variable == "Over" ~ "P(Outbreak Take-off)",
                              variable == "Out" ~ "P(Outbreak Fade-out)"),
         variable = factor(variable, levels =  c("No. Premises Infected", "No. Counties Infected", "Duration of Infection", "P(Outbreak Take-off)","P(Outbreak Fade-out)")))

setwd(path_output)
jpeg(filename = "FullModel_AllParameters_Availability.jpeg", width = 1440, height = 840, units = 'px', res = 100)
ggplot() + 
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
        axis.text.y.left = element_text(size = 22)) + 
  theme(strip.text = element_text(size = 17))
dev.off()
