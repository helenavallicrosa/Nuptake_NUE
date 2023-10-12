#### ///// DATA ANALYSIS //// ####
#### 1. Initial assumptions ####
load("~/Desktop/MIT/Recerca/NitUP/Project/v8/gapFilled_db_lats_ratios_ndep.RData")

library(ggpmisc)
library(ggpubr)

# Create Fig. S1a
N_stock <- ggplot(data=General_DB, aes(x=N_stocks_combi, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  stat_poly_line(color="cyan4") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.92) + #R2
  theme_classic() +
  theme(
        axis.title =element_text(size=13))+
  xlab(expression(Soil~N~stock~(kgN/m^{"2"}))) +
  # xlab("") +
  ylab("Plant N uptake \n log(kgN/ha/yr)") +
  ylim(2,7)

# Create Fig. S1b
N_stock_nue <- ggplot(data=General_DB, aes(x=N_stocks_combi, y=log(NUE)))+
  geom_point()+
  # coord_trans(y="log2", ylim=c(4,450)) +
  # geom_smooth(method="lm") +
  stat_poly_line(color="coral2") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.92) + #R2
  theme_classic() +
  theme(axis.title =element_text(size=13))+
  xlab(expression(Soil~N~stock~(kgN/m^{"2"}))) +
  ylab("Plant NUE \n log(kgC/kgN)") +
  ylim(3,7)

# Export Fig. S1
tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/non_sign_Nstock_woody.tif",
     width=1200, height = 600, res= 160)
ggarrange(N_stock, N_stock_nue, ncol=2, align="h", labels = c("a)","b)"))
dev.off()

#### 2.1 Nup:Linear models ####
load("~/gapFilled_db_lats_ratios_ndep.RData")

# Transform AM_combi into categorical variable
General_DB$AM_categoric <- "Med"

for (i in 1:159) {
  if(isTRUE(General_DB$AM_combi[i] < 40)) {
    General_DB$AM_categoric[i] <- "Low"
  }
}

for (i in 1:159) {
  if(isTRUE(General_DB$AM_combi[i] > 70)) {
    General_DB$AM_categoric[i] <- "High"
  }
}
General_DB$AM_categoric <- as.factor(General_DB$AM_categoric)

# Create and export Table S2
library(dplyr)
library(forcats)
cor <- cor(na.exclude(General_DB[,c(45,31,57,53,54,55,50,68)]))
write.csv(cor, file = "~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/variables_correlation.csv")

# Clean Na's
General_DB_neta <- subset(General_DB, !is.na(General_DB$AM_combi))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$N_stocks_combi))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$mic_N))
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$Soil_ph_combi))

# Aglomerated model
m_aglom <- glm(Net_Nuptake_Combi ~ Alt_combi+AM_combi+MAT_combi+mic_N+noy+woodiness+N_stocks_combi+MAP_combi+Soil_ph_combi, family=Gamma(link = "inverse"), na.action=na.fail, data=General_DB_neta)
summary(m_aglom)

# Model selection
library(MuMIn)
m_select_nup <- MuMIn::dredge(m_aglom)
a <- MuMIn::sw(m_select_nup)
importance_sw <- MuMIn::sw(subset(model.sel(m_select_nup, rank = AIC),
                                  cumsum(weight) <= .95))
m1_2 <- glm(Net_Nuptake_Combi ~ MAP_combi+MAT_combi+noy, family = Gamma(link = "inverse"), data=General_DB)
summary(m1_2)

# x <- summary(m1_1)
# resum1 <- as.data.frame(x$coefficients)
# resum1$Estimate <- exp(resum1$Estimate)
# for (i in 2:length(resum1)){
#   resum1$Estimate[i] <- (resum1$Estimate[i]-1)*resum1$Estimate[1]
# }
# varimp <- caret::varImp(m1_1)
# resum1$varimp <- c(NA,varimp$Overall)
# resum1$vars <- rownames(resum1)
# resum1$sign <- NA
# 
# options(scipen = 999)
# for (i in 2:nrow(resum1)){
#   if(resum1$`Pr(>|t|)`[i] < 0.05){
#     resum1$sign[i] <- "P.value < 0.05"
#   } else {
#     resum1$sign[i] <- "P.value > 0.05"
#   }
# }

# Fig 1 creation
names <- c("Nox deposition", "Temperature", "Precipitation", "Vegetation type", "Arbuscular Myco %", "Altitude",
           "Microbial N", "Soil pH", "Soil N stocks")
resum_Nup <- data.frame(importance_sw, names)

for (i in 1:nrow(resum_Nup)){
  if(resum_Nup$importance_sw[i] < 0.75){
    resum_Nup$imp[i] <- "No important"
  } else {
    resum_Nup$imp[i] <- "Imp"
  }
}

library(ggplot2)
library(ggpmisc)
library(forcats)

# Fig 1a
Nup_imp <- ggplot(data=resum_Nup, aes(y=importance_sw, x=fct_reorder(names, importance_sw), fill=imp)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("cyan4", "grey50")) +
  theme_classic()+
  theme(axis.text.y = element_text(size=13))+
  coord_flip() +
  geom_segment(aes(x=0,xend=10,y=0.75,yend=0.75),lty=2)+
  ylab("Variable importance")+
  theme(legend.position = "none")+
  labs(fill = "") +
  xlab("")

# Fig 1b
Nup_noy <- ggplot(data=General_DB, aes(x=noy, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (gN/m2)") +
  xlab(expression(Nox~deposition~(gN/m^{"2"}))) +
  ylab("Plant N uptake \n log(gN/ha/yr)")+
  ylim(2,6.3)

# Fig 1c
Nup_temp <- ggplot(data=General_DB, aes(x=MAT_combi, y=log(Net_Nuptake_Combi)))+ #MAT_combi
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  xlab("Temperature (°C)") +
  ylab("Plant N uptake \n log(kgN/ha/yr)")+
  ylim(2,6.3)

# Fig 1d
Nup_prec <- ggplot(data=General_DB, aes(x=MAP_combi, y=log(Net_Nuptake_Combi)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="cyan4") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  xlab("Precipitation (mm)") +
  ylab("Plant N uptake \n log(kgN/ha/yr)")+
  ylim(2,6.3)

# Export fig 1
tgrob <- text_grob("Plant nitrogen uptake", size = 18, face="bold")
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,-4.9, "cm"))

library(ggpubr)
tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/Fig1_Nup.tif",
     width=1400, height = 1200, res= 140)
ggarrange(plot_0, NULL,
          Nup_imp,Nup_noy,Nup_temp,Nup_prec, ncol=2, nrow = 3, 
                    labels = c("","", "a)","b)","c)","d)"),
          heights = c(1.5,10,10))
dev.off()

#### 2.2 NUE:Linear models ####
General_DB_neta <- subset(General_DB_neta, !is.na(General_DB_neta$NUE))

m_aglom_nue <- glm(NUE ~ MAT_combi+AM_combi+MAP_combi+mic_N+Alt_combi+N_stocks_combi+Soil_ph_combi+woodiness+noy,
                   family = Gamma(link="inverse"), na.action = na.fail, data=General_DB_neta)

m_select_nue <- MuMIn::dredge(m_aglom_nue)
importance_NUE <- sw(subset(model.sel(m_select_nue, rank = AIC),
                            cumsum(weight) <= .95))
m2 <- lm(log(NUE) ~ AM_combi+mic_N+Soil_ph_combi, data=General_DB)
summary(m2)

names <- c("Arbuscular Myco %", "Soil pH", "Microbial N", "Soil N stocks", "Altitude", "Nox deposition", "Temperature",
           "Precipitation","Vegetation type")
resum_Nue <- data.frame(importance_NUE, names)

for (i in 1:nrow(resum_Nue)){
  if(resum_Nue$importance_NUE[i] < 0.75){
    resum_Nue$imp[i] <- "No important"
  } else {
    resum_Nue$imp[i] <- "Imp"
  }
}

# Fig 2a creation
Nue_imp <- ggplot(data=resum_Nue, aes(y=importance_NUE, x=fct_reorder(names, importance_NUE), fill=imp)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("coral2", "grey50")) +
  theme_classic()+
  theme(axis.text.y = element_text(size=13))+
  coord_flip() +
  geom_segment(aes(x=0,xend=10,y=0.75,yend=0.75),lty=2)+
  ylab("Variable importance")+
  theme(legend.position = "none")+
  labs(fill = "") +
  xlab("")

# Fig 2b creation
Nue_am_cat <- ggplot(data=General_DB, aes(x=fct_relevel(AM_categoric, "Low", "Med", "High"), y=log(NUE))) +
  geom_violin(fill="gray", scale="width") +
  geom_boxplot(width=0.1, color="coral2", alpha=0.2) +
  geom_signif(comparisons = list(c("Low", "Med"), c("Med", "High"), c("Low", "High")),
              y_position = c(5.8,5.8,6.1),
              map_signif_level=TRUE)+
  theme_classic() +
  # theme(axis.text.x = element_text(size=13))+
  scale_x_discrete(labels=c("Low","Medium","High")) +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  xlab("Arbuscular Mycorrhizal %")+
  # xlab("")+
  ylim(3.5,6.3)

# Fig 2c creation
Nue_ph <- ggplot(data=General_DB, aes(x=Soil_ph_combi, y=log(NUE)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="coral2") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (kgN/m2)") +
  xlab("Soil pH") +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  ylim(3.5,6.3)

# Fig 2d creation
Nue_micN <- ggplot(data=General_DB, aes(x=mic_N, y=log(NUE)))+
  geom_point()+
  # geom_boxplot(aes(x=as.factor(AM_percent), y=log(Net_Nuptake_Combi))) +
  # coord_trans(y="log2", ylim=c(4,450)) +
  stat_poly_line(color="coral2") +
  # geom_abline(slope = -0.0733,
  #             intercept = 89.7, col="blue") +
  stat_poly_eq(use_label(c("eq", "p.value")), label.x = "right", label.y=0.95) + #R2
  theme_classic() +
  # xlab("Nox deposition (kgN/m2)") +
  xlab(expression(Microbial~N~stock~(gN/m^{"2"}))) +
  ylab("Nitrogen use efficiency \n (kgC/kgN)")+
  ylim(3.5,6.3)

# Exporting fig 2
library(ggpubr)
tgrob <- text_grob("Nitrogen use efficiency", size = 18, face="bold")
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,-4.5, "cm"))

tiff(filename="~/Fig2_Nue.tif",
     width=1400, height = 1200, res= 140)
ggarrange(plot_0, NULL,
          Nue_imp,Nue_am_cat,Nue_ph,Nue_micN, ncol=2, nrow = 3, 
          labels = c("","", "a)","b)","c)","d)"),
          heights = c(1.5,10,10))
dev.off()

#### 3. XGboost Models ####
load("~/gapFilled_db_lats_ratios_ndep.RData")

library(caret)
library(xgboost)
library(data.table)

# Function to train the model
func_boost <- function(db,iterations=5, variables, cont_var, y_var_pos,
                       max.depth=4, min_child_weight=0,eta=0.3){
  performance <- as.data.frame(matrix(ncol=3, nrow=iterations))
  colnames(performance) <- c("mse", "rmse", "r2")
  imp_matrix <- data.frame()
  var_behavior <- data.frame()
  models_list <- vector("list", iterations)
  training_data <- vector("list", iterations)
  validation_data <- vector("list", iterations)
  for ( i in 1:iterations) {
    parts = createDataPartition(db[,y_var_pos], p= .9, list=F)
    data=db[parts,]
    validation = db[-parts,]
    
    validation_x = data.matrix(validation[, variables])
    validation_y = validation[,y_var_pos]
    
    partió <- nrow(data)/5
    start <- data[,cont_var]
    selectId <- maxDissim(start, start, obj = minDiss, n = partió)
    
    dissimilar <- data[selectId, ]
    non_dissimilar <- data[-selectId, ]
    
    parts <- sample(1:dim(non_dissimilar)[1], partió)
    
    train = non_dissimilar[-parts, ]
    train <- rbind(dissimilar,train)
    test = data[parts, ]
    
    # parts = createDataPartition(db$Net_Nuptake_Combi, p = .8, list = F, groups=5)
    # train = db[parts, ]
    # test = db[-parts, ]
    
    train_x = data.matrix(train[, variables])
    train_y = train[,y_var_pos]
    test_x = data.matrix(test[, variables])
    test_y = test[,y_var_pos]
    
    xgb_train = xgb.DMatrix(data = train_x, label = train_y)
    xgb_test = xgb.DMatrix(data = test_x, label = test_y)
    
    watchlist = list(train=xgb_train, test=xgb_test)
    
    model = xgb.train(data = xgb_train, max.depth = max.depth, verbose=0,
                      watchlist=watchlist, nrounds = 150,objective="reg:gamma",
                      min_child_weight=min_child_weight, eta=eta)
    min <- min(model$evaluation_log$test_gamma_nloglik)
    min_iter <- model$evaluation_log[which(model$evaluation_log$test_gamma_nloglik==min),]
    number <- min_iter$iter[[1]]
    model_xgboost = xgboost(data = xgb_train, max.depth = max.depth, nrounds = number, verbose = 0, missing=NA,
                            objective="reg:gamma", min_child_weight=min_child_weight, eta=eta)
    pred_y = predict(model_xgboost, xgb_test)
    y_test_mean = mean(test_y)
    tss =  sum((test_y - y_test_mean)^2 )
    rss =  sum((pred_y - test_y)^2)
    
    performance[i,] <- c(mean((test_y - pred_y)^2),caret::RMSE(test_y, pred_y), 1 - (rss/tss))
    importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgboost)
    imp_matrix <- rbind(imp_matrix, importance_matrix)
    models_list[[i]] <- model_xgboost
    training_data[[i]] <- train_x
    validation_data[[i]] <- cbind(validation$Latitude, validation$Longitude, validation_y, validation_x)
  }
  total_performance <- colMeans(performance)
  names(total_performance) <- c("mse", "rmse", "r2")
  importance_matrix <- aggregate(imp_matrix, list(imp_matrix$Feature), FUN=mean)
  importance_matrix <- importance_matrix[,-2]
  colnames(importance_matrix)[1] <- "Feature"
  importance_matrix <- setDT(importance_matrix)
  return(list(total_performance, performance, importance_matrix,
              models_list, training_data, validation_data))
}

# Function to validate the model
validation <- function(model){
  r2mean <- length(model[[6]])
  for (i in 1:length(model[[6]])) {
    model_i <- model[[4]][[i]]
    data <- model[[6]][[i]]
    predi <- predict(model_i,data[,4:ncol(data)])
    m1 <- lm(predi ~ data[,3])
    sum <- summary(m1)
    r2mean[i] <- sum$r.squared
  }
  return(mean(r2mean))
}

#Function to create the pdp's
all_plot_pdp <- function(func_boost_output, out_var, ncol, nrow, categoric, ymin, ymax, color, ylab) {
  p <- vector("list", length(out_var))
  for (j in 1:length(out_var)){
    accum_db <- data.frame()
    for (i in 1:length(func_boost_output[[4]])) {
      model <- func_boost_output[[4]][[i]]
      training_data <- func_boost_output[[5]][[i]]
      p_data <- partial(model, pred.var = out_var[j], train=training_data)
      accum_db <- rbind(accum_db, p_data)
    }
    if (out_var[j] %in% categoric) {
      # accum_db$woodiness <- ifelse(accum_db$woodiness == 1, 1,2)
      p[[j]] <- local({
        j <- j
        accum_db[,1] <- as.character(accum_db[,1])
        m1 <- ggplot(data=accum_db, aes_string(x=out_var[j], y="yhat", group=out_var[j])) +
          # geom_smooth(span=0.75, se=TRUE, colour="red", method= "loess", formula = "y ~ x") +
          geom_violin(fill="gray") +
          geom_boxplot(width=0.1, color=color, alpha=0.2) +
          # geom_point(color="grey", size=0.8)+
          theme_bw() +
          # ylab("predicted N uptake \n (kgN/ha/yr)")+
          ylab(ylab)+
          xlab(out_var[j])+
          scale_x_discrete(labels=c("grassland","woody")) +
          # ylim(1,8)
          ylim(ymin,ymax)
      })
    } else {
      if (out_var[j] == "AM_percent") {
        p[[j]] <- local({
          j <- j
          means <- aggregate(accum_db[,1], by=list(accum_db[,2]), FUN=mean, na.rm=T)
          colnames(means) <- c("y","x")
          line <- aggregate(accum_db$yhat, list(accum_db$AM_percent), FUN=mean) 
          colnames(line) <- c("AM_percent", "yhat")
          m1 <- ggplot(data=line, aes_string(x=out_var[j], y="yhat")) +
            # geom_smooth(span=0.75, se=T, colour="red", method= "loess", formula = "y ~ x") +
            geom_point(data=means, aes(x=x, y=y), color="grey", size=0.8)+
            geom_line(colour=color, size=1)+
            # geom_smooth(method="lm", col="black")+
            theme_bw() +
            # ylab("predicted N uptake \n (kgN/ha/yr)")+
            ylab(ylab)+
            xlab(out_var[j]) +
            # coord_cartesian(ylim = c(1, 8))
            coord_cartesian(ylim = c(ymin, ymax))
        })
      } else {
        p[[j]] <- local({
          j <- j
          means <- aggregate(accum_db[,1], by=list(accum_db[,2]), FUN=mean, na.rm=T)
          colnames(means) <- c("y","x")
          m1 <- ggplot(data=accum_db, aes_string(x=out_var[j], y="yhat")) +
            geom_point(data=means, aes(x=x, y=y), color="grey", size=0.8)+
            # geom_point(color="grey", size=0.8)+
            geom_smooth(span=0.75, se=T, colour=color, method= "loess", formula = "y ~ x") +
            geom_smooth(method="lm", col="black")+
            theme_bw() +
            # ylab("predicted N uptake \n (kgN/ha/yr)")+
            ylab(ylab)+
            xlab(out_var[j])+
            # coord_cartesian(ylim = c(1,8))
            coord_cartesian(ylim = c(ymin, ymax))
        })
      }
    }
  }
  # plots <- ggpubr::ggarrange(plotlist = pdp_plots, ncol=4, nrow=2)
  return(p)
}

#### **3.1 Nup xgboost model ####
nup_model <- func_boost(db=General_DB, iterations = 20,
                     variables=c(37,45,31,57,53,54,55,50,68), y_var_pos=52,
                     max.depth=6, min_child_weight=1, eta=0.30,
                     cont_var=c(45,31,57,53,54,55,50,68))
load("~/nup_xgboost.RData") # Outcome used for manuscript developement

# Quick model info
nup_model[[1]]
xgb.plot.importance(nup_model[[3]])
validation(nup_model) # r-squared validation subset

# Figure S2 creation and export
im <- nup_model[[3]]
customNames <- c("Altitude", "AM %","Precipitation","Temperature",  "Microbial N stock", "Soil N stock","Nox deposition", "Soil pH", "Woodiness")
im$Feature <- customNames

Nup_imp_xgboost <- ggplot(im, aes(x=Gain, y=reorder(Feature, Gain)))+
  geom_bar(stat="identity", fill="deepskyblue3")+
  theme_classic()+
  ylab("")+
  annotate(geom="text", x=0.25, y=1.5, label="Model r2 = 0.54", cex=4)
Nup_imp_xgboost

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nup_xgboost_var_imp.tif",
     width=700, height = 500, res= 150)
Nup_imp_xgboost
dev.off()

# Fig. S3 creation and export
varnames <- colnames(nup_model[[5]][[1]])
categoric <- varnames[c(1)]
pdp_plots <- all_plot_pdp(func_boost_output=nup_model, out_var=varnames, ncol=4,
                          nrow=3, categoric = categoric, ymin=30, ymax=120,
                          color="deepskyblue3",ylab="Plant N uptake \n log(kgN/ha/yr)"
                          )
ggpubr::ggarrange(plotlist = pdp_plots, ncol=5, nrow=2)

o <- pdp_plots
pdp_plots <- list(o[[5]],o[[6]],o[[9]],o[[2]],o[[7]],o[[3]],o[[4]],o[[8]],o[[1]])
pdp_plots[[1]][["labels"]][["x"]] <- "MAT (ºC)"
pdp_plots[[2]][["labels"]][["x"]] <- "MAP (mm)"
pdp_plots[[3]][["labels"]][["x"]] <- "Nox deposition (gN/m2)"
pdp_plots[[4]][["labels"]][["x"]] <- "Soil microbial N stocks"
pdp_plots[[5]][["labels"]][["x"]] <- "Altitude (m)"
pdp_plots[[6]][["labels"]][["x"]] <- "AM %"
pdp_plots[[7]][["labels"]][["x"]] <- "Soil pH"
pdp_plots[[8]][["labels"]][["x"]] <- "Soil N stocks (kg/m2)"
pdp_plots[[9]][["labels"]][["x"]] <- "Woodiness"

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nup_pdp.tif",
     width=1800, height = 1800, res= 190)
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)
dev.off()

#### **3.2 NUE xgboost model ####
load("~/gapFilled_db_lats_ratios_ndep.RData")
nue_mod <- func_boost(db=General_DB, iterations = 20, y_var_pos=66,
                                 variables=c(37,45,31,57,53,54,55,50,68),
                                 max.depth=6, min_child_weight=1, eta=0.30,
                                 cont_var=c(45,31,57,53,54,55,50,68))
load("~/nue_xgboost.RData") # Outcome used for manuscript developement

# Quick model info
nue_mod[[1]]
xgb.plot.importance(nue_mod[[3]])
validation(nue_mod) # R2 of validation subset

# Fig. S5 creation and export
im <- nue_mod[[3]]
im$Feature
customNames <- c("Altitude", "AM %","Precipitation","Temperature",  "Microbial N stock", "Soil N stock", "Nox deposition", "Soil pH", "Woodiness")
im$Feature <- customNames

Nue_imp_xgboost <- ggplot(im, aes(x=Gain, y=reorder(Feature, Gain)))+
  geom_bar(stat="identity", fill="coral")+
  theme_classic()+
  ylab("")+
  annotate(geom="text", x=0.15, y=1.5, label="Model r2 = 0.44", cex=4)
Nue_imp_xgboost


tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nue_xgboost_var_imp.tif",
     width=700, height = 500, res= 150)
Nue_imp_xgboost
dev.off()

# Fig. S6 creation and export
varnames <- colnames(nue_mod[[5]][[1]])
categoric <- varnames[c(1)]

pdp_plots <- all_plot_pdp(func_boost_output=nue_mod, out_var=varnames, ncol=4,
                          nrow=3, categoric = categoric, ymin=65, ymax=150,
                          color="coral",ylab="Plant NUE \n log(KgC/KgN)")
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)

o <- pdp_plots
pdp_plots <- list(o[[2]],o[[7]],o[[6]],o[[4]],o[[3]],o[[8]],o[[5]],o[[9]],o[[1]])
pdp_plots[[1]][["labels"]][["x"]] <- "Soil microbial N stocks"
pdp_plots[[2]][["labels"]][["x"]] <- "Altitude (m)"
pdp_plots[[3]][["labels"]][["x"]] <- "MAP (mm)"
pdp_plots[[4]][["labels"]][["x"]] <- "Soil pH"
pdp_plots[[5]][["labels"]][["x"]] <- "AM %"
pdp_plots[[6]][["labels"]][["x"]] <- "Soil N stocks (kg/m2)"
pdp_plots[[7]][["labels"]][["x"]] <- "MAT (ºC)"
pdp_plots[[8]][["labels"]][["x"]] <- "Nox deposition"
pdp_plots[[9]][["labels"]][["x"]] <- "Woodiness"

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/Nue_pdp.tif",
     width=1800, height = 1800, res= 190)
ggpubr::ggarrange(plotlist = pdp_plots, ncol=3, nrow=3)
dev.off()

#### 4. Upscaling maps ####
load("~/nup_xgboost.RData")
load("~/nue_xgboost.RData")
ups_maps <- stack("~/training_raster.gri")

models <- nup_model[[4]]
models[[1]][["feature_names"]]
names(ups_maps) <- models[[1]][["feature_names"]]

# Parallel
library(doParallel)
library(foreach)

# Cut stack in pieces
lat <- seq(-90,90, by=30)
lon <- seq(-180,180, by=36)

ext_crop <- vector("list", 60)
for (j in 1:6) {
  for (i in 1:10){
    ext_crop[[i+(10*(j-1))]] <- c(lon[i], lon[1+i], lat[j], lat[j+1])
  }
}

# Cut training stack
library(foreach)
cl <- parallel::makeCluster(60, type="FORK")
doParallel::registerDoParallel(cl)
train_stack <- vector("list", 60)

train_stack <- foreach (i = 1:60) %dopar% {
  raster::crop(ups_maps, extent(ext_crop[[i]]))
}
parallel::stopCluster(cl)

cl <- parallel::makeCluster(6, type="FORK")
doParallel::registerDoParallel(cl)

predicted_pieces <- vector("list", 60)

# create function to calculate the predicitons per piece
calc_square <- function (model, train_stack2) {
  raster_list <- vector("list", length(model))
  for (j in 1:length(model)) {
    Nup_pred_map <- raster::predict(model[[j]], train_stack2[1:(nrow(train_stack2)*ncol(train_stack2))],
                                    missing=NA, outputmargin=FALSE)
    res <- raster::raster(train_stack2)
    res <- raster::setValues(res,Nup_pred_map)
    raster_list[[j]] <- res 
  }
  ras <- raster::stack(raster_list)
  mean_predicted_maps <- raster::calc(ras, mean) #mean or sd
  return(mean_predicted_maps)
}

library(foreach)
cl <- parallel::makeForkCluster(nnodes = 60)
doParallel::registerDoParallel(cl)

#run function
predicted_pieces <- foreach (i = 1:60) %dopar% {
  calc_square(model=models, train_stack2 = train_stack[[i]])
}
stopCluster(cl)

# Ensamble pieces together
install_github("PredictiveEcology/SpaDES.tools", dependencies = TRUE)
library(SpaDES.tools)
Nup_map <- SpaDES.tools::mergeRaster(predicted_pieces)
plot(Nup_map)
writeRaster(Nup_map, filename = "~/Final_map.grd")

#### 5. Tune maps ####
# Aggregate LC map
LC <- raster("~/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tiff")
LC1km <- raster::aggregate(LC, fact=3.3, fun=modal, na.rm=FALSE, filename="~/Desktop/MIT/Recerca/NitUP/Project/LM1km.grd")

# Reclassification process
from <- c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,
          110,120,121,122,130,140,150,151,152,153,160,170,180,190,200,201,202,210,220)
to <- c(NA,NA,   1 ,   2 ,NA,NA,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,
           2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   2 ,   1 ,   2 ,   2 ,
           2 ,   1 ,NA,   2 ,   2 ,   2 ,   1 ,   2 ,   2 ,   1 ,
        NA,NA,NA,NA,NA,NA)
conv <- data.frame(from,to)
LC_reclass <- reclassify(LC1km, conv, filename="~/Desktop/MIT/Recerca/NitUP/Project/v3/LC1km_woody_0.0.grd")
LC2km <- raster::aggregate(LC_reclass, fact=2, fun=modal, na.rm=FALSE, filename="~/Desktop/MIT/Recerca/NitUP/Project/LM2km.grd")

#Nup
Nup <- raster("~/Nup_2km.grd")
Nup_mean_masked <- mask(Nup, LC2km, filename="~/Nup_mean_masked_2km.grd")
sumatori <- cellStats(Nup_mean_masked, "sum")
sum_maskedTG_km <- sumatori*100*4/1000000000 #### 842.215 ### 

#Sd
nup_sd <- raster("~/Nup_2km_sd.grd")
Nup_sd_masked <- mask(nup_sd, LC2km, filename="~/Nup_sd_masked_2km.grd")
sumatori <- cellStats(Nup_sd_masked, "sum")
sum_maskedTG_km <- sumatori*100*4/1000000000 #### 236.110 ### 

# cv
Nup_cv <- (Nup_sd_masked/Nup_mean_masked)*100
writeRaster(Nup_cv, filename="~/Nup_cv_masked_2km.grd")

#NUE
Nue <- raster("~/Nue_2km.grd")
Nue_mean_masked <- mask(Nue, LC2km, filename="~/Nue_mean_masked_2km.grd")
sumatori <- cellStats(Nue_mean_masked, "mean") #### 110.262 ###

#sd
Nue_sd <- raster("~/Nue_2km_sd.grd")
Nue_sd_masked <- mask(Nue_sd, LC2km, filename="~/Nue_sd_masked_2km.grd")
sumatori <- cellStats(Nue_sd_masked, "mean") #### 19.40 ###

#cv
Nue_cv <- (Nue_sd_masked/Nue_mean_masked)*100
writeRaster(Nue_cv, filename="~/Nue_cv_masked_2km.grd")

#### **5.1 Maps plots ####
Nup <- raster("~/Nup_mean_masked_2km.grd")
Nup_coarse <- aggregate(Nup, fact=10)
Nup_df <- as.data.frame(Nup_coarse, xy=T)

library(maptools)
library(ggplot2)
library(viridis)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

# Figure 3a
means <- aggregate(Nup_df$layer, by=list(Nup_df$y), FUN=mean, na.rm=T)

bar_nup <- ggplot(means)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  ylim(c(0,150)) +
  geom_hline(yintercept = 0) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_Nup <- ggplot(Nup_df) + 
  geom_raster(aes(x, y, fill=layer)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="G")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "N uptake \n (KgN/ha/yr)") +
  ggtitle("a) Global nitrogen uptake: 842.215 ± 236.110 Tg N/yr")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

# Nue
Nue <- raster("~/Desktop/MIT/Recerca/NitUP/Project/v8/ProducedMaps/Nue_mean_masked_2km.grd")
Nue_coarse <- aggregate(Nue, fact=10)
Nue_df <- as.data.frame(Nue_coarse, xy=T)

library(maptools)
library(ggplot2)
library(viridis)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('~/Desktop/Batch maps/Global Maps/ne_50m_coastline/ne_50m_coastline.shp',
                          proj=proj)

# Fig 3b
means_nue <- aggregate(Nue_df$layer, by=list(Nue_df$y), FUN=mean, na.rm=T)

bar_nue <- ggplot(means_nue)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  ylim(c(0,150)) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = mean(na.exclude(Nue_df$layer)), col="red") +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_Nue <- ggplot(Nue_df) + 
  geom_raster(aes(x, y, fill=layer)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_viridis(na.value=NA, direction = -1, option="A")+
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "NUE \n (KgC/kgN)") +
  ggtitle("b) Average nitrogen use efficiency: 110.262 ± 19.40 kg C/Kg N")+
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

# Export fig 3
library(ggpubr)
tiff(filename="~/Nup_Nue_maps.tif",
     width=1500, height = 1400, res= 160)
ggarrange(map_Nup, bar_nup, map_Nue, bar_nue, 
          nrow=2, ncol=2, align = "h", widths = c(9, 1))
dev.off()

#### 6. TRENDY comparison ####
load("~/stack_correlation_TRENDY.RData") # TRENDY maps obtanied from nc
names(nup_stack_homogenized) <- c("CABLE-POP", "CLM5.0", "DLEM", "JSBACH", "JULES", "LPJ-GUESS", "LPX-Bern", "ORCHIDEE", "Upscaling")

nup_stack_homogenized[[3]] <- nup_stack_homogenized[[3]]/1000
nup_stack_homogenized <- stack(nup_stack_homogenized)
nup_stack_homogenized <-  nup_stack_homogenized[[1:8]]
mean_trendy <- calc(nup_stack_homogenized, fun = mean)

Nup <- raster("~/Nup_mean_masked_2km.grd")
Nup <- resample(Nup, mean_trendy)

# Difference between trendy and upscaled
dif <- mean_trendy-Nup

# Positive and negative values
dif_pos <- dif
dif_pos[dif_pos<0] <- NA

dif_neg <- dif
dif_neg[dif_neg>0] <- NA

# pond_dif <- (dif/Nup)

dif <- stack(mean_trendy, Nup, dif, dif_pos, dif_neg, pond_dif)
names(dif) <- c("mean_trendy", "Nup", "dif", "dif_pos", "dif_neg", "dif_pond")
save(dif, file = "~/dif_stack_TRENDY.Rdata")

load("~/dif_stack_TRENDY.Rdata") #provided
dif_map <- dif$dif
dif_map <- as.data.frame(dif_map,xy=T)

dev_map <- ((dif$mean_trendy-dif$Nup)/(dif$Nup))*100
dev_map <- as.data.frame(dev_map,xy=T)
colnames(dev_map)[3] <- "dev"


# Fig. 4
library(maptools)
library(ggplot2)
proj <- CRS('+proj=longlat +ellps=WGS84')
# https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-coastline/
mapaSHP <- readShapeLines('~/ne_50m_coastline.shp',
                          proj=proj)

means_dif <- aggregate(dif_map$dif, by=list(dif_map$y), FUN=mean, na.rm=T)
means_dev <- aggregate(dev_map$dev, by=list(dev_map$y), FUN=mean, na.rm=T)

bar_dif <- ggplot(means_dif)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(na.exclude(dif_map$dif)), col="red") +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_dif <- ggplot(dif_map) + 
  geom_raster(aes(x, y, fill=dif)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "red", midpoint = 0, na.value = NA) +
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "TRENDY \n       -\n empirical \n (kg N/ha/yr)") +
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

bar_dev <- ggplot(means_dev)+
  geom_bar(aes(x=Group.1, y=x), stat="identity", width = 1, col="gray60", fill="gray60")+
  coord_flip() +
  # ylim(c(-60,90)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(na.exclude(dev_map$dev)), col="red") +
  theme(axis.line=element_blank(),
        # axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map_dev <- ggplot(dev_map) + 
  geom_raster(aes(x, y, fill=dev)) +
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "red", midpoint = 0, na.value = NA) +
  theme_minimal()+
  ylim(c(-60,90)) +
  labs(fill = "TRENDY \n deviation %") +
  theme(legend.position = "left") +
  xlab("") +
  ylab("")

library(ggpubr)
tiff("~/TRENDY.tiff", width = 1800, height = 1600, res=180)
ggarrange(map_dif, bar_dif, map_dev, bar_dev,
          ncol=2, nrow=2, widths = c(8, 2), align = "h",
          labels = c("a)","","b)",""))
dev.off()

#### ///// Extra FIGURES ///// ####
#### 1. Sampling maps ####
library(ggmap)
library(maptools)
library(maps)
library(stringi)
library(grDevices)
library(sf)
library(rgdal)

load("~/gapFilled_db_lats_ratios_ndep.RData")
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readOGR('~/ne_50m_coastline.shp')

General_DB$Latitude <- round(General_DB$Latitude, 0)
General_DB$Longitude <- round(General_DB$Longitude, 0)

General_DB$Lat_id <- paste(General_DB$Latitude, General_DB$Longitude, sep="_")
a <- plyr::count(General_DB$Lat_id)

for (i in 1:nrow(General_DB)) {
  for (j in 1:nrow(a)) {
    if(isTRUE(General_DB$Lat_id[i]==a$x[j])) {
      General_DB$rep[i] <- a$freq[j]
    }
  }
}

map_sampling <- ggplot(General_DB, aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data = mapaSHP, aes(x = long, y = lat, group = group), size=0.3, colour = "black", fill = NA) +
  geom_point(aes(fill=woodiness, size=rep), shape=21, alpha=0.6)+
  theme_minimal() +
  labs(fill="Vegetation type", size="Sample size") +
  ylim(c(-60,90)) 
map_sampling

tiff(filename="~/Data_points.tif",
     width=2100, height = 1000, res= 200)
map_sampling
dev.off()

#### 2. Whittaker biomes ####
library(plotbiomes)
library(ggplot2)

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = Whittaker_biomes, aes(x    = temp_c,
                   y    = precp_cm, fill = biome),
               # adjust polygon borders
               colour = "gray98", size   = 1) +
  geom_point(data=General_DB, aes(x = MAT_wc, y = MAP_wc/10),
             size=3, position = position_jitter(width = 1.5, height = 1.5), shape  = 21,colour = "gray95",fill   = "black",stroke = 1,alpha  = 0.5) +
  theme_minimal() +
  xlab("Mean annual temperature") +
  ylab("Mean annual precipitation")

tiff(filename="~/Desktop/MIT/Recerca/NitUP/Project/v8/Figures/SM/whittaker_biomes.tif",
     width=1400, height = 1000, res= 150)
plot_1
dev.off()

#### 3. Fig. S8. TRENDY individual ####
load("~/TRENDY_individual.Rdata")
names(nup_stack_all)[9:10] <- c("Nup", "sd")

Nup_valor_final <- c()
for (i in 1:10){
  sumatori <- cellStats(nup_stack_all[[i]], "sum")
  Nup_valor_final[i] <- sumatori*302500/1000000000 #
}

Names <- c("CABLE-POP", "CLM5.0", "DLEM", "JSBACH", "JULES", "LPJ-GUESS", "LPX-Bern", "ORCHIDEE", "Upscaling")

Results <- data.frame(Names, Nup_valor_final[1:9])
Results[9,2] <- 842.215
Nup_valor_final[9] <- 842.215
Nup_valor_final[10] <- 236.110

library(ggplot2)
res <- ggplot(mapping = aes(x = Results$Names, y = Results$Nup_valor_final)) +
  geom_point(size=4) +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=Nup_valor_final[9]-(Nup_valor_final[10]),
                ymax=Nup_valor_final[9]+(Nup_valor_final[10])), fill="gray80") +
  coord_flip() + 
  theme(axis.text.x = element_text(size=18))+
  geom_point(size=4) +
  geom_point(aes(x=9,y=Results[9,2]), colour="red", size=4) +
  xlab("") + ylab("TgN/yr")+
  ylim(400,1700)+
  theme_minimal()

tiff("~/TRENDY_individual.tiff", width = 600, height = 500, res=150)
res
dev.off()

save(Results, file="~/Result_TRENDY_comparison.RData")
