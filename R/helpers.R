##############################
##
##    Helper Functions for
##       subStat
##
##############################

# Sample data for testing
# mmat <- matrix(NA,nrow=4,ncol=6)
# rownames(mmat) <- c("Length","Proximal","Midshaft","Distal")
# colnames(mmat) <- c("Femur","Tibia","Fibula","Humerus","Radius","Ulna")
# mdf <- as.data.frame(mmat)
# mdf[1,1] <- 150
# mdf[1,2] <- 135
# mdf[3,1] <- 35
# mdf[4,4] <- 50
# 
# sex="pooled"
# type="nonlinear"


# Version Control
gui_version <- function() {
  return("1.00")
}

# Identify given measurements; Convert to df of only present variables
investigate_data <- function(mdf) {
  # initialize empty vectors for final df
  vars <- c()
  vals <- c()
  
  # loop through each row and column
  for(r in 1:4){
    for(c in 1:6){
      value <- mdf[r,c]
      rname <- car::recode(rownames(mdf)[r],
                           "'Length'='DL';
                           'Proximal'='PB';
                           'Midshaft'='MSB';
                           'Distal'='DB'")
      cname <- car::recode(colnames(mdf)[c],
                           "'Femur'='F';
                           'Tibia'='T';
                           'Fibula'='FB';
                           'Humerus'='H';
                           'Radius'='R';
                           'Ulna'='U'")
      varname <- paste(cname,rname,sep="")
      
      if(is.na(value)){
        next
      } else {
        vars <- c(vars,varname)
        vals <- c(vals, value)
      }
    }
  }
  
  # convert input data to df format expected by formula
  out <- as.data.frame(matrix(vals,nrow=1,ncol=length(vals)))
  colnames(out) <- vars
  
  # check for upper and lower
  if ("HDL" %in% vars & "RDL" %in% vars) {
    out$upper <- out$HDL + out$RDL
  }
  if ("FDL" %in% vars & "TDL" %in% vars) {
    out$lower <- out$FDL + out$TDL
  }
  
  return(out)
}

# case_data <- investigate_data(mdf)  # test function

# Import the correct model list
import_model_list <- function(sex, type) {
  models <- readRDS(paste0("www/models/",type,"_models_",sex,".rds"))
  
  return(models)
}

# Predict stature and PI given for all possibilities
# given case_data and sex

predict_stature <- function(case_data, sex) {
  # import performance model and keep only those from case_data
  perf_full <- readRDS("www/models/model_performance_full.rds")
  perf_df <- perf_full[perf_full$vars %in% names(case_data) & 
                         perf_full$sex == sex,c(1,2,6,5,4)]
  perf_df$test_acc <- round(perf_df$test_acc * 100,2)
  perf_df$SEE <- round(perf_df$SEE,4)
  perf_df$MAD <- round(perf_df$MAD,4)
  
  # rank perf_df by increasing MAD, new subset with test_acc > 95%
  perf_df <- perf_df[order(perf_df$MAD),]  # for appendix
  
  # loop through each model in perf_df, load model, calculate prediction
  point <- lo <- hi <- c()  # empty vectors
  
  for(i in 1:nrow(perf_df)) {
    # initialize variables
    model_type <- perf_df[i,1]
    model_var <- perf_df[i,2]
    
    # import list, specify model
    model_list <- import_model_list(sex, model_type)
    model <- model_list[[model_var]]
    data_sub <- case_data[model_var]
    if (model_type=="linear") {
      pred <- predict(model, newdata=data_sub, interval="prediction")
      point <- c(point, round(pred[1],2))
      lo <- c(lo, round(pred[2],2))
      hi <- c(hi, round(pred[3],2))
    } else {
      names(data_sub) <- "x"
      pred <- suppressMessages(propagate::predictNLS(model, newdata=data_sub, 
                                    interval="prediction"))
      point <- c(point, round(pred$summary[[1]],2))
      lo <- c(lo, round(pred$summary[[5]],2))
      hi <- c(hi, round(pred$summary[[6]],2))
    }
  }
  
  # combine predictions with perf_df
  df0 <- data.frame("Point.Estimate"=point,"Range"=paste(lo,hi,sep=" - "))
  perf_df <- data.frame(perf_df[1:2],df0,perf_df[3:5])
  
  
  # munge for output
  # perf_df$vars <- car::recode(perf_df$vars,
  #                             "'lower'='FDL+TDL';
  #                             'upper'='HDL+RDL'")
  
  perf_rp <- perf_df[perf_df$test_acc>=95,]  # for output
  # perf_rp$vars <- recode_name(perf_rp$vars)
  perf_df$vars <- recode_name(perf_df$vars, "forward")
  
  names(perf_rp) <- names(perf_df) <- c("Model Type","Variable(s)",
                                        "Point Estimate (cm)","Range (cm)",
                                        "MAD (cm)","SEE (cm)","Test Accuracy (%)")
  perf_df$`Model Type` <- str_to_title(perf_df$`Model Type`)
  
  # package into list for single out object
  out <- list(output=perf_rp,
              appendix=perf_df)
  return(out)
}

# pred <- predict_stature(case_data, sex)  # test predict_stature
# output_df <- pred$output

# Recode variable name to full name
recode_name <- function(var, direction) {
  if (direction=="forward") {
    out <- car::recode(var,
                       "'FDL'='Femur Length';
                        'FMSB'='Femur Midshaft Breadth';
                        'FDB'='Femur Distal Breadth';
                        'TDL'='Tibia Length';
                        'TPB'='Tibia Proximal Breadth';
                        'TMSB'='Tibia Midshaft Breadth';
                        'TDB'='Tibia Distal Breadth';
                        'FBDL'='Fibula Length';
                        'HDL'='Humerus Length';
                        'HPB'='Humerus Proximal Breadth';
                        'HMSB'='Humerus Midshaft Breadth';
                        'HDB'='Humerus Distal Breadth';
                        'RDL'='Radius Length';
                        'RPB'='Radius Proximal Breadth';
                        'RMSB'='Radius Midshaft Breadth';
                        'RDB'='Radius Distal Breadth';
                        'UDL'='Ulna Length';
                        'UMSB'='Ulna Midshaft Breadth';
                        'upper'='Upper Limb Length';
                        'lower'='Lower Limb Length'")
  } else {
    out <- car::recode(var,
                       "'Femur Length'='FDL';
                        'Femur Midshaft Breadth'='FMSB';
                        'Femur Distal Breadth'='FDB';
                        'Tibia Length'='TDL';
                        'Tibia Proximal Breadth'='TPB';
                        'Tibia Midshaft Breadth'='TMSB';
                        'Tibia Distal Breadth'='TDB';
                        'Fibula Length'='FBDL';
                        'Humerus Length'='HDL';
                        'Humerus Proximal Breadth'='HPB';
                        'Humerus Midshaft Breadth'='HMSB';
                        'Humerus Distal Breadth'='HDB';
                        'Radius Length'='RDL';
                        'Radius Proximal Breadth'='RPB';
                        'Radius Midshaft Breadth'='RMSB';
                        'Radius Distal Breadth'='RDB';
                        'Ulna Length'='UDL';
                        'Ulna Midshaft Breadth'='UMSB';
                        'Upper Limb Length'='upper';
                        'Lower Limb Length'='lower'")
  }
  
  return(out)
}

# Plot chosen stature prediction and PI against all reference data
# source("R/elaine_theme.R")  # import plot theme

generate_pred_plot <- function(case_data, output_df, var, model_type, sex) {
  model_type <- tolower(model_type)  # convert back to lowercase
  var <- recode_name(var, "backward")
  
  if (var=="None") {
    out_plot <- ggplot() +
      annotate("text",
               x = 1,
               y = 1,
               size = 8,
               label = "Please pick a variable to plot.") + theme_void()
  } else {
    # import reference data, filter for sex
    df0 <- readRDS("www/data/data.rds")
    if (sex=="pooled") {
      df <- df0
    } else {
      df <- df0[df0$SEX==sex,]
    }
    
    # munge prediction
    # output_df$`Variable(s)` <- recode_name(output_df$`Variable(s)`)
    df_pred <- output_df[output_df$`Variable(s)`==var & 
                           output_df$`Model Type`==model_type,]
    df_pred$var <- case_data[[var]]
    names(df_pred)[3] <- "stature"
    range <- as.numeric(strsplit(df_pred$Range," - ")[[1]])
    df_pred$lower <- range[1]
    df_pred$upper <- range[2]
    
    # recode var for plot axis
    var_name <- recode_name(var, "forward")
    
    # initial plot with ref data, given var
    base_plot <- suppressWarnings(
      ggplot(df, aes_string(x=var, y="stature")) + 
      geom_point(color="grey50", pch=2, size=2, alpha=0.5) + elaine_theme + 
      labs(x=paste(var_name,"(mm)",sep=" "), y="Stature (cm)",
           title="Predicted Stature vs. Reference Data"))
    
    # add prediction
    out_plot <- suppressWarnings(
      base_plot + 
      geom_linerange(data=df_pred, aes(x=var, ymin=lower, ymax=upper), 
                     color="red", size=1.5) + 
      geom_point(data=df_pred, aes_string(x="var", y="stature"),
                 color="black", pch=16, size=3))
  }
  
    
  return(out_plot)
}

