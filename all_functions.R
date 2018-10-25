



#===============================================
#============remove extra libraries and packages in r
#===============================================
#this function is for converting some values that are actually NA but they are specified with the different values
#dataset: the dataset that has NAs, var_name_vector: vector for variables names
#var_na_vector: this is a vector that contains NA equivalent values associated var names in var_name_vector
#each cell in var_na_vector might have multiple values for NAs


dataset<-thoracic_data
var_name_vector<-as.character(NA_identifier$VARIABLE.NAME)
var_na_vector<-NA_identifier$nas
  
na_maker <- function(dataset,var_name_vector,var_na_vector){
i<-286
j<-232
k<-1
  for(i in 1:ncol(dataset)){
    for(j in 1:length(var_name_vector)){
      if(grepl(names(dataset[i]),var_name_vector[j])){
        print(i)
        print(j)
        if(length(as.character(var_na_vector[j]))>0){
          str(var_na_vector[j])
        for(k in 1:length(var_na_vector[j])){
          dataset[dataset[,i] == var_na_vector[j][k],i] <- NA}
        
        }
    }
  }
   
}



}





#===============================================
#============remove extra libraries and packages in r
#===============================================
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "MAC"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "MAC"
    if (grepl("linux-gnu", R.version$os))
      os <- "LINUX"
  }
  toupper(os)
}

#===============================================
lib_cleaner<-function(){
  if(!is.null(sessionInfo()$otherPkgs)){
    lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)}
  # options(warn = 0)
  return("extra libraries are removed")
}
#===============================================
#   library(caret); library(mlbench); library(gmodels);
#   #library(bnclassify);  
#   #library(mailR)
#   library(party); 
#   library(zoo); library(sandwich); 
#   #library(kernlab); 
#   #library(nnet); 
#   library(pROC); library(AUC)
#   library(plyr);  library(stringr); library(e1071);
#   library(gdata); library(ROSE);library(taRifx);library(Boruta);library(glmnet); 
#   library(Biocomb);
#   library(gRain);library(pROC)
#   # library(rio);  
#   library(mlr); library(arules);
#   #library(xlsx);
#   library(BiocGenerics);  library(graph);library(RBGL) ; library(rsig); library(rms); library(survAUC)
#   library(devtools);library(grid); library(modeltools); library(stats4); library(strucchange)
#   library(doParallel);  library(parallel)
#   #library(smotefamily)
#  # library(gbm)
# #dev.off(dev.list()["RStudioGD"])# clear plots
#_________________________________operational functions definition__________________________________
row_missing_function <- function(input_data){
  na_count_row <- apply(input_data, 1, function(y) length(which(is.na(y)))/ncol(input_data)) # counting nas in each row
  na_count_row <- data.frame(na_count_row) #transforming this into a data_frame
  return(na_count_row)
}

col_missing_function <- function(input_data){
  na_count_col <- apply(input_data, 2, function(y) length(which(is.na(y)))/nrow(input_data)) # counting nas in each column
  na_count_col <- data.frame(na_count_col) #transforming this into a data_frame
  return(na_count_col)
}

table_cleaner<-function(input_object){
  #_______________________________Required Libraries_______________________________________________________________________________________
  # Install any needed package with the following command: install.packages("Name", dependencies = c("Depends", "Suggests"))
  #comments
  set.seed(110)
  clean_folder_input<-input_object$clean_folder_input
  if(is.null(clean_folder_input)){clean_folder<-getwd()}else{clean_folder<-clean_folder_input}
  clean_file_location<-paste(clean_folder,"/clean_file.csv",sep="")
  
  #print(clean_file_location)
  
  Try_data<-input_object$data_base
  exclude_vars<-input_object$exclude_vars
  
  exclude_vars<-gsub(" ","",exclude_vars)
  exclude_vars<unlist(strsplit(exclude_vars, split=","))
  exclude_vars<-toupper(exclude_vars)
  
  NA_cells<-input_object$NA_cells
  NA_cells<-unlist(strsplit(NA_cells, split=","))
  
  for(i in 1:length(NA_cells)){
    
    Try_data[Try_data == NA_cells[i]] <- NA
    gc()}
  
  new_data<-input_object$file_new_data
  
  new_data_clean<-new_data[ , colSums(is.na(new_data)) == 0]
  Try_data<-Try_data[,colnames(new_data_clean)]
  
  paper<-matrix(0, ncol = 4, nrow = 100000)
  paper<-as.data.frame(paper)
  
  names(paper)<- c("remained_col", "column_max","remained_row","row_max")
  
  max_col<-0
  max_row<-0
  Try_data_temp <<- Try_data
  
  for(i in 1:100000){
    Try_data<-Try_data_temp
    gc()
    count_col <- col_missing_function(Try_data)
    gc()
    count_row <- row_missing_function(Try_data)
    max_col<-max(count_col)
    max_row<-max(count_row)
    max_emp<- max(max_col,max_row)
    
    if(max_col==0){break()}
    if(max_row==0){break()}
    
    if (max_emp==max_col)
    {
      if((ncol(Try_data))>2){
        f<-which(count_col$na_count_col==max_col)
        i<-length(f)
        while(i>0){
          
          if(grepl(toupper(names(Try_data[f[i]])),exclude_vars)){
            if(length(setdiff(f,f[i]))>0){
              f<-setdiff(f,f[i])}
          }
          i<-i-1
        }
        if(length(f)>0){
          Try_data_temp<<- Try_data[, -f]}else{
            
            if((nrow(Try_data))>3){
              a<-which(count_row$na_count_row==max_row)
              Try_data_temp <<- Try_data[-a,]
            }
          }
        
      }
    }
    
    if (max_emp==max_row)
    {
      if((nrow(Try_data))>3){
        a<-which(count_row$na_count_row==max_row)
        
        Try_data_temp <<- Try_data[-a,]
        
      }
    }
    
    if(i<100000){
      paper$remained_col[i]<-ncol(Try_data)
      paper$column_max[i]<-max_col
      paper$row_max[i]<-max_row
      paper$remained_row[i]<-nrow(Try_data)}
    
  }
  input_object<-list()
  input_object$data_base<-Try_data_temp
  write.csv(input_object$data_base,clean_file_location)
  print("Data Cleaning is Done! Exit the tool. Run the tool again, load the cleaned file for Data Analysis")
  
  
  return(input_object)
}

#this function yield ID columns and change variable with high numbers of categories to 5 levels
leveler<-function(data,cat_vars){
  
  data<-data[which(names(data) %in% cat_vars)]
  high_levels<-as.data.frame(apply(data, 2, function(x) nlevels(as.factor(x))))
  high_levels$rnames<-rownames(high_levels)
  high_cats<-rownames(high_levels[which((high_levels[1]>5)&(high_levels[1]<(nrow(data)/2))),])
  
  
  IDs<-rownames(high_levels[which(high_levels==nrow(data)),])
  
  if(length(IDs)>0){
    ID_cols<-data[IDs]
  }
  
  
  if(length(high_cats)>0){
    temp_col<-matrix(0, ncol = 1, nrow = nrow(data))
    temp_col<-as.data.frame(temp_col)
    
    names(temp_col)<- c("temp_col")
    temp_col<-cbind(temp_col,data[high_cats])
    
    
    new_labels<-data[high_cats]
    new_labels[,1:ncol(new_labels)]<-NA
    names(new_labels)<-paste(names(new_labels),"_new",sep="")
    
    temp_col<-cbind(temp_col,new_labels)
    
    for(j in 2: (length(high_cats)+1)){
      temp_freq<-data.frame(table(temp_col[j]))
      temp_freq$perc<-(temp_freq$Freq/nrow(data))
      temp_freq$perc_comu<-cumsum(temp_freq$perc)
      temp_freq$cat<-"A"
      
      for(i in 1:nrow(temp_freq)){
        if(temp_freq[i,"perc_comu"]>.2){temp_freq$cat[i]<-"B"}
        if(temp_freq[i,"perc_comu"]>.4){temp_freq$cat[i]<-"C"}
        if(temp_freq[i,"perc_comu"]>.6){temp_freq$cat[i]<-"D"}
        if(temp_freq[i,"perc_comu"]>.8){temp_freq$cat[i]<-"E"}
      }
      
      for(k in 1:nrow(temp_col)){
        temp_col[k,(j+length(high_cats))]<-temp_freq$cat[which(temp_freq$Var1 %in% temp_col[k,j])]
      }
      
    }
    
  }
  partitions<-list()
  partitions$ID_cols<-ID_cols
  partitions$high_col_orig<-temp_col[,2:(length(high_cats)+1)]
  partitions$high_col_new<-temp_col[,(length(high_cats)+2):ncol(temp_col)]
  
  for(i in 1:ncol(high_col_new)){
    high_col_new[,i]<-as.factor(high_col_new[,i])
  }
  return(partitions)
  
}
###

# The the invariant function

invariant_checker <- function(vector){
  vector_sort = sort(vector)
  if(head(vector_sort,1) == tail(vector_sort,1)){
    return("Two")
  }else{
    return("No")
  }
}
#_________________________
# Using Random Forrest for Finding importing features for binomial prediction
R_Forrest_bin<-function(data,target){
  dataset<-data
  dataset$TARGET<-as.factor(target)
  
  Random_Forrest.train <- Boruta(TARGET~., data = dataset, doTrace = 2)
  Random_Forrest_fs<-as.data.frame(Random_Forrest.train$finalDecision)
  names(Random_Forrest_fs)[1]<-paste("criteria")
  Random_Forrest_imp<-as.data.frame.table(subset(Random_Forrest_fs, Random_Forrest_fs[,1] == "Confirmed"))
  names(Random_Forrest_imp)[1]<-paste("variables")
  Random_Forrest_imp_tmp<-Random_Forrest_imp
  #checking to see if variables are from before 2014
  names(Random_Forrest_imp)[2]<-c("version")
  R_Forrest<-as.data.frame(Random_Forrest_imp$variables)
  colnames(R_Forrest)<-c("variables")
  return(R_Forrest)
}
#_________________________
# Using LASSO for Finding importing features for binomial prediction
##returns variables from lasso variable selection, use alpha=0 for ridge

# year0_df<-input_object$data_ind
# year0_df$TARGET<-input_object$data_targets$year0
# df<-year0_df
# yvar<-"TARGET"
# trace=F
# alpha=1
# folds=5
# str(input_object$data_ind)
# main_object<-input_object
Lasso_bin<-function(df,yvar,main_object,folds=5,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar,"~.")),data=df)
  x=x[,-1] ##remove intercept
  
  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)
  
  co<-coef(glmnet1,s = "lambda.1se")
  #co<-coef(glmnet1,s = "lambda.min")
  
  inds<-which(co[,1]!=0)
  variables<-row.names(co)[inds]
  variables<-as.data.frame(variables[!(variables %in% '(Intercept)')])
  colnames(variables)<-c("variables")
  
  seeker<-as.data.frame(names(main_object$data_ind))
  colnames(seeker)<-c("variables")
  if(nrow(variables)>0){
    variables<-var_finder(seeker,variables)}
  
  return(variables)
}
#_________________________
# year1_df<-input_object$data_ind
# year1_df$TARGET<-input_object$data_targets$year1
# data<-year1_df

FFS_bin<-function(data){
  disc<-"MDL"
  threshold=0.001
  attrs.nominal=numeric()
  FF_vars=select.fast.filter(data, disc.method=disc, threshold=threshold,
                             attrs.nominal=attrs.nominal)
  
  FF_vars$Information.Gain<-NULL
  FF_vars$NumberFeature<-NULL
  names(FF_vars)<-"variables"
  FFS<-as.data.frame(FF_vars$variables)
  colnames(FFS)<-c("variables")
  return(FFS)
}

#_________________________
# Using Machine learning algorithm for Finding importing features for binomial prediction
# x<-input_object$data_ind
# TARGET<-input_object$data_targets$year0

# x<-input_object$data_ind
# TARGET<-input_object$data_targets$year0

itself_bin<-function(x,TARGET){
  TARGET<-as.factor(TARGET)
  for(i in 1: length(levels(TARGET))){levels(TARGET)[i] <- paste("Level_",i,sep = "")}
  subsets <- c(1:ncol(round(x)/4))
  
  caretFuncs$summary <- twoClassSummary
  ctrl <- rfeControl(functions=caretFuncs, 
                     number = 1,
                     returnResamp="final", verbose = TRUE)
  
  trainctrl <- trainControl(classProbs= TRUE,
                            summaryFunction = twoClassSummary)
  
  rfeControl = rfeControl(functions = rfFuncs,
                          number = 1,
                          verbose = TRUE,
                          saveDetails = TRUE,
                          allowParallel = TRUE)
  
  data_tan<-x
  for(i in 1:ncol(data_tan)){
    if(is.numeric(data_tan[,i])){
      data_tan[,i]<-discretize(data_tan[,i],method="interval",categories = 5)
    }
  }
  
  tasks <- list(
    svm_Profile = function() rfe(x, TARGET,
                                 sizes=subsets,
                                 rfeControl=ctrl,
                                 method="svmRadial",
                                 metric = "ROC",
                                 trControl = trainctrl),
    log_Profile = function() rfe(x, TARGET,
                                 sizes=subsets,
                                 rfeControl=ctrl,
                                 method="glm",
                                 metric="ROC",
                                 trControl=trainctrl),
    CRT_Profile = function() rfe(x, TARGET,
                                 sizes=subsets,
                                 rfeControl=rfeControl,
                                 method="rpart1SE",
                                 metric="ROC",
                                 trControl=trainctrl),
    TAN_Profile = function() rfe(data_tan, TARGET,
                                 sizes=subsets,
                                 rfeControl=rfeControl,
                                 method="tan",
                                 metric="ROC",
                                 trControl=trainctrl,
                                 tuneLength = NULL),
    NNET_Profile = function() rfe(x, TARGET,
                                  sizes=subsets,
                                  rfeControl=ctrl,
                                  method="nnet",
                                  metric="ROC",
                                  trControl=trainctrl))
  
  # Using fork()
  if(input_object$parameters["platform","content"]=="MAC OS"){
    server_cores<-min((detectCores()-2),(length(tasks)))
    out <- mclapply( 
      tasks, 
      function(f) f(), 
      mc.cores = server_cores
    )
  }
  
  # Using socket()
  if(input_object$parameters["platform","content"]=="Windows"){
    r_cores<-detectCores(all.tests = FALSE, logical = TRUE)-1
    cl <- makeCluster( min(length(tasks),r_cores ))
    out <- clusterApply( 
      cl,
      tasks,
      function(f) f()
    )
    getDoParWorkers()
    stopCluster(cl)
  }
  
  svm_itself<-as.data.frame(out$svm_Profile$optVariables)
  colnames(svm_itself)<-c("variables")
  
  log_itself<-as.data.frame(out$log_Profile$optVariables)
  colnames(log_itself)<-c("variables")
  
  CRT_itself<-as.data.frame(out$CRT_Profile$optVariables)
  colnames(CRT_itself)<-c("variables")
  
  TAN_itself<-as.data.frame(out$TAN_Profile$optVariables)
  colnames(TAN_itself)<-c("variables")
  
  NNET_itself<-as.data.frame(out$NNET_Profile$optVariables)
  colnames(NNET_itself)<-c("variables")
  
  features_itself<-list()
  features_itself$svm_Profile<-svm_itself
  features_itself$log_Profile<-log_itself
  features_itself$CRT_Profile<-CRT_itself
  features_itself$TAN_Profile<-TAN_itself
  features_itself$NNET_Profile<-NNET_itself
  return(features_itself)
}

#_________________________
# binomial prediction function

# data<-input_object$data_ind
# TARGET<-input_object$data_targets$year6
# year<-"year6"
# output_type<-"numerics"

pred_bin<-function(input_object,output_type,year){
  
  TARGET<-eval(parse(text = paste("input_object$data_targets$",year,sep = "")))
  
  year_picked<-substr(year, 5, 10)
  var_years<-eval(parse(text = paste("input_object$bin_features$consolidated",year_picked,"$variables",sep = "")))
  data<-input_object$data_pred[var_years]
  data$id<-input_object$data_pred$id
  data$TARGET<-as.factor(TARGET)
  
  
  balanc_proc<-input_object$parameters["balanc_proc","content"]
  n_folds<-as.numeric(input_object$parameters["n_folds","content"])
  B_alg<-input_object$parameters["B_alg","content"]
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  method_fs<-input_object$parameters["method_fs","content"]
  is_multi<-input_object$parameters["is_multi","content"]
  is_bal<-input_object$parameters["is_bal","content"]
  
  log_imp_folds_file<-input_object$settings["log_imp_folds_file","content"]
  svm_imp_folds_file<-input_object$settings["svm_imp_folds_file","content"]
  CRT_imp_folds_file<-input_object$settings["CRT_imp_folds_file","content"]
  NNET_imp_folds_file<-input_object$settings["NNET_imp_folds_file","content"]
  tan_imp_folds_file<-input_object$settings["tan_imp_folds_file","content"]
  
  log_perf_folds_file<-input_object$settings["log_perf_folds_file","content"]
  svm_perf_folds_file<-input_object$settings["svm_perf_folds_file","content"]
  CRT_perf_folds_file<-input_object$settings["CRT_perf_folds_file","content"]
  NNET_perf_folds_file<-input_object$settings["NNET_perf_folds_file","content"]
  tan_perf_folds_file<-input_object$settings["tan_perf_folds_file","content"]
  
  Avg_log_perf_folds_file<-input_object$settings["Avg_log_perf_folds_file","content"]
  Avg_svm_perf_folds_file<-input_object$settings["Avg_svm_perf_folds_file","content"]
  Avg_CRT_perf_folds_file<-input_object$settings["Avg_CRT_perf_folds_file","content"]
  Avg_NNET_perf_folds_file<-input_object$settings["Avg_NNET_perf_folds_file","content"]
  Avg_tan_perf_folds_file<-input_object$settings["Avg_tan_perf_folds_file","content"]
  
  levels(data$TARGET)[1] <- "One"
  levels(data$TARGET)[2] <- "Two"
  
  
  #____________________making the n folds_____________________________________
  
  Train_Two_No<-length(which(data$TARGET=="Two"))
  Train_One_No<-length(which(data$TARGET=="One"))
  
  # Create the training and testing data sets
  
  kept_rows_Train_Two<-floor(Train_Two_No/n_folds)*n_folds
  kept_rows_Train_One<-floor(Train_One_No/n_folds)*n_folds
  
  Train_Two <- data[ which(data$TARGET=="Two"), ]
  Train_One <- data[ which(data$TARGET=="One"), ]
  
  #shuffling
  Train_Two<-Train_Two[sample(nrow(Train_Two),kept_rows_Train_Two),]
  Train_One<-Train_One[sample(nrow(Train_One),kept_rows_Train_One),]
  
  #naming the folds ID
  folds_Train_Two <- cut(seq(1,nrow(Train_Two)),breaks=n_folds,labels=FALSE)
  folds_Train_One <- cut(seq(1,nrow(Train_One)),breaks=n_folds,labels=FALSE)
  #
  Train_Two$folds<-folds_Train_Two
  Train_One$folds<-folds_Train_One
  #
  Train<-rbind(Train_Two,Train_One)
  
  ##### end of modeling for ten folds
  
  #____________setup ensemble modeling ____
  
  control_<- trainControl(method = "none",  savePredictions = TRUE,
                          verboseIter = FALSE,returnResamp = "all",classProbs = TRUE, summaryFunction = twoClassSummary)
  
  rows_no<-floor(nrow(Train)/n_folds)
  
  resul_log_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_log_raw<-as.data.frame(resul_log_raw)
  
  resul_log_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_log_prob<-as.data.frame(resul_log_prob)
  
  resul_svm_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_svm_raw<-as.data.frame(resul_svm_raw)
  
  resul_svm_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_svm_prob<-as.data.frame(resul_svm_prob)
  
  resul_CRT_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_CRT_raw<-as.data.frame(resul_CRT_raw)
  
  resul_CRT_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_CRT_prob<-as.data.frame(resul_CRT_prob)
  
  resul_NNET_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_NNET_raw<-as.data.frame(resul_NNET_raw)
  
  resul_NNET_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_NNET_prob<-as.data.frame(resul_NNET_prob)
  
  resul_tan_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_tan_raw<-as.data.frame(resul_tan_raw)
  
  resul_tan_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_tan_prob<-as.data.frame(resul_tan_prob)
  
  resul_fusion_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
  resul_fusion_prob<-as.data.frame(resul_fusion_prob)
  
  resul_log_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
  resul_log_imp<-as.data.frame(resul_log_imp)
  names(resul_log_imp) <- paste("fold", 1:n_folds, sep = "")
  
  resul_svm_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
  resul_svm_imp<-as.data.frame(resul_svm_imp)
  names(resul_svm_imp) <- paste("fold", 1:n_folds, sep = "")
  
  resul_CRT_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
  resul_CRT_imp<-as.data.frame(resul_CRT_imp)
  names(resul_CRT_imp) <- paste("fold", 1:n_folds, sep = "")
  
  resul_NNET_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
  resul_NNET_imp<-as.data.frame(resul_NNET_imp)
  names(resul_NNET_imp) <- paste("fold", 1:n_folds, sep = "")
  
  resul_tan_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
  resul_tan_imp<-as.data.frame(resul_tan_imp)
  names(resul_tan_imp) <- paste("fold", 1:n_folds, sep = "")
  
  rownames(resul_log_imp)<-colnames(data[,1:ncol(data)-1])
  resul_log_imp$variables<-colnames(data[,1:ncol(data)-1])
  rownames(resul_svm_imp)<-colnames(data[,1:ncol(data)-1])
  resul_svm_imp$variables<-colnames(data[,1:ncol(data)-1])
  rownames(resul_CRT_imp)<-colnames(data[,1:ncol(data)-1])
  resul_CRT_imp$variables<-colnames(data[,1:ncol(data)-1])
  rownames(resul_NNET_imp)<-colnames(data[,1:ncol(data)-1])
  resul_NNET_imp$variables<-colnames(data[,1:ncol(data)-1])
  rownames(resul_tan_imp)<-colnames(data[,1:ncol(data)-1])
  resul_tan_imp$variables<-colnames(data[,1:ncol(data)-1])
  
  vars<-names(data[,1:(ncol(data)-2)])
  tan_vars<-vars
  formula_format<-paste("TARGET ~ ",paste(vars, collapse="+"),sep = "")
  formula<-as.formula(formula_format)
  
  #border
  #_______model training and testing_____
  
  
  #dir.create(paste(c(analysis_folder,"balanced data/"),collapse = ""))
  
  #begining of predicting part__
  #i<-10
  major_data<-list()
  all_models<-list()
  runnable<-list()
  
  # Recording train and test folds
  # Train$id<-as.numeric(rownames(Train))
  # major_data$data_folded<-Train
  CRT_counter<-rep(0, n_folds)
  log_counter<-rep(0, n_folds)
  svm_counter<-rep(0, n_folds)
  NNET_counter<-rep(0, n_folds)
  tan_counter<-rep(0, n_folds)
  
  for ( i in 1: n_folds){
    test_data<-Train[Train$folds == i,]
    train_data<-Train[Train$folds != i,]
    
    # Recording the row id of the data if there is no sample balancing like RUS/SMOTE
    
    
    obs_id<-paste(c("data_id_fold_",i),collapse = "")
    major_data[[obs_id]]<-test_data$id
    
    
    tt<-as.data.frame(table(train_data$TARGET))
    #table(train_data$TARGET)
    common<-tt$Freq[1]
    rare<-tt$Freq[2]
    k<-1
    if(common<rare){
      temp_<-common
      common<-rare
      rare<-temp_
    }
    if(B_alg=="RUS"){
      train_data<-RUS_func(train_data)
      
      # Recording the row id of RUS data
      
      RUS_fold<-paste(c("Train_RUS_id_fold_",i),collapse = "")
      major_data[[RUS_fold]]<-train_data$id
    }
    
    if(B_alg=="SMOTE"){
      train_data$TARGET<-as.factor(train_data$TARGET)
      H_por_low<-1
      H_por_high<-1
      H_por_low<-(1/H_por_low)
      H_por_high<-H_por_high*100
      perc_over<-100*(common-rare)/(rare*H_por_low)*k
      perc_under<-100*(1/perc_over)*common
      smoted_data<-DMwR::SMOTE(TARGET ~ ., train_data, perc.over = perc_over,perc.under = (((perc_over/100)+1)/(perc_over/100))*H_por_high)
      SMOTE_synthesized<-smoted_data[(nrow(train_data)+1):nrow(smoted_data),]
      train_data<-smoted_data[complete.cases(smoted_data), ]
      # Recording the added smoted data
      
      synthetic_data<-paste(c("Train_SMOTE_synthesized_fold_",i),collapse = "")
      major_data[[synthetic_data]]<-SMOTE_synthesized
    }
    
    balanced_data_train_add<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_bal,"_",year,"_fold-",i,"_","balanced-train.csv"),collapse = "")
    balanced_data_test_add<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_bal,"_",year,"_fold-",i,"_","balanced-test.csv"),collapse = "")
    
    #write.csv(train_data,balanced_data_train_add)
    #write.csv(test_data,balanced_data_test_add)
    
    
    # # keeping the record of Target distribution in each fold
    # dis_train<-paste(c("K_",i),collapse = "")
    # input_object$folds_dist[[dis_train]]<-table(train_data$TARGET)
    
    # Recording address of train and test folds
    
    name_train_add<-paste(c("train_fold_add_",i),collapse = "")
    major_data[[name_train_add]]<-balanced_data_train_add
    
    name_test_add<-paste(c("test_fold_add_",i),collapse = "")
    major_data[[name_test_add]]<-balanced_data_test_add
    
    # train_data and test data distribution
    name_dist<-paste(c("fold_dist_",i),collapse = "")
    major_data[[name_dist]]<-table(train_data$TARGET)
    
    #===================
    
    mod_log <-caret::train(formula,  data=train_data, method="glm", family="binomial",
                           trControl = control_, tuneLength = 10, metric="ROC")
    
    mod_CRT <-caret::train(formula,  data=train_data, method="rpart1SE",
                           trControl = control_, tuneLength = 10, metric="ROC")
    
    # if(input_object$parameters["tuning","content"]=="Yes"){
    #   control_tune_svm <- trainControl(method="repeatedcv", number=5, repeats=5)
    #   grid <- expand.grid(C=seq(.2,5,.1), sigma=seq(.000005,.0001,.00001))
    #   model_tune_svm <- train(TARGET~., data=data, method="svmRadial", trControl=control_tune_svm, tuneGrid=grid)
    #   svm_sigma<-as.numeric(model_tune_svm$bestTune$sigma)
    #   svm_cost<-as.numeric(model_tune_svm$bestTune$C)}else{
    #     svm_cost<-.5
    #     svm_sigma<-.0001
    #   }
    svm_cost<-.5
    svm_sigma<-.0001
    
    mod_svm<-caret::train(formula,  data=train_data, method="svmRadial", family="binomial",
                          trControl = control_,  tuneGrid=expand.grid(C=svm_cost, sigma=svm_sigma),
                          tuneLength = 10)
    
    
    mod_NNET<-caret::train(formula,  data=train_data, method="nnet", family="binomial",
                           trControl = control_,  tuneGrid=expand.grid(size=5, decay=0.1), MaxNWts=20000,
                           tuneLength = 10)
    
    #remove.packages("fastAdaboost")
    #remove.packages("adabag")
    # mod_svm$times
    # library(fastAdaboost)
    # library(adabag)
    # ada_boosting <- adaboost(formula, train_data, 300)
    # ada_boosting <- real_adaboost(formula, train_data, 300)
    # 
    # ada_bost_target<-as.factor(train_data$TARGET)
    # levels(ada_bost_target)[1] <- -1
    # levels(ada_bost_target)[2]  <- 1
    # indipenent<-train_data[vars]
    # data <- indipenent[1:100,1:20 ]
    # targ<-train_data$TARGET[complete.cases(train_data$TARGET)]
    # length(targ)
    # targ<-targ[1:100]
    # 
    # ada<- adaboost(train_data[1:10], train_data$TARGET, tree_depth = 12,
    #                n_rounds = 100, verbose = TRUE)
    # ada<- adaboost(train_data[vars], train_data$TARGET, tree_depth = 12,
    #                n_rounds = 100, verbose = TRUE)
    # 
    # mfinal <- 10
    # maxdepth <- 5
    # ada<- boosting(formula,data=train_data,mfinal=mfinal, coeflearn="Breiman",
    #                              control=rpart.control(maxdepth=maxdepth))
    # 
    # 
    # ada<-predict(ada_boosting, newdata=test_data, type="raw")
    # log_test<-predict(mod_log, newdata=test_data, type="raw")
    # 
    # 
    # performance_ada_sen<-caret:: sensitivity(ada$class,test_data$TARGET)
    # performance_ada_spe<-caret:: specificity(ada$class,test_data$TARGET)
    # performance_ada_acu<-(as.data.frame(confusionMatrix(ada$class,test_data$TARGET)$overall))[1,]
    # 
    # performance_log_sen<-caret:: sensitivity(log_test,test_data$TARGET)
    # performance_log_spe<-caret:: specificity(log_test,test_data$TARGET)
    # performance_log_acu<-(as.data.frame(confusionMatrix(log_test,test_data$TARGET)$overall))[1,]
    # 
    
    
    
    
    ##===making data ready for TAN
    # disceretizing train and test
    
    data_tan_train<-train_data
    y_tr<-data_tan_train$TARGET
    data_tan_train$TARGET<-NULL
    data_tan_train$folds<-NULL
    data_tan_train$type<-"tr"
    
    data_tan_test<-test_data
    y_ts<-data_tan_test$TARGET
    data_tan_test$TARGET<-NULL
    data_tan_test$folds<-NULL
    data_tan_test$type<-"ts"
    data_tan<-rbind(data_tan_train,data_tan_test)
    
    
    
    for(j in 1:(ncol(data_tan)-1)){
      if(is.numeric(data_tan_train[,j])){if(is.numeric(data_tan[,j])){
        
        tester<-as.data.frame(data_tan[,j])
        if(tester[1,1]!=tester[nrow(data_tan),1]){
          data_tan[,j]<-discretize(data_tan[,j],method="interval",categories = floor(log(nrow(data_tan))))}else{
            data_tan[,j]<-discretize(data_tan[,j],method="interval",categories = 1)
          }
        
      }
        
      }
    }
    
    
    data_tan_train<-data_tan[which(data_tan$type=="tr"),]
    data_tan_test<-data_tan[which(data_tan$type=="ts"),]
    data_tan_train$type<-NULL
    data_tan_test$type<-NULL
    
    data_tan_train<-data_tan_train[, which(names(data_tan_train) %in% tan_vars)]
    mod_tan <- caret::train(data_tan_train, y_tr, method="tan",
                            trControl = control_, tuneGrid = expand.grid(score= "aic", smooth=0.5),
                            tuneLength = 10)
    
    
    VarsData_Phase<-as.data.frame(colnames(input_object$data_base))
    names(VarsData_Phase)<-c("variables")
    
    
    if(!is.null(mod_log$finalModel)){log_counter[i]<-1}
    if(!is.null(mod_svm$finalModel)){svm_counter[i]<-1}
    if(!is.null(mod_CRT$finalModel)){CRT_counter[i]<-1}
    if(!is.null(mod_NNET$finalModel)){NNET_counter[i]<-1}
    if(!is.null(mod_tan$finalModel)){tan_counter[i]<-1}
    
    
    if(log_counter[i]==1){
      #for avoiding errors, I added the next code
      if(class(try(varImp(mod_log),silent = TRUE))!="try-error"){
        resul_log_imp<-matrix_filler(i,resul_log_imp,model_imp_exc(mod_log,VarsData_Phase),"variables")}
      resul_log_raw[,i]<-predict(mod_log, newdata=test_data, type="raw")
      resul_log_prob[,i]<-predict(mod_log, newdata=test_data, type="prob")}
    
    if(svm_counter[i]==1){
      #for avoiding errors, I added the next code
      if(class(try(varImp(mod_svm),silent = TRUE))!="try-error"){
        resul_svm_imp<-matrix_filler(i,resul_svm_imp,model_imp_exc(mod_svm,VarsData_Phase),"variables")}
      resul_svm_raw[,i]<-predict(mod_svm, newdata=test_data, type="raw")
      resul_svm_prob[,i]<-predict(mod_svm, newdata=test_data, type="prob")}
    
    if(CRT_counter[i]==1){
      if(class(try(varImp(mod_CRT),silent = TRUE))!="try-error"){
        resul_CRT_imp<-matrix_filler(i,resul_CRT_imp,model_imp_exc(mod_CRT,VarsData_Phase),"variables")}
      resul_CRT_raw[,i]<-predict(mod_CRT, newdata=test_data, type="raw")
      resul_CRT_prob[,i]<-predict(mod_CRT, newdata=test_data, type="prob")}
    
    if(NNET_counter[i]==1){
      #for avoiding errors, I added the next code
      if(class(try(varImp(mod_NNET),silent = TRUE))!="try-error"){
        resul_NNET_imp<-matrix_filler(i,resul_NNET_imp,model_imp_exc(mod_NNET,VarsData_Phase),"variables")}
      resul_NNET_raw[,i]<-predict(mod_NNET, newdata=test_data, type="raw")
      resul_NNET_prob[,i]<-predict(mod_NNET, newdata=test_data, type="prob")}
    # detach("package:Biocomb", unload=TRUE)
    # detach("package:Boruta", unload=TRUE)
    # detach("package:xlsx", unload=TRUE)
    # detach("package:Boruta", unload=TRUE)
    # detach("package:Biocomb", unload=TRUE)
    # detach("package:Boruta", unload=TRUE)
    # detach("package:rio", unload=TRUE)
    # detach("package:party", unload=TRUE)
    if(tan_counter[i]==1){
      #for avoiding errors, I added the next code
      if(class(try(varImp(mod_tan),silent = TRUE))!="try-error"){
        resul_tan_imp<-matrix_filler(i,resul_tan_imp,model_imp_exc(mod_tan,VarsData_Phase),"variables")}
      resul_tan_raw[,i]<-predict(mod_tan, newdata=data_tan_test, type="raw")
      resul_tan_prob[,i]<-predict(mod_tan, newdata=data_tan_test, type="prob")}
    
    # recording trained models it is too big so I didn't report it
    # if(output_type=="models"){
    #   
    #   test_log<-mod_log
    #   test_svm<-mod_svm
    #   test_CRT<-mod_CRT
    #   test_tan<-mod_tan
    #   test_NNET<-mod_NNET
    #   
    #   #reducing size of logarithm model
    #   {
    #     test_log$trainingData<-NULL
    #     #test_log$terms<-NULL
    #     test_log$control$index<-NULL
    #     test_log$coefnames<-NULL
    #     test_log$xlevels<-NULL
    #     test_log$times<-NULL
    #     test_log$levels<-NULL
    #     test_log$yLimits<-NULL
    #     test_log$maximize<-NULL
    #     test_log$perfNames<-NULL
    #     test_log$resampledCM<-NULL
    #     test_log$resample<-NULL
    #     test_log$preProcess<-NULL
    #     
    #     test_log$dots<-NULL
    #     test_log$metric<-NULL
    #     test_log$call<-NULL
    #     test_log$bestTune<-NULL
    #     
    #     test_log$results<-NULL
    #     test_log$pred<-NULL
    #     #test_log$modelInfo<-NULL
    #     test_log$method<-NULL
    #     test_log$control<-NULL
    #     test_log$finalModel$R<-NULL
    #     test_log$finalModel$data<-NULL
    #     test_log$finalModel$qr$qr<-NULL
    #     test_log$finalModel$tuneValue<-NULL
    #     test_log$finalModel$model<-NULL
    #     test_log$finalModel$param<-NULL
    #     # test_log$finalModel$obsLevels<-NULL
    #     test_log$finalModel$problemType<-NULL
    #     test_log$finalModel$xNames<-NULL
    #     test_log$finalModel$xlevels<-NULL
    #     test_log$finalModel$contrasts<-NULL
    #     test_log$finalModel$method<-NULL
    #     test_log$finalModel$control<-NULL
    #     test_log$finalModel$offset<-NULL
    #     # test_log$finalModel$terms<-NULL
    #     test_log$finalModel$converged<-NULL
    #     test_log$finalModel$y<-NULL
    #     test_log$finalModel$boundary<-NULL
    #     test_log$finalModel$df.null<-NULL
    #     test_log$finalModel$df.residual<-NULL
    #     test_log$finalModel$prior.weights<-NULL
    #     test_log$finalModel$weights<-NULL
    #     test_log$finalModel$iter<-NULL
    #     test_log$finalModel$null.deviance<-NULL
    #     test_log$finalModel$aic<-NULL
    #     test_log$finalModel$deviance<-NULL
    #     # test_log$finalModel$rank<-NULL
    #     test_log$finalModel$effects<-NULL
    #     test_log$finalModel$fitted.values<-NULL
    #     test_log$finalModel$residuals<-NULL
    #     test_log$finalModel$linear.predictors<-NULL
    #     test_log$finalModel$family$link<-NULL
    #     test_log$finalModel$family$initialize<-NULL
    #     test_log$finalModel$family$family<-NULL
    #     test_log$finalModel$formula<-NULL
    #     #test_log$finalModel$coefficients<-NULL
    #     test_log$finalModel$qr$qraux<-NULL
    #     test_log$finalModel$qr$tol<-NULL
    #     #test_log$finalModel$qr$pivot<-NULL
    #     test_log$finalModel$qr$rank<-NULL
    #     test_log$finalModel$family$aic<-NULL
    #     test_log$finalModel$family$simulate<-NULL
    #     test_log$finalModel$family$valideta<-NULL
    #     test_log$finalModel$family$validmu<-NULL
    #     test_log$finalModel$family$mu.eta<-NULL
    #     test_log$finalModel$family$dev.resids<-NULL
    #     test_log$finalModel$family$variance<-NULL
    #     #test_log$finalModel$family$linkinv<-NULL
    #     test_log$finalModel$family$linkfun<-NULL
    #     test_log$modelInfo$label<-NULL
    #     test_log$modelInfo$parameters<-NULL
    #     test_log$modelInfo$library<-NULL
    #     test_log$modelInfo$type<-NULL
    #     test_log$modelInfo$loop<-NULL
    #     test_log$modelInfo$tags<-NULL
    #     test_log$modelInfo$sort<-NULL
    #     test_log$modelInfo$trim<-NULL
    #     test_log$modelInfo$levels<-NULL
    #     test_log$modelInfo$varImp<-NULL
    #     # test_log$modelInfo$prob<-NULL
    #     test_log$modelInfo$grid<-NULL
    #     test_log$modelInfo$fit<-NULL
    #     test_log$modelInfo$predict<-NULL
    #     test_log$modelInfo$predictors<-NULL
    #   }
    #   ######
    #   
    #   #reducing size of CRT model
    #   {
    #     test_CRT$xlevels<-NULL
    #     test_CRT$coefnames<-NULL
    #     test_CRT$levels<-NULL
    #     test_CRT$yLimits<-NULL
    #     test_CRT$maximize<-NULL
    #     test_CRT$perfNames<-NULL
    #     test_CRT$resampledCM<-NULL
    #     test_CRT$resample<-NULL
    #     test_CRT$times<-NULL
    #     test_CRT$trainingData<-NULL
    #     test_CRT$preProcess<-NULL
    #     test_CRT$control<-NULL
    #     test_CRT$metric<-NULL
    #     test_CRT$dots<-NULL
    #     test_CRT$call<-NULL
    #     test_CRT$bestTune<-NULL
    #     test_CRT$pred<-NULL
    #     test_CRT$results<-NULL
    #     test_CRT$method<-NULL
    #     test_CRT$modelInfo$sort<-NULL
    #     test_CRT$modelInfo$label<-NULL
    #     test_CRT$modelInfo$library<-NULL
    #     test_CRT$modelInfo$type<-NULL
    #     test_CRT$modelInfo$parameters<-NULL
    #     test_CRT$modelInfo$grid<-NULL
    #     test_CRT$modelInfo$loop<-NULL
    #     test_CRT$modelInfo$fit<-NULL
    #     test_CRT$modelInfo$predict<-NULL
    #     test_CRT$modelInfo$predictors<-NULL
    #     test_CRT$modelInfo$varImp<-NULL
    #     test_CRT$modelInfo$levels<-NULL
    #     test_CRT$modelInfo$trim<-NULL
    #     test_CRT$modelInfo$notes<-NULL
    #     test_CRT$modelInfo$tags<-NULL
    #     # test_CRT$finalModel$frame<-NULL
    #     test_CRT$finalModel$where<-NULL
    #     test_CRT$finalModel$call<-NULL
    #     test_CRT$finalModel$cptable<-NULL
    #     test_CRT$finalModel$method<-NULL
    #     test_CRT$finalModel$parms<-NULL
    #     test_CRT$finalModel$control<-NULL
    #     test_CRT$finalModel$functions<-NULL
    #     test_CRT$finalModel$numresp<-NULL
    #     # test_CRT$finalModel$splits<-NULL
    #     test_CRT$finalModel$variable.importance<-NULL
    #     test_CRT$finalModel$y<-NULL
    #     test_CRT$finalModel$ordered<-NULL
    #     test_CRT$finalModel$xNames<-NULL
    #     test_CRT$finalModel$problemType<-NULL
    #     test_CRT$finalModel$tuneValue<-NULL
    #     test_CRT$finalModel$param<-NULL
    #   }
    #   #######
    #   
    #   #reducing size of SVM model
    #   {
    #     test_svm$xlevels<-NULL
    #     test_svm$coefnames<-NULL
    #     test_svm$levels<-NULL
    #     test_svm$times<-NULL
    #     test_svm$yLimits<-NULL
    #     test_svm$maximize<-NULL
    #     test_svm$perfNames<-NULL
    #     test_svm$resampledCM<-NULL
    #     test_svm$resample<-NULL
    #     test_svm$trainingData<-NULL
    #     test_svm$preProcess<-NULL
    #     test_svm$control<-NULL
    #     test_svm$metric<-NULL
    #     test_svm$dots<-NULL
    #     test_svm$call<-NULL
    #     test_svm$bestTune<-NULL
    #     test_svm$pred<-NULL
    #     test_svm$results<-NULL
    #     test_svm$method<-NULL
    #     test_svm$modelInfo$label<-NULL
    #     test_svm$modelInfo$library<-NULL
    #     test_svm$modelInfo$type<-NULL
    #     test_svm$modelInfo$parameters<-NULL
    #     test_svm$modelInfo$grid<-NULL
    #     test_svm$modelInfo$loop<-NULL
    #     test_svm$modelInfo$fit<-NULL
    #     test_svm$modelInfo$predict<-NULL
    #     test_svm$modelInfo$predictors<-NULL
    #     # test_svm$modelInfo$tags<-NULL
    #     # test_svm$modelInfo$levels<-NULL
    #     #attr(test_svm$finalModel,"xmatrix")<-0
    #   }
    #   ##########
    #   
    #   #reducing size of NNET model
    #   {
    #     test_NNET$trainingData<-NULL
    #     test_NNET$results<-NULL
    #     test_NNET$method<-NULL
    #     test_NNET$pred<-NULL
    #     test_NNET$bestTune<-NULL
    #     test_NNET$call<-NULL
    #     test_NNET$dots<-NULL
    #     test_NNET$metric<-NULL
    #     test_NNET$control<-NULL
    #     test_NNET$preProcess<-NULL
    #     test_NNET$preProcess<-NULL
    #     test_NNET$resample<-NULL
    #     test_NNET$resampledCM<-NULL
    #     test_NNET$perfNames<-NULL
    #     test_NNET$maximize<-NULL
    #     test_NNET$yLimits<-NULL
    #     test_NNET$times<-NULL
    #     test_NNET$levels<-NULL
    #     test_NNET$coefnames<-NULL
    #     test_NNET$xlevels<-NULL
    #     test_NNET$modelInfo$label<-NULL
    #     test_NNET$modelInfo$library<-NULL
    #     test_NNET$modelInfo$loop<-NULL
    #     test_NNET$modelInfo$type<-NULL
    #     test_NNET$modelInfo$parameters<-NULL
    #     test_NNET$modelInfo$grid<-NULL
    #     test_NNET$modelInfo$fit<-NULL
    #     test_NNET$modelInfo$predict<-NULL
    #     test_NNET$modelInfo$varImp<-NULL
    #     test_NNET$modelInfo$tags<-NULL
    #     test_NNET$modelInfo$predictors<-NULL
    #     test_NNET$modelInfo$levels<-NULL
    #     test_NNET$modelInfo$sort<-NULL
    #     test_NNET$finalModel$tuneValue<-NULL
    #     # test_NNET$finalModel$n<-NULL
    #     # test_NNET$finalModel$nunits<-NULL
    #     # test_NNET$finalModel$nconn<-NULL
    #     # test_NNET$finalModel$conn<-NULL
    #     # test_NNET$finalModel$nsunits<-NULL
    #     # test_NNET$finalModel$decay<-NULL
    #     # test_NNET$finalModel$entropy<-NULL
    #     test_NNET$finalModel$param<-NULL
    #     test_NNET$finalModel$xlevels<-NULL
    #     test_NNET$finalModel$residuals<-NULL
    #     test_NNET$finalModel$fitted.values<-NULL
    #   }
    #   ##############
    #   
    #   #reducing size of tan model
    #   {
    #     test_tan$method<-NULL
    #     test_tan$results<-NULL
    #     test_tan$pred<-NULL
    #     test_tan$bestTune<-NULL
    #     test_tan$call<-NULL
    #     test_tan$dots<-NULL
    #     test_tan$metric<-NULL
    #     test_tan$control<-NULL
    #     test_tan$preProcess<-NULL
    #     test_tan$trainingData<-NULL
    #     test_tan$resample<-NULL
    #     test_tan$resampledCM<-NULL
    #     test_tan$yLimits<-NULL
    #     test_tan$perfNames<-NULL
    #     test_tan$maximize<-NULL
    #     test_tan$levels<-NULL
    #     test_tan$results<-NULL
    #     test_tan$bestTune<-NULL
    #     test_tan$call<-NULL
    #     test_tan$method<-NULL
    #     test_tan$pred<-NULL
    #     test_tan$modelInfo$label<-NULL
    #     test_tan$modelInfo$library<-NULL
    #     test_tan$modelInfo$type<-NULL
    #     test_tan$modelInfo$parameters<-NULL
    #     test_tan$modelInfo$grid<-NULL
    #     test_tan$modelInfo$loop<-NULL
    #     test_tan$modelInfo$fit<-NULL
    #     test_tan$modelInfo$predict<-NULL
    #     test_tan$modelInfo$predictors<-NULL
    #     test_tan$modelInfo$levels<-NULL
    #     test_tan$modelInfo$tags<-NULL
    #     test_tan$modelInfo$sort<-NULL
    #     test_tan$modelInfo$notes<-NULL
    #     test_tan$times<-NULL
    #     #test_tan$finalModel$xNames<-NULL
    #     test_tan$finalModel$problemType<-NULL
    #     # test_tan$finalModel$param<-NULL
    #     test_tan$finalModel$.dag<-NULL
    #     # test_tan$finalModel$.class<-NULL
    #     # test_tan$finalModel$.families<-NULL
    #     test_tan$finalModel$.call_struct<-NULL
    #     # test_tan$finalModel$.params<-NULL
    #     test_tan$finalModel$.call_bn<-NULL
    #     test_tan$finalModel$tuneValue<-NULL
    #   }
    #   ###########
    #   if(log_counter[i]==1){
    #     log_fold<-paste(c("log_fold_",i),collapse = "")
    #     all_models[[log_fold]]<-test_log}else{all_models[[log_fold]]<-"k"}
    #   
    #   if(CRT_counter[i]==1){
    #     CRT_fold<-paste(c("CRT_fold_",i),collapse = "")
    #     all_models[[CRT_fold]]<-test_CRT}else{all_models[[CRT_fold]]<-"k"}
    #   
    #   if(svm_counter[i]==1){
    #     svm_fold<-paste(c("svm_fold_",i),collapse = "")
    #     all_models[[svm_fold]]<-test_svm}else{all_models[[svm_fold]]<-"k"}
    #   
    #   if(NNET_counter[i]==1){
    #     NNET_fold<-paste(c("NNET_fold_",i),collapse = "")
    #     all_models[[NNET_fold]]<-test_NNET}else{all_models[[NNET_fold]]<-"k"}
    #   
    #   if(tan_counter[i]==1){
    #     tan_fold<-paste(c("tan_fold_",i),collapse = "")
    #     all_models[[tan_fold]]<-test_tan}else{all_models[[tan_fold]]<-"k"}
    # }
  }
  #end of predicting part
  
  # write.csv(resul_log_imp,log_imp_folds_file)
  # write.csv(resul_svm_imp,svm_imp_folds_file)
  # if(run_CRT=="Yes"){
  # write.csv(resul_CRT_imp,CRT_imp_folds_file)}
  # write.csv(resul_NNET_imp,NNET_imp_folds_file)
  # write.csv(resul_tan_imp,tan_imp_folds_file)
  
  
  #___________Specifying Tables for YES and NO probabilities_____________
  if(sum(log_counter)==n_folds){resul_log_prob_YES<-1-resul_log_prob
  performance_log<-matrix(0, ncol = n_folds, nrow = 4)
  performance_log<-as.data.frame(performance_log)
  row.names(performance_log) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
  names(performance_log)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
  }
  
  if(sum(svm_counter)==n_folds){resul_svm_prob_YES<-1-resul_svm_prob
  performance_svm<-matrix(0, ncol = n_folds, nrow = 4)
  performance_svm<-as.data.frame(performance_svm)
  row.names(performance_svm) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
  names(performance_svm)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
  }
  
  if(sum(CRT_counter)==n_folds){resul_CRT_prob_YES<-1-resul_CRT_prob
  performance_CRT<-matrix(0, ncol = n_folds, nrow = 4)
  performance_CRT<-as.data.frame(performance_CRT)
  row.names(performance_CRT) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
  names(performance_CRT)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
  }
  
  if(sum(NNET_counter)==n_folds){resul_NNET_prob_YES<-1-resul_NNET_prob
  performance_NNET<-matrix(0, ncol = n_folds, nrow = 4)
  performance_NNET<-as.data.frame(performance_NNET)
  row.names(performance_NNET) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
  names(performance_NNET)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
  }
  
  if(sum(tan_counter)==n_folds){resul_tan_prob_YES<-1-resul_tan_prob
  performance_tan<-matrix(0, ncol = n_folds, nrow = 4)
  performance_tan<-as.data.frame(performance_tan)
  row.names(performance_tan) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
  names(performance_tan)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
  }
  
  #______a change just for calculating accuracy
  
  levels(Train$TARGET)[1] <- 0
  levels(Train$TARGET)[2]  <- 1
  
  #_________________________calculating the performance of the models_____________
  #i<-1
  
  for (i in 1:n_folds){
    
    temp<-Train[Train$folds == i,]
    
    levels(temp$TARGET)[1] <- "One"
    levels(temp$TARGET)[2]  <- "Two"
    test_table<-temp$TARGET
    
    if(log_counter[i]==1){
      performance_log[1,i]<-AUC:: auc(roc(resul_log_prob_YES[,i],test_table))
      performance_log[3,i]<-caret:: sensitivity(resul_log_raw[,i],test_table)
      performance_log[4,i]<-caret:: specificity(resul_log_raw[,i],test_table)
      performance_log[2,i]<-(as.data.frame(confusionMatrix(resul_log_raw[,i],test_table)$overall))[1,]}
    
    if(svm_counter[i]==1){
      performance_svm[1,i]<-AUC:: auc(roc(resul_svm_prob_YES[,i],test_table))
      performance_svm[3,i]<-caret:: sensitivity(resul_svm_raw[,i],test_table)
      performance_svm[4,i]<-caret:: specificity(resul_svm_raw[,i],test_table)
      performance_svm[2,i]<-(as.data.frame(confusionMatrix(resul_svm_raw[,i],test_table)$overall))[1,]}
    
    if(CRT_counter[i]==1){
      performance_CRT[1,i]<-AUC:: auc(roc(resul_CRT_prob_YES[,i],test_table))
      performance_CRT[3,i]<-caret:: sensitivity(resul_CRT_raw[,i],test_table)
      performance_CRT[4,i]<-caret:: specificity(resul_CRT_raw[,i],test_table)
      performance_CRT[2,i]<-(as.data.frame(confusionMatrix(resul_CRT_raw[,i],test_table)$overall))[1,]}
    
    if(NNET_counter[i]==1){
      performance_NNET[1,i]<-AUC:: auc(roc(resul_NNET_prob_YES[,i],test_table))
      performance_NNET[3,i]<-caret:: sensitivity(resul_NNET_raw[,i],test_table)
      performance_NNET[4,i]<-caret:: specificity(resul_NNET_raw[,i],test_table)
      performance_NNET[2,i]<-(as.data.frame(confusionMatrix(resul_NNET_raw[,i],test_table)$overall))[1,]}
    
    if(tan_counter[i]==1){
      performance_tan[1,i]<-AUC:: auc(roc(resul_tan_prob_YES[,i],test_table))
      performance_tan[3,i]<-caret:: sensitivity(resul_tan_raw[,i],test_table)
      performance_tan[4,i]<-caret:: specificity(resul_tan_raw[,i],test_table)
      performance_tan[2,i]<-(as.data.frame(confusionMatrix(resul_tan_raw[,i],test_table)$overall))[1,]}
    
  }
  
  for(i in 1:nrow(performance_log)){
    if(sum(log_counter)==n_folds){
      performance_log$avg[i]<-(sum(performance_log[i,1:n_folds])/n_folds)}
    if(sum(svm_counter)==n_folds){
      performance_svm$avg[i]<-(sum(performance_svm[i,1:n_folds])/n_folds)}
    if(sum(CRT_counter)==n_folds){
      performance_CRT$avg[i]<-(sum(performance_CRT[i,1:n_folds])/n_folds)}
    if(sum(NNET_counter)==n_folds){
      performance_NNET$avg[i]<-(sum(performance_NNET[i,1:n_folds])/n_folds)}
    if(sum(tan_counter)==n_folds){
      performance_tan$avg[i]<-(sum(performance_tan[i,1:n_folds])/n_folds)}
  }
  
  # write.csv(performance_log,log_perf_folds_file)
  # write.csv(performance_svm,svm_perf_folds_file)
  # if(run_CRT=="Yes"){
  # write.csv(performance_CRT,CRT_perf_folds_file)}
  # write.csv(performance_NNET,NNET_perf_folds_file)
  # write.csv(performance_tan,tan_perf_folds_file)
  
  
  
  #______This for finding weighte average for importance of variables based on AUC (ROC)
  if(sum(log_counter)==n_folds){
    resul_log_imp_tmp<-resul_log_imp[,1:n_folds]
    resul_log_imp_tmp[]<-NA}
  if(sum(svm_counter)==n_folds){
    resul_svm_imp_tmp<-resul_svm_imp[,1:n_folds]
    resul_svm_imp_tmp[]<-NA}
  if(sum(CRT_counter)==n_folds){
    resul_CRT_imp_tmp<-resul_CRT_imp[,1:n_folds]
    resul_CRT_imp_tmp[]<-NA}
  if(sum(NNET_counter)==n_folds){
    resul_NNET_imp_tmp<-resul_NNET_imp[,1:n_folds]
    resul_NNET_imp_tmp[]<-NA}
  if(sum(tan_counter)==n_folds){
    resul_tan_imp_tmp<-resul_tan_imp[,1:n_folds]
    resul_tan_imp_tmp[]<-NA}
  
  for(i in 1:n_folds){
    if(sum(log_counter)==n_folds){
      resul_log_imp_tmp[,i]<-resul_log_imp[,i]*performance_log[1,i]}
    if(sum(svm_counter)==n_folds){
      resul_svm_imp_tmp[,i]<-resul_svm_imp[,i]*performance_svm[1,i]}
    if(sum(CRT_counter)==n_folds){
      resul_CRT_imp_tmp[,i]<-resul_CRT_imp[,i]*performance_CRT[1,i]}
    if(sum(NNET_counter)==n_folds){
      resul_NNET_imp_tmp[,i]<-resul_NNET_imp[,i]*performance_NNET[1,i]}
    if(sum(tan_counter)==n_folds){
      resul_tan_imp_tmp[,i]<-resul_tan_imp[,i]*performance_tan[1,i]}
  }
  
  for(i in 1:nrow(resul_log_imp_tmp)){
    if(sum(log_counter)==n_folds){
      resul_log_imp_tmp$weigh_avg[i]<-(sum(resul_log_imp_tmp[i,1:n_folds]))/sum(performance_log[1,1:n_folds])}
    if(sum(svm_counter)==n_folds){
      resul_svm_imp_tmp$weigh_avg[i]<-(sum(resul_svm_imp_tmp[i,1:n_folds]))/sum(performance_svm[1,1:n_folds])}
    if(sum(CRT_counter)==n_folds){
      resul_CRT_imp_tmp$weigh_avg[i]<-(sum(resul_CRT_imp_tmp[i,1:n_folds]))/sum(performance_CRT[1,1:n_folds])}
    if(sum(NNET_counter)==n_folds){
      resul_NNET_imp_tmp$weigh_avg[i]<-(sum(resul_NNET_imp_tmp[i,1:n_folds]))/sum(performance_NNET[1,1:n_folds])}
    if(sum(tan_counter)==n_folds){
      resul_tan_imp_tmp$weigh_avg[i]<-(sum(resul_tan_imp_tmp[i,1:n_folds]))/sum(performance_tan[1,1:n_folds])}
  }
  
  if(sum(log_counter)==n_folds){
    resul_log_imp$Weigh_AUC_AVG<-resul_log_imp_tmp$weigh_avg
    resul_log_imp_tmp<-NULL}
  
  if(sum(svm_counter)==n_folds){
    resul_svm_imp$Weigh_AUC_AVG<-resul_svm_imp_tmp$weigh_avg
    resul_svm_imp_tmp<-NULL}
  
  if(sum(CRT_counter)==n_folds){
    resul_CRT_imp$Weigh_AUC_AVG<-resul_CRT_imp_tmp$weigh_avg
    resul_CRT_imp_tmp<-NULL}
  
  if(sum(NNET_counter)==n_folds){
    resul_NNET_imp$Weigh_AUC_AVG<-resul_NNET_imp_tmp$weigh_avg
    resul_NNET_imp_tmp<-NULL}
  
  if(sum(tan_counter)==n_folds){
    resul_tan_imp$Weigh_AUC_AVG<-resul_tan_imp_tmp$weigh_avg
    resul_tan_imp_tmp<-NULL}
  
  # write.csv(resul_log_imp,Avg_log_perf_folds_file)
  # write.csv(resul_svm_imp,Avg_svm_perf_folds_file)
  # if(run_CRT=="Yes"){
  # write.csv(resul_CRT_imp,Avg_CRT_perf_folds_file)}
  # write.csv(resul_NNET_imp,Avg_NNET_perf_folds_file)
  # write.csv(resul_tan_imp,Avg_tan_perf_folds_file)
  
  # input_object$performance$performance_log<-performance_log
  # input_object$performance$performance_svm<-performance_svm
  # if(run_CRT=="Yes"){
  #   input_object$performance$performance_CRT<-performance_CRT}
  # input_object$performance$performance_NNET<-performance_NNET
  # input_object$performance$performance_tan<-performance_tan
  # 
  # input_object$performance$log_imp_folds_file<-log_imp_folds_file
  # input_object$performance$svm_imp_folds_file<-svm_imp_folds_file
  # if(run_CRT=="Yes"){
  #   input_object$performance$CRT_imp_folds_file<-CRT_imp_folds_file}
  # input_object$performance$NNET_imp_folds_file<-NNET_imp_folds_file
  # input_object$performance$tan_imp_folds_file<-tan_imp_folds_file
  # 
  # input_object$performance$Avg_log_perf_folds_file<-resul_log_imp
  # input_object$performance$Avg_svm_perf_folds_file<-resul_svm_imp
  # if(run_CRT=="Yes"){
  #   input_object$performance$Avg_CRT_perf_folds_file<-resul_CRT_imp}
  # input_object$performance$Avg_NNET_perf_folds_file<-resul_NNET_imp
  # input_object$performance$Avg_tan_perf_folds_file<-resul_tan_imp
  
  # input_object$svm_cost<-svm_cost
  # input_object$svm_sigma<-svm_sigma
  
  #______end of the weighted average module_______
  
  #this for running the binomial module
  
  # input_object$settings["run_log","content"]<-run_log
  # input_object$settings["run_CRT","content"]<-run_CRT
  # input_object$settings["run_svm","content"]<-run_svm
  # input_object$settings["run_NNET","content"]<-run_NNET
  # input_object$settings["run_tan","content"]<-run_tan
  
  
  if(output_type=="numerics"){
    pred_job<-list()
    performance<-list()
    
    if(sum(log_counter)==n_folds){
      performance$performance_log<-performance_log
      pred_job$resul_log_raw<-resul_log_raw  
      pred_job$resul_log_prob<-resul_log_prob
      performance$resul_log_imp<-resul_log_imp
      performance$log_imp_folds_file<-log_imp_folds_file
    }
    if(sum(NNET_counter)==n_folds){
      performance$performance_NNET<-performance_NNET
      pred_job$resul_NNET_raw<-resul_NNET_raw  
      pred_job$resul_NNET_prob<-resul_NNET_prob
      performance$resul_NNET_imp<-resul_NNET_imp
      performance$NNET_imp_folds_file<-NNET_imp_folds_file
    }
    if(sum(svm_counter)==n_folds){
      performance$performance_svm<-performance_svm
      pred_job$resul_svm_raw<-resul_svm_raw  
      pred_job$resul_svm_prob<-resul_svm_prob
      performance$resul_svm_imp<-resul_svm_imp
      performance$svm_imp_folds_file<-svm_imp_folds_file
    }
    if(sum(tan_counter)==n_folds){
      performance$performance_tan<-performance_tan
      pred_job$resul_tan_raw<-resul_tan_raw  
      pred_job$resul_tan_prob<-resul_tan_prob
      performance$resul_tan_imp<-resul_tan_imp
      performance$tan_imp_folds_file<-tan_imp_folds_file
    }
    if(sum(CRT_counter)==n_folds){
      performance$performance_CRT<-performance_CRT
      pred_job$resul_CRT_raw<-resul_CRT_raw  
      pred_job$resul_CRT_prob<-resul_CRT_prob
      performance$resul_CRT_imp<-resul_CRT_imp
      performance$CRT_imp_folds_file<-CRT_imp_folds_file
    }
    
    pred_job$performance<-performance
    
    runnable$CRT_counter<-CRT_counter
    runnable$log_counter<-log_counter
    runnable$svm_counter<-svm_counter
    runnable$NNET_counter<-NNET_counter
    runnable$tan_counter<-tan_counter
    pred_job$runnable<-runnable
    pred_job$major_data<-major_data
    return(pred_job)
  }
  
  if(output_type=="models"){
    runnable$CRT_counter<-CRT_counter
    runnable$log_counter<-log_counter
    runnable$svm_counter<-svm_counter
    runnable$NNET_counter<-NNET_counter
    runnable$tan_counter<-tan_counter
    all_models$runnable<-runnable
    all_models$major_data<-major_data
    return(all_models)}
}

#_________________________
# finding survival probability on each year base on the best algorithm

sample_prob<-function(input_object,year){
  pred_var<-paste("pred",year,sep = "") 
  
  roc_CRT<-0
  roc_log<-0
  roc_svm<-0
  roc_NNET<-0
  roc_tan<-0
  input_object$pred0$runnable$CRT_counter
  counters<-eval(parse(text = paste("input_object$",pred_var,"$runn",sep = "")))
  n_folds<-as.numeric(input_object$parameters["n_folds",])
  if(sum(counters$CRT_counter)==n_folds){
    roc_CRT<-eval(parse(text = paste("input_object$",pred_var,"$performance$performance_CRT$avg[1]",sep = "")))
  }
  if(sum(counters$log_counter)==n_folds){
    roc_log<-eval(parse(text = paste("input_object$",pred_var,"$performance$performance_log$avg[1]",sep = "")))
  }
  
  if(sum(counters$svm_counter)==n_folds){
    roc_svm<-eval(parse(text = paste("input_object$",pred_var,"$performance$performance_svm$avg[1]",sep = "")))
  }
  if(sum(counters$NNET_counter)==n_folds){
    roc_NNET<-eval(parse(text = paste("input_object$",pred_var,"$performance$performance_NNET$avg[1]",sep = "")))
  }
  if(sum(counters$tan_counter)==n_folds){
    roc_tan<-eval(parse(text = paste("input_object$",pred_var,"$performance$performance_tan$avg[1]",sep = "")))
  }
  best_alg<-c("roc_CRT", "roc_log", "roc_svm","roc_NNET", "roc_tan")[which.max(c(roc_CRT,roc_log,roc_svm,roc_NNET,roc_tan))]
  
  
  data<-eval(parse(text = paste("input_object$data_ind",year,sep = "")))
  data$TARGET<-as.factor(eval(parse(text = paste("input_object$data_targets$year",year,sep = ""))))
  levels(data$TARGET)[1] <- "One"
  levels(data$TARGET)[2] <- "Two"
  
  vars<-names(data[,1:(ncol(data)-1)])
  tan_vars<-vars
  formula_format<-paste("TARGET ~ ",paste(vars, collapse="+"),sep = "")
  formula<-as.formula(formula_format)
  
  control_<- trainControl(method = "none",  savePredictions = TRUE,
                          verboseIter = FALSE,returnResamp = "all",classProbs = TRUE, summaryFunction = twoClassSummary)
  
  
  if(input_object$data_lit=="nul"){
    
    new_data<-input_object$new_data_ADP
  }else{new_data<-cbind(input_object$new_data_ADP,input_object$new_data_lit_ADP)}
  
  
  if(best_alg=="roc_log"){
    mod_log <-caret::train(formula,  data=data, method="glm", family="binomial",
                           trControl = control_, tuneLength = 10, metric="ROC")
    
    assign("best_prediction",predict(mod_log, newdata=new_data, type="prob"))
  }
  if(best_alg=="roc_svm"){
    svm_cost<-.5
    svm_sigma<-.0001
    
    mod_svm<-caret::train(formula,  data=data, method="svmRadial", family="binomial",
                          trControl = control_,  tuneGrid=expand.grid(C=svm_cost, sigma=svm_sigma),
                          tuneLength = 10)
    
    
    assign("best_prediction",predict(mod_svm, newdata=new_data, type="prob"))
  }
  if(best_alg=="roc_NNET"){
    mod_NNET<-caret::train(formula,  data=data, method="nnet", family="binomial",
                           trControl = control_,  tuneGrid=expand.grid(size=5, decay=0.1), MaxNWts=20000,
                           tuneLength = 10)
    
    assign("best_prediction",predict(mod_NNET, newdata=new_data, type="prob"))
  }
  
  if(best_alg=="roc_CRT"){
    mod_CRT <-caret::train(formula,  data=data, method="rpart1SE",
                           trControl = control_, tuneLength = 10, metric="ROC")
    
    assign("best_prediction",predict(mod_CRT, newdata=new_data, type="prob"))
  }
  
  
  if(best_alg=="roc_tan"){
    
    train_data<-data
    new_data<-new_data[, which(names(new_data) %in% tan_vars)]
    test_data<-new_data
    
    data_tan_train<-train_data
    y_tr<-data_tan_train$TARGET
    data_tan_train$TARGET<-NULL
    data_tan_train$folds<-NULL
    data_tan_train$type<-"tr"
    
    data_tan_test<-test_data
    y_ts<-data_tan_test$TARGET
    data_tan_test$TARGET<-NULL
    data_tan_test$folds<-NULL
    data_tan_test$type<-"ts"
    data_tan<-rbind(data_tan_train,data_tan_test)
    
    
    
    for(j in 1:(ncol(data_tan)-1)){
      if(is.numeric(data_tan_train[,j])){if(is.numeric(data_tan[,j])){
        
        tester<-as.data.frame(data_tan[,j])
        if(tester[1,1]!=tester[nrow(data_tan),1]){
          data_tan[,j]<-discretize(data_tan[,j],method="interval",categories = floor(log(nrow(data_tan))))}else{
            data_tan[,j]<-discretize(data_tan[,j],method="interval",categories = 1)
          }
        
        
        
      }
        
      }
    }
    
    
    data_tan_train<-data_tan[which(data_tan$type=="tr"),]
    data_tan_test<-data_tan[which(data_tan$type=="ts"),]
    data_tan_train$type<-NULL
    data_tan_test$type<-NULL
    
    data_tan_train<-data_tan_train[, which(names(data_tan_train) %in% tan_vars)]
    
    mod_tan <- caret::train(data_tan_train, y_tr, method="tan",
                            trControl = control_, tuneGrid = expand.grid(score= "aic", smooth=0.5),
                            tuneLength = 10)
    
    
    assign("best_prediction",predict(mod_tan, newdata=data_tan_test, type="prob"))
  }
  
  #we just want to return survival probability not death
  #so lets get rid of $Two which is related to the death probability
  
  best_prediction$Two<-NULL
  names(best_prediction)<-paste("surv_",year,sep = "")
  
  object<-list()
  object$best_prediction<-best_prediction
  object$best_alg<-best_alg
  return(object)
  
} 

#_________________________
# Reporter function function
reporter<-function(input_object,data,TARGET,date){
  balanc_proc<-input_object$parameters["balanc_proc","content"]
  method_fs<-input_object$settings["method_fs","content"]
  n_folds<-input_object$parameters["n_folds","content"]
  is_bal<-input_object$settings["is_bal","content"]
  is_multi<-input_object$settings["is_multi","content"]
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  H_por_low<-as.numeric(input_object$parameters["H_por_low","content"])
  H_por_high<-as.numeric(input_object$parameters["H_por_high","content"])
  
  data$TARGET<-TARGET
  
  loc_init<-(input_object$settings["Out_put_file","content"])
  file_loc<-paste(c(name_init,date,".txt"),collapse = "")
  name_init<-input_object$settings["Out_put_file_name","content"]
  name<-paste(c(name_init,date,".txt"),collapse = "")
  sink(file_loc)
  
  cat("The feature selection method is: ","\n")
  cat(input_object$settings["FS_Method_output","content"],"\n")
  cat(paste(c("Type of the dependant variable is: ",input_object$settings["Method_output","content"]),collapse = ""),"\n")
  cat(paste(c("Number of folds is: ",n_folds),collapse = ""),"\n")
  if(balanc_proc=="Yes"){
    cat("Sample balancing is used","\n")
    cat("The Sample balancing algorithm is: ",is_bal,"\n")
    
    if(input_object$parameters["B_alg","content"]=="Hybrid"){
      cat("Proportion of differences that you want to increase the minority is : ",H_por_low,"\n")
      cat("Multiplication for the size of minority as the size of majority : ",H_por_high,"\n")
    }
    
  }else{cat("Sample balancing is not used","\n")}
  cat("=====================================\n")
  cat("The distribution of data without Sample Balancing is:","\n")
  cat(table(data$TARGET),"\n")
  
  cat("The distribution of data with Sample Balancing in each loop is:","\n")
  for(i in 1:n_folds){
    cat(paste(c("fold ",i,":"),collapse = ""),"\n")
    dis_train<-paste(c("K_",i),collapse = "")
    cat(eval(parse(text = paste("input_object$",date,"$folds_dist[[dis_train]]",sep = ""))),"\n")
  }
  cat("=====================================\n")
  cat("The performance of logistic Regression is:","\n")
  print(rowMeans(eval(parse(text = paste("input_object$",date,"$performance$performance_log",sep = "")))))
  
  cat("\n")
  cat("The performance of SVM is:","\n")
  print(rowMeans(eval(parse(text = paste("input_object$",date,"$performance$performance_svm",sep = "")))))
  
  cat("\n")
  if(eval(parse(text = paste("input_object$",date,"$run_CRT",sep = "")))=="Yes"){
    cat("The performance of CRT is:","\n")
    print(rowMeans(eval(parse(text = paste("input_object$",date,"$performance$performance_CRT",sep = "")))))
  } 
  cat("The performance of Neural Network is:","\n")
  print(rowMeans(eval(parse(text = paste("input_object$",date,"$performance$performance_NNET",sep = "")))))
  
  cat("The performance of TAN is:","\n")
  print(rowMeans(eval(parse(text = paste("input_object$",date,"$performance$performance_tan",sep = "")))))
  cat("=====================================\n")
  cat("Ouput of Analysis:\n",file_loc,"\n")
  cat("=====================================\n")
  cat("Important Features achieved by specific Machine Learning algorithms: \n")
  if(as.numeric(input_object$parameters["FS_Method_RF","content"])>0){
    cat("Random Forest :\n",paste("input_object$bin_features$R_Forrest_year",substr(date, 5, nchar(date)),sep = ""),"\n")}
  
  if(as.numeric(input_object$parameters["FS_Method_Lasso","content"])>0){
    cat("LASSO :\n",paste("input_object$bin_features$LASSO_year",substr(date, 5, nchar(date)),sep = ""),"\n")}
  
  if(as.numeric(input_object$parameters["FS_Method_FCBF","content"])>0){
    cat("Fast Feature Selection :\n",paste("input_object$bin_features$FFS_year",substr(date, 5, nchar(date)),sep = ""),"\n")}
  
  if(as.numeric(input_object$parameters["FS_Method_itself","content"])>0){
    cat("SVM Features :\n",paste("input_object$bin_features$svm_year",substr(date, 5, nchar(date)),sep = ""),"\n")
    if(eval(parse(text = paste("input_object$",date,"$run_CRT",sep = "")))=="Yes"){
      cat("CRT Features :\n",paste("input_object$bin_features$log_year",substr(date, 5, nchar(date)),sep = ""),"\n")}
    
    cat("Logistic Features :\n",paste("input_object$bin_features$CRT_year",substr(date, 5, nchar(date)),sep = ""),"\n")
    cat("Neural Network Features :\n",paste("input_object$bin_features$TAN_year",substr(date, 5, nchar(date)),sep = ""),"\n")
    cat("TAN Features :\n",paste("input_object$bin_features$NNET_year",substr(date, 5, nchar(date)),sep = ""),"\n")}
  
  if(as.numeric(input_object$parameters["FS_Method_lit","content"])>0){
    cat("Literature Selected: ",paste("input_object$lit",sep = ""),"\n")
    cat("Literature Selected: ",as.character(input_object$lit$variables),"\n")
    cat("Cleaned Variables: ",colnames(input_object$data_lit),"\n")}
  
  if(input_object$parameters["conso_feat","content"]=="Yes"){
    cat("Consolidation of all the Features :\n",paste("input_object$bin_features$consolidated",substr(date, 5, nchar(date)),sep = ""),"\n")}
  
  cat("========================================================================== \n")
  cat("Location of the (train/test) folds used in the prediction: \n") 
  years<-paste("year",substr(date, 5, nchar(date)),sep = "")
  for(i in 1:n_folds){
    balanced_data_train<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_bal,"_",years,"_fold-",i,"_","balanced-train.csv"),collapse = "")
    balanced_data_test<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_bal,"_",years,"_fold-",i,"_","balanced-test.csv"),collapse = "")
    cat("Location of balanced train fold",i,":",balanced_data_train,"\n")
    cat("Location of balanced test fold",i,":",balanced_data_test,"\n")
  }
  input_object$pred0$performance$
    cat("========================================================================== \n")
  cat("Performance of Machine Learning Algorithms in predictions: \n")
  cat("Performance of the Logistic Regression :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$performance_log",sep = ""),"\n")
  cat("Performance of the SVM :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$performance_svm",sep = ""),"\n")
  if(eval(parse(text = paste("input_object$",date,"$run_CRT",sep = "")))=="Yes"){
    cat("Performance of the CRT :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$performance_CRT",sep = ""),"\n")}
  cat("Performance of the Neural Network :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$performance_NNET",sep = ""),"\n")
  cat("Performance of the TAN :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$performance_tan",sep = ""),"\n")
  cat("========================================================================== \n")
  cat("Importance of the selected features used by specific Machine Learning Algorithms for prediction: \n")
  cat("Important variables in Logistic regression :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$log_imp_folds_file",sep = ""),"\n")
  cat("Important variables in SVM :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$svm_imp_folds_file",sep = ""),"\n")
  if(eval(parse(text = paste("input_object$",date,"$run_CRT",sep = "")))=="Yes"){
    cat("Important variables in CRT :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$CRT_imp_folds_file",sep = ""),"\n")}
  cat("Important variables in Neural Network :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$NNET_imp_folds_file",sep = ""),"\n")
  cat("Important variables in TAN :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$tan_imp_folds_file",sep = ""),"\n")
  cat("========================================================================== \n")
  cat("Weighted mportance of the selected features used by specific Machine Learning Algorithms for prediction (Weighted on AUC) : \n")
  cat("Weighted Performance and Average of the Logistic Regression :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$Avg_log_perf_folds_file",sep = ""),"\n")
  cat("Weighted Performance and Average of the SVM :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$Avg_svm_perf_folds_file",sep = ""),"\n")
  if(eval(parse(text = paste("input_object$",date,"$run_CRT",sep = "")))=="Yes"){
    cat("Weighted Performance and Average of the CRT :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$Avg_CRT_perf_folds_file",sep = ""),"\n")}
  cat("Weighted Performance and Average of the Neural Network :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$Avg_NNET_perf_folds_file",sep = ""),"\n")
  cat("Weighted Performance and Average of the TAN :\n",paste("input_object$pred",substr(date, 5, nchar(date)),"$performance$Avg_tan_perf_folds_file",sep = ""),"\n")
  cat("svm_sigma is : ",paste("input_object$pred",substr(date, 5, nchar(date)),"$svm_sigma",sep = ""), "SVM cost is : ",
      paste("input_object$pred",substr(date, 5, nchar(date)),"$svm_cost",sep = ""))
  # colsing the file
  sink()
  
  #sending email to Hamid and Bin about the predicted stock
  
  send.mail(from = "auburn.datascience@gmail.com",
            to = c(input_object$parameters["email","content"]),
            subject = c(name),
            body = sprintf("The address of the output file is:  %s", file_loc) ,
            
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "auburn.datascience", passwd = "machinelearning", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = file_loc,
            file.names = name, # optional parameter
            file.descriptions = c("It contains output of the analysis that you set"), # optional parameter
            debug = TRUE)
  
  #======================================
  
  cat("Ouput of Analysis:\n",file_loc,"\n")
  
}
#_________________________
# consolidating all of variables from different feature selection methods
conso_bin<-function(data_lit,Random_Forrest,Lasso,FFS,itself_svm,itself_log,itself_crt,itself_tan,itself_nnet,input_object){
  
  consolidated<-data.frame(variables=character())
  
  if(input_object$parameters["FS_Method_RF","content"]>0){
    if(!is.null(Random_Forrest)){
      if(nrow(Random_Forrest)>0){consolidated<-rbind(consolidated,Random_Forrest)}}}
  
  if(input_object$parameters["FS_Method_Lasso","content"]>0){
    if(!is.null(Lasso)){
      if(nrow(Lasso)>0){consolidated<-rbind(consolidated,Lasso)}}}
  
  if(input_object$parameters["FS_Method_FCBF","content"]>0){
    if(!is.null(FFS)){
      if(nrow(FFS)){consolidated<-rbind(consolidated,FFS)}}}
  
  if(input_object$parameters["FS_Method_itself","content"]>0){
    
    if(!is.null(itself_svm)){
      if(nrow(itself_svm)>0){consolidated<-rbind(consolidated,itself_svm)}}
    
    if(!is.null(itself_log)){
      if(nrow(itself_log)>0){consolidated<-rbind(consolidated,itself_log)}}
    
    if(!is.null(itself_crt)){
      if(nrow(itself_crt)>0){consolidated<-rbind(consolidated,itself_crt)}}
    
    if(!is.null(itself_tan)){
      if(nrow(itself_tan)>0){consolidated<-rbind(consolidated,itself_tan)}}
    
    if(!is.null(itself_nnet)){
      if(nrow(itself_nnet)>0){consolidated<-rbind(consolidated,itself_nnet)}}
  }
  
  lit<-as.data.frame(colnames(data_lit))
  if(nrow(lit)>0){
    names(lit)<-c("variables")
  }
  
  if(regexpr(pattern ='Literature',input_object$parameters["method_fs","content"])>-1){
    if(!is.null(lit)){
      if(nrow(lit)>0){
        consolidated<-rbind(consolidated,lit)}}}
  
  
  consolidated<-as.data.frame(consolidated[!duplicated(consolidated$variables), ])
  names(consolidated)<-"variables"
  return(consolidated)
}

#_________________________
# TARGET generator for Bionomial
class_generator_bino <- function(gstatus, gtime, p_unit,predict_length){
  p_unit<-as.numeric(p_unit)
  predict_length<-as.numeric(predict_length)
  
  if(gtime < p_unit*predict_length){
    if(gstatus == 0){
      return(NA)
    }else {
      return(1)
    }
  }else{
    return(0)
  }
}
#_________________________
#finding appreance of one dataset's rows in another dataset, 
#The variables column of each dataset should be the interesting elements

var_finder <- function(input1,input2){
  
  if(is.null(input1$variables)){input1$variables<-input1[1]}
  seeker<-as.data.frame(input1$variables)
  names(seeker)<-"variables"
  seeker$results<-NA
  
  if(is.null(input2$variables)){input2$variables<-input2[1]}  
  seeked<-as.data.frame(input2$variables)
  names(seeked)<-"variables"
  
  for (i in 1:nrow(seeker)){
    for (j in 1:nrow(seeked)) {
      if(grepl(seeker$variables[i],seeked$variables[j])){
        if(str_count(seeker$variables[i],"_")==str_count(seeked$variables[j],"_")){
          seeker$results[i]<-"X"}
      }
    }
  }    
  seeker<-seeker[complete.cases(seeker),]
  seeker$results<-NULL
  
  return(seeker)
  
}

#finding importance of variables from a model

model_imp_exc <- function(input_object,vars_org){
  imp_model<-varImp(input_object)
  imp_model<-as.data.frame(imp_model$importance)
  imp_model$variables<-rownames(imp_model)
  imp_model$importance<-as.numeric(imp_model[,1])
  
  model_imp<-matrix(0, ncol = 1, nrow = nrow(imp_model))
  model_imp<-as.data.frame(model_imp)
  
  
  model_imp$variables<-imp_model$variables
  model_imp$importance<-imp_model$importance
  model_imp$V1<-NULL 
  
  seeker<-model_imp
  seeker$real_name<-NA
  
  seeked<-as.data.frame(vars_org[,1])
  names(seeked)<-"variables"
  
  for (i in 1:nrow(seeker)){
    for (j in 1:nrow(seeked)) {
      if(grepl(seeked$variables[j],seeker$variables[i])){
        if(str_count(seeker$variables[i],"_")==str_count(seeked$variables[j],"_")){
          seeker$real_name[i]<-as.character(seeked$variables[j])}
      }
    }
  }
  # It's for the variables that created by us and are not in the original dataset
  for (i in 1:nrow(seeker)){
    if(is.na(seeker$real_name[i])){seeker$real_name[i]<-seeker$variables[i]}
  }
  str(seeker$importance)
  res.by <- by(seeker$importance, seeker$real_name, mean)
  res.by
  
  seeker<-as.data.frame(res.by)
  names(seeker)<-c("variables","importance")
  
  return(seeker) 
}

###################filling a matrix from another matrix based on ellements of the common matrix
# col_no is index of the column that we want to get filled
# filling_matrix is the matrix that has empty columns
# filler_matrix is a n*2 matrix wich has an index column and values in the second column
# common_col is the common column that has indeces for matching the rows
matrix_filler<-function(col_no,filling_matrix,filler_matrix,common_col){
  common_col<-as.character(common_col)
  
  filler_col<-match(common_col,names(filler_matrix))
  filling_col<-match(common_col,names(filling_matrix))
  
  if(filler_col-2==0){
    values<-1
  }else{values<-2}
  
  for (k in 1:nrow(filling_matrix)){
    for (j in 1:nrow(filler_matrix)) {
      if(grepl(filler_matrix[j,filler_col],filling_matrix[k,filling_col])){
        if(str_count(filling_matrix[k,filling_col],"_")==str_count(filler_matrix[j,filler_col],"_")){
          filling_matrix[k,col_no]<-filler_matrix[j,values]}
      }
    }
  }        
  return(filling_matrix) 
}

#__________________________________converting categorical variables to dummy variables_____________________
dummy_maker<-function(data,begin,end){
  for(i in begin:end){
    if(class(data[,i])!="numeric"){data[,i]<-as.factor(data[,i])}
  }
  new_cols<-matrix(0, ncol = 1, nrow = nrow(data))
  new_cols<-as.data.frame(new_cols)
  new_cols[,1]<-NULL
  T<-NULL
  
  for(i in begin:end){
    if(nlevels(data[,i])>1){
      formula<-as.formula(paste("~",colnames(data)[i],"-1",collapse = " "))
      dummy_matrix<-as.data.frame(model.matrix(formula, data=data ))
      dummy_matrix[,1]<-NULL
      new_cols<-cbind(new_cols,dummy_matrix)
      T<-c(T,i)
    }
  }
  if(!is.null(T)){
    data<-data[ ,-T]
    data<-cbind(new_cols,data)}
  return(data)
}
pre_process<-function(x){
  # removing near zero columns
  nzv<-nearZeroVar(x)
  x<- x[,-nzv]
  #removing highly correlated columns
  descrCor <- cor(x)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
  x <- x[, -highlyCorDescr]
  #removing columns that have linear relationship
  comboInfo <- findLinearCombos(x)
  if(!is.null(comboInfo$remove)){x<-x[, -comboInfo$remove]}
  return(x)}

RUS_func <- function(input_data){
  
  Train_Two <- input_data[ which(input_data$TARGET=="Two"), ]
  Train_One <- input_data[ which(input_data$TARGET=="One"), ]
  if(nrow(Train_Two)<=nrow(Train_One)){
    sample_size<-nrow(Train_Two)
    Train_One<-Train_One[sample(nrow(Train_One), sample_size), ]
  }else{
    sample_size<-nrow(Train_One)
    Train_Two<-Train_Two[sample(nrow(Train_Two), sample_size), ]
  }
  input_data<-rbind(Train_One,Train_Two)
  
  return(input_data)
}

#=========================================================================================================
#     Reading Module
#=========================================================================================================
reading <- function(inputss) {
  
  #_______________________________Required Libraries_______________________________________________________________________________________
  #  # Install any needed package with the following command: install.packages("Name", dependencies = c("Depends", "Suggests"))
  #  # library(DMwR); library(WriteXLS);  library(data.table);   library(randomForest); library(mvtnorm); 
  #  # library(haven); #install.packages("haven") to be able to read the STATA file .DTA
  #  # library(ggplot2); 
  #  library(caret); library(mlbench); library(gmodels);
  #  library(bnclassify);  
  #  #library(mailR)
  #  library(party); 
  #  library(zoo); library(sandwich); library(kernlab); library(nnet); library(pROC); library(AUC)
  #  library(plyr);  library(stringr); library(e1071);
  #  library(gdata); library(ROSE);library(taRifx);library(Boruta);library(glmnet); 
  #  library(Biocomb);
  #  library(gRain);library(pROC)
  #  # library(rio);  
  #  library(mlr); library(arules);
  #  #library(xlsx);
  #  library(BiocGenerics);  library(graph);library(RBGL) ; library(rsig); library(rms); library(survAUC)
  #  library(devtools);library(grid); library(modeltools); library(stats4); library(strucchange)
  #  library(doParallel); 
  #  #library(smotefamily)
  #  if(inputss$platform=="MAC OS"){library(doMC)}
  #  #
  #  set.seed(110)
  # # print(Sys.time())
  #  
  #  #save(inputss,file=paste("/Users/hamid/OneDrive - Auburn University/Transplant/chronical/11-1-2017/data_pile/input_object_test.RData"),collapse = "")
  input_object<-list()
  
  NA_cleaning<-"kk"
  RF_Method<-"kk"
  Lasso_Method<-"kk"
  itself_Method<-"kk"
  lit_Method<-"kk"
  #tuning<-"kk"
  conso_feat<-"kk"
  lit_vars<-"kk"
  n_folds<-"kk"
  lit_run<-"kk"
  lit_var_nul<-"nul"
  cat_vars<-"kk"
  num_vars<-"kk"
  pred_type<-"kk"
  n_folds<-0
  
  new_data<-inputss$new_data
  platform<-inputss$platform
  pred_type<-inputss$pred_type
  
  method_fs<-inputss$method_fs
  if(regexpr(pattern ='Literature',method_fs)>0){lit_run<-"Yes"}else{lit_run=="No"}
  
  FS_Method_RF=regexpr(pattern ='Random Forrest',method_fs)[1]
  FS_Method_Lasso=regexpr(pattern ='Lasso',method_fs)[1]
  FS_Method_FCBF=regexpr(pattern ='Fast Selection',method_fs)[1]
  FS_Method_itself=regexpr(pattern ='itself',method_fs)[1]
  FS_Method_lit=regexpr(pattern ='Literature',method_fs)[1]
  
  
  if(regexpr(pattern ='Literature',method_fs)[1]>-1){
    if(length(inputss$lit_vars)>0){
      lit_vars<-inputss$lit_vars
    }else{lit_var_nul<-"nul"
    method_fs<-gsub("_Literature","",method_fs)}}else{lit_var_nul<-"nul"}
  
  if(length(inputss$cat_vars)>0){cat_vars<-inputss$cat_vars
  input_object$use_cat_vars<-cat_vars}
  if(length(inputss$num_vars)>0){num_vars<-inputss$num_vars
  input_object$use_num_vars<-num_vars}
  
  n_folds<-inputss$n_folds
  n_folds<-as.numeric(n_folds)
  
  balanc_proc<-inputss$balanc_proc
  B_alg<-inputss$B_alg
  version<-inputss$version
  
  H_por_low<-inputss$H_por_low
  H_por_low<-as.numeric(H_por_low)
  
  H_por_high<-inputss$H_por_high
  H_por_high<-as.numeric(H_por_high)
  
  file_name<-inputss$file_name
  conso_feat<-inputss$conso_feat
  if(inputss$d_prep=="Yes"){d_prep<-"DP"}else{d_prep<-""}
  
  root_folder_input<-inputss$root_folder_input
  if(is.null(root_folder_input)){root_folder<-getwd()}else{root_folder<-root_folder_input}
  if(platform=="OSC Server"){root_folder<-getwd()}
  analysis_folder<-paste(root_folder,"/analysis/",sep="")
  dir.create(analysis_folder)
  ind_vars<-inputss$ind_vars
  
  event<-inputss$event
  time<-inputss$time
  
  if(conso_feat=="Yes"){consolidate<-"yes"}else{consolidate<-"no"}
  is_bal<-"No-balancing"
  if(balanc_proc=="Yes"){
    if(B_alg=="SMOTE"){is_bal<-"SMOTE"}
    if(B_alg=="RUS"){is_bal<-"RUS"}
    if(B_alg=="Hybrid"){is_bal<-"Hybrid"}
  }else{is_bal<-"No-balancing"}
  
  email<-inputss$email
  
  time_unit<-"DAY"
  prediction_unit<-"YEAR"
  
  time_unit<-toupper(inputss$time_unit)
  prediction_unit<-toupper(inputss$prediction_unit)
  
  p_unit<-c(1,1,1,1,1,1,1,1,1,1,1)
  predict_length<-1
  
  if(time_unit=="DAY" & prediction_unit=="YEAR"){p_unit<-c(1/12,1,2,3,4,5,6,7,8,9,10)
  predict_length<-365}
  if(time_unit=="MONTH" & prediction_unit=="YEAR"){p_unit<-c(1/2,1,2,3,4,5,6,7,8,9,10)
  predict_length<-12}
  if(time_unit=="QUARTER" & prediction_unit=="YEAR"){p_unit<-c(1,2,3,4,5,6,7,8,9,10,11)
  predict_length<-3}
  if(time_unit=="HALF YEAR" & prediction_unit=="YEAR"){p_unit<-c(1,2,3,4,5,6,7,8,9,10,11)
  predict_length<-2}
  
  if(time_unit=="DAY" & prediction_unit=="HALF YEAR"){p_unit<-c(1/2,1,2,3,4,5,6,7,8,9,10)
  predict_length<-182}
  if(time_unit=="MONTH" & prediction_unit=="HALF YEAR"){p_unit<-c(1/2,1,3/2,2,3,4,5,6,7,8,9)
  predict_length<-6}
  if(time_unit=="QUARTER" & prediction_unit=="HALF YEAR"){p_unit<-c(1,2,3,4,5,6,7,8,9,10,11)
  predict_length<-2}
  
  if(time_unit=="DAY" & prediction_unit=="QUARTER"){p_unit<-c(1/2,1,2,3,4,5,6,7,8,9,10)
  predict_length<-91}
  if(time_unit=="MONTH" & prediction_unit=="QUARTER"){p_unit<-c(1,2,3,4,5,6,7,8,9,10,11)
  predict_length<-3}
  
  
  if(time_unit=="DAY" & prediction_unit=="MONTH"){p_unit<-c(1/2,1,2,3,4,5,6,7,8,9,10)
  predict_length<-30}
  
  if(time_unit=="HOUR" & prediction_unit=="DAY"){p_unit<-c(1/2,1,2,3,4,5,6,7,8,9,10)
  predict_length<-24}
  
  
  input_object$data_base<-inputss$data_base
  input_object$data_ind<-inputss$data_ind
  prim_target<-input_object$data_base[,c(time,event)]
  
  cl_names<-names(input_object$data_ind)
  
  #if you don't select gtime gstatus , data_ind (for selecting independent variables) whould be empty so I put below mentioned 
  if(sum(which(names(input_object$data_ind) %in% c(time,event)))>0){
    input_object$data_ind<-input_object$data_ind[ , -which(names(input_object$data_ind) %in% c(time,event))]}
  
  #the next two lines are needed for column manipulations
  
  if(lit_var_nul!="nul"){
    
    if(is.null(ncol(inputss$data_lit))){
      input_object$data_lit<-as.data.frame(inputss$data_lit)
      colnames(input_object$data_lit)<-inputss$lit_vars
    }else{input_object$data_lit<-inputss$data_lit}
    
    lit_names<-names(input_object$data_lit)
    
    if(length(which(names(input_object$data_ind) %in% lit_names))!=0){
      input_object$data_ind<-as.data.frame(input_object$data_ind[ , -which(names(input_object$data_ind) %in% lit_names)])}
    ind_names<-names(input_object$data_ind)
    input_object$dataset<-cbind(input_object$data_ind,input_object$data_lit,prim_target)
  }else{input_object$dataset<-cbind(input_object$data_ind,prim_target)
  ind_names<-names(input_object$data_ind)}
  
  
  for(i in 1:nrow(input_object$dataset)){
    input_object$dataset$year0[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[1],predict_length)
    input_object$dataset$year1[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[2],predict_length)
    input_object$dataset$year2[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[3],predict_length)
    input_object$dataset$year3[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[4],predict_length)
    input_object$dataset$year4[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[5],predict_length)
    input_object$dataset$year5[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[6],predict_length)
    input_object$dataset$year6[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[7],predict_length)
    input_object$dataset$year7[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[8],predict_length)
    input_object$dataset$year8[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[9],predict_length)
    input_object$dataset$year9[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[10],predict_length)
    input_object$dataset$year10[i]<-class_generator_bino(input_object$dataset[i,event], input_object$dataset[i,time] ,p_unit[11],predict_length)
  }
  targets<-c("year0","year1","year2","year3","year4","year5","year6","year7","year8","year9","year10")
  
  input_object$data<-input_object$dataset[complete.cases(input_object$dataset),]
  #changing type of variables based on what the user specified
  nc_base<-ncol(input_object$data)
  cn_base<-names(input_object$data)
  
  
  for (i in 1:nc_base) {
    if(names(input_object$data[i]) %in% num_vars){input_object$data[,i]<-as.numeric(input_object$data[,i])}
    if(names(input_object$data[i]) %in% cat_vars){input_object$data[,i]<-as.factor(input_object$data[,i])}}
  
  
  if(lit_var_nul!="nul"){
    input_object$data_lit<-input_object$data[ , which(names(input_object$data) %in% lit_names)]
    input_object$data_ind<-input_object$data[ , which(names(input_object$data) %in% ind_names)]
    input_object$data_targets<-input_object$data[ , which(names(input_object$data) %in% targets)]
  }else{
    input_object$data_ind<-input_object$data[ , which(names(input_object$data) %in% ind_names)]
    input_object$data_targets<-input_object$data[ , which(names(input_object$data) %in% targets)]}
  
  VarsData <- as.data.frame(ind_vars)
  VarsData<-as.data.frame(VarsData)
  names(VarsData)<-"variables"
  VarsData_Phase <- VarsData
  VarsData_Phase$selected<-NA
  
  input_object$VarsData_Phase<-VarsData_Phase
  input_object$ind_vars<-ind_vars
  
  if(lit_var_nul!="nul"){
    lit<-as.data.frame(lit_vars)
    colnames(lit)<-"variables"
    input_object$lit<-lit}
  
  params<-c(platform,NA_cleaning,pred_type,method_fs,lit_run,lit_var_nul, FS_Method_RF,FS_Method_Lasso,
            FS_Method_FCBF,FS_Method_itself, FS_Method_lit,d_prep,
            n_folds,balanc_proc,B_alg,H_por_low,H_por_high,file_name,conso_feat,root_folder,
            analysis_folder,time,event,consolidate,is_bal,email,version,prediction_unit,time_unit)
  
  params<-as.data.frame(params)
  params[]<- lapply(params, as.character)
  row.names(params)<-c("platform","NA_cleaning","pred_type","method_fs","lit_run","lit_var_nul","FS_Method_RF","FS_Method_Lasso",
                       "FS_Method_FCBF","FS_Method_itself","FS_Method_lit","d_prep",
                       "n_folds","balanc_proc","B_alg","H_por_low","H_por_high","file_name","conso_feat","root_folder",
                       "analysis_folder","time","event","consolidate","is_bal","email","version","prediction_unit","time_unit")
  
  names(params)<-c("content")
  input_object$parameters<-params
  input_object$new_data<-new_data
  input_object$data_ind$id_main<-rownames(input_object$data_ind)
  # #delete later
  # {
  #   app_gui<-list()
  #   app_gui$platform<-inputss$platform
  #   app_gui$pred_type<-inputss$pred_type
  #   app_gui$method_fs<-inputss$method_fs
  #   app_gui$lit_vars<-inputss$lit_vars
  #   app_gui$cat_vars<-inputss$cat_vars
  #   app_gui$num_vars<-inputss$num_vars
  #   app_gui$n_folds<-inputss$n_folds
  #   app_gui$balanc_proc<-inputss$balanc_proc
  #   app_gui$B_alg<-inputss$B_alg
  #   app_gui$version<-inputss$version
  #   app_gui$H_por_low<-inputss$H_por_low
  #   app_gui$H_por_high<-inputss$H_por_high
  #   app_gui$file_name<-inputss$file_name
  #   app_gui$conso_feat<-inputss$conso_feat
  #   app_gui$d_prep<-inputss$d_prep
  #   app_gui$root_folder_input<-inputss$root_folder_input
  #   app_gui$ind_vars<-inputss$ind_vars
  #   app_gui$event<-inputss$event
  #   app_gui$time<-inputss$time
  #   app_gui$email<-inputss$email
  #   app_gui$time_unit<-inputss$time_unit
  #   app_gui$prediction_unit<-inputss$prediction_unit
  #   app_gui$data_base<-inputss$data_base
  #   app_gui$data_ind<-inputss$data_ind
  #   app_gui$data_lit<-inputss$data_lit
  #   app_gui$lit_vars<-inputss$lit_vars
  #   app_gui$new_data<-inputss$file_new_data
  # }
  # if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  # save(input_object,file=paste(c(save_folder,"/input_object_reading.RData"),collapse = ""))
  # save(app_gui,file=paste(c(save_folder,"/app_gui.RData"),collapse = ""))
  # #delete later
  
  #for ensuring type of variables
  if(!is.null(input_object$data_ind)){if(ncol(input_object$data_ind)*nrow(input_object$data_ind)!=0){
    for(i in 1:ncol(input_object$data_ind)){
      if(names(input_object$data_ind[i]) %in% input_object$use_cat_vars){
        input_object$data_ind[,i]<-as.factor(input_object$data_ind[,i])
      }
      if(names(input_object$data_ind[i]) %in% input_object$use_num_vars){
        input_object$data_ind[,i]<-as.numeric(input_object$data_ind[,i])
      }
    }
  }}
  
  if(!is.null(input_object$data_lit)){if(ncol(input_object$data_lit)*nrow(input_object$data_lit)!=0){
    for(i in 1:ncol(input_object$data_lit)){
      if(names(input_object$data_lit[i]) %in% input_object$use_cat_vars){
        input_object$data_lit[,i]<-as.factor(input_object$data_lit[,i])
      }
      if(names(input_object$data_lit[i]) %in% input_object$use_num_vars){
        input_object$data_lit[,i]<-as.numeric(input_object$data_lit[,i])
      }
    }
  }}
  
  if(!is.null(input_object$data_base)){if(ncol(input_object$data_base)*nrow(input_object$data_base)!=0){
    for(i in 1:ncol(input_object$data_base)){
      if(names(input_object$data_base[i]) %in% input_object$use_cat_vars){
        input_object$data_base[,i]<-as.factor(input_object$data_base[,i])
      }
      if(names(input_object$data_base[i]) %in% input_object$use_num_vars){
        input_object$data_base[,i]<-as.numeric(input_object$data_base[,i])
      }
    }
  }}
  
  if(!is.null(input_object$new_data)){if(ncol(input_object$new_data)*nrow(input_object$new_data)!=0){
    for(i in 1:ncol(input_object$new_data)){
      if(names(input_object$new_data[i]) %in% input_object$use_cat_vars){
        input_object$new_data[,i]<-as.factor(input_object$new_data[,i])
      }
      if(names(input_object$new_data[i]) %in% input_object$use_num_vars){
        input_object$new_data[,i]<-as.numeric(input_object$new_data[,i])
      }
    }
  }}
  
  
  return(input_object)
}

#==========================================================================================================
#      The Data prepararion function is defined here
#==========================================================================================================
initialization<-function(input_object){
  
  #__________________________________Path Initialization_____________________________
  
  root_folder<-input_object$parameters["root_folder","content"] 
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  is_bal<-input_object$parameters["is_bal","content"]
  file_name<-input_object$parameters["file_name","content"]
  method_fs<-input_object$parameters["method_fs","content"]
  d_prep<-input_object$parameters["d_prep","content"]
  consolidate<-input_object$parameters["consolidate","content"]
  #tuning<-input_object$parameters["tuning","content"]
  pred_type<-input_object$parameters["pred_type","content"]
  
  Random_Forrest_Location<-paste(c(root_folder,"/","Random_Forrest_",d_prep,"_variables_",file_name),collapse = "")
  Lasso_location<-paste(c(root_folder,"/","Lasso_",d_prep,"_variables_",file_name),collapse = "")
  Fast_Feature_Location<-paste(c(root_folder,"/","Fast_Feature_",d_prep,"_variables_",file_name),collapse = "")
  Literature_file<-paste(c(root_folder,"/","Literature_",d_prep,"_Litrature_Review_",file_name),collapse = "")
  consolidate_Location<-paste(c(root_folder,"/","consolidate_",d_prep,"_variables_",file_name),collapse = "")
  removed_vars_Location<-paste(c(root_folder,"/","removed_vars_",d_prep,"_variables_",file_name),collapse = "")
  lremoved_vars_Location_lit<-paste(c(root_folder,"/","removed_vars_lit_",d_prep,"_variables_",file_name),collapse = "")
  
  base_file<-paste(c(root_folder,"/",file_name),collapse = "")
  Out_put_file_name<-paste(c(file_name,"_",d_prep,"_",method_fs,"_",consolidate,"_",is_bal,"_"),collapse = "")
  Out_put_file<-paste(c(analysis_folder,file_name,"_",d_prep,"_",method_fs,"_",consolidate,"_"),collapse = "")
  
  prediction_data<-paste(c(root_folder,"/","prediction_data","_",method_fs,"_",consolidate,"_",is_bal,"_",file_name,"_",d_prep,"_",".csv"),collapse = "")
  
  dir.create(paste(c(analysis_folder,"importance"),collapse = ""))
  dir.create(paste(c(analysis_folder,"performance"),collapse = ""))
  
  log_imp_folds_file<-paste(c(analysis_folder,"importance/","log_imp_vars_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  svm_imp_folds_file<-paste(c(analysis_folder,"importance/","svm_imp_vars_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  CRT_imp_folds_file<-paste(c(analysis_folder,"importance/","CRT_imp_vars_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  NNET_imp_folds_file<-paste(c(analysis_folder,"importance/","NNET_imp_vars_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  tan_imp_folds_file<-paste(c(analysis_folder,"importance/","tan_imp_vars_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  
  log_perf_folds_file<-paste(c(analysis_folder,"performance/","log_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  svm_perf_folds_file<-paste(c(analysis_folder,"performance/","svm_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  CRT_perf_folds_file<-paste(c(analysis_folder,"performance/","CRT_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  NNET_perf_folds_file<-paste(c(analysis_folder,"performance/","NNET_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  tan_perf_folds_file<-paste(c(analysis_folder,"performance/","tan_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  
  Avg_log_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_log_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  Avg_svm_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_svm_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  Avg_CRT_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_CRT_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  Avg_NNET_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_NNET_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  Avg_tan_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_tan_perf_folds_",method_fs,"_",consolidate,"_",is_bal,"_",d_prep,"_",file_name),collapse = "")
  
  itself_svm_Location<-paste(c(root_folder,"/","itself_svm_variables_",d_prep,"_variables_",file_name),collapse = "")
  itself_log_Location<-paste(c(root_folder,"/","itself_log_variables_",d_prep,"_variables_",file_name),collapse = "")
  itself_CRT_Location<-paste(c(root_folder,"/","itself_CRT_variables_",d_prep,"_variables_",file_name),collapse = "")
  itself_NNET_Location<-paste(c(root_folder,"/","itself_NNET_variables_",d_prep,"_variables_",file_name),collapse = "")
  itself_tan_Location<-paste(c(root_folder,"/","itself_tan_variables_",d_prep,"_variables_",file_name),collapse = "")
  
  # _______________________________Output setup________________________________________
  
  if(pred_type=="Multinomial"){Method_output<-"Multinomial"}else{Method_output<-"Binomial"}
  FS_Method_output<-method_fs
  
  settings<-c(Random_Forrest_Location,Lasso_location,consolidate_Location,
              Fast_Feature_Location,base_file,Out_put_file,
              Literature_file,prediction_data,consolidate, removed_vars_Location,lremoved_vars_Location_lit,
              log_imp_folds_file,svm_imp_folds_file,CRT_imp_folds_file,NNET_imp_folds_file,tan_imp_folds_file,
              log_perf_folds_file,svm_perf_folds_file,CRT_perf_folds_file,NNET_perf_folds_file,tan_perf_folds_file,
              Avg_log_perf_folds_file,Avg_svm_perf_folds_file,Avg_CRT_perf_folds_file,
              Avg_NNET_perf_folds_file,Avg_tan_perf_folds_file,Method_output,FS_Method_output,
              itself_svm_Location,itself_log_Location,itself_CRT_Location,itself_tan_Location,
              itself_NNET_Location,Out_put_file_name)
  
  settings<-as.data.frame(settings)
  settings[] <- lapply(settings, as.character)
  row.names(settings)<-c("Random_Forrest_Location","Lasso_location","consolidate_Location",
                         "Fast_Feature_Location","base_file","Out_put_file",
                         "Literature_file","prediction_data","consolidate", "removed_vars_Location","lremoved_vars_Location_lit",
                         "log_imp_folds_file","svm_imp_folds_file","CRT_imp_folds_file","NNET_imp_folds_file","tan_imp_folds_file",
                         "log_perf_folds_file","svm_perf_folds_file","CRT_perf_folds_file","NNET_perf_folds_file","tan_perf_folds_file",
                         "Avg_log_perf_folds_file","Avg_svm_perf_folds_file","Avg_CRT_perf_folds_file",
                         "Avg_NNET_perf_folds_file", "Avg_tan_perf_folds_file","Method_output","FS_Method_output",
                         "itself_svm_Location","itself_log_Location","itself_CRT_Location","itself_tan_Location",
                         "itself_NNET_Location","Out_put_file_name")
  names(settings)<-"content"
  
  input_object$settings<-settings
  
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_object_initialization.RData"),collapse = ""))
  # #delete later
  
  
  return(input_object)
}
#input_object$parameters["d_prep","content"]<-"k"
#=============================================================
#==============Automated Data Preparation
#=============================================================
ADP<-function(input_object){
  if(input_object$parameters["d_prep","content"]=="DP"){
    if(ncol(input_object$data_ind)>2){
      
      
      data2<-input_object$data_ind
      data<-data2
      data$id_main<-NULL
      
      new_data<-input_object$new_data[colnames(data)]
      data$id<-"other"
      new_data$id<-"spd"
      
      data<-rbind(data,new_data)
      
      #invariant columns
      if(ncol(data)>0){
        invariant_result <- list()
        
        for(i in 1:ncol(data)){
          invariant_temp = data[,i]
          invariant_result_temp = invariant_checker(invariant_temp)
          invariant_result = c(invariant_result, invariant_result_temp) }
        
        invariant_index <- which(invariant_result == "Two")
        
        if(length(invariant_index)>0){
          invariant_data<-as.data.frame(data[, invariant_index])
          cnames<-colnames(data)[invariant_index]
          names(invariant_data)<-cnames
          data <- data[, -invariant_index]
        }
        if(length(invariant_index)>0){input_object$remove$invariant_data<-invariant_data
        input_object$remove$invariant_index<-invariant_index}
      }
      #end of invariant columns
      
      #too levels
      if(ncol(data)>0){
        toolevels_result <- list()
        for(i in 1:ncol(data)){
          toolevels_temp = data[,i]
          toolevels_results_temp = if(nlevels(toolevels_temp)>27){"Two"}else{"No"}
          toolevels_result = c(toolevels_result, toolevels_results_temp) }
        
        toolevels_index <- which(toolevels_result == "Two")
        
        if(length(toolevels_index)>0){
          toolevels_data<-as.data.frame(data[, toolevels_index])
          cnames<-colnames(data)[toolevels_index]
          names(toolevels_data)<-cnames
          data <- data[, -toolevels_index]}
        
        if(length(toolevels_index)>0){input_object$remove$toolevels_data<-toolevels_data
        input_object$remove$toolevels_index<-toolevels_index}}
      #end of too levels
      
      #check if we have numerical/non_numerical columns
      if(ncol(data)>0){
        num_exis<-0
        non_num_exis<-0
        for(i in 1:ncol(data)){
          if(is.numeric(data[,i])){num_exis<-1}else(non_num_exis<-1)}}
      
      #working on numeric values
      if(num_exis==1){
        data$drop<-1
        numeric <- sapply(data, is.numeric)
        names<-names(data[numeric])
        data_numeric<-as.data.frame(data[,numeric])
        names(data_numeric)<-names
        data_numeric$drop<-NULL
        data$drop<-NULL
        
        #Excluding near zero variances
        if(ncol(data_numeric)>0){
          nzv <-nearZeroVar(data_numeric, saveMetrics = TRUE)
          cnzv<-nzv$nzv
          
          check_ZV<-0
          if(TRUE %in% cnzv){
            
            cl_names<-colnames(data_numeric)
            zcnames<-cl_names[cnzv]
            nzcnames<-cl_names[!cnzv]
            
            nzvar<-as.data.frame(data_numeric[ , (names(data_numeric) %in% zcnames)])
            names(nzvar)<-zcnames
            
            data_numeric<-as.data.frame(data_numeric[ , (names(data_numeric) %in% nzcnames)])
            names(data_numeric)<-nzcnames
            
            check_ZV<-1
          }
          
          if(check_ZV==1){
            if(length(nzvar)>0){input_object$remove$nzvar<-nzvar}}
          
        }
        
        #Excluding highly correlated columns
        if(ncol(data_numeric)>1){
          descrCor <- cor(data_numeric)
          highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
          if(length(highlyCorDescr)>0){
            cL_numerics <- colnames(data_numeric[highlyCorDescr])
            high_corr<-as.data.frame(data_numeric[, highlyCorDescr])
            names(high_corr)<-cL_numerics
            
            UNcL_numerics <- colnames(data_numeric[-highlyCorDescr])
            data_numeric<- as.data.frame(data_numeric[, -highlyCorDescr])
            names(data_numeric)<-UNcL_numerics
            
            input_object$remove$high_corr<-high_corr}
          
        }
        
        #Excluding Linear dependencies in columns
        if(ncol(data_numeric)>0){
          comboInfo <- findLinearCombos(data_numeric)
          if(!is.null(comboInfo$remove)){
            
            LDnames<-colnames(data_numeric)[comboInfo$remove]
            lin_rel<-as.data.frame(data_numeric[, comboInfo$remove])
            names(lin_rel)<-LDnames
            
            NDnames<-colnames(data_numeric)[-comboInfo$remove]
            data_numeric<-as.data.frame(data_numeric[, -comboInfo$remove])
            names(data_numeric)<-NDnames
            
            input_object$remove$lin_rel<-lin_rel }
          
        }
        
        #Scaling and Centering
        if(ncol(data_numeric)>0){
          preProcValues <- preProcess(data_numeric, method = c("center", "scale"))
          data_numeric<- predict(preProcValues, data_numeric)
          
          #End of Scaling and Centering
          
        }
      }
      #######==========#####
      #making dummy variables
      if(non_num_exis==1){
        
        data$drop<-"YNYN"
        non_numeric <- !sapply(data, is.numeric)
        data_non_numeric<-data[,non_numeric]
        data_non_numeric$drop<-NULL
        data$drop<-NULL
        
        datanames<-colnames(data_non_numeric)
        ncoldum<-ncol(data_non_numeric)
        
        for(i in 1:ncol(data_non_numeric)){
          if(!is.numeric(data_non_numeric[,i])){
            temp<-createDummyFeatures(data_non_numeric[,i])
            names(temp)<-paste(datanames[i],levels(data_non_numeric[,i]),sep="_")
            data_non_numeric<-cbind(data_non_numeric,temp)
            data_non_numeric[,ncol(data_non_numeric)]<-NULL
          }
        }
        data_non_numeric[,1:ncoldum]<-NULL
        for(i in 1:ncol(data_non_numeric)){
          data_non_numeric[,i]<-as.factor(data_non_numeric[,i])
        }
      }
      
      
      #merging numerioc and non_numeric data together
      if(num_exis==1){if(non_num_exis==1){pdata<-cbind(data_numeric,data_non_numeric)}else{(pdata<-data_numeric)}}else{
        if(non_num_exis==1){pdata<-data_non_numeric}}
      
      #delete later
      dir.create(paste(c(input_object$parameters["root_folder","content"],"/data_pile"),collapse = ""))
      save(input_object,file=paste(c(input_object$parameters["root_folder","content"],"/data_pile/input_object.RData"),collapse = ""))
      #delete later
      
      if(num_exis+non_num_exis>0){
        
        input_object$data_ind<-pdata[which(pdata$id==1),]
        input_object$new_data_ADP<-pdata[which(pdata$id==0),]
        input_object$data_ind$id_<-NULL
        input_object$new_data_ADP$id_<-NULL
        
        
      }else{input_object$data_ind<-"nul"}
      
    } 
  }else{
    #if there is no data preparation we just have 
    
    data2<-input_object$data_ind
    data<-data2
    data$id_main<-NULL
    
    
    
    new_data<-input_object$new_data[colnames(data)]
    data$id<-"other"
    new_data$id<-"spd"
    
    data<-rbind(data,new_data)
    
    #invariant columns
    if(ncol(data)>0){
      invariant_result <- list()
      
      for(i in 1:ncol(data)){
        invariant_temp = data[,i]
        invariant_result_temp = invariant_checker(invariant_temp)
        invariant_result = c(invariant_result, invariant_result_temp) }
      
      invariant_index <- which(invariant_result == "Two")
      
      if(length(invariant_index)>0){
        invariant_data<-as.data.frame(data[, invariant_index])
        cnames<-colnames(data)[invariant_index]
        names(invariant_data)<-cnames
        data <- data[, -invariant_index]
      }
      if(length(invariant_index)>0){input_object$remove$invariant_data<-invariant_data
      input_object$remove$invariant_index<-invariant_index}
    }
    #end of invariant columns
    
    #too levels
    if(ncol(data)>0){
      toolevels_result <- list()
      for(i in 1:ncol(data)){
        toolevels_temp = data[,i]
        toolevels_results_temp = if(nlevels(toolevels_temp)>27){"Two"}else{"No"}
        toolevels_result = c(toolevels_result, toolevels_results_temp) }
      
      toolevels_index <- which(toolevels_result == "Two")
      
      if(length(toolevels_index)>0){
        toolevels_data<-as.data.frame(data[, toolevels_index])
        cnames<-colnames(data)[toolevels_index]
        names(toolevels_data)<-cnames
        data <- data[, -toolevels_index]}
      
      if(length(toolevels_index)>0){input_object$remove$toolevels_data<-toolevels_data
      input_object$remove$toolevels_index<-toolevels_index}}
    #end of too levels
    
    if(ncol(data)>0){
      num_exis<-0
      non_num_exis<-0
      for(i in 1:ncol(data)){
        if(is.numeric(data[,i])){num_exis<-1}else(non_num_exis<-1)}}
    
    if(num_exis==1){
      data$drop<-1
      numeric <- sapply(data, is.numeric)
      names<-names(data[numeric])
      data_numeric<-as.data.frame(data[,numeric])
      names(data_numeric)<-names
      data_numeric$drop<-NULL
      data$drop<-NULL}
    
    #making dummy variables
    if(non_num_exis==1){
      
      data$drop<-"YNYN"
      non_numeric <- !sapply(data, is.numeric)
      data_non_numeric<-data[,non_numeric]
      data_non_numeric$drop<-NULL
      data$drop<-NULL
      
      datanames<-colnames(data_non_numeric)
      ncoldum<-ncol(data_non_numeric)
      
      for(i in 1:ncol(data_non_numeric)){
        if(!is.numeric(data_non_numeric[,i])){
          temp<-createDummyFeatures(data_non_numeric[,i])
          names(temp)<-paste(datanames[i],levels(data_non_numeric[,i]),sep="_")
          data_non_numeric<-cbind(data_non_numeric,temp)
          data_non_numeric[,ncol(data_non_numeric)]<-NULL
        }
      }
      data_non_numeric[,1:ncoldum]<-NULL
      for(i in 1:ncol(data_non_numeric)){
        data_non_numeric[,i]<-as.factor(data_non_numeric[,i])
      }
    }
    
    #merging numerioc and non_numeric data together
    if(num_exis==1){if(non_num_exis==1){pdata<-cbind(data_numeric,data_non_numeric)}else{(pdata<-data_numeric)}}else{
      if(non_num_exis==1){pdata<-data_non_numeric}}
    
    if(num_exis+non_num_exis>0){
      
      input_object$data_ind<-pdata[which(pdata$id==1),]
      input_object$new_data_ADP<-pdata[which(pdata$id==0),]
      input_object$data_ind$id_<-NULL
      input_object$new_data_ADP$id_<-NULL
      
    }else{input_object$data_ind<-"nul"}
    
    
  }
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_object_ADP.RData"),collapse = ""))
  # #delete later
  
  
  
  
  input_object$data_ind$id_main<-data2$id_main
  
  return(input_object)
}

#=============================================================
#==============Automated Data Preparation
#=============================================================
ADP_lit<-function(input_object){
  
  if(input_object$parameters["lit_var_nul","content"]!="nul"){
    if(input_object$parameters["d_prep","content"]=="DP"){
      data2<-input_object$data_lit
      data<-data2
      data$id_main<-NULL
      
      new_data<-input_object$new_data[colnames(input_object$data_lit)]
      data$id<-"other"
      new_data$id<-"spd"
      
      data<-rbind(data,new_data)
      
      
      
      # input_object$data_lit2<-input_object$data_lit
      runing<-"Yes"
      
      #invariant columns
      if(ncol(data)>0){
        invariant_result <- list()
        
        for(i in 1:ncol(data)){
          invariant_temp = data[,i]
          invariant_result_temp = invariant_checker(invariant_temp)
          invariant_result = c(invariant_result, invariant_result_temp) }
        
        invariant_index <- which(invariant_result == "Two")
        
        if(length(invariant_index)>0){
          invariant_data<-as.data.frame(data[, invariant_index])
          cnames<-colnames(data)[invariant_index]
          names(invariant_data)<-cnames
          data <- data[, -invariant_index]
        }
        
        if(length(invariant_index)>0){input_object$remove_lit$linvariant_lit<-invariant_data
        input_object$remove_lit$invariant_litindex<-invariant_index
        
        }
        
      }
      #end of invariant columns
      
      #too levels
      if(ncol(data)>0){
        toolevels_result <- list()
        for(i in 1:ncol(data)){
          toolevels_temp = data[,i]
          toolevels_results_temp = if(nlevels(toolevels_temp)>27){"Two"}else{"No"}
          toolevels_result = c(toolevels_result, toolevels_results_temp) }
        
        toolevels_index <- which(toolevels_result == "Two")
        
        if(length(toolevels_index)>0){
          toolevels_data<-as.data.frame(data[, toolevels_index])
          cnames<-colnames(data)[toolevels_index]
          names(toolevels_data)<-cnames
          data <- data[, -toolevels_index]}
        
        if(length(toolevels_index)>0){input_object$remove_lit$ltoolevels_lit<-toolevels_data
        input_object$remove_lit$ltoolevels_index<-toolevels_index}}
      #end of too levels
      
      #check if we have numerical/non_numerical columns
      if(ncol(data)>0){
        num_exis<-0
        non_num_exis<-0
        for(i in 1:ncol(data)){
          if(is.numeric(data[,i])){num_exis<-1}else(non_num_exis<-1)}}
      
      #working on numeric values
      if(num_exis==1){
        data$drop<-1
        numeric <- sapply(data, is.numeric)
        names<-names(data[numeric])
        data_numeric<-as.data.frame(data[,numeric])
        names(data_numeric)<-names
        data_numeric$drop<-NULL
        data$drop<-NULL
        
        #Excluding near zero variances
        if(ncol(data_numeric)>0){
          
          nzv <-nearZeroVar(data_numeric, saveMetrics = TRUE)
          cnzv<-nzv$nzv
          print(cnzv)
          
          check_ZV<-0
          if(TRUE %in% cnzv){
            
            cl_names<-colnames(data_numeric)
            zcnames<-cl_names[cnzv]
            nzcnames<-cl_names[!cnzv]
            
            nzvar<-as.data.frame(data_numeric[ , (names(data_numeric) %in% zcnames)])
            names(nzvar)<-zcnames
            
            data_numeric<-as.data.frame(data_numeric[ , (names(data_numeric) %in% nzcnames)])
            names(data_numeric)<-nzcnames
            
            check_ZV<-1
          }
          
          if(check_ZV==1){
            input_object$remove_lit$lnzvar<-nzvar}
        }
        #End of zero variance variables
        
        # #Excluding highly correlated columns
        # if(ncol(data_numeric)>1){
        #   descrCor <- cor(data_numeric)
        #   highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
        #   if(length(highlyCorDescr)>0){
        #     cL_numerics <- colnames(data_numeric[highlyCorDescr])
        #     high_corr<-as.data.frame(data_numeric[, highlyCorDescr])
        #     names(high_corr)<-cL_numerics
        #     
        #     UNcL_numerics <- colnames(data_numeric[-highlyCorDescr])
        #     data_numeric<- as.data.frame(data_numeric[, -highlyCorDescr])
        #     names(data_numeric)<-UNcL_numerics
        #     
        #     input_object$remove_lit$lhigh_corr<-high_corr
        #   }}
        # #End of highly correlated columns
        # 
        # #Excluding Linear dependencies in columns
        # if(ncol(data_numeric)>1){
        #   comboInfo <- findLinearCombos(data_numeric)
        #   if(!is.null(comboInfo$remove)){
        #     
        #     LDnames<-colnames(data_numeric)[comboInfo$remove]
        #     lin_rel<-as.data.frame(data_numeric[, comboInfo$remove])
        #     names(lin_rel)<-LDnames
        #     
        #     NDnames<-colnames(data_numeric)[-comboInfo$remove]
        #     data_numeric<-as.data.frame(data_numeric[, -comboInfo$remove])
        #     names(data_numeric)<-NDnames
        #     
        #     input_object$remove_lit$llin_rel<-lin_rel
        #   }}
        
        # Scaling and Centering
        if(ncol(data_numeric)>0){
          preProcValues <- preProcess(data_numeric, method = c("center", "scale"))
          data_numeric<- predict(preProcValues, data_numeric)
          #End of Scaling and Centering
          
        }
        #End of Linear dependencies
        
      }
      
      
      #making dummy variables
      if(non_num_exis==1){
        
        data$drop<-"YNYN"
        non_numeric <- !sapply(data, is.numeric)
        data_non_numeric<-data[,non_numeric]
        data_non_numeric$drop<-NULL
        data$drop<-NULL
        
        datanames<-colnames(data_non_numeric)
        ncoldum<-ncol(data_non_numeric)
        
        for(i in 1:ncol(data_non_numeric)){
          if(!is.numeric(data_non_numeric[,i])){
            temp<-createDummyFeatures(data_non_numeric[,i])
            names(temp)<-paste(datanames[i],levels(data_non_numeric[,i]),sep="_")
            data_non_numeric<-cbind(data_non_numeric,temp)
            data_non_numeric[,ncol(data_non_numeric)]<-NULL
          }
        }
        
        data_non_numeric[,1:ncoldum]<-NULL
        for(i in 1:ncol(data_non_numeric)){
          data_non_numeric[,i]<-as.factor(data_non_numeric[,i])
        }
      }
      
      #merging numerioc and non_numeric data together
      if(num_exis==1){if(non_num_exis==1){pdata<-cbind(data_numeric,data_non_numeric)}else{(pdata<-data_numeric)}}else{
        if(non_num_exis==1){pdata<-data_non_numeric}}
      
      #delete later
      root_folder<-input_object$parameters["root_folder","content"] 
      dir.create(paste(c(root_folder,"/data_pile"),collapse = ""))
      write.csv(input_object$data_ind,paste(c(root_folder,"/data_pile/data_ind.csv"),collapse = ""))
      write.csv(input_object$data_lit,paste(c(root_folder,"/data_pile/data_lit.csv"),collapse = ""))
      write.csv(input_object$data_targets,paste(c(root_folder,"/data_pile/data_targets.csv"),collapse = ""))
      write.csv(input_object$dataset,paste(c(root_folder,"/data_pile/dataset.csv"),collapse = ""))
      write.csv(input_object$data,paste(c(root_folder,"/data_pile/data.csv"),collapse = ""))
      write.csv(input_object$parameters,paste(c(root_folder,"/data_pile/parameters.csv"),collapse = ""))
      write.csv(input_object$settings,paste(c(root_folder,"/data_pile/settings.csv"),collapse = ""))
      save(input_object,file=paste(c(root_folder,"/data_pile/input_object.RData"),collapse = ""))
      #delete later
      
      if(num_exis+non_num_exis>0){
        
        input_object$data_lit<-pdata[which(pdata$id==1),]
        input_object$new_data_lit_ADP<-pdata[which(pdata$id==0),]
        input_object$data_lit$id_<-NULL
        input_object$new_data_lit_ADP$id_<-NULL
        
        
      }else{input_object$data_lit<-"nul"}
    }else{
      #if there is no data preparation we just have 
      
      data2<-input_object$data_lit
      data<-data2
      data$id_main<-NULL
      
      
      new_data<-input_object$new_data[colnames(data)]
      data$id<-"other"
      new_data$id<-"spd"
      
      data<-rbind(data,new_data)
      
      #too levels
      if(ncol(data)>0){
        toolevels_result <- list()
        for(i in 1:ncol(data)){
          toolevels_temp = data[,i]
          toolevels_results_temp = if(nlevels(toolevels_temp)>27){"Two"}else{"No"}
          toolevels_result = c(toolevels_result, toolevels_results_temp) }
        
        toolevels_index <- which(toolevels_result == "Two")
        
        if(length(toolevels_index)>0){
          toolevels_data<-as.data.frame(data[, toolevels_index])
          cnames<-colnames(data)[toolevels_index]
          names(toolevels_data)<-cnames
          data <- data[, -toolevels_index]}
        
        if(length(toolevels_index)>0){input_object$remove_lit$ltoolevels_lit<-toolevels_data
        input_object$remove_lit$ltoolevels_index<-toolevels_index}}
      #end of too levels
      
      if(ncol(data)>0){
        num_exis<-0
        non_num_exis<-0
        for(i in 1:ncol(data)){
          if(is.numeric(data[,i])){num_exis<-1}else(non_num_exis<-1)}}
      
      if(num_exis==1){
        data$drop<-1
        numeric <- sapply(data, is.numeric)
        names<-names(data[numeric])
        data_numeric<-as.data.frame(data[,numeric])
        names(data_numeric)<-names
        data_numeric$drop<-NULL
        data$drop<-NULL}
      
      #making dummy variables
      if(non_num_exis==1){
        
        data$drop<-"YNYN"
        non_numeric <- !sapply(data, is.numeric)
        data_non_numeric<-data[,non_numeric]
        data_non_numeric$drop<-NULL
        data$drop<-NULL
        
        datanames<-colnames(data_non_numeric)
        ncoldum<-ncol(data_non_numeric)
        
        for(i in 1:ncol(data_non_numeric)){
          if(!is.numeric(data_non_numeric[,i])){
            temp<-createDummyFeatures(data_non_numeric[,i])
            names(temp)<-paste(datanames[i],levels(data_non_numeric[,i]),sep="_")
            data_non_numeric<-cbind(data_non_numeric,temp)
            data_non_numeric[,ncol(data_non_numeric)]<-NULL
          }
        }
        
        data_non_numeric[,1:ncoldum]<-NULL
        for(i in 1:ncol(data_non_numeric)){
          data_non_numeric[,i]<-as.factor(data_non_numeric[,i])
        }
      }
      
      #merging numerioc and non_numeric data together
      if(num_exis==1){if(non_num_exis==1){pdata<-cbind(data_numeric,data_non_numeric)}else{(pdata<-data_numeric)}}else{
        if(non_num_exis==1){pdata<-data_non_numeric}}
      
      if(num_exis+non_num_exis>0){
        
        input_object$data_lit<-pdata
        
        input_object$data_lit<-pdata[which(pdata$id==1),]
        input_object$new_data_lit_ADP<-pdata[which(pdata$id==0),]
        input_object$data_lit$id_<-NULL
        input_object$new_data_lit_ADP$id_<-NULL
        
        
        
      }else{input_object$data_lit<-"nul"}
      
      
    }
    input_object$data_lit$id_main<-data2$id_main
  }else{input_object$data_lit<-"nul"}
  
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_object_ADP_lit.RData"),collapse = ""))
  # #delete later
  
  
  return(input_object)
}

#=============================================================
#==============Consolidating the removed Features
#=============================================================  
remove_conso<-function(input_object){
  if(input_object$parameters["d_prep","content"]=="DP"){
    template<-data.frame(variables=character(),reason=character())
    remove_conso<-template
    names(remove_conso)<-c("variables","reason")
    
    
    invariant_data<-input_object$remove$invariant_data
    toolevels_data<-input_object$remove$toolevels_data
    nzvar<-input_object$remove$nzvar
    high_corr<-input_object$remove$high_corr
    lin_rel<-input_object$remove$lin_rel
    
    if(!is.null(invariant_data)){
      VarsData_inv <- as.data.frame(colnames(input_object$remove$invariant_data))
      names(VarsData_inv)<-"variables"
      VarsData_inv$reason<-"invariant"
      remove_conso<-rbind(remove_conso,VarsData_inv)
    }
    
    if(!is.null(toolevels_data)){
      VarsData_lvl<- as.data.frame(colnames(input_object$remove$toolevels_data))
      names(VarsData_lvl)<-"variables"
      VarsData_lvl$reason<-"too levels"
      remove_conso<-rbind(remove_conso,VarsData_lvl)
    }
    
    if(!is.null(nzvar)){
      VarsData_nzv<- as.data.frame(colnames(input_object$remove$nzvar))
      names(VarsData_nzv)<-"variables"
      VarsData_nzv$reason<-"near zero variance"
      remove_conso<-rbind(remove_conso,VarsData_nzv)
    }
    
    if(!is.null(high_corr)){
      VarsData_cor<- as.data.frame(colnames(input_object$remove$high_corr))
      names(VarsData_cor)<-"variables"
      VarsData_cor$reason<-"high correlation"
      remove_conso<-rbind(remove_conso,VarsData_cor)
    }
    
    if(!is.null(lin_rel)){
      VarsData_rel<- as.data.frame(colnames(input_object$remove$lin_rel))
      names(VarsData_rel)<-"variables"
      VarsData_rel$reason<-"linear relation"
      remove_conso<-rbind(remove_conso,VarsData_rel)
    }
    
    remove_conso<-as.data.frame(remove_conso[!duplicated(remove_conso$variables), ])
    names(remove_conso)<-c("variables","reason")
    
    if(nrow(remove_conso)>0){
      input_object$remove$removed_vars_conso<-remove_conso}
  }
  
  return(input_object)
}

#===========================================================================
#==============Consolidating the removed Features in Literature
#===========================================================================  
lremove_conso<-function(input_object){
  if(input_object$parameters["d_prep","content"]=="DP"){
    template<-data.frame(variables=character(),reason=character())
    remove_conso<-template
    names(remove_conso)<-c("variables","reason")
    
    linvariant_lit<-input_object$remove_lit$linvariant_lit
    ltoolevels_lit<-input_object$remove_lit$ltoolevels_lit
    lnzvar<-input_object$remove_lit$lnzvar
    lhigh_corr<-input_object$remove_lit$lhigh_corr
    llin_rel<-input_object$remove_lit$llin_rel
    
    if(!is.null(linvariant_lit)){
      VarsData_inv <- as.data.frame(colnames(input_object$remove_lit$linvariant_lit))
      names(VarsData_inv)<-"variables"
      VarsData_inv$reason<-"invariant"
      remove_conso<-rbind(remove_conso,VarsData_inv)
    }
    
    if(!is.null(ltoolevels_lit)){
      VarsData_lvl<- as.data.frame(colnames(input_object$remove_lit$ltoolevels_lit))
      names(VarsData_lvl)<-"variables"
      VarsData_lvl$reason<-"too levels"
      remove_conso<-rbind(remove_conso,VarsData_lvl)
    }
    
    if(!is.null(lnzvar)){
      VarsData_nzv<- as.data.frame(colnames(input_object$remove_lit$lnzvar))
      names(VarsData_nzv)<-"variables"
      VarsData_nzv$reason<-"near zero variance"
      remove_conso<-rbind(remove_conso,VarsData_nzv)
    }
    
    if(!is.null(lhigh_corr)){
      VarsData_cor<- as.data.frame(colnames(input_object$remove_lit$lhigh_corr))
      names(VarsData_cor)<-"variables"
      VarsData_cor$reason<-"high correlation"
      remove_conso<-rbind(remove_conso,VarsData_cor)
    }
    
    if(!is.null(llin_rel)){
      VarsData_rel<- as.data.frame(colnames(input_object$remove_lit$llin_rel))
      names(VarsData_rel)<-"variables"
      VarsData_rel$reason<-"linear relation"
      remove_conso<-rbind(remove_conso,VarsData_rel)
    }
    
    remove_conso<-as.data.frame(remove_conso[!duplicated(remove_conso$variables), ])
    names(remove_conso)<-c("variables","reason")
    
    if(nrow(remove_conso)>0){
      input_object$remove_lit$removed_litvars_conso<-remove_conso}
  }
  
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_lremove_conso.RData"),collapse = ""))
  # #delete later
  
  return(input_object)
}

#=============================================
#==============Random Forest Feature Selection
#=============================================
Random_Forrest<-function(input_object){
  if(as.numeric(input_object$parameters["FS_Method_RF","content"]>0)){
    input_object$bin_features<-list()
    data<-input_object$data_ind
    data$id_main<-NULL
    
    input_object$bin_features$R_Forrest_year0<-R_Forrest_bin(data,input_object$data_targets$year0)
    #input_object$bin_features$R_Forrest_year1<-R_Forrest_bin(data,input_object$data_targets$year1)
    # input_object$bin_features$R_Forrest_year2<-R_Forrest_bin(data,input_object$data_targets$year2)
    # input_object$bin_features$R_Forrest_year3<-R_Forrest_bin(data,input_object$data_targets$year3)
    # input_object$bin_features$R_Forrest_year4<-R_Forrest_bin(data,input_object$data_targets$year4)
    # input_object$bin_features$R_Forrest_year5<-R_Forrest_bin(data,input_object$data_targets$year5)
    # input_object$bin_features$R_Forrest_year6<-R_Forrest_bin(data,input_object$data_targets$year6)
    # input_object$bin_features$R_Forrest_year7<-R_Forrest_bin(data,input_object$data_targets$year7)
    # input_object$bin_features$R_Forrest_year8<-R_Forrest_bin(data,input_object$data_targets$year8)
    # input_object$bin_features$R_Forrest_year9<-R_Forrest_bin(data,input_object$data_targets$year9)
    # input_object$bin_features$R_Forrest_year10<-R_Forrest_bin(data,input_object$data_targets$year10)
  }
  # # #delete later
  # if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  # save(input_object,file=paste(c(save_folder,"/input_Forrest.RData"),collapse = ""))
  # # #delete later
  return(input_object)}

#=============================================================
#==============Lasso Feature selection for Binomial TARGETS
#=============================================================
Lasso_Binomial<-function(input_object){
  if(input_object$parameters["pred_type","content"]=="Binary"){
    if(as.numeric(input_object$parameters["FS_Method_Lasso","content"])>0){
      data<-input_object$data_ind
      data$id_main<-NULL
      
      year0_df<-data
      year0_df$TARGET<-input_object$data_targets$year0
      input_object$bin_features$LASSO_year0<-Lasso_bin(year0_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year1_df<-data
      year1_df$TARGET<-input_object$data_targets$year1
      input_object$bin_features$LASSO_year1<-Lasso_bin(year1_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year2_df<-data
      year2_df$TARGET<-input_object$data_targets$year2
      input_object$bin_features$LASSO_year2<-Lasso_bin(year2_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year3_df<-data
      year3_df$TARGET<-input_object$data_targets$year3
      input_object$bin_features$LASSO_year3<-Lasso_bin(year3_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year4_df<-data
      year4_df$TARGET<-input_object$data_targets$year4
      input_object$bin_features$LASSO_year4<-Lasso_bin(year4_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year5_df<-data
      year5_df$TARGET<-input_object$data_targets$year5
      input_object$bin_features$LASSO_year5<-Lasso_bin(year5_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year6_df<-data
      year6_df$TARGET<-input_object$data_targets$year6
      input_object$bin_features$LASSO_year6<-Lasso_bin(year6_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year7_df<-data
      year7_df$TARGET<-input_object$data_targets$year7
      input_object$bin_features$LASSO_year7<-Lasso_bin(year7_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year8_df<-data
      year8_df$TARGET<-input_object$data_targets$year8
      input_object$bin_features$LASSO_year8<-Lasso_bin(year8_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year9_df<-data
      year9_df$TARGET<-input_object$data_targets$year9
      input_object$bin_features$LASSO_year9<-Lasso_bin(year9_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
      year10_df<-data
      year10_df$TARGET<-input_object$data_targets$year10
      input_object$bin_features$LASSO_year10<-Lasso_bin(year10_df,"TARGET",input_object,folds=5,trace=F,alpha=1)
      
    }}
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_LASSO.RData"),collapse = ""))
  # #delete later
  return(input_object)}

#=============================================================
#==============Fast Feature selection
#=============================================================
FFS<-function(input_object){
  if(input_object$parameters["pred_type","content"]=="Binary"){
    if(as.numeric(input_object$parameters["FS_Method_FCBF","content"])>0){
      
      data<-input_object$data_ind
      data$id_main<-NULL
      
      year0_df<-data
      year0_df$TARGET<-input_object$data_targets$year0
      input_object$bin_features$FFS_year0<-FFS_bin(year0_df)
      
      year1_df<-data
      year1_df$TARGET<-input_object$data_targets$year1
      input_object$bin_features$FFS_year1<-FFS_bin(year1_df)
      
      year2_df<-data
      year2_df$TARGET<-input_object$data_targets$year2
      input_object$bin_features$FFS_year2<-FFS_bin(year2_df)
      
      year3_df<-data
      year3_df$TARGET<-input_object$data_targets$year3
      input_object$bin_features$FFS_year3<-FFS_bin(year3_df)
      
      year4_df<-data
      year4_df$TARGET<-input_object$data_targets$year4
      input_object$bin_features$FFS_year4<-FFS_bin(year4_df)
      
      year5_df<-data
      year5_df$TARGET<-input_object$data_targets$year5
      input_object$bin_features$FFS_year5<-FFS_bin(year5_df)
      
      year6_df<-data
      year6_df$TARGET<-input_object$data_targets$year6
      input_object$bin_features$FFS_year6<-FFS_bin(year6_df)
      
      year7_df<-data
      year7_df$TARGET<-input_object$data_targets$year7
      input_object$bin_features$FFS_year7<-FFS_bin(year7_df)
      
      year8_df<-data
      year8_df$TARGET<-input_object$data_targets$year8
      input_object$bin_features$FFS_year8<-FFS_bin(year8_df)
      
      year9_df<-data
      year9_df$TARGET<-input_object$data_targets$year9
      input_object$bin_features$FFS_year9<-FFS_bin(year9_df)
      
      year10_df<-data
      year10_df$TARGET<-input_object$data_targets$year10
      input_object$bin_features$FFS_year10<-FFS_bin(year10_df)
      
    }}
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_FFS.RData"),collapse = ""))
  # #delete later
  return(input_object)}  

#============================================================================================
#==============Itself Feature Selection
#============================================================================================
itself<-function(input_object){
  if(input_object$parameters["pred_type","content"]=="Binomial"){
    if(as.numeric(input_object$parameters["FS_Method_itself","content"]>0)){
      
      tasks <- list(
        year0 = function() itself_bin(input_object$data_ind,input_object$data_targets$year0),
        
        year1 = function() itself_bin(input_object$data_ind,input_object$data_targets$year1),
        
        year2 = function() itself_bin(input_object$data_ind,input_object$data_targets$year2),
        
        year3 = function() itself_bin(input_object$data_ind,input_object$data_targets$year3),
        
        year4 = function() itself_bin(input_object$data_ind,input_object$data_targets$year4),
        
        year5 = function() itself_bin(input_object$data_ind,input_object$data_targets$year5)
      )
      
      
      
      # Using fork()
      if(input_object$parameters["platform","content"]=="MAC OS"){
        server_cores<-min((detectCores()-2),(length(tasks)*5))
        out <- mclapply( 
          tasks, 
          function(f) f(), 
          mc.cores = server_cores
        )
      }
      
      # Using socket()
      if(input_object$parameters["platform","content"]=="Windows"){
        r_cores<-detectCores(all.tests = FALSE, logical = TRUE)-1
        cl <- makeCluster( min(length(tasks),r_cores ))
        out <- clusterApply( 
          cl,
          tasks,
          function(f) f()
        )
        getDoParWorkers()
        stopCluster(cl)
      }
      
      year0_features<-out$year0
      
      input_object$bin_features$svm_year0<-year0_features$svm_Profile
      input_object$bin_features$log_year0<-year0_features$log_Profile
      input_object$bin_features$CRT_year0<-year0_features$CRT_Profile
      input_object$bin_features$TAN_year0<-year0_features$TAN_Profile
      input_object$bin_features$NNET_year0<-year0_features$NNET_Profile
      
      year1_features<-out$year1
      input_object$bin_features$svm_year1<-year1_features$svm_Profile
      input_object$bin_features$log_year1<-year1_features$log_Profile
      input_object$bin_features$CRT_year1<-year1_features$CRT_Profile
      input_object$bin_features$TAN_year1<-year1_features$TAN_Profile
      input_object$bin_features$NNET_year1<-year1_features$NNET_Profile
      
      year2_features<-out$year2
      input_object$bin_features$svm_year2<-year2_features$svm_Profile
      input_object$bin_features$log_year2<-year2_features$log_Profile
      input_object$bin_features$CRT_year2<-year2_features$CRT_Profile
      input_object$bin_features$TAN_year2<-year2_features$TAN_Profile
      input_object$bin_features$NNET_year2<-year2_features$NNET_Profile
      
      year3_features<-out$year3
      input_object$bin_features$svm_year3<-year3_features$svm_Profile
      input_object$bin_features$log_year3<-year3_features$log_Profile
      input_object$bin_features$CRT_year3<-year3_features$CRT_Profile
      input_object$bin_features$TAN_year3<-year3_features$TAN_Profile
      input_object$bin_features$NNET_year3<-year3_features$NNET_Profile
      
      year4_features<-out$year4
      input_object$bin_features$svm_year4<-year4_features$svm_Profile
      input_object$bin_features$log_year4<-year4_features$log_Profile
      input_object$bin_features$CRT_year4<-year4_features$CRT_Profile
      input_object$bin_features$TAN_year4<-year4_features$TAN_Profile
      input_object$bin_features$NNET_year4<-year4_features$NNET_Profile
      
      year5_features<-out$year5
      input_object$bin_features$svm_year5<-year5_features$svm_Profile
      input_object$bin_features$log_year5<-year5_features$log_Profile
      input_object$bin_features$CRT_year5<-year5_features$CRT_Profile
      input_object$bin_features$TAN_year5<-year5_features$TAN_Profile
      input_object$bin_features$NNET_year5<-year5_features$NNET_Profile
      
      
      
      tasks <- list(
        year6 = function() itself_bin(input_object$data_ind,input_object$data_targets$year6),
        
        year7 = function() itself_bin(input_object$data_ind,input_object$data_targets$year7),
        
        year8 = function() itself_bin(input_object$data_ind,input_object$data_targets$year8),
        
        year9 = function() itself_bin(input_object$data_ind,input_object$data_targets$year9),
        
        year10 = function() itself_bin(input_object$data_ind,input_object$data_targets$year10)
      )
      
      
      
      # Using fork()
      if(input_object$parameters["platform","content"]=="MAC OS"){
        server_cores<-min((detectCores()-2),(length(tasks)))
        out2 <- mclapply( 
          tasks, 
          function(f) f(), 
          mc.cores = server_cores
        )
      }
      
      # Using socket()
      if(input_object$parameters["platform","content"]=="Windows"){
        r_cores<-detectCores(all.tests = FALSE, logical = TRUE)-1
        cl <- makeCluster( min(length(tasks),r_cores ))
        out2 <- clusterApply( 
          cl,
          tasks,
          function(f) f()
        )
        getDoParWorkers()
        stopCluster(cl)
      }
      
      
      year6_features<-out2$year6
      input_object$bin_features$svm_year6<-year6_features$svm_Profile
      input_object$bin_features$log_year6<-year6_features$log_Profile
      input_object$bin_features$CRT_year6<-year6_features$CRT_Profile
      input_object$bin_features$TAN_year6<-year6_features$TAN_Profile
      input_object$bin_features$NNET_year6<-year6_features$NNET_Profile
      
      year7_features<-out2$year7
      input_object$bin_features$svm_year7<-year7_features$svm_Profile
      input_object$bin_features$log_year7<-year7_features$log_Profile
      input_object$bin_features$CRT_year7<-year7_features$CRT_Profile
      input_object$bin_features$TAN_year7<-year7_features$TAN_Profile
      input_object$bin_features$NNET_year7<-year7_features$NNET_Profile
      
      year8_features<-out2$year8
      input_object$bin_features$svm_year8<-year8_features$svm_Profile
      input_object$bin_features$log_year8<-year8_features$log_Profile
      input_object$bin_features$CRT_year8<-year8_features$CRT_Profile
      input_object$bin_features$TAN_year8<-year8_features$TAN_Profile
      input_object$bin_features$NNET_year8<-year8_features$NNET_Profile
      
      year9_features<-out2$year9
      input_object$bin_features$svm_year9<-year9_features$svm_Profile
      input_object$bin_features$log_year9<-year9_features$log_Profile
      input_object$bin_features$CRT_year9<-year9_features$CRT_Profile
      input_object$bin_features$TAN_year9<-year9_features$TAN_Profile
      input_object$bin_features$NNET_year9<-year9_features$NNET_Profile
      
      year10_features<-out2$year10
      input_object$bin_features$svm_year10<-year10_features$svm_Profile
      input_object$bin_features$log_year10<-year10_features$log_Profile
      input_object$bin_features$CRT_year10<-year10_features$CRT_Profile
      input_object$bin_features$TAN_year10<-year10_features$TAN_Profile
      input_object$bin_features$NNET_year10<-year10_features$NNET_Profile
      
    }
  }
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_itself.RData"),collapse = ""))
  # #delete later
  return(input_object)}

#=============================================================
#==============Consolidating the Features
#=============================================================  

consolidate<-function(input_object){
  
  
  input_object$bin_features$consolidated0<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year0,
                                                     input_object$bin_features$LASSO_year0,input_object$bin_features$FFS_year0,
                                                     input_object$bin_features$svm_year0,input_object$bin_features$log_year0,
                                                     input_object$bin_features$CRT_year0,input_object$bin_features$TAN_year0,
                                                     input_object$bin_features$NNET_year0,input_object)
  
  
  input_object$bin_features$consolidated1<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year1,
                                                     input_object$bin_features$LASSO_year1,input_object$bin_features$FFS_year1,
                                                     input_object$bin_features$svm_year1,input_object$bin_features$log_year1,
                                                     input_object$bin_features$CRT_year1,input_object$bin_features$TAN_year1,
                                                     input_object$bin_features$NNET_year1,input_object)
  
  
  input_object$bin_features$consolidated2<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year2,
                                                     input_object$bin_features$LASSO_year2,input_object$bin_features$FFS_year2,
                                                     input_object$bin_features$svm_year2,input_object$bin_features$log_year2,
                                                     input_object$bin_features$CRT_year2,input_object$bin_features$TAN_year2,
                                                     input_object$bin_features$NNET_year2,input_object)
  
  
  input_object$bin_features$consolidated3<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year3,
                                                     input_object$bin_features$LASSO_year3,input_object$bin_features$FFS_year3,
                                                     input_object$bin_features$svm_year3,input_object$bin_features$log_year3,
                                                     input_object$bin_features$CRT_year3,input_object$bin_features$TAN_year3,
                                                     input_object$bin_features$NNET_year3,input_object)
  
  
  input_object$bin_features$consolidated4<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year4,
                                                     input_object$bin_features$LASSO_year4,input_object$bin_features$FFS_year4,
                                                     input_object$bin_features$svm_year4,input_object$bin_features$log_year4,
                                                     input_object$bin_features$CRT_year4,input_object$bin_features$TAN_year4,
                                                     input_object$bin_features$NNET_year4,input_object)
  
  
  input_object$bin_features$consolidated5<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year5,
                                                     input_object$bin_features$LASSO_year5,input_object$bin_features$FFS_year5,
                                                     input_object$bin_features$svm_year5,input_object$bin_features$log_year5,
                                                     input_object$bin_features$CRT_year5,input_object$bin_features$TAN_year5,
                                                     input_object$bin_features$NNET_year5,input_object)
  
  
  input_object$bin_features$consolidated6<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year6,
                                                     input_object$bin_features$LASSO_year6,input_object$bin_features$FFS_year6,
                                                     input_object$bin_features$svm_year6,input_object$bin_features$log_year6,
                                                     input_object$bin_features$CRT_year6,input_object$bin_features$TAN_year6,
                                                     input_object$bin_features$NNET_year6,input_object)
  
  
  input_object$bin_features$consolidated7<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year7,
                                                     input_object$bin_features$LASSO_year7,input_object$bin_features$FFS_year7,
                                                     input_object$bin_features$svm_year7,input_object$bin_features$log_year7,
                                                     input_object$bin_features$CRT_year7,input_object$bin_features$TAN_year7,
                                                     input_object$bin_features$NNET_year7,input_object)
  
  
  input_object$bin_features$consolidated8<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year8,
                                                     input_object$bin_features$LASSO_year8,input_object$bin_features$FFS_year8,
                                                     input_object$bin_features$svm_year8,input_object$bin_features$log_year8,
                                                     input_object$bin_features$CRT_year8,input_object$bin_features$TAN_year8,
                                                     input_object$bin_features$NNET_year8,input_object)
  
  
  input_object$bin_features$consolidated9<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year9,
                                                     input_object$bin_features$LASSO_year9,input_object$bin_features$FFS_year9,
                                                     input_object$bin_features$svm_year9,input_object$bin_features$log_year9,
                                                     input_object$bin_features$CRT_year9,input_object$bin_features$TAN_year9,
                                                     input_object$bin_features$NNET_year9,input_object)
  
  
  input_object$bin_features$consolidated10<-conso_bin(input_object$data_lit,input_object$bin_features$R_Forrest_year10,
                                                      input_object$bin_features$LASSO_year10,input_object$bin_features$FFS_year10,
                                                      input_object$bin_features$svm_year10,input_object$bin_features$log_year10,
                                                      input_object$bin_features$CRT_year10,input_object$bin_features$TAN_year10,
                                                      input_object$bin_features$NNET_year10,input_object)
  
  
  return(input_object)
  
}

#=============================================================
#==============Binomial prediction
#=============================================================  
Bino_pred<-function(input_object){
  if(input_object$parameters["pred_type","content"]=="Binary"){
    
    input_object$pred0<-pred_bin(input_object,"numerics","year0")
    input_object$pred1<-pred_bin(input_object,"numerics","year1")
    input_object$pred2<-pred_bin(input_object,"numerics","year2")
    input_object$pred3<-pred_bin(input_object,"numerics","year3")
    input_object$pred4<-pred_bin(input_object,"numerics","year4")
    input_object$pred5<-pred_bin(input_object,"numerics","year5")
    input_object$pred6<-pred_bin(input_object,"numerics","year6")
    input_object$pred7<-pred_bin(input_object,"numerics","year7")
    input_object$pred8<-pred_bin(input_object,"numerics","year8")
    input_object$pred9<-pred_bin(input_object,"numerics","year9")
    input_object$pred10<-pred_bin(input_object,"numerics","year10")
    
    # #delete later
    if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
    save(input_object,file=paste(c(save_folder,"/input_object_Bino_pred.RData"),collapse = ""))
    # #delete later
    return(input_object)
  }
}

#=============================================================
#==============iso-tonic probabilities of new_data survival
#============================================================= 
isotonic_maker<-function(input_object){
  surv_0<-sample_prob(input_object,0)
  surv_1<-sample_prob(input_object,1)
  surv_2<-sample_prob(input_object,2)
  surv_3<-sample_prob(input_object,3)
  surv_4<-sample_prob(input_object,4)
  surv_5<-sample_prob(input_object,5)
  surv_6<-sample_prob(input_object,6)
  surv_7<-sample_prob(input_object,7)
  surv_8<-sample_prob(input_object,8)
  surv_9<-sample_prob(input_object,9)
  surv_10<-sample_prob(input_object,10)
  survivals<-cbind(surv_10$best_prediction,surv_9$best_prediction,surv_8$best_prediction,surv_7$best_prediction,
                   surv_6$best_prediction,surv_5$best_prediction,surv_4$best_prediction,surv_3$best_prediction,
                   surv_2$best_prediction,surv_1$best_prediction,surv_0$best_prediction)
  survivals_isotonic<-survivals
  for (i in 1:nrow(survivals)) {
    isoreg<-isoreg(c(as.numeric(survivals[i,])))
    survivals_isotonic[i,]<-isoreg$yf
  }
  input_object$survivals<-survivals
  input_object$survivals_isotonic<-survivals_isotonic
  
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_object_isotonic.RData"),collapse = ""))
  # #delete later
  
  return(input_object)
}

#=============================================
#==============Cox Regression
#=============================================
cont_Cox_reg<-function(input_object){
  if(input_object$parameters["pred_type","content"]=="Continuous"){
    input_object$cont_linear<-list()
    data<-cbind(input_object$data_ind,input_object$data$GTIME, input_object$data$GSTATUS)
    names(data)<-c(colnames(input_object$data_ind),"GTIME","GSTATUS")
    #I'm doing the next line just for doing regression
    data[which(data$GTIME==0),"GTIME"]<-1 
    data2<-data[,c("GTIME","GSTATUS")]
    SS<-Surv(data$GTIME, data$GSTATUS)
    data<-data[,1:(ncol(data)-2)]
    # feature selection for cox regression
    fit_rs_lasso = rsig(SS, data, "rs.lasso", n.rep=2)
    
    coefs<-as.data.frame(fit_rs_lasso$beta)
    coefs$vars<-rownames(coefs)
    coefs[,1]<-NULL
    
    input_object$cont_linear$cont_features<-coefs
    
    formula_format<-paste("SS ~ ",paste(coefs$vars, collapse="+"),sep = "")
    formula<-as.formula(formula_format)
    fit <- psm(formula,data=data, dist="weibull", x=TRUE, y=TRUE)
    
    formula_format_all<-paste("SS ~ ",paste(colnames(data), collapse="+"),sep = "")
    formula_all<-as.formula(formula_format_all)
    fit_all <- psm(formula_all,data=data, dist="weibull", x=TRUE, y=TRUE)
    
    fit_auc <- coxph(formula, x=TRUE, y=TRUE, method="breslow", data=data)
    prediction <- predict(fit_auc)
    mult<-(max(data2$GTIME)-min(data2$GTIME))%/%10
    times <- seq(min(data2$GTIME), mult*10, 10)
    AUC_by_CD <- AUC.cd(SS, SS, prediction, prediction, times)
    
    fit_auc_all <- coxph(formula_all, x=TRUE, y=TRUE, method="breslow", data=data)
    prediction_all <- predict(fit_auc_all)
    mult<-(max(data2$GTIME)-min(data2$GTIME))%/%10
    times <- seq(min(data2$GTIME), mult*10, 10)
    AUC_by_CD_all <- AUC.cd(SS, SS, prediction_all, prediction_all, times)
    
    cox_pred<-data.frame(fit$stats)
    cox_pred_all<-data.frame(fit_all$stats)
    
    input_object$cont_linear$cox<-list()
    input_object$cont_linear$cox$R2_feature_slected<-cox_pred["R2",]   
    input_object$cont_linear$cox$R2_feature_all<-cox_pred_all["R2",]  
    input_object$cont_linear$cox$AUC_selected<-AUC_by_CD$iauc
    input_object$cont_linear$cox$AUC_selected_all<-AUC_by_CD_all$iauc
    
    QFUN <- Quantile(fit)
    QFUN_all <- Quantile(fit_all)
    
    # Now use QFUN to get the predicted median survival for the new_data in your data
    #pred.med.surv <- QFUN(predict(fit,type="lp"))
    
    # When asking for only one quantile (here 0.5), it drops dimensions of the output, so 
    # you would have to use the following to bind the new variable to your data.frame 
    #data$pred.med.surv <- as.numeric(dimnames(pred.med.surv)[[2]])
    
    # For multiple quantiles of predicted survival you can use the following:
    preds <- QFUN(q=c(0.25,0.5), predict(fit,data,type="lp"))
    preds_all <- QFUN(q=c(0.25,0.5), predict(fit_all,data,type="lp"))
    data$surv0.5 <- preds[,"0.50"] 
    data$surv0.5_all <- preds_all[,"0.50"] 
    
    input_object$cont_linear$cox$MAD_feature_slected<-sum(abs(data$surv0.5-data2$GTIME))/nrow(data)
    input_object$cont_linear$cox$MAD_feature_all<-sum(abs(data$surv0.5_all-data2$GTIME))/nrow(data)
    input_object$cont_linear$cox$Model_selected<-fit
    input_object$cont_linear$cox$Model_all<-fit_all
    
  }
  # #delete later
  if(input_object$parameters["platform","content"]=="OSC Server"){save_folder<-getwd()}else{save_folder<-input_object$parameters["root_folder","content"]}
  save(input_object,file=paste(c(save_folder,"/input_object_cox.RData"),collapse = ""))
  # #delete later
  return(input_object)
}

#=============================================================
#==============Reporting Module
#============================================================= 

# Start writing to a Report file

Report<-function(input_object){
  
  reporter(input_object,input_object$data_ind0,input_object$data_targets$year0,"pred0")
  reporter(input_object,input_object$data_ind1,input_object$data_targets$year1,"pred1")
  reporter(input_object,input_object$data_ind2,input_object$data_targets$year2,"pred2")
  reporter(input_object,input_object$data_ind3,input_object$data_targets$year3,"pred3")
  reporter(input_object,input_object$data_ind4,input_object$data_targets$year4,"pred4")
  reporter(input_object,input_object$data_ind5,input_object$data_targets$year5,"pred5")
  reporter(input_object,input_object$data_ind6,input_object$data_targets$year6,"pred6")
  reporter(input_object,input_object$data_ind7,input_object$data_targets$year7,"pred7")
  reporter(input_object,input_object$data_ind8,input_object$data_targets$year8,"pred8")
  reporter(input_object,input_object$data_ind9,input_object$data_targets$year9,"pred9")
  reporter(input_object,input_object$data_ind10,input_object$data_targets$year10,"pred10")
  
  return(input_object)
}



#this function make 10 folds of data

data_iso_testing<-function(input_object){
  
  input_object$data_ind$id<-NULL
  
  lit<-as.data.frame(colnames(input_object$data_lit))
  if(nrow(lit)>0){
    names(lit)<-c("variables")
  }
  
  if(regexpr(pattern ='Literature',input_object$parameters["method_fs","content"])>-1){
    if(!is.null(lit)){
      if(nrow(lit)>0){
        data<-cbind(input_object$data_ind,input_object$data_lit)}else{data<-input_object$data_ind}}else{data<-input_object$data_ind}}else{data<-input_object$data_ind}
  for(i in 1:ncol(data)){
    if(!names(data[i]) %in% input_object$use_num_vars){
      data[,i]<-as.factor(data[,i])
    }
  }
  
  n_folds<-as.numeric(input_object$parameters["n_folds","content"])
  year0to10<-data
  
  targets0to10<-input_object$data_targets
  
  data<-cbind(year0to10,targets0to10)
  
  start_C<-which(colnames(data)=="year0")
  end_C<-which(colnames(data)=="year10")
  
  for(i in start_C:end_C){
    data[,i]<-as.factor(data[,i])
    levels(data[,i])[1] <- "One"
    levels(data[,i])[2] <- "Two"
  }
  
  #____________________making the n folds_____________________________________
  
  
  # Create the training and testing data sets
  
  kept_rows<-floor(nrow(data)/n_folds)*n_folds
  
  
  
  #shuffling
  data<-data[sample(nrow(data),kept_rows),]
  #naming the folds ID
  #
  data$folds<-cut(seq(1,nrow(data)),breaks=n_folds,labels=FALSE)
  
  input_object$iso_data<-data
  
  
  return(input_object)
}
#input_object<-Heart_object
# a function for training over the data
#============================================================
iso_year<-function(input_object,alg,year_counter){
  
  balanc_proc<-input_object$parameters["balanc_proc","content"]
  n_folds<-as.numeric(input_object$parameters["n_folds","content"])
  B_alg<-input_object$parameters["B_alg","content"]
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  method_fs<-input_object$parameters["method_fs","content"]
  is_multi<-input_object$parameters["is_multi","content"]
  H_por_low<-as.numeric(input_object$parameters["H_por_low","content"])
  H_por_high<-as.numeric(input_object$parameters["H_por_high","content"])
  
  data<-input_object$iso_data
  
  ##### end of modeling for ten folds
  
  #____________setup ensemble modeling ____
  
  control_<- caret::trainControl(method = "none",  savePredictions = TRUE,
                                 verboseIter = FALSE,returnResamp = "all",classProbs = TRUE, summaryFunction = twoClassSummary)
  
  rows_no<-floor(nrow(data)/n_folds)
  
  formula0<-as.formula(paste("year0 ~ ",paste(input_object$bin_features$consolidated0$variables, collapse="+"),sep = ""))
  formula1<-as.formula(paste("year1 ~ ",paste(input_object$bin_features$consolidated1$variables, collapse="+"),sep = ""))
  formula2<-as.formula(paste("year2 ~ ",paste(input_object$bin_features$consolidated2$variables, collapse="+"),sep = ""))
  formula3<-as.formula(paste("year3 ~ ",paste(input_object$bin_features$consolidated3$variables, collapse="+"),sep = ""))
  formula4<-as.formula(paste("year4 ~ ",paste(input_object$bin_features$consolidated4$variables, collapse="+"),sep = ""))
  formula5<-as.formula(paste("year5 ~ ",paste(input_object$bin_features$consolidated5$variables, collapse="+"),sep = ""))
  formula6<-as.formula(paste("year6 ~ ",paste(input_object$bin_features$consolidated6$variables, collapse="+"),sep = ""))
  formula7<-as.formula(paste("year7 ~ ",paste(input_object$bin_features$consolidated7$variables, collapse="+"),sep = ""))
  formula8<-as.formula(paste("year8 ~ ",paste(input_object$bin_features$consolidated8$variables, collapse="+"),sep = ""))
  formula9<-as.formula(paste("year9 ~ ",paste(input_object$bin_features$consolidated9$variables, collapse="+"),sep = ""))
  formula10<-as.formula(paste("year10 ~ ",paste(input_object$bin_features$consolidated10$variables, collapse="+"),sep = ""))
  
  time_results<-list() 
  
  
  for ( i in 1: n_folds){ 
    test_data<-data[data$folds == i,]
    train_data<-data[data$folds != i,]
    train_data<-train_data[c(as.character(eval(parse(text = paste("input_object$bin_features$consolidated",year_counter,"$variables",sep = "")))),paste("year",year_counter,sep=""))]
    test_data<-test_data[c(as.character(eval(parse(text = paste("input_object$bin_features$consolidated",year_counter,"$variables",sep = "")))),paste("year",year_counter,sep=""))]
    
    tt<-as.data.frame(table(eval(parse(text = paste("train_data$year",year_counter,sep = "")))))
    #table(train_data$TARGET)
    common<-tt$Freq[1]
    rare<-tt$Freq[2]
    k<-1
    if(common<rare){
      temp_<-common
      common<-rare
      rare<-temp_
    }
    if(B_alg=="RUS"){
      
      Train_Two <- train_data[ which(eval(parse(text = paste("train_data$year",year_counter,sep = "")))=="Two"), ]
      Train_One <- train_data[ which(eval(parse(text = paste("train_data$year",year_counter,sep = "")))=="One"), ]
      
      if(nrow(Train_Two)<=nrow(Train_One)){
        sample_size<-nrow(Train_Two)
        Train_One<-Train_One[sample(nrow(Train_One), sample_size), ]
      }else{
        sample_size<-nrow(Train_One)
        Train_Two<-Train_Two[sample(nrow(Train_Two), sample_size), ]
      }
      train_data<-rbind(Train_One,Train_Two)
      
      
    }
    
    if(B_alg=="SMOTE"){
      
      formul<-as.formula(paste("year",year_counter," ~ ",paste(input_object$bin_features$consolidated10$variables, collapse="+"),sep = ""))
      
      H_por_low<-1
      H_por_high<-1
      H_por_low<-(1/H_por_low)
      H_por_high<-H_por_high*100
      perc_over<-100*(common-rare)/(rare*H_por_low)*k
      perc_under<-100*(1/perc_over)*common
      row_no<-nrow(train_data)
      smoted_data<-DMwR::SMOTE(formul, train_data, perc.over = perc_over,perc.under = (((perc_over/100)+1)/(perc_over/100))*H_por_high)
      train_data<-smoted_data[complete.cases(smoted_data), ]
      
    }
    
    if(B_alg=="Hybrid"){
      formul<-as.formula(paste("year",year_counter," ~ ",paste(input_object$bin_features$consolidated10$variables, collapse="+"),sep = ""))
      
      H_por_low_<-(1/H_por_low)
      H_por_high_<-H_por_high*100
      perc_over<-100*(common-rare)/(rare*(((common-rare)/(rare*H_por_low_))))
      
      smoted_data<-DMwR::SMOTE(formul, train_data, perc.over = perc_over,perc.under = (((perc_over/100)+1)/(perc_over/100))*H_por_high_)
      
      train_data<-smoted_data[complete.cases(smoted_data), ]
    }
    
    formula<-eval(parse(text = paste("formula",year_counter,sep = "")))
    
    if(alg=="log"){
      
      log_model<-caret::train(formula,data=train_data, method="glm", family="binomial",trControl = control_, tuneLength = 10, metric="ROC")
      if(class(try(predict(log_model, newdata=test_data, type="prob"),silent = TRUE))!="try-error"){
        time_results[[paste("log_prob","_fold",i,sep="")]]<-predict(log_model, newdata=test_data, type="prob")[2]
        time_results[[paste("log_raw","_fold",i,sep="")]]<-predict(log_model, newdata=test_data, type="raw")
        time_results[[paste("log_merg","_fold",i,sep="")]]<-sum(!is.na(time_results[[paste("log_prob","_fold",i,sep="")]]))
      }
    } 
    
    if(alg=="CRT"){
      
      CRT_model<-caret::train(formula,  data=train_data, method="rpart1SE", trControl = control_, tuneLength = 10, metric="ROC")
      if(class(try(predict(CRT_model, newdata=test_data, type="prob"),silent = TRUE))!="try-error"){
        time_results[[paste("CRT_prob","_fold",i,sep="")]]<-predict(CRT_model, newdata=test_data, type="prob")[2]
        time_results[[paste("CRT_raw","_fold",i,sep="")]]<-predict(CRT_model, newdata=test_data, type="raw")
        time_results[[paste("CRT_merg","_fold",i,sep="")]]<-sum(!is.na(time_results[[paste("CRT_prob","_fold",i,sep="")]]))
        
      }
    } 
    svm_cost<-.5
    svm_sigma<-.001
    if(alg=="svm"){
      svm_model<-caret::train(formula,  data=train_data, method="svmRadial", family="binomial",
                              trControl = control_,  tuneGrid=expand.grid(sigma=svm_sigma , C=svm_cost),
                              tuneLength = 10)    
      if(class(try(predict(svm_model, newdata=test_data, type="prob"),silent = TRUE))!="try-error"){
        time_results[[paste("svm_prob","_fold",i,sep="")]]<-predict(svm_model, newdata=test_data, type="prob")[2]
        time_results[[paste("svm_raw","_fold",i,sep="")]]<-predict(svm_model, newdata=test_data, type="raw")
        time_results[[paste("svm_merg","_fold",i,sep="")]]<-sum(!is.na(time_results[[paste("svm_prob","_fold",i,sep="")]]))
      }
    } 
    
    if(alg=="NNET"){
      NNET_model<-caret::train(formula, data=train_data,method="nnet", family="binomial",
                               trControl = control_,
                               tuneGrid=expand.grid(size=5, decay=0.1), 
                               MaxNWts=20000,tuneLength = 10)
      if(class(try(predict(NNET_model, newdata=test_data, type="prob"),silent = TRUE))!="try-error"){
        time_results[[paste("NNET_prob","_fold",i,sep="")]]<-predict(NNET_model, newdata=test_data, type="prob")[2]
        time_results[[paste("NNET_raw","_fold",i,sep="")]]<-predict(NNET_model, newdata=test_data, type="raw")
        time_results[[paste("NNET_merg","_fold",i,sep="")]]<-sum(!is.na(time_results[[paste("NNET_prob","_fold",i,sep="")]]))
      }
    }
    
    if(alg=="tan"){
      
      ##===making data ready for TAN
      # disceretizing train and test
      
      data_tan_train<-train_data
      y_tr<-train_data[[paste("year",year_counter,sep="")]]
      data_tan_train$type<-"tr"
      
      data_tan_test<-test_data
      data_tan_test$type<-"ts"
      
      data_tan<-rbind(data_tan_train,data_tan_test)
      
      num_vars<-which(names(data_tan) %in% input_object$use_num_vars)
      
      for(j in num_vars){
        if(is.numeric(data_tan_train[,j])){if(is.numeric(data_tan[,j])){
          
          tester<-as.data.frame(data_tan[,j])
          if(tester[1,1]!=tester[nrow(data_tan),1]){
            data_tan[,j]<-arules::discretize(data_tan[,j],method="interval",categories = floor(log(nrow(data_tan))))}else{
              data_tan[,j]<-arules::discretize(data_tan[,j],method="interval",categories = 1)
            }
          
          
          
        }
          
        }
      }
      
      
      data_tan_train<-data_tan[which(data_tan$type=="tr"),]
      data_tan_test<-data_tan[which(data_tan$type=="ts"),]
      
      data_tan_train$type<-NULL
      data_tan_test$type<-NULL
      str(data_tan_train)
      
      # time_results[[paste("data_tan_test_fold_",i,sep="")]]<-data_tan_test
      
      #paste(input_object$bin_features$consolidated0$variables, collapse="+")
      # ind_vars<-as.character(eval(parse(text=paste("input_object$bin_features$consolidated",year_counter,"$variables",sep=""))))
      # tan_vars<-c(ind_vars,paste("year",year_counter,sep="")) 
      # 
      # data_tan_train<-data_tan_train[, which(names(data_tan_train) %in% ind_vars)]
      
      tan_model<- caret::train(data_tan_train, y_tr, 
                               method="tan",trControl = control_, tuneGrid = expand.grid(score= "aic", smooth=0.5),tuneLength = 10)
      if(class(try(predict(tan_model, newdata=data_tan_test, type="prob"),silent = TRUE))!="try-error"){
        time_results[[paste("tan_prob_fold",i,sep="")]]<-predict(tan_model, newdata=data_tan_test, type="prob")[2]   
        time_results[[paste("tan_raw_fold",i,sep="")]]<-predict(tan_model, newdata=data_tan_test, type="raw") 
        time_results[[paste("tan_merg","_fold",i,sep="")]]<-sum(!is.na(time_results[[paste("tan_prob","_fold",i,sep="")]]))
        
      }
    }
    
    
  }
  return(time_results)
}

#============================================================
#input_object$parameters["B_alg","content"]<-"RUS"
#alg<-"NNET"
#year_counter<-0

iso_train<-function(input_object,algorithm){
  
  ##### end of modeling for ten folds
  
  trained_models<-list() 
  
  tasks <- list(
    year0= function() iso_year(input_object,alg,0),
    year1= function() iso_year(input_object,alg,1),
    year2= function() iso_year(input_object,alg,2),
    year3= function() iso_year(input_object,alg,3),
    year4= function() iso_year(input_object,alg,4),
    year5= function() iso_year(input_object,alg,5),
    year6= function() iso_year(input_object,alg,6),
    year7= function() iso_year(input_object,alg,7),
    year8= function() iso_year(input_object,alg,8),
    year9= function() iso_year(input_object,alg,9),
    year10= function() iso_year(input_object,alg,10)
  )
  
  
  
  if(get_os() %in% c("LINUX" ,"MAC")){
    alg<-algorithm
    server_cores<-parallel::detectCores()-2
    out <- mclapply( 
      tasks, 
      function(f) f(), 
      mc.cores = server_cores
    )
    
    trained_models$year0<-out$year0
    trained_models$year1<-out$year1
    trained_models$year2<-out$year2
    trained_models$year3<-out$year3
    trained_models$year4<-out$year4
    trained_models$year5<-out$year5
    trained_models$year6<-out$year6
    trained_models$year7<-out$year7
    trained_models$year8<-out$year8
    trained_models$year9<-out$year9
    trained_models$year10<-out$year10
    
    
  }
  
  if(get_os()=="WINDOWS"){
    #next line is necessary in windows machines
    input_object<<-input_object
    
    alg<<-algorithm
    
    r_cores<-parallel::detectCores(all.tests = FALSE, logical = TRUE)-1
    cl <- makeCluster( min(length(tasks),r_cores ))
    #parallel::makeCluster( min(length(tasks),r_cores ))
    clusterExport(cl, c("iso_year","input_object","alg","twoClassSummary"))
    
    out = clusterApply( 
      cl,
      tasks,
      function(f) f()
    )
    stopCluster(cl)
    
    if(out[[1]]$id==paste("algorithm_",alg,"_year_",0,sep="")){trained_models$year0<-out[[1]]}
    if(out[[2]]$id==paste("algorithm_",alg,"_year_",1,sep="")){trained_models$year1<-out[[2]]}
    if(out[[3]]$id==paste("algorithm_",alg,"_year_",2,sep="")){trained_models$year2<-out[[3]]}
    if(out[[4]]$id==paste("algorithm_",alg,"_year_",3,sep="")){trained_models$year3<-out[[4]]}
    if(out[[5]]$id==paste("algorithm_",alg,"_year_",4,sep="")){trained_models$year4<-out[[5]]}
    if(out[[6]]$id==paste("algorithm_",alg,"_year_",5,sep="")){trained_models$year5<-out[[6]]}
    if(out[[7]]$id==paste("algorithm_",alg,"_year_",6,sep="")){trained_models$year6<-out[[7]]}
    if(out[[8]]$id==paste("algorithm_",alg,"_year_",7,sep="")){trained_models$year7<-out[[8]]}
    if(out[[9]]$id==paste("algorithm_",alg,"_year_",8,sep="")){trained_models$year8<-out[[9]]}
    if(out[[10]]$id==paste("algorithm_",alg,"_year_",9,sep="")){trained_models$year9<-out[[10]]}
    if(out[[11]]$id==paste("algorithm_",alg,"_year_",10,sep="")){trained_models$year10<-out[[11]]}
    
  }
  
  return(trained_models)
}
#======================================

#now I bring all of the trained data model to here
#so I just go to session and load workspaces into Rstudio
#I had to do for each algorithm seperately because data is too big

#alg<-"log"

#given the actual y value (in 0,1 format), dataframe of probabilties and simple prediction (raw) in exch fold,
#your dependent variable(y), and column that specifies folder dedication
#it gives you accuracy, sensivity, specificity, and AUC of models
#y_actual is a fold that is in "One" "Two" format
#raw table is a table in "One" "Two" format
performance_fold<-function(y_actual,raw_table,prob_yes_table){
  
  {
    n_fold<-ncol(prob_yes_table)
    performance<-matrix(0, ncol = n_fold, nrow = 4)
    performance<-as.data.frame(performance)
    
    row.names(performance) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance)[1:n_fold] <- paste("Fold", 1:n_fold, sep="")
    
  }
  #############
  #_________________________calculating the performance of the models_____________
  
  for (i in 1:n_fold){
    #I used pROC because the "AUC" package mess up and sometime I should use (1- AUC package out put)
    performance[1,i]<-as.numeric(pROC::auc(y_actual, prob_yes_table[,i]))
    performance[3,i]<-caret:: sensitivity(raw_table[,i],y_actual)
    performance[4,i]<-caret:: specificity(raw_table[,i],y_actual)
    performance[2,i]<-(as.data.frame(confusionMatrix(raw_table[,i],y_actual)$overall))[1,]
  }
  
  for(i in 1:nrow(performance)){
    
    performance$avg[i]<-(sum(performance[i,1:n_fold])/n_fold)}
  
  return(performance)
}
#____________________

# it gives performance measures: "accuracy, sensivity, specificity, and AUC of models", 
# you give atual table of dependent variables (y_table), 
# and predicted values (raw prediction: raw_table, probability prediction: prob_yes_table),
# it gives performance measures for each column of dependent variables' table and overal performance
performance_table<-function(y_table,raw_table,prob_yes_table){
  
  
  #defining a matrix for performance
  {
    n_year<-ncol(y_table)
    performance<-matrix(0, ncol = n_year, nrow = 4)
    performance<-as.data.frame(performance)
    
    row.names(performance) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance)[1:n_year] <- paste("Year", 0:(n_year-1), sep="")
    
  }
  #############
  #_________________________calculating the performance of the models_____________
  
  for (i in 1:n_year){
    #I used pROC because the "AUC" package mess up and sometime I should use (1- AUC package out put)
    performance[1,i]<-as.numeric(pROC::auc(y_table[,i], prob_yes_table[,i]))
    performance[3,i]<-caret:: sensitivity(as.factor(raw_table[,i]),as.factor(y_table[,i]))
    performance[4,i]<-caret:: specificity(as.factor(raw_table[,i]),as.factor(y_table[,i]))
    performance[2,i]<-(as.data.frame(confusionMatrix(raw_table[,i],y_table[,i])$overall))[1,]
  }
  
  #
  
  for(i in 1:nrow(performance)){
    performance$avg[i]<-(sum(performance[i,1:n_year])/n_year)}
  
  return(performance)
}
#____________________

#this function is for finding probability and performance of an algorithm over all the folds
# it yields probability and prediction over all the folds

iso_evluation_year<-function(input_object,year,alg){
  n_folds<-as.numeric(input_object$parameters["n_folds","content"])
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  
  rows_no<-floor(nrow(input_object$iso_data)/n_folds)
  data<-input_object$iso_data
  
  #making data frames for putting results of predictions
  {
    resul_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_raw<-as.data.frame(resul_raw)
    
    resul_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_prob<-as.data.frame(resul_prob)
    
  }
  #
  #i<-1
  #doing prediction
  for (i in 1:n_folds) {
    
    run_log<-"Yes"
    run_CRT<-"Yes"
    run_svm<-"Yes"
    run_NNET<-"Yes"
    run_tan<-"Yes"
    
    #checking if any of the models is available
    {
      if(alg=="log"){
        if(is.null(eval(parse(text =
                              paste("log_train$mod_log_year",year,"_fold",i,"$finalModel$coefficients",sep = ""))))){
          run_log<-"No"}}
      
      if(alg=="CRT"){
        if(is.null(eval(parse(text = 
                              paste("CRT_train$mod_CRT_year",year,"_fold",i,"$finalModel$coefficients",sep = ""))))){
          run_CRT<-"No"}}
      
      if(alg=="svm"){
        if(is.null(eval(parse(text =
                              paste("svm_train$mod_svm_year",year,"_fold",i,"$finalModel",sep = ""))))){
          run_svm<-"No"}}
      
      if(alg=="NNET"){
        if(is.null(eval(parse(text =
                              paste("NNET_train$mod_NNETT_year",year,"_fold",i,"$finalModel$coefnames",sep = ""))))){
          run_NNET<-"No"}}
      
      
      if(alg=="tan"){
        if(is.null(eval(parse(text =
                              paste("tan_train$mod_tan_year",year,"_fold",i,"$finalModel$coefficients",sep = ""))))){
          run_tan<-"No"}}
    }
    
    test_data<-input_object$iso_data[which(input_object$iso_data$folds==i ),]  
    
    if(run_log=="Yes"){
      if(alg=="log"){
        mod_log<-eval(parse(text = paste("log_train$mod_log_year",year,"_fold",i,sep = "")))
        resul_raw[,i]<-predict(mod_log, newdata=test_data, type="raw")
        resul_prob[,i]<-predict(mod_log, newdata=test_data, type="prob")
      }}
    
    if(run_svm=="Yes"){
      if(alg=="svm"){
        mod_svm<-eval(parse(text = paste("svm_train$mod_svm_year",year,"_fold",i,sep = "")))
        resul_raw[,i]<-predict(mod_svm, newdata=test_data, type="raw")
        resul_prob[,i]<-predict(mod_svm, newdata=test_data, type="prob")
      }}
    
    if(run_CRT=="CRT"){
      if(alg=="CRT"){
        mod_CRT<-eval(parse(text = paste("CRT_train$mod_CRT_year",year,"_fold",i,sep = "")))
        resul_raw[,i]<-predict(mod_CRT, newdata=test_data, type="raw")
        resul_prob[,i]<-predict(mod_CRT, newdata=test_data, type="prob")}}
    
    if(run_NNET=="NNET"){
      if(alg=="NNET"){
        mod_NNET<-eval(parse(text = paste("NNET_train$mod_NNET_year",year,"_fold",i,sep = "")))
        resul_raw[,i]<-predict(mod_NNET, newdata=test_data, type="raw")
        resul_prob[,i]<-predict(mod_NNET, newdata=test_data, type="prob")}}
    
    if(run_tan=="tan"){
      if(alg=="tan"){
        mod_tan<-eval(parse(text = paste("tan_train$mod_tan_year",year,"_fold",i,sep = "")))
        resul_raw[,i]<-predict(mod_tan, newdata=data_tan_test, type="raw")
        resul_prob[,i]<-predict(mod_tan, newdata=data_tan_test, type="prob")}}
    
  }
  
  if(run_log=="Yes"|run_svm=="Yes"|run_NNET=="Yes"|run_CRT=="Yes"|run_tan=="Yes"){
    if(alg=="log"|alg=="svm"|alg=="CRT"|alg=="NNET"|alg=="tan"){
      resul_prob_YES<-1-resul_prob}}
  
  y_col<-paste("year",year,sep="")
  temp<-data[data["folds"] == i,]
  temp[y_col]<-as.factor(temp[[y_col]])
  
  # levels(temp[[y_col]])[1]<- 0
  # levels(temp[[y_col]])[2]<- 1
  
  test_table<-(temp[[y_col]])
  
  
  performance<-performance_fold(test_table,resul_raw,resul_prob_YES)
  
  #reporting
  {
    objecter<-list()
    if(run_log=="Yes"|run_svm=="Yes"|run_NNET=="Yes"|run_CRT=="Yes"|run_tan=="Yes"){
      if(alg=="log"|alg=="svm"|alg=="CRT"|alg=="NNET"|alg=="tan"){
        objecter$performance<-performance
        objecter$resul_raw<-resul_raw
        objecter$resul_prob<-resul_prob
      }}
    
  }
  
  return(objecter)
}
