#!/usr/bin/env Rscript

# Function to summarize coda
coda_rows_to_cols <- function(var_list, coda_sum = NULL, df = NULL, colnams = NULL, to_join = NULL){
  
  # if we are working directly with the coda object
  if(is.null(df)){
    sum_tb <- coda_sum[["statistics"]]
    quan_tb <- coda_sum[["quantiles"]]
    
    voi_list <- list()
    column_names <- list()
    j = 1
    
    for(i in c(1:length(var_list))){
      
      searchterm <- paste("^", var_list[i], "\\[", sep = "")
      
      # Check if there is more than instance of the variable or not
      if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we find nothing
        searchterm <- paste("^", var_list[i], sep = "") # check if there is only one instance and correct the search term
        if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
          searchterm <- paste(utils::glob2rx(var_list[i]), sep = "") # check if user is using *
          if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
            print(paste("Warning: ", var_list[i], " not found in coda summary output", sep = ""))
            next
          }
        }
      }
      
      voi_list[[j]] <- sum_tb[grep(searchterm, row.names(sum_tb)),1]
      voi_list[[j+1]] <- quan_tb[grep(searchterm, row.names(quan_tb)),1]
      voi_list[[j+2]] <- quan_tb[grep(searchterm, row.names(quan_tb)),5]
      
      if(is.null(colnams)){
        
        column_names[[j]] <- paste("B_", var_list[i], sep = "")
        column_names[[j+1]] <- paste("pc2.5_", var_list[i], sep = "")
        column_names[[j+2]] <- paste("pc97.5_", var_list[i], sep = "")
        
      }
      
      if(!is.null(colnams)){
        
        column_names[[j]] <- paste(colnams[i], sep = "")
        column_names[[j+1]] <- paste("pc2.5_", colnams[i], sep = "")
        column_names[[j+2]] <- paste("pc97.5_", colnams[i], sep = "")
        
      }
      
      j = j + 3
      
    }
    suppressMessages(out_df <- dplyr::bind_cols(voi_list))
    colnames(out_df) <- column_names
    return(out_df)
  }
  
  # If we have already converted the coda object to a tidyverse-style df
  if(!is.null(df)){
    
    #voi_list <- list()
    #column_names <- list()
    #j = 1
    
    for(i in c(1:length(var_list))){
      
      searchterm <- var_list[i]
      
      # Check if there is more than instance of the variable or not
      if(length(grep(searchterm, df$var)) == 0){ # if we find nothing
        print(paste("Warning: ", var_list[i], " not found in df summary output", sep = ""))
        next
      }
    }
      
      df <- df %>%
        dplyr::filter(var %in% var_list) %>%
        tidyr::pivot_wider(id_cols = c(ID1, site), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5))
      
      if(!is.null(to_join)){
        out_df <- cbind(to_join, df)
      } else{
        out_df <- df
      }
      
    #   voi_list[[j]] <- df$mean[grep(searchterm, df$var)]
    #   voi_list[[j+1]] <- "ID1"
    #   voi_list[[j+1]] <- df$pc2.5[grep(searchterm, df$var)]
    #   voi_list[[j+2]] <- df$pc97.5[grep(searchterm, df$var)]
    #   
    #   if(is.null(colnams)){
    #     
    #     column_names[[j]] <- paste("B_", var_list[i], sep = "")
    #     column_names[[j+1]] <- "ID1"
    #     column_names[[j+2]] <- paste("pc2.5_", var_list[i], sep = "")
    #     column_names[[j+3]] <- paste("pc97.5_", var_list[i], sep = "")
    #     
    #   }
    #   
    #   if(!is.null(colnams)){
    #     
    #     column_names[[j]] <- paste(colnams[i], sep = "")
    #     column_names[[j+1]] <- "ID1"
    #     column_names[[j+2]] <- paste("pc2.5_", colnams[i], sep = "")
    #     column_names[[j+3]] <- paste("pc97.5_", colnams[i], sep = "")
    #     
    #   }
    #   
    #   j = j + 4
    #   
    # }
    # suppressMessages(out_df <- dplyr::bind_cols(voi_list))
    # colnames(out_df ) <- column_names
    return(out_df)
    
  }
  
  
}

# Function to extract posterior means, and 2.5 and 97.5 CI quantiles
# takes a list of variable names and the coda summary
# all variables in the list MUST have the same length posterior outputs
coda_pivot_longer <- function(var_list, coda_sum, colnams = NULL){
  
  sum_tb <- coda_sum[["statistics"]]
  quan_tb <- coda_sum[["quantiles"]]
  
  voi_list <- list()
  column_names <- list()
  j = 1
  
  for(i in c(1:length(var_list))){
    
    searchterm <- paste("^", var_list[i], "\\[", sep = "")
    
    # Check if there is more than instance of the variable or not
    if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we find nothing
      searchterm <- paste("^", var_list[i], sep = "") # check if there is only one instance and correct the search term
      if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
        searchterm <- paste(utils::glob2rx(var_list[i]), sep = "") # check if user is using *
        if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
          print(paste("Warning: ", var_list[i], " not found in coda summary output", sep = ""))
          next
        }
      }
    }
    
    voi_list[[j]] <- sum_tb[grep(searchterm, row.names(sum_tb)),1]
    voi_list[[j+1]] <- quan_tb[grep(searchterm, row.names(quan_tb)),1]
    voi_list[[j+2]] <- quan_tb[grep(searchterm, row.names(quan_tb)),5]
    
    if(is.null(colnams)){
      
      column_names[[j]] <- paste("B_", var_list[i], sep = "")
      column_names[[j+1]] <- paste("ci2.5_", var_list[i], sep = "")
      column_names[[j+2]] <- paste("ci97.5_", var_list[i], sep = "")
      
    }
    
    if(!is.null(colnams)){
      
      column_names[[j]] <- paste(colnams[i], sep = "")
      column_names[[j+1]] <- paste("ci2.5_", colnams[i], sep = "")
      column_names[[j+2]] <- paste("ci97.5_", colnams[i], sep = "")
      
    }
    
    j = j + 3
    
  }
  suppressMessages(df <- dplyr::bind_cols(voi_list))
  colnames(df) <- column_names
  
  df <- df %>%
    rowid_to_column("lagID")
  
  # pivot longer posteriors
  if(!is.null(colnams)){
    var_list <- colnams
  }
  df <- df %>%
    pivot_longer(cols = c(var_list), names_to = "var")
  
  # pivot_longer ci2.5
  temp <- c()
  
  col_list <- unlist(column_names[grep("2.5",column_names)])
  for( i in c(1:length(var_list))){
    for( j in c(1:nrow(df))){
      
      # if the var name in the df row matches i
      # then assign the correct ci value to temp
      if(grepl(var_list[i],df$var[j])){
        temp2 <- select(df, ends_with(col_list[i]))[j,]
        temp[[j]] <- as.numeric(temp2)
      }
      
    }
  }
  df_longer <- df %>%
    mutate(ci2.5 = temp) %>%
    select(-contains(col_list))
  
  # pivot_longer ci97.5
  temp <- c()
  if(!is.null(colnams)){
    var_list <- colnams
  }
  col_list <- unlist(column_names[grep("97.5",column_names)])
  for( i in c(1:length(var_list))){
    for( j in c(1:nrow(df_longer))){
      
      # if the var name in the df row matches i
      # then assign the correct ci value to temp
      if(grepl(var_list[i],df_longer$var[j])){
        temp2 <- select(df_longer, ends_with(col_list[i]))[j,]
        temp[[j]] <- as.numeric(temp2)
      }
      
    }
  }
  df_longer <- df_longer %>%
    mutate(ci97.5 = temp) %>%
    select(-contains(col_list))
  
  #df_longer$ci2.5 <- as.numeric(df_longer$ci2.5)
  #df_longer$ci97.5 <- as.numeric(df_longer$ci97.5)
  
  
  return(df_longer)
  
}


sum_timeseries <- function(s){
  
  # Create necessary folders if they do not already exist
  if(!file.exists("output_dfs")) { dir.create("output_dfs")}
  
  # key for which site corresponds to which site ID
  if(s == 1){
    key = "seg"
  } else if(s == 2){
    key = "ses"
  } else if(s == 3){
    key = "wjs"
  } else if(s == 4){
    key = "mpj"
  } else if(s == 5){
    key = "vcp"
  } else if(s == 6){
    key = "vcm1"
  } else if(s == 7){
    key = "vcm2"
  } else if(s == 8){
    key = "vcs"
  }
  
  # Read in summary data
  dffilename <- paste("./output_dfs/df_sum_noECO_", key, ".csv", sep = "")
  df_sum <- read.csv(dffilename)
  
  # Define summary filenames based on key
  out_daily_dffilename <- paste("./output_dfs/df_daily_", key, ".csv", sep = "")
  out_weekly_dffilename <- paste("./output_dfs/df_weekly_", key, ".csv", sep = "")
  out_yearly_dffilename <- paste("./output_dfs/df_yearly_", key, ".csv", sep = "")
  
  
  # Load join data for the correct site/key
  load(paste("./clean_data/dataIN_",key,".RData",sep=""))
  load(paste("./clean_data/dataIN_wue_",key,".RData",sep=""))
  load(paste("./clean_data/dataIN_gpp_",key,".RData",sep=""))
  
  # define join df names based on key
  dataIN <- get(paste("dataIN_",key,sep="")) # daily time series
  dataIN_wue <- get(paste("dataIN_wue_",key,sep="")) # WUE time series, by block length
  dataIN_gpp <- get(paste("dataIN_gpp_",key,sep="")) # seasonal time series, for WUE weighted by GPP
  
  # Summarize into timeseries format
  varlist <- c("ET.pred", "ET.rep", "E.model", "T.pred", "T.ratio")
  daily_df <- coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN)
  write.csv(daily_df, out_daily_dffilename)
  
  varlist <- c("WUE.pred", "WUE.wght")
  weekly_df <- coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN_wue)
  
  write.csv(weekly_df, out_weekly_dffilename)
  
  varlist <- c("WUE.annual", "WUE.winter", "WUE.spring", "WUE.summer")
  yearly_df <- coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN_gpp)
  
  write.csv(yearly_df, out_yearly_dffilename)
  
}

###############################################################################
# Written for work on the Multicomp project
# Updated by Michael Fell 9/10/2018
#   Added an option for an arbitrary function
#   Added more informative error messages
###############################################################################
# A function to summarize output from a JAGS or OpenBUGS model.
coda.fast <- function(coda=NULL, thin=1, FUN=NULL, colname = "optfun", ...){
  
  if(is.null(coda)){
    message("No coda object provided. Summarizing nothing is too philosophical")
    message("a task for this function.")
    stop()
  }
  
  # Get the number of chains
  chains <- length(coda)
  
  codal <- length(coda[[1]][,1])
  
  # Combine chains
  Ftab <- numeric()
  for(i in 1:chains){
    Ftab <- rbind(Ftab, coda[[i]][(0:(codal-1))%%thin==0,])
  }
  
  # mean, sd, 95% CrI table
  pred <- matrix(nrow=dim(Ftab)[2], ncol=5)
  colnames(pred)<-c("mean", "median", "sd","pc2.5","pc97.5")
  
  # Fill table with stats
  pred[,1] <- colMeans(Ftab) #calculate predicted mean RW's
  pred[,2] <- apply(X=Ftab, MARGIN=2, FUN=median, na.rm=TRUE)
  pred[,3] <- apply(X=Ftab, MARGIN=2, FUN=sd,na.rm=TRUE) #calculate sd, apply column wise
  pred[,4] <- apply(X=Ftab, MARGIN=2, FUN=quantile,probs=c(0.025),na.rm=TRUE) 
  pred[,5] <- apply(X=Ftab, MARGIN=2, FUN=quantile,probs=c(0.975),na.rm=TRUE)
  
  pred <- data.frame(pred)
  if(length(rownames(pred)) == length(colnames(coda[[1]]))){
    rownames(pred) <- colnames(coda[[1]])
  }else{
    message("Could not determine row (variable) names from coda.")
  }
  
  # Optional Function
  if(!is.null(FUN))
  {
    placeholder <- tryCatch(
      {
        out <- apply(X=Ftab, MARGIN=2, FUN=FUN, na.rm=TRUE, ...)
        out <- as.matrix(out)
        if(ncol(out) == nrow(pred)){
          out <- t(out)
        }
        
        pred <- cbind(pred, out)
        colnames(pred) <- c("mean", "median", "sd","pc2.5","pc97.5", colname)
      },
      error=function(cond){
        message(paste0("A problem led to an error executing the optional function."))
        message("The result without the added function will be returned.")
        message("Here is the original error:")
        message(cond)
      },
      warning=function(cond){
        message("A warning occurred executing the optional function.")
        message("The result without the added function will be returned.")
        message("Here is the original warning:")
        message(cond)
      },
      finally={
        return(pred)
      }
    )
  }
  
  # Return the summary values
  return(pred)
}

# A function to find initial values for a JAGS or OpenBUGS model.
# Output:
# The output from this function is a list containing two elements. The first
# contains the names of the variables and their indicies. These are useful 
# when using removevars to remove variables that don't need initial values
# in JAGS. The second element contains a list of initial values (this is a 
# list of lists).


initfind <- function(coda, iteration=0, OpenBUGS=FALSE){
  mcmcin <- coda # TODO change all mcmcin to coda in the future MKF 11/27/18
  # If mcmc.list convert to mcmc
  if(is.mcmc.list(mcmcin)==TRUE){
    mcmcin <- mcmc(data=mcmcin, thin=1)
  }
  
  # Get the number of chains
  n.chains <- length(mcmcin)
  
  # get variable names from a list
  var.names <- colnames(mcmcin[[1]])
  var.dims <- dim(mcmcin[[1]])
  if(iteration==0){
    iteration <- var.dims[1]
  }
  
  if(sum(var.names=="deviance")>0){
    var.names <- var.names[-which(var.names=="deviance")]
    var.dims[2] <- var.dims[2]-1 # correct for removing deviance
  }
  
  # Get names and dimension of each variable since the output is a table
  var.names2 <- apply(X=as.matrix(var.names), MARGIN=c(1), FUN=strsplit, split="\\x5B", perl=TRUE)
  var.names2 <- lapply(X=var.names2, FUN=unlist)
  var.names2 <- lapply(X=var.names2, FUN=gsub, pattern="\\x5D", replacement="", perl=TRUE)
  
  # Create a table of names and dimensions
  # Column 1 is the variable me column 2 has the dimensions
  var.info <- matrix(nrow=length(var.names), ncol=3)
  for(i in 1:length(var.names2)){
    if(length(var.names2[[i]]) > 1){
      var.info[i,] <- c(var.names2[[i]], var.names[i])
    }else if(length(var.names2[[i]]) == 1){
      var.info[i,] <- c(var.names2[[i]], 1, var.names[i])
      #print(i)
      #print(var.names2[[i]])
    }else{
      stop("A variable name has incorrect dimensions for parsing.") 
    }
  }
  
  # Get variable names
  unique.names <- unique(var.info[,1])
  initsoutall <- list()
  
  
  for(k in 1:n.chains){
    initsout <- list()
    for(i in 1:length(unique.names)){
      sel <- which(var.info[,1]==unique.names[i])
      #sel2 <- grep(pattern=paste0("^",unique.names[i],"\\x5B"), x=var.names)
      
      # Make sure the above selections worked
      #if(length(sel) != length(sel2)){
      #  stop("Error matching unique variable names with MCMC output")  
      #}
      name.sel <- var.info[sel,3]
      index <- apply(X=as.matrix(var.info[sel,2]), MARGIN=1, FUN=strsplit, split=",", perl=TRUE)
      index <- lapply(X=index, FUN=unlist)
      index <- matrix(data=as.numeric(unlist(index)), nrow=length(index), ncol=length(index[[1]]), byrow=TRUE)
      
      # There are possibly easier ways to do this but they make more assumptions
      dims <- as.numeric(apply(X=index, MARGIN=2, FUN=max))
      variable <- array(data=NA, dim=dims)
      
      # Fill the new variable with the correct values
      for(j in 1:dim(index)[1]){
        # The output into mcmc objects lists names in the order R stacks them
        # in arrays so the single index for the variable references the 
        # correct array location.
        variable[j] <- mcmcin[[k]][iteration, which(colnames(mcmcin[[k]])==name.sel[j])]
      }
      
      # Use dims to produce a new array to store the data
      initsout[[i]] <- variable
    } # End of variable loop
    names(initsout) <- unique.names
    initsoutall[[k]] <- initsout
  } # End of chain loop
  
  listout <- list(unique.names, initsoutall)
  names(listout) <- c("variables", "initials")
  
  # Account for OpenBUGS by outputing 1 dimensional arrays as vectors.
  if(OpenBUGS==TRUE){
    for(i in 1:n.chains){
      for(j in 1:length(listout[[2]][[i]])){
        if(length(dim(listout[[2]][[i]][[j]]))==1){
          listout[[2]][[i]][[j]] <- as.vector(listout[[2]][[i]][[j]])
        }
      }
    }
  }
  
  return(listout)
} # End of function


###############################################################################
#
# Removes specific variables from the initial values
#
###############################################################################

# A function to remove variables that don't need initial values in JAGS.

removevars <- function(initsin, variables){
  n.chains <- length(initsin[[2]])
  n.vars <- 1:length(initsin[[1]])
  n.vars <- n.vars[-variables]
  
  var.names <- initsin[[1]][n.vars]
  
  new.inits <- list()
  for(i in 1:n.chains){
    chain.inits <- list()
    for(k in 1:length(n.vars)){
      chain.inits[[k]] <- initsin[[2]][[i]][[n.vars[k]]] 
    } # End of k loop
    names(chain.inits) <- var.names
    new.inits[[i]] <- chain.inits
  } # End of i loop
  
  output <- list(var.names, new.inits)
  names(output) <- c("variables", "initials")
  
  return(output)
  
} # End of function
