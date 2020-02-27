
library(pbapply)

# # test different models (here at the level of random effects) to see which performs best

# rse_denom <- sum(
#     (mean(temp_shapefile_data_inla[!is.na(temp_shapefile_data_inla$CLUSTER), "raw_prev_rate"]) -
#          (temp_shapefile_data_inla[!is.na(temp_shapefile_data_inla$CLUSTER), "raw_prev_rate"])) ** 2)

formulae_list <- list("formulae_list1" = formulae_list1)
# formulae_list <- list("formulae_list2" = formulae_list2)
# formulae_list <- list("formulae_list3" = formulae_list3)
# formulae_list <- list("formulae_list4" = formulae_list4)
# formulae_list <- list("formulae_list5" = formulae_list5)
# formulae_list <- list("formulae_list6" = formulae_list6)
# formulae_list <- list("formulae_list7" = formulae_list7)

# j <- 1
for (j in 1:length(formulae_list)) {
    
    covar_formulae <- formulae_list[[j]]
    
    # print(covar_formulae)
    print(start_time <- Sys.time())
    
    cl <- makeCluster(detectCores())
    clusterExport(cl, "temp_shapefile_data_inla")
    clusterExport(cl, "rse_denom")
    clusterExport(cl, "covar_formulae")
    clusterExport(cl, "pred_measure")
    clusterExport(cl, "NGA.adj")
    clusterExport(cl, "start_time")
    clusterEvalQ(cl, library(INLA))
    
    # x <- 1
    inla_perform <- function(x) {
        
        # print(paste(x, " of ", length(covar_formulae), ": ", paste(trimws(format(covar_formulae[[x]]), "l"), collapse = ""), sep = ""))
        # if (x %% 100 == 0) {
        #     print(Sys.time() - start_time)
        # }
        
        cluster_list <- sort(temp_shapefile_data_inla$CLUSTER[!is.na(temp_shapefile_data_inla$CLUSTER)])
        pred_list <- list()
        pred_list_glm <- list()
        
        formula <- covar_formulae[[x]]
        family <- "binomial"
        # family <- "zeroinflatedbinomial1"
        
        # cluster_num <- 2
        for (cluster_num in 1:length(cluster_list)) {
            
            message(sprintf("\rFit cross-validation step %d/%d", cluster_num,
                            length(cluster_list)), appendLF = FALSE)
            
            temp_shapefile_data_inla_copy <- temp_shapefile_data_inla
            cluster_pos <- which(temp_shapefile_data_inla_copy$CLUSTER == cluster_list[cluster_num])
            temp_shapefile_data_inla_copy[cluster_pos, "response"] <- NA
            
            inla_loo <- inla(formula = formula,
                             family = family,
                             # family = "binomial",
                             # family = "zeroinflatedbinomial1",
                             Ntrials = count,
                             data = temp_shapefile_data_inla_copy,
                             control.family = list(link = "logit"),
                             control.predictor = list(compute = TRUE, link = 1),
                             control.fixed = list(prec = 0.0001),
                             control.compute = list(dic = TRUE),
                             verbose = FALSE)
            
            # pred_loo <- data.frame(
            #     CLUSTER = cluster_list[cluster_num],
            #     raw_prev_rate = temp_shapefile_data_inla[cluster_pos, "raw_prev_rate"],
            #     urban = temp_shapefile_data_inla[cluster_pos, "urban"],
            #     fit = inla_loo$summary.fitted.values$mean[cluster_pos]
            # )
            
            pred_loo <- inla_loo$summary.fitted.values[cluster_pos, ]
            pred_loo <- data.frame(temp_shapefile_data_inla_copy[cluster_pos, c("CLUSTER", "raw_prev_rate", "urban")],
                                   pred_loo)
            # pred_loo$sq_err <- (pred_loo$fit - pred_loo$raw_prev_rate)**2
            
            pred_list[[cluster_num]] <- pred_loo
            
            ##########
            
            # # run glm on uncomplicated formulas
            temp_shapefile_data_inla_sub <- temp_shapefile_data_inla[!is.na(temp_shapefile_data_inla$CLUSTER), ]
            cluster_pos_sub <- which(temp_shapefile_data_inla_sub$CLUSTER == cluster_list[cluster_num])
            
            if (sum(grepl("f\\(", as.character(covar_formulae[[x]]))) == 0) {
                
                glm_loo <- glm(formula = as.formula(paste(gsub("response", "raw_prev_rate",
                                                               as.character(covar_formulae[[x]]))[c(2,1,3)], collapse = " ")),
                               data = temp_shapefile_data_inla_sub[-cluster_pos_sub, ],
                               weight = count,
                               family = binomial)
                pred_loo_glm <- predict(glm_loo, newdata = temp_shapefile_data_inla_sub[cluster_pos_sub, ],
                                        type = "response")
                pred_loo_glm <- data.frame(temp_shapefile_data_inla_sub[cluster_pos_sub, c("CLUSTER", "raw_prev_rate", "urban")],
                                           fit = pred_loo_glm)
                
            } else {
                
                pred_loo_glm <- data.frame(temp_shapefile_data_inla_sub[cluster_pos_sub, c("CLUSTER", "raw_prev_rate", "urban")],
                                           fit = NA)
                
            }
            
            pred_list_glm[[cluster_num]] <- pred_loo_glm
            
        }
        
        pred_prov <- do.call(rbind, pred_list)
        # # Can change pred to mean or mode or X0.5quant
        # # binomial with x0.5quant best approx the glm
        # pred_prov$fit <- pred_prov$X0.5quant
        pred_prov$fit <- pred_prov[, pred_measure]
        pred_prov$sq_err <- (pred_prov$fit - pred_prov$raw_prev_rate) ** 2
        
        mse_prov  <- mean(pred_prov$sq_err)
        rse_denom <- sum((mean(temp_shapefile_data_inla_sub$raw_prev_rate) - (temp_shapefile_data_inla_sub$raw_prev_rate)) ** 2)
        (rse_prov <- sum(pred_prov$sq_err) / rse_denom)
        (cor_prov <- cor(x = pred_prov$fit, y = pred_prov$raw_prev_rate, method = "pearson"))
        
        pred_cov2  <- do.call(rbind, pred_list_glm)
        pred_cov2$sq_err <- (pred_cov2$fit - pred_cov2$raw_prev_rate) ** 2
        
        mse_cov2   <- mean(pred_cov2$sq_err)
        rse_denom2 <- sum((mean(temp_shapefile_data_inla_sub$raw_prev_rate) - (temp_shapefile_data_inla_sub$raw_prev_rate)) ** 2)
        (rse_cov2  <- sum(pred_cov2$sq_err) / rse_denom2)
        (cor_cov2  <- cor(x = pred_cov2$fit, y = pred_cov2$raw_prev_rate, method = "pearson"))
        
        pred_prov <- data.frame(pred_prov,
                                glm_fit = pred_cov2$fit,
                                glm_sq_err = pred_cov2$sq_err)
        
        write.csv(pred_prov, file = paste0("output/inla_output/temp/inla_raw_perform_results_", names(covar_formulae)[[x]], ".csv"))
        
        return(c(formula = paste(trimws(format(formula), "l"), collapse = ""),
                 mse = mse_prov, rse = rse_prov, cor_coef = cor_prov,
                 mse_glm = mse_cov2, rse_glm = rse_cov2, cor_coef_glm = cor_cov2))
    }
    
    perform_list <- pblapply(cl = cl, X = 1:length(covar_formulae), FUN = inla_perform)
    stopCluster(cl)
    
    print(Sys.time() - start_time)
    
    perform_df <- as.data.frame(do.call(rbind, perform_list))
    perform_df$mse <- as.numeric(as.character(perform_df$mse))
    perform_df$rse <- as.numeric(as.character(perform_df$rse))
    perform_df$cor_coef <- as.numeric(as.character(perform_df$cor_coef))
    # perform_df$rank_cor <- as.numeric(as.character(perform_df$rank_cor))
    perform_df <- perform_df[order(perform_df$rse, decreasing = F), ]
    (head(perform_df, 5))
    
    write.csv(x = perform_df,
              file = paste0("output/inla_output/temp/inla_perform_results_", names(formulae_list), ".csv"),
              row.names = F)
}


################################################################################

(perform_fh <- grep("inla_perform_results_\\w+.csv", dir(path = "output/inla_output/temp"), value = T))

perform_list <- lapply(perform_fh, function(x) {
    # print(x)
    read.csv(paste0("output/", x))
})
perform_df <- do.call(rbind, perform_list)
perform_df <- unique(perform_df)
dim(perform_df)

perform_df <- perform_df[order(perform_df$rse, decreasing = F), ]

# # save for future reference
write.csv(perform_df, file = "output/inla_output/temp/inla_perform_results_grand.total.csv", row.names = F)
