
##### Run INLA BYM model to get predictions for LGA prevalence

##### Directions: Run each section separately to catch screen output

################################################################################

##### Load needed packages and file locations

library(rgdal)
library(ggplot2)
library(broom)
library(raster)
library(ggrepel)
library(spdep)
library(INLA)
library(stringr)
library(scales)

shapefile_name <- "data/raw/shapefiles/nigeria_polbnda_admin_2_unsalb/Admin_2/NGA_cnty_admin2/nga_polnda_adm2_1m_salb.shp"
shapefile_adm1_name <- "data/raw/shapefiles/nigeria_polbnda_admin_1_unsalb/Admin_1/NGA_cnty_admin1/nga_polbnda_adm1_1m_salb.shp"
shapefile_data_name <- "data/processed/shapefiles/temp_shapefile_data.csv"
zone_list_name <- "data/raw/shapefiles/zone_list.csv"
prev_survey_clean_df_covar_file_name <- "data/processed/prev_survey/prev_survey_clean_df_covar.csv"

topRSE_file_name <- "output/glm_output/glm_perform_results_grand.topRSE.csv"
topCor_file_name <- "output/glm_output/glm_perform_results_grand.topCor.csv"

inla_results_file_name <- "output/inla_output/temp/inla_perform_results_grand.total.csv"


################################################################################

##### This section is for loading shapefiles and data

### Read in the shapefiles
### https://gist.github.com/lmullen/8375785#file-shapefile-r-L12

shapefile <- rgdal::readOGR(shapefile_name, stringsAsFactors = FALSE)
shapefile_df <- ggplot2::fortify(shapefile, region = "ADM2_CODE")
plot(shapefile)

shapefile_adm1 <- rgdal::readOGR(shapefile_adm1_name, stringsAsFactors = FALSE)
shapefile_adm1_df <- ggplot2::fortify(shapefile, region = "ADM1_NAME")
plot(shapefile_adm1)

zone_list <- read.csv(zone_list_name)
shapefile_adm1@data$zone <- zone_list$Zone[match(as.character(shapefile_adm1@data$ADM1_NAME), as.character(zone_list$State))]
shapefile_zone <- maptools::unionSpatialPolygons(SpP = shapefile_adm1, IDs = shapefile_adm1@data$zone)
shapefile_zone_df <- ggplot2::fortify(shapefile_zone)
plot(shapefile_zone)

### Load the processed LGA data frame

temp_shapefile_data <- read.csv(shapefile_data_name, stringsAsFactors = FALSE)
shapefile@data <- temp_shapefile_data

### Load the processed prev survey data frame

prev_survey_clean_df_covar <- read.csv(prev_survey_clean_df_covar_file_name, stringsAsFactors = FALSE)


################################################################################

##### This section is for running the GLM version of the model

### Load the "winning" formulas

topRSE <- read.csv(topRSE_file_name, stringsAsFactors = FALSE)
topCor <- read.csv(topCor_file_name, stringsAsFactors = FALSE)

# (formula_base <- raw_prev_rate ~ 1 + ( age_1524_pop * zone ) + ( mv149_lessThanSecondaryEduc_2013 * zone ) + ( assets_fn12 * zone))
# (formula_base <- raw_prev_rate ~ 1 + zone + ( age_1524_pop + age_1524_pop:zone ) + ( mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone ) + ( assets_fn12 + assets_fn12:zone ))
(formula_base <- as.character(topCor$formula[1]))

### Run the GLM version

glm_cov_out <- glm(formula = formula_base, data = prev_survey_clean_df_covar,
                   family = binomial, weights = count)

### Display the GLM results

summary(glm_cov_out)
# confint.default(glm_cov_out)


################################################################################

##### This section for running the full model in INLA

### Refer to the INLA green book
### http://www.statistica.it/marta/stbook/Chapter6.R

temp_shapefile_data_inla <- shapefile@data
temp_shapefile_data_inla$ADM2_CODE <- as.factor(temp_shapefile_data_inla$ADM2_CODE)
temp_shapefile_data_inla$CLUSTER2 <- temp_shapefile_data_inla$CLUSTER
temp_shapefile_data_inla$zone2 <- temp_shapefile_data_inla$zone
temp_shapefile_data_inla$zone3 <- temp_shapefile_data_inla$zone
temp_shapefile_data_inla$zone4<- temp_shapefile_data_inla$zone

temp <- spdep::poly2nb(shapefile)
spdep::nb2INLA(file = "NGA.graph", nb = temp)

### Plot the shapefile and neighbor graph
### https://mbjoseph.github.io/posts/2018-12-27-plotting-spatial-neighbors-in-ggplot2/

# plot(shapefile, col = "white")
# plot(temp, coordinates(shapefile), add = TRUE, col = "black")
nb_sf <- as(nb2lines(temp, coords = coordinates(shapefile)), "sf")
nb_sf <- sf::st_set_crs(nb_sf, st_crs(shapefile))

p_nb <- ggplot() + 
    geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),
                 fill = "white", color = "gray70", size = 0.2) +
    geom_sf(data = nb_sf, color = "gray30", size = 0.5) +
    geom_point(data = shapefile@data[!is.na(shapefile@data$CLUSTER), ],
               aes(x = longnum, y = latnum), color = "red") +
    # theme_bw() +
    coord_sf() +
    ylab("Latitude") +
    xlab("Longitude")
print(p_nb)

### Plot the number of neighbors of each LGA

NGA.adj <- paste0(getwd(), "/NGA.graph")
H <- inla.read.graph(filename = "NGA.graph")
# image(inla.graph2matrix(H), xlab = "", ylab = "")

hist(unlist(lapply(temp, length)))
table(unlist(lapply(temp, length)))
p_nb_dist <- ggplot(data = data.frame(nb = unlist(lapply(temp, length))),
                    aes(x = nb)) + 
    geom_histogram(color = "black", fill = "gray80", binwidth = 1) +
    theme_bw() +
    xlab("Number of neighbors") +
    ylab("Count")
print(p_nb_dist)

### Write to file

pdf("output/inla_output/nb_plot.pdf", height = 5, width = 7)
plot(shapefile, col = "white")
plot(temp, coordinates(shapefile), add = TRUE, col = "black")
print(p_nb)
print(p_nb_dist)
dev.off()


################################################################################

##### Define function for running INLA

run_inla <- function(formula, data = temp_shapefile_data_inla,
                     family = "binomial", verbose = TRUE, ...) {

    # # http://www.r-inla.org/faq#TOC-Does-INLA-support-the-use-of-different-link-functions-
    mod.tb <- inla(formula = formula,
                   data = data,
                   family = family,
                   Ntrials = count,
                   control.family = list(link = "logit"),
                   control.predictor = list(compute = TRUE, link = 1),
                   control.compute = list(dic = TRUE),
                   verbose = verbose)

    summary(mod.tb)

    return(mod.tb)
}


################################################################################

##### Choose a model (e.g., best-fit) and make pretty plots

### Choose central measure
### Can change pred to mean or mode or X0.5quant

pred_measure <- "X0.5quant"

### If needed, run run_inla_utils.R

### Load all model results

inla_results <- read.csv(inla_results_file_name)
# plot(x = inla_results$rse, y = inla_results$rse_glm)
# plot(x = inla_results$cor_coef, y = inla_results$cor_coef_glm)

### Get different possible formulas for use in inla

source("scripts/run_inla_formulae.R")
(formulae_list_all <- unlist(lapply(ls(pattern = "formulae_list\\d"),
                                   function(x) {get(x)})))

### Choose formula of interest

# formula_OI_name <- "f_covar_zone_bym"
# formula_OI_short <- "(age_1524_pop * zone) + (mv149_lessThanSecondaryEduc_2013 * zone) + (assets_fn12 * zone) + BYM"

# formula_OI_name <- "f_covar_zone_bym_alt"
# formula_OI_short <- "(age_1524_pop * zone) + (mv149_lessThanSecondaryEduc_2013 * zone) + (assets_fn12 * zone) + BYM"

formula_OI_name <- "f_covar_zone_bym_alt2"
formula_OI_short <- "(age_1524_pop * zone) + (mv149_lessThanSecondaryEduc_2013 * zone) + (assets_fn12 * zone) + BYM"

# formula_OI_name <- "f_covar4_zone_bym"
# formula_OI_short <- "(v394_notVisitedHF_2013 * zone) + (assets_fn12 * zone) + (assets_fn8 * urban) + BYM"

# formula_OI_name <- "f_covar_zone"
# formula_OI_short <- "(age_1524_pop * zone) + (mv149_lessThanSecondaryEduc_2013 * zone) + (assets_fn12 * zone)"

# formula_OI_name <- "f_bym"
# formula_OI_short <- "BYM"

# formula_OI_name <- "f_zone"
# formula_OI_short <- "zone"

# formula_OI_name <- "f_urban"
# formula_OI_short <- "urban"

# formula_OI_name <- "f_high_hiv"
# formula_OI_short <- "high_hiv"

# formula_OI_name <- "f_4554_pop"
# formula_OI_short <- "age_4554_pop"

# formula_OI_name <- "f_male_pop"
# formula_OI_short <- "male_pop"

### Get the results for the formula of interest

formula_OI <- formulae_list_all[[formula_OI_name]]
formula_OI_loo_res <- read.csv(paste0("output/inla_output/temp/inla_raw_perform_results_", formula_OI_name, ".csv"))


################################################################################

##### Run the in-sample prediction including predictions at other locations

(start_time <- Sys.time())
mod.tb <- run_inla(formula = formula_OI)
(Sys.time() - start_time)

(names(mod.tb$summary.fitted.values) <- make.names(names(mod.tb$summary.fitted.values)))


################################################################################

##### Check predictions

### Plot predictions with actual values (other LGAs in red)

plot(mod.tb$summary.fitted.values[, pred_measure])
plot(x = temp_shapefile_data_inla$raw_prev_rate,
     y = mod.tb$summary.fitted.values[, pred_measure])
points(x = rep(0, length(which(is.na(temp_shapefile_data_inla$raw_prev_rate)))),
       y = mod.tb$summary.fitted.values[which(is.na(temp_shapefile_data_inla$raw_prev_rate)), pred_measure], col = "red")
abline(a = 0, b = 1, col = "blue")
(cor_res <- cor.test(x = temp_shapefile_data_inla$response/temp_shapefile_data_inla$count,
                     y = mod.tb$summary.fitted.values[, pred_measure]))

### Plot only the predicted values

plot(mod.tb$summary.fitted.values[which(is.na(temp_shapefile_data_inla$raw_prev_rate)), pred_measure])

### Plot predicted values by zone

pred_values_df <- data.frame(pred_values = mod.tb$summary.fitted.values[which(is.na(temp_shapefile_data_inla$raw_prev_rate)), pred_measure],
                             zone = temp_shapefile_data_inla$zone[is.na(temp_shapefile_data_inla$raw_prev_rate)])
boxplot(pred_values ~ zone, data = pred_values_df)

### Make better-looking plot of fitted vs actual

shapefile_data_with_pred <- data.frame(temp_shapefile_data_inla,
                                       pred_insamp = mod.tb$summary.fitted.values[, pred_measure])

outsamp_match_pos <- match(formula_OI_loo_res$CLUSTER, temp_shapefile_data_inla$CLUSTER)
shapefile_data_with_pred$pred_outsamp <- NA
shapefile_data_with_pred$pred_outsamp[outsamp_match_pos] <- formula_OI_loo_res[, pred_measure]

(cor_res_outsamp <- cor.test(x = formula_OI_loo_res$raw_prev_rate,
                             y = formula_OI_loo_res[, pred_measure]))

p_pred_insample <- ggplot(data = shapefile_data_with_pred[!is.na(shapefile_data_with_pred$CLUSTER), ]) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.7) +
    geom_hline(yintercept = mean(shapefile_data_with_pred[!is.na(shapefile_data_with_pred$CLUSTER), "raw_prev_rate"]),
               linetype = "dashed", alpha = 0.7) +
    geom_point(aes(x = raw_prev_rate, y = pred_insamp, shape = zone, color = setting), size = 2) +
    xlab("Actual prev") +
    ylab("Predicted prev") +
    ggtitle(paste0(str_wrap(formula_OI_short, width = 80), "\nCC (in-sample) = ", format(cor_res$estimate, digits = 3))) +
    theme_bw()
print(p_pred_insample)

p_pred_outsample <- ggplot(data = shapefile_data_with_pred[!is.na(shapefile_data_with_pred$CLUSTER), ]) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.7) +
    geom_hline(yintercept = mean(shapefile_data_with_pred[!is.na(shapefile_data_with_pred$CLUSTER), "raw_prev_rate"]),
               linetype = "dashed", alpha = 0.7) +
    geom_point(aes(x = raw_prev_rate, y = pred_outsamp, shape = zone, color = setting), size = 2) +
    xlab("Actual prev") +
    ylab("Predicted prev") +
    ggtitle(paste0(str_wrap(formula_OI_short, width = 80), "\nCC (LOO-CV) = ", format(cor_res_outsamp$estimate, digits = 3))) +
    theme_bw()
print(p_pred_outsample)

pdf(paste0("output/inla_output/pred_vs_actual_", formula_OI_name, ".pdf"), height = 7, width = 9)
print(p_pred_insample)
print(p_pred_outsample)
dev.off()


################################################################################

##### Generate summaries of effects

### Predictions for each cluster

# dim(mod.tb$summary.fitted.values)
head(mod.tb$summary.fitted.values)

### Fixed effects

round(mod.tb$summary.fixed, 3)

### Random effects

# names(mod.tb$summary.random)
head(mod.tb$summary.random[[1]])


################################################################################

# # make maps and plot predictions and std dev by zone

temp_shapefile_data_inla_ext <- data.frame(temp_shapefile_data_inla,
                                           pred_val = mod.tb$summary.fitted.values[, pred_measure],
                                           pred_sd  = mod.tb$summary.fitted.values$sd)

# plot(x = temp_shapefile_data_inla_ext$raw_prev_rate,
#      y = temp_shapefile_data_inla_ext$pred_val)
# abline(a = 0, b = 1, col = "blue")

shapefile_df_pos <- unlist(lapply(shapefile_df$id, function(x) {
    match(x, temp_shapefile_data_inla_ext$ADM2_CODE)
}))

shapefile_df_ext <- data.frame(shapefile_df,
                               pred_val = temp_shapefile_data_inla_ext[shapefile_df_pos, "pred_val"],
                               pred_sd = temp_shapefile_data_inla_ext[shapefile_df_pos, "pred_sd"])

(min_val <- round(min(temp_shapefile_data_inla_ext$pred_val), 3))
(mid_val <- round(median(temp_shapefile_data_inla_ext$pred_val), 3))
(max_val <- round(max(temp_shapefile_data_inla_ext$pred_val), 3))
(upper_quant_val <- quantile(temp_shapefile_data_inla_ext$pred_val, 0.975))
# hist(temp_shapefile_data_inla_ext$pred_val, breaks = 50)

shapefile_df_ext$pred_val_e5 <- 1e5 * shapefile_df_ext$pred_val

p_pred_map <- ggplot() +
    geom_polygon(data = shapefile_df_ext, aes(x = long, y = lat, group = group, fill = pred_val_e5),
                 color = NA, size = 0.2, alpha = 1) +
    geom_polygon(data = shapefile_adm1_df, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray70", size = 0.5, alpha = 1) +
    geom_polygon(data = shapefile_zone_df, aes(x = long, y = lat, group = group),
                 fill = NA, color = "black", size = 1, alpha = 1) +
    geom_point(data = temp_shapefile_data_inla_ext[!is.na(temp_shapefile_data_inla_ext$CLUSTER), ],
               aes(x = longnum, y = latnum), shape = 21, fill = NA, size = 2, alpha = 0.5) +
    theme_bw() +
    coord_map() +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(str_wrap(formula_OI_short, width = 80)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 1e5 * mid_val,
                         breaks = round(seq(from = 1e5 * min_val, to = 1e5 * 0.015, length.out = 8)),
                         limits = c(1e5 * min_val, 1e5 * 0.015),
                         oob = scales::squish,
                         guide = guide_legend(title = "Predicted\nbac pos prev\nper 100 000\nfor 2012"))
print(p_pred_map)

(min_val <- round(min(temp_shapefile_data_inla_ext$pred_sd) - 0.00005, 4))
(mid_val <- round(mean(temp_shapefile_data_inla_ext$pred_sd), 4))
(max_val <- round(quantile(temp_shapefile_data_inla_ext$pred_sd, 0.950), 4))

hist(shapefile_df_ext$pred_sd, breaks = 50)
shapefile_df_ext$pred_sd_e5 <- 1e5 * shapefile_df_ext$pred_sd

p_pred_sd_map <- ggplot() +
    geom_polygon(data = shapefile_df_ext, aes(x = long, y = lat, group = group, fill = pred_sd_e5),
                 color = NA, size = 0.2, alpha = 1) +
    geom_polygon(data = shapefile_adm1_df, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray70", size = 0.5, alpha = 1) +
    geom_polygon(data = shapefile_zone_df, aes(x = long, y = lat, group = group),
                 fill = NA, color = "black", size = 1, alpha = 1) +
    geom_point(data = temp_shapefile_data_inla_ext[!is.na(temp_shapefile_data_inla_ext$CLUSTER), ],
               aes(x = longnum, y = latnum), shape = 21, fill = NA, size = 2) +
    theme_bw() +
    coord_map() +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(str_wrap(formula_OI_short, width = 80)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 1e5 * mid_val,
                         breaks = round(seq(from = 1e5 * min_val, to = 1e5 * 0.01, length.out = 11)),
                         limits = c(1e5 * min_val, 1e5 * 0.01),
                         oob = scales::squish,
                         guide = guide_legend(title = "Predicted\nstd dev\nper 100 000\nfor 2012"))
print(p_pred_sd_map)

##### Other summary plots that are not maps

### Plot predicted prevalence by zone

# head(temp_shapefile_data_inla_ext[order(temp_shapefile_data_inla_ext$pred_val, decreasing = T),])

p_boxplot_zone <- ggplot(data = temp_shapefile_data_inla_ext, aes(x = zone, y = pred_val)) + 
    geom_boxplot(aes(fill = zone), width = 3 / length(unique(temp_shapefile_data_inla_ext$zone))) +
    ylim(0, 0.03) +
    xlab("Zone") +
    ylab("Predicted prev") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    # theme_bw() +
    ggtitle(str_wrap(formula_OI_short, width = 80)) +
    theme(legend.position = "none")
print(p_boxplot_zone)

### Plot predicted uncertainty by zone

p_sd_boxplot_zone <- ggplot(data = temp_shapefile_data_inla_ext, aes(x = zone, y = pred_sd)) + 
    geom_boxplot(aes(fill = zone)) +
    ylim(0, 0.03) +
    xlab("Zone") +
    ylab("Predicted prev std dev") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    # theme_bw() +
    ggtitle(str_wrap(formula_OI_short, width = 80)) +
    theme(legend.position = "none")
print(p_sd_boxplot_zone)

### Plot predicted uncertainty vs prevalence

p_pred_vs_sd <- ggplot(data = temp_shapefile_data_inla_ext[temp_shapefile_data_inla_ext$pred_val < 0.04, ]) + 
    geom_point(aes(x = pred_val, y = pred_sd, shape = zone, color = zone), alpha = 0.7) +
    geom_text_repel(data = temp_shapefile_data_inla_ext[(temp_shapefile_data_inla_ext$pred_val < 0.04) &
                                                        (temp_shapefile_data_inla_ext$pred_val > 0.01), ],
                aes(x = pred_val, y = pred_sd, label = ADM2_NAME),
                box.padding = 0.5, force = 0.5, direction = "both",
                color = "black", size = 4, segment.size = 0.05, segment.color = "black", alpha = 0.3) +
    theme_bw() +
    xlab("Predicted prev median") +
    ylab("Predicted prev std dev") +
    ggtitle(str_wrap(formula_OI_short, width = 80)) +
    scale_color_brewer(palette = "Dark2", direction = 1)
print(p_pred_vs_sd)


################################################################################

##### Write maps to files

pdf(paste0("output/inla_output/pred_map_2012_", formula_OI_name, ".pdf"), height = 6, width = 8)
print(p_pred_map)
print(p_pred_sd_map)
print(p_boxplot_zone)
print(p_sd_boxplot_zone)
print(p_pred_vs_sd)
dev.off()

##### Write results to CSV files

write.csv(temp_shapefile_data_inla_ext, "output/inla_output/temp_shapefile_data_inla_ext.csv", row.names = FALSE)
