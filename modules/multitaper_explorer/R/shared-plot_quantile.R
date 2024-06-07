# plot_quantile_plot <- function(
#     power_over_time_data, trial = NULL, soz_electrodes = NULL, resect_electrodes = NULL,
#     name_type = c("name", "number"), value_range = NULL,
#     scale = c("None", "Min_Max_Normalized_Time_Window"),
#     palette = plot_preferences$get('heatmap_palette'), save_path = NULL) {
#   # users can and only can select from given choices, i.e. one of c("name", "number")
#   name_type <- match.arg(name_type)
#   scale <- match.arg(scale)
#
#   if(length(palette) < 101) {
#     palette <- colorRampPalette(palette)(101)
#   }
#
#   # copy variables
#   time <- power_over_time_data$time
#   epoch_table <- power_over_time_data$epoch_table
#   electrode_table <- power_over_time_data$electrode_table
#   actual_range <- power_over_time_data$value_range
#   project_name <- power_over_time_data$project_name
#   subject_code <- power_over_time_data$subject_code
#
#   if(length(value_range) > 0 && !anyNA(value_range)) {
#     value_range <- range(value_range, na.rm = TRUE)
#     if( value_range[[2]] == value_range[[1]] ) {
#       if( scale == "Min_Max_Normalized_Time_Window") {
#         value_range <- c(0,1)
#       } else {
#         value_range <- actual_range
#       }
#     }
#   } else {
#     if( scale == "Min_Max_Normalized_Time_Window") {
#       value_range <- c(0,1)
#     } else {
#       value_range <- actual_range
#     }
#   }
#
#   # determine the y-axis labels
#   if( name_type == "name" ) {
#     y_labels <- electrode_table$Label
#   } else {
#     y_labels <- electrode_table$Electrode
#   }
#
#   # dipsaus::parse_svec is the builtin function to parse text to integer channels
#   soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
#   resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
#   is_soz <- electrode_table$Electrode %in% soz_electrodes
#   is_resect <- electrode_table$Electrode %in% resect_electrodes
#
#   if(length(trial)) {
#     if(is.numeric(trial)) {
#       trial_sel <- which(epoch_table$Trial %in% trial)
#     } else {
#       trial_sel <- which(epoch_table$Condition2 %in% trial)
#     }
#   } else {
#     trial_sel <- NULL
#   }
#
#   group_data_is_valid <- !sapply(power_over_time_data$group_data, is.null)
#
#   if(!any(group_data_is_valid)) { stop("No valid data; please check analysis frequency and time range.") }
#
#   layout_heat_maps(sum(group_data_is_valid), max_col = 2, layout_color_bar = TRUE)
#   par("mar" = c(4.5, 5, 3, 0.2), cex = 1.2)
#
#   sapply(power_over_time_data$group_data[group_data_is_valid], function(group_item) {
#     # No data is selected
#     if(is.null(group_item)) { return(FALSE) }
#
#     data <- group_item$data_over_time_trial_per_elec
#     if(length(trial_sel)) {
#       data <- data[, trial_sel ,, drop = FALSE]
#     }
#     ntrials <- dim(data)[[2]]
#     nchanns <- dim(data)[[3]]
#
#     # Time x Trial (collapse) x Electrode
#     data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)
#
#     if( scale == "Min_Max_Normalized_Time_Window") {
#       qval <- value_range[[2]]
#       if( qval <= 0 || qval > 1) {
#         qval <- 1
#       }
#
#       data_over_time_per_elec <- t(data_over_time_per_elec)
#
#       mincol <- apply(data_over_time_per_elec, 2, min)
#       maxcol <- apply(data_over_time_per_elec, 2, max)
#
#       data_over_time_per_elec_temp <- data_over_time_per_elec
#
#       for (col in 1:ncol(data_over_time_per_elec_temp)) {
#         for (row in 1:nrow(data_over_time_per_elec_temp)) {
#           # Avoid division by zero if max equals min
#           if(maxcol[col] != mincol[col]) {
#             data_over_time_per_elec_temp[row, col] <- (data_over_time_per_elec[row, col] - mincol[col]) / (maxcol[col] - mincol[col])
#           } else {
#             # Handle case where max equals min, potentially setting to 0, 0.5, or another appropriate value
#             data_over_time_per_elec_temp[row, col] <- 0
#           }
#         }
#       }
#
#       data_over_time_per_elec <- t(data_over_time_per_elec_temp)
#
#       value_range <- c(0, qval)
#     }
#
#     # Compute average power across time for each electrode
#     if (any(is_soz) & !any(is_resect)) {
#
#       soz_columns <- which(is_soz)
#       soz_data <- data_over_time_per_elec[, soz_columns]
#
#       non_soz_columns <- which(!is_soz)
#       non_soz_data <- data_over_time_per_elec[, non_soz_columns]
#
#       target_electrodes <- t(soz_data)
#       other_electrodes <- t(non_soz_data)
#
#       quantilematrixsozsozc=matrix(0,20,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colsoz=target_electrodes[,i]
#         colsozc=other_electrodes[,i]
#
#         f10colsoz<-quantile(colsoz,probs=c(0.1))
#         f20colsoz<-quantile(colsoz,probs=c(0.2))
#         f30colsoz<-quantile(colsoz,probs=c(0.3))
#         f40colsoz<-quantile(colsoz,probs=c(0.4))
#         f50colsoz<-quantile(colsoz,probs=c(0.5))
#         f60colsoz<-quantile(colsoz,probs=c(0.6))
#         f70colsoz<-quantile(colsoz,probs=c(0.7))
#         f80colsoz<-quantile(colsoz,probs=c(0.8))
#         f90colsoz<-quantile(colsoz,probs=c(0.9))
#         f100colsoz<-quantile(colsoz,probs=c(1.0))
#
#         f10colsozc<-quantile(colsozc,probs=c(0.1))
#         f20colsozc<-quantile(colsozc,probs=c(0.2))
#         f30colsozc<-quantile(colsozc,probs=c(0.3))
#         f40colsozc<-quantile(colsozc,probs=c(0.4))
#         f50colsozc<-quantile(colsozc,probs=c(0.5))
#         f60colsozc<-quantile(colsozc,probs=c(0.6))
#         f70colsozc<-quantile(colsozc,probs=c(0.7))
#         f80colsozc<-quantile(colsozc,probs=c(0.8))
#         f90colsozc<-quantile(colsozc,probs=c(0.9))
#         f100colsozc<-quantile(colsoz,probs=c(1.0))
#
#         quantilematrixsozsozc[1,i]=f10colsoz
#         quantilematrixsozsozc[2,i]=f20colsoz
#         quantilematrixsozsozc[3,i]=f30colsoz
#         quantilematrixsozsozc[4,i]=f40colsoz
#         quantilematrixsozsozc[5,i]=f50colsoz
#         quantilematrixsozsozc[6,i]=f60colsoz
#         quantilematrixsozsozc[7,i]=f70colsoz
#         quantilematrixsozsozc[8,i]=f80colsoz
#         quantilematrixsozsozc[9,i]=f90colsoz
#         quantilematrixsozsozc[10,i]=f100colsoz
#         quantilematrixsozsozc[11,i]=f10colsozc
#         quantilematrixsozsozc[12,i]=f20colsozc
#         quantilematrixsozsozc[13,i]=f30colsozc
#         quantilematrixsozsozc[14,i]=f40colsozc
#         quantilematrixsozsozc[15,i]=f50colsozc
#         quantilematrixsozsozc[16,i]=f60colsozc
#         quantilematrixsozsozc[17,i]=f70colsozc
#         quantilematrixsozsozc[18,i]=f80colsozc
#         quantilematrixsozsozc[19,i]=f90colsozc
#         quantilematrixsozsozc[20,i]=f100colsozc
#
#       }
#
#       quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
#                        'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
#                        'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
#                        'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixsozsozc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#       mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3, col = "#00bfff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)
#
#       if(!is.null(save_path)) {
#         save_data <- matrix(quantileplot$Value,
#                             ncol = length(unique(quantileplot$Time)),
#                             byrow = TRUE)
#         colnames(save_data) <- unique(quantileplot$Time)
#         rownames(save_data) <- all_stats_labels
#         filename <- paste0(subject_code, "_epoch_statistical_plot_", trial, ".csv")
#         filename <- paste0(save_path, filename)
#         write.csv(save_data, filename, row.names = TRUE)
#       }
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     }
#     else if (!any(is_soz) & any(is_resect)) {
#
#       resect_columns <- which(is_resect)
#       resect_data <- data_over_time_per_elec[, resect_columns]
#
#       non_resect_columns <- which(!is_resect)
#       non_resect_data <- data_over_time_per_elec[, non_resect_columns]
#
#       target_electrodes <- t(resect_data)
#       other_electrodes <- t(non_resect_data)
#
#       quantilematrixresectc=matrix(0,20,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colresect=target_electrodes[,i]
#         colresectc=other_electrodes[,i]
#
#         f10colresect<-quantile(colresect,probs=c(0.1))
#         f20colresect<-quantile(colresect,probs=c(0.2))
#         f30colresect<-quantile(colresect,probs=c(0.3))
#         f40colresect<-quantile(colresect,probs=c(0.4))
#         f50colresect<-quantile(colresect,probs=c(0.5))
#         f60colresect<-quantile(colresect,probs=c(0.6))
#         f70colresect<-quantile(colresect,probs=c(0.7))
#         f80colresect<-quantile(colresect,probs=c(0.8))
#         f90colresect<-quantile(colresect,probs=c(0.9))
#         f100colresect<-quantile(colresect,probs=c(1.0))
#
#         f10colresectc<-quantile(colresectc,probs=c(0.1))
#         f20colresectc<-quantile(colresectc,probs=c(0.2))
#         f30colresectc<-quantile(colresectc,probs=c(0.3))
#         f40colresectc<-quantile(colresectc,probs=c(0.4))
#         f50colresectc<-quantile(colresectc,probs=c(0.5))
#         f60colresectc<-quantile(colresectc,probs=c(0.6))
#         f70colresectc<-quantile(colresectc,probs=c(0.7))
#         f80colresectc<-quantile(colresectc,probs=c(0.8))
#         f90colresectc<-quantile(colresectc,probs=c(0.9))
#         f100colresectc<-quantile(colresect,probs=c(1.0))
#
#         quantilematrixresectc[1,i]=f10colresect
#         quantilematrixresectc[2,i]=f20colresect
#         quantilematrixresectc[3,i]=f30colresect
#         quantilematrixresectc[4,i]=f40colresect
#         quantilematrixresectc[5,i]=f50colresect
#         quantilematrixresectc[6,i]=f60colresect
#         quantilematrixresectc[7,i]=f70colresect
#         quantilematrixresectc[8,i]=f80colresect
#         quantilematrixresectc[9,i]=f90colresect
#         quantilematrixresectc[10,i]=f100colresect
#         quantilematrixresectc[11,i]=f10colresectc
#         quantilematrixresectc[12,i]=f20colresectc
#         quantilematrixresectc[13,i]=f30colresectc
#         quantilematrixresectc[14,i]=f40colresectc
#         quantilematrixresectc[15,i]=f50colresectc
#         quantilematrixresectc[16,i]=f60colresectc
#         quantilematrixresectc[17,i]=f70colresectc
#         quantilematrixresectc[18,i]=f80colresectc
#         quantilematrixresectc[19,i]=f90colresectc
#         quantilematrixresectc[20,i]=f100colresectc
#
#       }
#
#       quantilesname<-c('RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
#                        'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
#                        'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
#                        'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixresectc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#
#
#       mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#bf00ff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#       # Add axis labels
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     }
#     else if (any(is_soz) & any(is_resect)) {
#       resect_columns <- which(is_resect)
#       resect_data <- data_over_time_per_elec[, resect_columns]
#       resect_data <- t(resect_data)
#       soz_columns <- which(is_soz)
#       soz_data <- data_over_time_per_elec[, soz_columns]
#       soz_data <- t(soz_data)
#
#       non_resect_and_soz_columns <- which(!is_resect & !is_soz)
#       non_resect_and_soz_data <- data_over_time_per_elec[, non_resect_and_soz_columns]
#       non_resect_and_soz_data <- t(non_resect_and_soz_data)
#
#       quantilematrixresectsozc=matrix(0,30,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colresect=resect_data[,i]
#         colsoz=soz_data[,i]
#         colother=non_resect_and_soz_data[,i]
#
#         f10colsoz<-quantile(colsoz,probs=c(0.1))
#         f20colsoz<-quantile(colsoz,probs=c(0.2))
#         f30colsoz<-quantile(colsoz,probs=c(0.3))
#         f40colsoz<-quantile(colsoz,probs=c(0.4))
#         f50colsoz<-quantile(colsoz,probs=c(0.5))
#         f60colsoz<-quantile(colsoz,probs=c(0.6))
#         f70colsoz<-quantile(colsoz,probs=c(0.7))
#         f80colsoz<-quantile(colsoz,probs=c(0.8))
#         f90colsoz<-quantile(colsoz,probs=c(0.9))
#         f100colsoz<-quantile(colsoz,probs=c(1.0))
#
#         f10colresect<-quantile(colresect,probs=c(0.1))
#         f20colresect<-quantile(colresect,probs=c(0.2))
#         f30colresect<-quantile(colresect,probs=c(0.3))
#         f40colresect<-quantile(colresect,probs=c(0.4))
#         f50colresect<-quantile(colresect,probs=c(0.5))
#         f60colresect<-quantile(colresect,probs=c(0.6))
#         f70colresect<-quantile(colresect,probs=c(0.7))
#         f80colresect<-quantile(colresect,probs=c(0.8))
#         f90colresect<-quantile(colresect,probs=c(0.9))
#         f100colresect<-quantile(colresect,probs=c(1.0))
#
#         f10colresectsozc<-quantile(colother,probs=c(0.1))
#         f20colresectsozc<-quantile(colother,probs=c(0.2))
#         f30colresectsozc<-quantile(colother,probs=c(0.3))
#         f40colresectsozc<-quantile(colother,probs=c(0.4))
#         f50colresectsozc<-quantile(colother,probs=c(0.5))
#         f60colresectsozc<-quantile(colother,probs=c(0.6))
#         f70colresectsozc<-quantile(colother,probs=c(0.7))
#         f80colresectsozc<-quantile(colother,probs=c(0.8))
#         f90colresectsozc<-quantile(colother,probs=c(0.9))
#         f100colresectsozc<-quantile(colother,probs=c(1.0))
#
#         quantilematrixresectsozc[1,i]=f10colsoz
#         quantilematrixresectsozc[2,i]=f20colsoz
#         quantilematrixresectsozc[3,i]=f30colsoz
#         quantilematrixresectsozc[4,i]=f40colsoz
#         quantilematrixresectsozc[5,i]=f50colsoz
#         quantilematrixresectsozc[6,i]=f60colsoz
#         quantilematrixresectsozc[7,i]=f70colsoz
#         quantilematrixresectsozc[8,i]=f80colsoz
#         quantilematrixresectsozc[9,i]=f90colsoz
#         quantilematrixresectsozc[10,i]=f100colsoz
#         quantilematrixresectsozc[11,i]=f10colresect
#         quantilematrixresectsozc[12,i]=f20colresect
#         quantilematrixresectsozc[13,i]=f30colresect
#         quantilematrixresectsozc[14,i]=f40colresect
#         quantilematrixresectsozc[15,i]=f50colresect
#         quantilematrixresectsozc[16,i]=f60colresect
#         quantilematrixresectsozc[17,i]=f70colresect
#         quantilematrixresectsozc[18,i]=f80colresect
#         quantilematrixresectsozc[19,i]=f90colresect
#         quantilematrixresectsozc[20,i]=f100colresect
#         quantilematrixresectsozc[21,i]=f10colresectsozc
#         quantilematrixresectsozc[22,i]=f20colresectsozc
#         quantilematrixresectsozc[23,i]=f30colresectsozc
#         quantilematrixresectsozc[24,i]=f40colresectsozc
#         quantilematrixresectsozc[25,i]=f50colresectsozc
#         quantilematrixresectsozc[26,i]=f60colresectsozc
#         quantilematrixresectsozc[27,i]=f70colresectsozc
#         quantilematrixresectsozc[28,i]=f80colresectsozc
#         quantilematrixresectsozc[29,i]=f90colresectsozc
#         quantilematrixresectsozc[30,i]=f100colresectsozc
#
#       }
#
#       quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
#                        'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
#                        'RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
#                        'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
#                        'OTHERc(10th)','OTHERc(20th)','OTHERc(30th)','OTHERc(40th)','OTHERc(50th)',
#                        'OTHERc(60th)','OTHERc(70th)','OTHERc(80th)','OTHERc(90th)','OTHERc(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixresectsozc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#
#       # Add axis labels
#       mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#00bfff", cex = 1.2)
#       mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "#bf00ff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 22.5, col = "black", cex = 1.2)
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#       graphics::abline(h = 20 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     }
#     else {
#
#       non_resect_and_soz_data <- data_over_time_per_elec
#       non_resect_and_soz_data <- t(non_resect_and_soz_data)
#
#       quantilematrixresectsozc=matrix(0,10,nrow(data_over_time_per_elec))
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colother=non_resect_and_soz_data[,i]
#
#         f10colresectsozc<-quantile(colother,probs=c(0.1))
#         f20colresectsozc<-quantile(colother,probs=c(0.2))
#         f30colresectsozc<-quantile(colother,probs=c(0.3))
#         f40colresectsozc<-quantile(colother,probs=c(0.4))
#         f50colresectsozc<-quantile(colother,probs=c(0.5))
#         f60colresectsozc<-quantile(colother,probs=c(0.6))
#         f70colresectsozc<-quantile(colother,probs=c(0.7))
#         f80colresectsozc<-quantile(colother,probs=c(0.8))
#         f90colresectsozc<-quantile(colother,probs=c(0.9))
#         f100colresectsozc<-quantile(colother,probs=c(1.0))
#
#         quantilematrixresectsozc[1,i]=f10colresectsozc
#         quantilematrixresectsozc[2,i]=f20colresectsozc
#         quantilematrixresectsozc[3,i]=f30colresectsozc
#         quantilematrixresectsozc[4,i]=f40colresectsozc
#         quantilematrixresectsozc[5,i]=f50colresectsozc
#         quantilematrixresectsozc[6,i]=f60colresectsozc
#         quantilematrixresectsozc[7,i]=f70colresectsozc
#         quantilematrixresectsozc[8,i]=f80colresectsozc
#         quantilematrixresectsozc[9,i]=f90colresectsozc
#         quantilematrixresectsozc[10,i]=f100colresectsozc
#
#       }
#
#       quantilesname<-c(
#                        'POWER(10th)','POWER(20th)','POWER(30th)','POWER(40th)','POWER(50th)',
#                        'POWER(60th)','POWER(70th)','POWER(80th)','POWER(90th)','POWER(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixresectsozc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#
#       # Add axis labels
#       mtext("ALL ELECTRODES", side = 4, col = "black", cex = 1.2)
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     }
#     if (any(is_soz) | any(is_resect)) {
#       # Add subtitle
#       freq_range <- range(group_item$frequency)
#       graphics::title(sprintf("# Channel=%s, # Epoch=%s, Freq=%.0f~%.0f Hz, Unit=%s", nchanns, trial, freq_range[[1]], freq_range[[2]], scale), adj = 0, line = 1.5)
#       # sub-title
#       graphics::title(sprintf("%s/%s - Analysis Group %d", project_name, subject_code, group_item$group_id), adj = 0, line = 0.5, cex.main = 0.8)
#
#
#     }
#
#     return(TRUE)
#   }, USE.NAMES = FALSE)
#   if (any(is_soz) | any(is_resect)) {
#     par("mar" = c(3.1, 1, 3, 3.1))
#     pal_val <- seq(value_range[[1]], value_range[[2]], length.out = 101)
#     graphics::image(matrix(pal_val, nrow = 1), x = 0, y = pal_val, axes = FALSE, xlab = "", ylab = "", col = palette, xlim = c(0, 0.1))
#     graphics::axis(side = 4, at = c(value_range[[2]], 0), labels = c(sprintf("%.1f", value_range[[2]]), "0"), las = 1)
#
#     if(scale == "Min_Max_Normalized_Time_Window") {
#       graphics::title("Max\nNormalized", line = 0.6, adj = 0, cex.main = 0.8)
#     } else {
#       actual_range_text <- paste(sprintf("%.1f", actual_range), collapse = " ~ ")
#       graphics::title(sprintf("[%s]", actual_range_text), line = 0.6, adj = 0, cex.main = 0.8)
#     }
#   }
# }
#
#

plot_quantile_plot <- function(
    power_over_time_data, trial = NULL, soz_electrodes = NULL, resect_electrodes = NULL,
    name_type = c("name", "number"), value_range = NULL,
    scale = c("None", "Min_Max_Normalized_Time_Window"),
    palette = plot_preferences$get('heatmap_palette'), save_path = NULL) {
  # users can and only can select from given choices, i.e. one of c("name", "number")
  name_type <- match.arg(name_type)
  scale <- match.arg(scale)

  if(length(palette) < 101) {
    palette <- colorRampPalette(palette)(101)
  }

  # copy variables
  time <- power_over_time_data$time
  epoch_table <- power_over_time_data$epoch_table
  electrode_table <- power_over_time_data$electrode_table
  actual_range <- power_over_time_data$value_range
  project_name <- power_over_time_data$project_name
  subject_code <- power_over_time_data$subject_code

  if(length(value_range) > 0 && !anyNA(value_range)) {
    value_range <- range(value_range, na.rm = TRUE)
    if( value_range[[2]] == value_range[[1]] ) {
      if( scale == "Min_Max_Normalized_Time_Window") {
        value_range <- c(0,1)
      } else {
        value_range <- actual_range
      }
    }
  } else {
    if( scale == "Min_Max_Normalized_Time_Window") {
      value_range <- c(0,1)
    } else {
      value_range <- actual_range
    }
  }

  # determine the y-axis labels
  if( name_type == "name" ) {
    y_labels <- electrode_table$Label
  } else {
    y_labels <- electrode_table$Electrode
  }

  # dipsaus::parse_svec is the builtin function to parse text to integer channels
  soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
  resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
  is_soz <- electrode_table$Electrode %in% soz_electrodes
  is_resect <- electrode_table$Electrode %in% resect_electrodes

  # plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
  #   geom_tile() +
  #   labs(x = "Time (s)", y = "Electrode") +
  #   scale_fill_viridis(option = "turbo") +
  #   theme_minimal() +
  #   theme(
  #     axis.text.y = element_text(size = 5, color = sapply(levels(heatmap_data$Electrode), color_electrodes))
  #   )

  if(length(trial)) {
    if(is.numeric(trial)) {
      trial_sel <- which(epoch_table$Trial %in% trial)
    } else {
      trial_sel <- which(epoch_table$Condition2 %in% trial)
    }
  } else {
    trial_sel <- NULL
  }

  group_data_is_valid <- !sapply(power_over_time_data$group_data, is.null)

  if(!any(group_data_is_valid)) { stop("No valid data; please check analysis frequency and time range.") }

  layout_heat_maps(sum(group_data_is_valid), max_col = 2, layout_color_bar = TRUE)
  par("mar" = c(4.5, 5, 3, 0.2), cex = 1.2)

  # loop 1: calculate value ranges & data for figures
  plot_data <- lapply(power_over_time_data$group_data[group_data_is_valid], function(group_item) {
    # No data is selected
    if(is.null(group_item)) { return(NULL) }

    data <- group_item$data_over_time_trial_per_elec
    if(length(trial_sel)) {
      data <- data[, trial_sel ,, drop = FALSE]
    }
    ntrials <- dim(data)[[2]]
    nchanns <- dim(data)[[3]]

    # Time x Trial (collapse) x Electrode
    data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)

    elec_order_temp <- electrode_table$Electrode

    # calculate the value range
    time_range <- group_item$time_range_for_analysis
    time_range <- time_range[!is.na(time_range)]
    if(length(time_range) < 2) {
      time_range <- range(time)
    } else {
      time_range <- range(time_range)
    }

    # which time points are within the analysis time range
    time_selection <- time >= time_range[[1]] & time <= time_range[[2]]

    # overall data range
    data_range_all <- range(data_over_time_per_elec, na.rm = TRUE)

    # data range within the analysis window
    data_range_analysis <- range(data_over_time_per_elec[time_selection, ], na.rm = TRUE)

    list(
      # Time x Trial (collapse) x Electrode
      data_over_time_per_elec = data_over_time_per_elec,
      # bind two data ranges in one vector
      data_ranges_max = c(max(abs(data_range_all)), max(abs(data_range_analysis))),
      data_ranges_min = c(min(data_range_all), min(data_range_analysis)),

      # Tiem related
      time = time,
      time_range_for_analysis = group_item$time_range_for_analysis,

      # label-related
      labels = y_labels,
      is_soz = is_soz,
      is_resect = is_resect,

      # sub-title
      freq_range = range(group_item$frequency),
      nchanns = nchanns,
      ntrials = ntrials,
      group_id = group_item$group_id,
      epoch = trial_sel
    )
  })

  # get ranges
  plot_data <- dipsaus::drop_nulls(plot_data)
  data_ranges_max <- sapply(plot_data, "[[", "data_ranges_max")
  data_ranges_min <- sapply(plot_data, "[[", "data_ranges_min")
  data_max <- max(data_ranges_max[2, ])
  data_min <- min(data_ranges_min[2, ])
  baselined <- power_over_time_data$baselined
  freq_range <- NULL

  lapply(plot_data, function(group_item) {

    data_over_time_per_elec <- group_item$data_over_time_per_elec
    time <- group_item$time
    y_labels <- group_item$labels
    is_soz <- group_item$is_soz
    is_resect <- group_item$is_resect
    freq_range <- group_item$freq_range
    ntrials <- group_item$ntrials
    nchanns <- group_item$nchanns
    group_id <- group_item$group_id
    epoch <- group_item$epoch

    if( scale == "Min_Max_Normalized_Time_Window") {
      # use group_item$data_ranges_max[2] as the value cap
      max_value <- group_item$data_ranges_max[2]
      data_over_time_per_elec <- t(data_over_time_per_elec)

      maxcol <- apply(data_over_time_per_elec, 2, max)
      data_over_time_per_elec_temp <- data_over_time_per_elec

      mincol <- apply(data_over_time_per_elec, 2, min)
      maxcol <- apply(data_over_time_per_elec, 2, max)

      data_over_time_per_elec_temp <- data_over_time_per_elec

      for (col in 1:ncol(data_over_time_per_elec_temp)) {
        for (row in 1:nrow(data_over_time_per_elec_temp)) {
          # Avoid division by zero if max equals min
          if(maxcol[col] != mincol[col]) {
            data_over_time_per_elec_temp[row, col] <- (data_over_time_per_elec[row, col] - mincol[col]) / (maxcol[col] - mincol[col])
          } else {
            # Handle case where max equals min, potentially setting to 0, 0.5, or another appropriate value
            data_over_time_per_elec_temp[row, col] <- 0
          }
        }
      }

      data_over_time_per_elec <- t(data_over_time_per_elec_temp)

      if( baselined ) {
        data_over_time_per_elec[data_over_time_per_elec < -1] <- -1
        value_range <- c(-1, 1)
      } else {
        data_over_time_per_elec[data_over_time_per_elec < 0] <- 0
        value_range <- c(0, 1)
      }

    } else {
      data_over_time_per_elec[data_over_time_per_elec > data_max] <- data_max
      if( baselined ) {
        data_over_time_per_elec[data_over_time_per_elec < data_min] <- data_min
        value_range <- c(data_min, data_max)
      } else {
        data_over_time_per_elec[data_over_time_per_elec < data_min] <- data_min
        value_range <- c(data_min, data_max)
      }
    }

    # Compute average power across time for each electrode
    if (any(is_soz) & !any(is_resect)) {

      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec[, soz_columns]

      non_soz_columns <- which(!is_soz)
      non_soz_data <- data_over_time_per_elec[, non_soz_columns]

      target_electrodes <- t(soz_data)
      other_electrodes <- t(non_soz_data)

      quantilematrixsozsozc=matrix(0,20,nrow(data_over_time_per_elec))


      for(i in 1:nrow(data_over_time_per_elec)){

        colsoz=target_electrodes[,i]
        colsozc=other_electrodes[,i]

        f10colsoz<-quantile(colsoz,probs=c(0.1))
        f20colsoz<-quantile(colsoz,probs=c(0.2))
        f30colsoz<-quantile(colsoz,probs=c(0.3))
        f40colsoz<-quantile(colsoz,probs=c(0.4))
        f50colsoz<-quantile(colsoz,probs=c(0.5))
        f60colsoz<-quantile(colsoz,probs=c(0.6))
        f70colsoz<-quantile(colsoz,probs=c(0.7))
        f80colsoz<-quantile(colsoz,probs=c(0.8))
        f90colsoz<-quantile(colsoz,probs=c(0.9))
        f100colsoz<-quantile(colsoz,probs=c(1.0))

        f10colsozc<-quantile(colsozc,probs=c(0.1))
        f20colsozc<-quantile(colsozc,probs=c(0.2))
        f30colsozc<-quantile(colsozc,probs=c(0.3))
        f40colsozc<-quantile(colsozc,probs=c(0.4))
        f50colsozc<-quantile(colsozc,probs=c(0.5))
        f60colsozc<-quantile(colsozc,probs=c(0.6))
        f70colsozc<-quantile(colsozc,probs=c(0.7))
        f80colsozc<-quantile(colsozc,probs=c(0.8))
        f90colsozc<-quantile(colsozc,probs=c(0.9))
        f100colsozc<-quantile(colsoz,probs=c(1.0))

        quantilematrixsozsozc[1,i]=f10colsoz
        quantilematrixsozsozc[2,i]=f20colsoz
        quantilematrixsozsozc[3,i]=f30colsoz
        quantilematrixsozsozc[4,i]=f40colsoz
        quantilematrixsozsozc[5,i]=f50colsoz
        quantilematrixsozsozc[6,i]=f60colsoz
        quantilematrixsozsozc[7,i]=f70colsoz
        quantilematrixsozsozc[8,i]=f80colsoz
        quantilematrixsozsozc[9,i]=f90colsoz
        quantilematrixsozsozc[10,i]=f100colsoz
        quantilematrixsozsozc[11,i]=f10colsozc
        quantilematrixsozsozc[12,i]=f20colsozc
        quantilematrixsozsozc[13,i]=f30colsozc
        quantilematrixsozsozc[14,i]=f40colsozc
        quantilematrixsozsozc[15,i]=f50colsozc
        quantilematrixsozsozc[16,i]=f60colsozc
        quantilematrixsozsozc[17,i]=f70colsozc
        quantilematrixsozsozc[18,i]=f80colsozc
        quantilematrixsozsozc[19,i]=f90colsozc
        quantilematrixsozsozc[20,i]=f100colsozc

      }

      quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
                       'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
                       'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
                       'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')

      time_vector <- seq(from = group_item$time_range_for_analysis[1],
                         to = group_item$time_range_for_analysis[2],
                         length.out = nrow(data_over_time_per_elec))


      quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
      quantileplot$Value <- c(t(quantilematrixsozsozc))

      all_stats_labels <- levels(quantileplot$Stats)

      # Plot the heatmap using image()
      graphics::image(1:length(unique(quantileplot$Time)),
                      1:length(all_stats_labels),
                      t(matrix(quantileplot$Value,
                               ncol = length(unique(quantileplot$Time)),
                               byrow = TRUE)),
                      col = palette,
                      xlab = "Time (s)",
                      ylab = "",
                      axes = FALSE)
      # mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3, col = "#00bfff", cex = 1.2)
      # mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)


      graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")

      min_time <- min(unique(quantileplot$Time))
      max_time <- max(unique(quantileplot$Time))

      # graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
      #                labels = unique(quantileplot$Time), cex.axis = 1)

      unique_times <- sort(unique(quantileplot$Time))
      quantile_values <- quantile(unique_times, probs = seq(0, 1, length.out = 5))
      tick_positions <- sapply(quantile_values, function(x) {
        which.min(abs(unique_times - x))
      })
      tick_labels <- unique_times[tick_positions]
      tick_labels <- round(unique_times[tick_positions], 2)
      graphics::axis(1, at = tick_positions, labels = tick_labels, cex.axis = 1)

      graphics::axis(2, at = 1:length(all_stats_labels),
                     labels = all_stats_labels,
                     las = 2, cex.axis = 0.6)

    }
    else if (!any(is_soz) & any(is_resect)) {

      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec[, resect_columns]

      non_resect_columns <- which(!is_resect)
      non_resect_data <- data_over_time_per_elec[, non_resect_columns]

      target_electrodes <- t(resect_data)
      other_electrodes <- t(non_resect_data)

      quantilematrixresectc=matrix(0,20,nrow(data_over_time_per_elec))


      for(i in 1:nrow(data_over_time_per_elec)){

        colresect=target_electrodes[,i]
        colresectc=other_electrodes[,i]

        f10colresect<-quantile(colresect,probs=c(0.1))
        f20colresect<-quantile(colresect,probs=c(0.2))
        f30colresect<-quantile(colresect,probs=c(0.3))
        f40colresect<-quantile(colresect,probs=c(0.4))
        f50colresect<-quantile(colresect,probs=c(0.5))
        f60colresect<-quantile(colresect,probs=c(0.6))
        f70colresect<-quantile(colresect,probs=c(0.7))
        f80colresect<-quantile(colresect,probs=c(0.8))
        f90colresect<-quantile(colresect,probs=c(0.9))
        f100colresect<-quantile(colresect,probs=c(1.0))

        f10colresectc<-quantile(colresectc,probs=c(0.1))
        f20colresectc<-quantile(colresectc,probs=c(0.2))
        f30colresectc<-quantile(colresectc,probs=c(0.3))
        f40colresectc<-quantile(colresectc,probs=c(0.4))
        f50colresectc<-quantile(colresectc,probs=c(0.5))
        f60colresectc<-quantile(colresectc,probs=c(0.6))
        f70colresectc<-quantile(colresectc,probs=c(0.7))
        f80colresectc<-quantile(colresectc,probs=c(0.8))
        f90colresectc<-quantile(colresectc,probs=c(0.9))
        f100colresectc<-quantile(colresect,probs=c(1.0))

        quantilematrixresectc[1,i]=f10colresect
        quantilematrixresectc[2,i]=f20colresect
        quantilematrixresectc[3,i]=f30colresect
        quantilematrixresectc[4,i]=f40colresect
        quantilematrixresectc[5,i]=f50colresect
        quantilematrixresectc[6,i]=f60colresect
        quantilematrixresectc[7,i]=f70colresect
        quantilematrixresectc[8,i]=f80colresect
        quantilematrixresectc[9,i]=f90colresect
        quantilematrixresectc[10,i]=f100colresect
        quantilematrixresectc[11,i]=f10colresectc
        quantilematrixresectc[12,i]=f20colresectc
        quantilematrixresectc[13,i]=f30colresectc
        quantilematrixresectc[14,i]=f40colresectc
        quantilematrixresectc[15,i]=f50colresectc
        quantilematrixresectc[16,i]=f60colresectc
        quantilematrixresectc[17,i]=f70colresectc
        quantilematrixresectc[18,i]=f80colresectc
        quantilematrixresectc[19,i]=f90colresectc
        quantilematrixresectc[20,i]=f100colresectc

      }

      quantilesname<-c('RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
                       'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
                       'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
                       'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')

      time_vector <- seq(from = group_item$time_range_for_analysis[1],
                         to = group_item$time_range_for_analysis[2],
                         length.out = nrow(data_over_time_per_elec))

      quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
      quantileplot$Value <- c(t(quantilematrixresectc))

      all_stats_labels <- levels(quantileplot$Stats)

      # Plot the heatmap using image()
      graphics::image(1:length(unique(quantileplot$Time)),
                      1:length(all_stats_labels),
                      t(matrix(quantileplot$Value,
                               ncol = length(unique(quantileplot$Time)),
                               byrow = TRUE)),
                      col = palette,
                      xlab = "Time (s)",
                      ylab = "",
                      axes = FALSE)


      # mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#bf00ff", cex = 1.2)
      # mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)

      graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")

      # Add axis labels

      min_time <- min(unique(quantileplot$Time))
      max_time <- max(unique(quantileplot$Time))

      # graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
      #                labels = unique(quantileplot$Time), cex.axis = 1)

      unique_times <- sort(unique(quantileplot$Time))
      quantile_values <- quantile(unique_times, probs = seq(0, 1, length.out = 5))
      tick_positions <- sapply(quantile_values, function(x) {
        which.min(abs(unique_times - x))
      })
      tick_labels <- unique_times[tick_positions]
      tick_labels <- round(unique_times[tick_positions], 2)
      graphics::axis(1, at = tick_positions, labels = tick_labels, cex.axis = 1)

      graphics::axis(2, at = 1:length(all_stats_labels),
                     labels = all_stats_labels,
                     las = 2, cex.axis = 0.6)

    }
    else if (any(is_soz) & any(is_resect)) {
      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec[, resect_columns]
      resect_data <- t(resect_data)
      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec[, soz_columns]
      soz_data <- t(soz_data)

      non_resect_and_soz_columns <- which(!is_resect & !is_soz)
      non_resect_and_soz_data <- data_over_time_per_elec[, non_resect_and_soz_columns]
      non_resect_and_soz_data <- t(non_resect_and_soz_data)

      quantilematrixresectsozc=matrix(0,30,nrow(data_over_time_per_elec))


      for(i in 1:nrow(data_over_time_per_elec)){

        colresect=resect_data[,i]
        colsoz=soz_data[,i]
        colother=non_resect_and_soz_data[,i]

        f10colsoz<-quantile(colsoz,probs=c(0.1))
        f20colsoz<-quantile(colsoz,probs=c(0.2))
        f30colsoz<-quantile(colsoz,probs=c(0.3))
        f40colsoz<-quantile(colsoz,probs=c(0.4))
        f50colsoz<-quantile(colsoz,probs=c(0.5))
        f60colsoz<-quantile(colsoz,probs=c(0.6))
        f70colsoz<-quantile(colsoz,probs=c(0.7))
        f80colsoz<-quantile(colsoz,probs=c(0.8))
        f90colsoz<-quantile(colsoz,probs=c(0.9))
        f100colsoz<-quantile(colsoz,probs=c(1.0))

        f10colresect<-quantile(colresect,probs=c(0.1))
        f20colresect<-quantile(colresect,probs=c(0.2))
        f30colresect<-quantile(colresect,probs=c(0.3))
        f40colresect<-quantile(colresect,probs=c(0.4))
        f50colresect<-quantile(colresect,probs=c(0.5))
        f60colresect<-quantile(colresect,probs=c(0.6))
        f70colresect<-quantile(colresect,probs=c(0.7))
        f80colresect<-quantile(colresect,probs=c(0.8))
        f90colresect<-quantile(colresect,probs=c(0.9))
        f100colresect<-quantile(colresect,probs=c(1.0))

        f10colresectsozc<-quantile(colother,probs=c(0.1))
        f20colresectsozc<-quantile(colother,probs=c(0.2))
        f30colresectsozc<-quantile(colother,probs=c(0.3))
        f40colresectsozc<-quantile(colother,probs=c(0.4))
        f50colresectsozc<-quantile(colother,probs=c(0.5))
        f60colresectsozc<-quantile(colother,probs=c(0.6))
        f70colresectsozc<-quantile(colother,probs=c(0.7))
        f80colresectsozc<-quantile(colother,probs=c(0.8))
        f90colresectsozc<-quantile(colother,probs=c(0.9))
        f100colresectsozc<-quantile(colother,probs=c(1.0))

        quantilematrixresectsozc[1,i]=f10colsoz
        quantilematrixresectsozc[2,i]=f20colsoz
        quantilematrixresectsozc[3,i]=f30colsoz
        quantilematrixresectsozc[4,i]=f40colsoz
        quantilematrixresectsozc[5,i]=f50colsoz
        quantilematrixresectsozc[6,i]=f60colsoz
        quantilematrixresectsozc[7,i]=f70colsoz
        quantilematrixresectsozc[8,i]=f80colsoz
        quantilematrixresectsozc[9,i]=f90colsoz
        quantilematrixresectsozc[10,i]=f100colsoz
        quantilematrixresectsozc[11,i]=f10colresect
        quantilematrixresectsozc[12,i]=f20colresect
        quantilematrixresectsozc[13,i]=f30colresect
        quantilematrixresectsozc[14,i]=f40colresect
        quantilematrixresectsozc[15,i]=f50colresect
        quantilematrixresectsozc[16,i]=f60colresect
        quantilematrixresectsozc[17,i]=f70colresect
        quantilematrixresectsozc[18,i]=f80colresect
        quantilematrixresectsozc[19,i]=f90colresect
        quantilematrixresectsozc[20,i]=f100colresect
        quantilematrixresectsozc[21,i]=f10colresectsozc
        quantilematrixresectsozc[22,i]=f20colresectsozc
        quantilematrixresectsozc[23,i]=f30colresectsozc
        quantilematrixresectsozc[24,i]=f40colresectsozc
        quantilematrixresectsozc[25,i]=f50colresectsozc
        quantilematrixresectsozc[26,i]=f60colresectsozc
        quantilematrixresectsozc[27,i]=f70colresectsozc
        quantilematrixresectsozc[28,i]=f80colresectsozc
        quantilematrixresectsozc[29,i]=f90colresectsozc
        quantilematrixresectsozc[30,i]=f100colresectsozc

      }

      quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
                       'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
                       'RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
                       'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
                       'OTHERc(10th)','OTHERc(20th)','OTHERc(30th)','OTHERc(40th)','OTHERc(50th)',
                       'OTHERc(60th)','OTHERc(70th)','OTHERc(80th)','OTHERc(90th)','OTHERc(100th)')

      time_vector <- seq(from = group_item$time_range_for_analysis[1],
                         to = group_item$time_range_for_analysis[2],
                         length.out = nrow(data_over_time_per_elec))



      quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
      quantileplot$Value <- c(t(quantilematrixresectsozc))

      all_stats_labels <- levels(quantileplot$Stats)



      # Plot the heatmap using image()
      graphics::image(1:length(unique(quantileplot$Time)),
                      1:length(all_stats_labels),
                      t(matrix(quantileplot$Value,
                               ncol = length(unique(quantileplot$Time)),
                               byrow = TRUE)),
                      col = palette,
                      xlab = "Time (s)",
                      ylab = "",
                      axes = FALSE)

      # Add axis labels
      # mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#00bfff", cex = 1.2)
      # mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "#bf00ff", cex = 1.2)
      # mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 22.5, col = "black", cex = 1.2)

      graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
      graphics::abline(h = 20 + 0.5, col = "red", lwd = 10, lty = "dashed")


      min_time <- min(unique(quantileplot$Time))
      max_time <- max(unique(quantileplot$Time))

      # graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
      #                labels = unique(quantileplot$Time), cex.axis = 1)

      unique_times <- sort(unique(quantileplot$Time))
      quantile_values <- quantile(unique_times, probs = seq(0, 1, length.out = 5))
      tick_positions <- sapply(quantile_values, function(x) {
        which.min(abs(unique_times - x))
      })
      tick_labels <- unique_times[tick_positions]
      tick_labels <- round(unique_times[tick_positions], 2)
      graphics::axis(1, at = tick_positions, labels = tick_labels, cex.axis = 1)

      graphics::axis(2, at = 1:length(all_stats_labels),
                     labels = all_stats_labels,
                     las = 2, cex.axis = 0.6)

    }
    else {

      non_resect_and_soz_data <- data_over_time_per_elec
      non_resect_and_soz_data <- t(non_resect_and_soz_data)

      quantilematrixresectsozc=matrix(0,10,nrow(data_over_time_per_elec))

      for(i in 1:nrow(data_over_time_per_elec)){

        colother=non_resect_and_soz_data[,i]

        f10colresectsozc<-quantile(colother,probs=c(0.1))
        f20colresectsozc<-quantile(colother,probs=c(0.2))
        f30colresectsozc<-quantile(colother,probs=c(0.3))
        f40colresectsozc<-quantile(colother,probs=c(0.4))
        f50colresectsozc<-quantile(colother,probs=c(0.5))
        f60colresectsozc<-quantile(colother,probs=c(0.6))
        f70colresectsozc<-quantile(colother,probs=c(0.7))
        f80colresectsozc<-quantile(colother,probs=c(0.8))
        f90colresectsozc<-quantile(colother,probs=c(0.9))
        f100colresectsozc<-quantile(colother,probs=c(1.0))

        quantilematrixresectsozc[1,i]=f10colresectsozc
        quantilematrixresectsozc[2,i]=f20colresectsozc
        quantilematrixresectsozc[3,i]=f30colresectsozc
        quantilematrixresectsozc[4,i]=f40colresectsozc
        quantilematrixresectsozc[5,i]=f50colresectsozc
        quantilematrixresectsozc[6,i]=f60colresectsozc
        quantilematrixresectsozc[7,i]=f70colresectsozc
        quantilematrixresectsozc[8,i]=f80colresectsozc
        quantilematrixresectsozc[9,i]=f90colresectsozc
        quantilematrixresectsozc[10,i]=f100colresectsozc

      }

      quantilesname<-c(
        'POWER(10th)','POWER(20th)','POWER(30th)','POWER(40th)','POWER(50th)',
        'POWER(60th)','POWER(70th)','POWER(80th)','POWER(90th)','POWER(100th)')

      time_vector <- seq(from = group_item$time_range_for_analysis[1],
                         to = group_item$time_range_for_analysis[2],
                         length.out = nrow(data_over_time_per_elec))



      quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
      quantileplot$Value <- c(t(quantilematrixresectsozc))

      all_stats_labels <- levels(quantileplot$Stats)



      # Plot the heatmap using image()
      graphics::image(1:length(unique(quantileplot$Time)),
                      1:length(all_stats_labels),
                      t(matrix(quantileplot$Value,
                               ncol = length(unique(quantileplot$Time)),
                               byrow = TRUE)),
                      col = palette,
                      xlab = "Time (s)",
                      ylab = "",
                      axes = FALSE)

      # Add axis labels
      # mtext("ALL ELECTRODES", side = 4, col = "black", cex = 1.2)

      min_time <- min(unique(quantileplot$Time))
      max_time <- max(unique(quantileplot$Time))

      # graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
      #                labels = unique(quantileplot$Time), cex.axis = 1)

      unique_times <- sort(unique(quantileplot$Time))
      quantile_values <- quantile(unique_times, probs = seq(0, 1, length.out = 5))
      tick_positions <- sapply(quantile_values, function(x) {
        which.min(abs(unique_times - x))
      })
      tick_labels <- unique_times[tick_positions]
      tick_labels <- round(unique_times[tick_positions], 2)
      graphics::axis(1, at = tick_positions, labels = tick_labels, cex.axis = 1)


      graphics::axis(2, at = 1:length(all_stats_labels),
                     labels = all_stats_labels,
                     las = 2, cex.axis = 0.6)

    }

    if(!is.null(save_path)) {
      save_data <- matrix(quantileplot$Value,
                          ncol = length(unique(quantileplot$Time)),
                          byrow = TRUE)
      colnames(save_data) <- unique(quantileplot$Time)
      rownames(save_data) <- all_stats_labels
      filename <- paste0(subject_code, "_epoch_statistical_plot_", trial, ".csv")
      filename <- paste0(save_path, filename)
      write.csv(save_data, filename, row.names = TRUE)
    }

    graphics::title(sprintf("# Channel=%s, # Epoch=%s, Freq=%.0f~%.0f Hz, Unit=%s", nchanns, trial, freq_range[[1]], freq_range[[2]], scale), adj = 0, line = 1.5)
    # sub-title
    graphics::title(sprintf("%s/%s - Analysis Group %d", project_name, subject_code, group_item$group_id), adj = 0, line = 0.5, cex.main = 0.8)

    return(TRUE)
  })

  if( scale == "Min_Max_Normalized_Time_Window" ) {
    if( baselined ) {
      legend_range <- c(-1, 1)
    } else {
      legend_range <- c(0, 1)
    }

  } else {
    # if( baselined ) {
    #   legend_range <- c(data_min, data_max)
    # } else {
    #   legend_range <- c(0, data_max)
    # }
    legend_range <- c(data_min, data_max)
  }
  par("mar" = c(3.1, 0.5, 3, 4.5))
  pal_val <- seq(legend_range[[1]], legend_range[[2]], length.out = 101)
  graphics::image(matrix(pal_val, nrow = 1), x = 0, y = pal_val, axes = FALSE, xlab = "", ylab = "", col = palette)

  # Formatting axis labels using the custom function
  legend_at <- unique(c(legend_range, 0))
  formatted_legend_at <- format_numbers(legend_at)
  graphics::axis(side = 4, at = legend_at, labels = formatted_legend_at, las = 1, cex.axis=0.9)

  # Adjusting titles based on scale type
  if(scale == "Min_Max_Normalized_Time_Window") {
    graphics::title("Max Normalized", line = 0.6, adj = 0, cex.main = 0.9)
  } else {
    # Apply formatting to max and min data
    data_max_text <- format_numbers(data_max)
    data_min_text <- format_numbers(data_min)

    # Constructing actual range text based on baseline status
    # if(baselined) {
    #   actual_range_text <- paste0("-", data_min_text, " ~ ", data_max_text)
    # } else {
    #   actual_range_text <- paste0("0 ~ ", data_max_text)
    # }
    #
    # # Set title with formatted range
    # graphics::title(sprintf("[%s]", actual_range_text), line = 0.6, adj = 0, cex.main = 0.5)
  }

}

