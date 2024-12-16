# Read the patient data CSV file
# Replace this path with the path to your data file
path_to_data_file <- "/Volumes/bigbrain/scripts/sean/rave-pipeline-sean/modules/multitaper_explorer/pipelining_example/patient_data.csv"

patient_data <- read.csv(path_to_data_file, header = TRUE, stringsAsFactors = FALSE, skipNul = TRUE)

# Initialize an empty list to store the processed patient data
patients_processed <- list()

# Loop through each row of the patient data to extract relevant fields
for (i in 1:nrow(patient_data)) {
  patient <- list(
    subject_code = patient_data[i, "subject_code"],
    project_name = patient_data[i, "project_name"],
    epoch_file_name = patient_data[i, "epoch_file_name"],
    load_electrodes = patient_data[i, "load_electrodes"],
    reference_name = patient_data[i, "reference_name"],
    time_window = as.numeric(unlist(strsplit(patient_data[i, "time_window"], ","))),
    condition = patient_data[i, "condition"],
    baseline = patient_data[i, "baseline"],
    baseline_duration = patient_data[i, "baseline_duration"],
    Resect = patient_data[i, "Resect"],
    SOZ = patient_data[i, "SOZ"]
  )
  patients_processed[[i]] <- patient
}

# Load the pipeline and shared environment
# Replace this path with the path to your rave modules
path_to_module <- "/Volumes/bigbrain/scripts/sean/rave-pipeline-sean/modules/"

pipeline <- raveio::pipeline(pipeline_name = "multitaper_explorer", paths = path_to_module)
pipeline_library <- pipeline$shared_env()

# Define the six frequency bands with their corresponding ranges
frequency_bands <- list(
  delta = c(0.5, 4),
  theta = c(4, 8),
  alpha = c(8, 13),
  beta = c(13, 30),
  gamma = c(30, 90),
  highgamma = c(90, 150)
)

# Base directory for saving plots
base_dir <- "path_to_output_directory/plots"

# Function to create directories if they do not exist
create_dir_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# Loop through each processed patient
for (patient_data in patients_processed) {
  
  # Extract patient information
  subject_code <- patient_data$subject_code
  project_name <- patient_data$project_name
  epoch_file_name <- patient_data$epoch_file_name
  load_electrodes <- patient_data$load_electrodes
  reference_name <- patient_data$reference_name
  time_window <- patient_data$time_window
  condition <- patient_data$condition
  baseline <- patient_data$baseline
  baseline_duration <- patient_data$baseline_duration
  Resect <- patient_data$Resect
  SOZ <- patient_data$SOZ
  scale <- "None"
  name_type <- "number"
  
  # Create a folder for the patient if it doesn't exist
  patient_folder <- file.path(base_dir, subject_code)
  create_dir_if_not_exists(patient_folder)
  
  # Loop through each frequency band for analysis
  for (freq_name in names(frequency_bands)) {
    freq_range <- frequency_bands[[freq_name]]
    
    # Create a folder for the frequency band within the patient folder
    frequency_folder <- file.path(patient_folder, freq_name)
    create_dir_if_not_exists(frequency_folder)
    
    # Update the save path for the frequency folder
    save_path <- frequency_folder
    
    # Set the pipeline settings for the current patient and frequency band
    pipeline$set_settings(subject_code = subject_code, 
                          project_name = project_name,
                          epoch_file_name = epoch_file_name,
                          load_electrodes = load_electrodes, 
                          reference_name = reference_name, 
                          time_window = time_window, 
                          condition = condition,
                          baseline = baseline, 
                          start_time_baseline = 0,
                          end_time_baseline = baseline_duration,
                          analysis_time_frequencies = list(`1` = list(frequency_range = freq_range,     time_range = c(0, 20))), 
                          baselined = TRUE,
                          window_params = c(2.5, 0.5),
                          frequency_range = c(0.5, 150),
                          time_bandwidth = 3,
                          decibal = FALSE)
    
    # Run the pipeline for ML prediction and heatmap generation
    ML_prediction_electrode <- pipeline$run("ML_prediction_electrode")
    heatmap <- pipeline$run("heatmap_result")
    
    # Create full file paths for each plot in the frequency folder
    power_plot_path <- file.path(save_path, "power_over_time_data.png")
    line_plot_path <- file.path(save_path, "line_plot.png")
    quantile_plot_path <- file.path(save_path, "quantile_plot.png")
    
    # Plot power over time and save the result in the frequency folder
    pipeline_library$plot_power_over_time_data(power_over_time_data = heatmap,
                                               trial = condition,
                                               soz_electrodes = SOZ,
                                               resect_electrodes = Resect,
                                               name_type = name_type,
                                               save_path = power_plot_path,
                                               ML_prediction_electrode = ML_prediction_electrode,
                                               show_ML = TRUE)
    
    # Generate line plot for the power-over-time data and save in the frequency folder
    pipeline_library$plot_power_over_time_data_line(power_over_time_data = heatmap,
                                                    trial = condition,
                                                    soz_electrodes = SOZ,
                                                    resect_electrodes = Resect,
                                                    name_type = name_type,
                                                    save_path = line_plot_path)
    
    # Generate quantile plot for the power-over-time data and save in the frequency folder
    pipeline_library$plot_quantile_plot(power_over_time_data = heatmap,
                                        trial = condition,
                                        soz_electrodes = SOZ,
                                        resect_electrodes = Resect,
                                        name_type = name_type,
                                        save_path = quantile_plot_path)
  }
}

