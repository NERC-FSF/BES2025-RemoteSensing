#Remember to read in your libraries!
#You only have to do this once per session, but as good practice, we'll do so again...
#Where you see ellipses below, fill in with the name of the FSF package used for processing field spectroscopy data
#Recall that another library was used. What was it called, and how would you call it?
library(...)

#Read in your data
#There are three important arguments in the "reader" function - instrument_type, data_type, and file_path
#You can set the file_path argument in a number of ways. As an example:

tutorial2_path <- file.path("Tutorial Two Data/")
svc_files <- list.files(tutorial2_path, pattern = "\\.sig$",
                        full.names = TRUE)

#This sets a variable which is assigned to the path name of the Tutorial Two Data
#The next line of code assigns the tutorial2_path folder to a new variable called svc_files
#This is not the only way to do this! You could provide the path name fully to file_path
#Alternatively, you can set your working directory to the data folder itself, and leave file_path blank.
#With the file path known, though, how would you read in the SVC data?

svc_data <- ...?

#Recall that the SVC data is NOT interpolated or overlap removed:

svc_names <- names(svc_data)
head(svc_data[[svc_names[1]]]$wavelength, 20)

#How would you fix this? Interpolate data to 1 nm interval, from 350 nm to 2500 nm.

svc_data <- ...?

#There is a third step for the reflectance spectra processing, that removes artefacts from the detector overlap.
#What was it, and how is it called?

svc_data <- ...?


#The next stage is to group your data. We're going to use a positional argument of 1, i.e., group by the first underscore

svc_data <- group_spectra(...)

#Summarise the data, and plot the output.

svc_data_summary <- ...

#We can also inspect the dataframes in R Studio as well.
#Click the svc_data_summary line in the "Data" section of the Environment tab, directly to the right.

#Finally, export the data to a tibble:

svc_tidy_data <- ...?

#During the practicals, we have mentioned that the measurements taken are of relative reflectance.
#To convert our values to absolute reflectance, we must multiply our values by the calibration certificate of the panel
#For our SVC practical, this is SRT_SVCRP_20241206.csv
#We first read in the calibration file as a tibble:

panel_cal <- readr::read_csv("SRT_SVCRP_20241206.csv)

#And use the following code to correct:
svc_tidy_data <- svc_tidy_data %>%
  dplyr::left_join(panel_cal, by = "wavelength") %>%
  dplyr::mutate(
    mean = mean * reflectance,
    sd = sd * reflectance,
    se = se * reflectance
  ) %>%
  dplyr::select(-reflectance)

#Finally, we can also plot our data using ggplot2:

ggplot(svc_tidy_data, aes(x = wavelength, y = mean, color = spectrum_id, fill = spectrum_id)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, color = NA) +
  geom_line() +
  labs(
    x = "Wavelength (nm)",
    y = "Absolute Reflectance",
    color = "Spectrum ID",
    fill = "Spectrum ID",
    title = "Calibrated Spectral Reflectance"
  ) +
  theme_minimal()






