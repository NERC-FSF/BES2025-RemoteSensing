#Importing your libraries
library(RUFFS)
library(tidyverse)

#Setting your filepath
#Here, we take the files stored in the RUFFS vignette.
extdata_path <- system.file("extdata", package = "RUFFS")
list.files(extdata_path)

#Reading ASD Files
asd_files <- list.files(extdata_path, pattern = "\\.asd$",
                        full.names = TRUE)
asd_data <- reader(asd_files, data_type = "reflectance")
asd_data

#Reading SVC files
svc_files <- list.files(extdata_path, pattern = "\\.sig$",
                        full.names = TRUE)
svc_data <- reader(svc_files, data_type = "reflectance")
svc_data

#Reading PSR files
psr_files <- list.files(extdata_path, pattern = "\\.sed$",
                        full.names = TRUE)
psr_data <- reader(psr_files, data_type = "reflectance")
psr_data

#Inspecting files
asd_names <- names(asd_data)
head(asd_data[[asd_names[1]]]$wavelength, 20)

psr_names <- names(psr_data)
head(psr_data[[psr_names[1]]]$wavelength, 20)

svc_names <- names(svc_data)
head(svc_data[[svc_names[1]]]$wavelength, 20)

#Plotting SVC data with overlap retained
first_svc <- svc_data[[svc_names[1]]]
plot(first_svc$wavelength, first_svc$reflectance, type = "s",
     xlab = "Wavelength (nm)", ylab = "Reflectance",
     main = "SVC spectrum with detector overlaps")

#Stitching SVC data
svc_data <- apply_to_collection(svc_data, overlap_stitching,
                                method = "average")

#Interpolating SVC data to 1 nm between 350 nm and 2500 nm
svc_data <- apply_to_collection(svc_data, interpolation,
                                seq(350, 2500, 1))


# Plotting the first of the SVC spectrum after correction
first_svc_processed <- svc_data[[svc_names[1]]]
plot(first_svc_processed$wavelength,
     first_svc_processed$reflectance,
     type = "s",
     xlab = "Wavelength (nm)", ylab = "Reflectance",
     main = "SVC spectrum after overlap stitching and interpolation")

# Jump correction
svc_data <- apply_to_collection(svc_data, jump_correct,
                                splices = c(990, 1810),
                                reference = 2)

asd_data <- apply_to_collection(asd_data, jump_correct,
                                splices = c(1000, 1800),
                                reference = 2)

psr_data <- apply_to_collection(psr_data, jump_correct,
                                splices = c(1000, 1800),
                                reference = 2)

#Grouping ASD data
names(asd_data)

asd_data <- group_spectra(asd_data,
                          separator = "_",
                          position = 1,
                          ignore_extension = TRUE)

#Grouping SVC data, illustrating use of the position argument
svc_data <- group_spectra(svc_data,
                          separator = "_",
                          position = 2,
                          ignore_extension = TRUE)

# Use of the summarise function
asd_data_summary <- summarise_spectra(asd_data)

plot_summary(asd_data_summary)

#Exporting to tibble

asd_tidy_data <- to_tibble(asd_data_summary)
head(asd_tidy_data)


