#In the last basic exercise, the framework for reading and processing data was provided.
#In this basic task, we'll go further, providing only a list of steps that needs to be conducted on the data.
#Follow through until you arrive at a final tibble:
#1. Read in libraries for RUFFS and TidyVerse
#2. Read in the data found in the "Tutorial Three Data" data. This is PSR+3500 data collected in the first session.
#3. Conduct processing steps (note - this is PSR+3500 data. Is interpolation required? What about overlap matching?)
#4. Group the data by plant type, and by "R" (reflectance) and "T" transmission.
#NOTE - for above, you may have to use multiple arguments for the position.Use the c() functionality to pass multiple arguments, e.g. c(1,4)
#5. Summarise the data, and plot
#6. Export to a tibble, with the final name
#7. Correct to absolute reflectance, using the SRT_PSR-R_20251010.csv as the panel calibration file.
#8. Keep the final name as "psr_tidy_data"

library(...

#Notice from the plots that the data is a little noisy after 2000 nm.
#This is because the light output at this range has decreased
#The signal to noise ratio of the spectrometer at this range has also decreased.
#We can conduct smoothing of the data to lessen this effect.
#To do this, we will install a package called "signal"

install.packages("signal")
library(signal)

# Apply Savitzky-Golay filter
psr_tidy_data <- psr_tidy_data %>%
  group_by(spectrum_id) %>%
  mutate(mean_smoothed = sgolayfilt(mean, p = 2, n = 41)) %>%
  ungroup()

# And plot, to see the difference (note, the use of mean_smoothed for the y values)

ggplot(psr_tidy_data, aes(x = wavelength, y = mean_smoothed, color = spectrum_id, fill = spectrum_id)) +
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

