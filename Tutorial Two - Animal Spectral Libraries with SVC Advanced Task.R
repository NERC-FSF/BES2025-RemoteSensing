# As part of this task, we'll need to integrate over areas in your data.
# We can do this using a library called "pracma"
# To install a package, we can run code like this:

install.packages("pracma")

#Now we can call the pacakge:
library(pracma)


# We will also need to read in the spectral responses for the satellite of interest, in this case, ESA Sentinel-2.
# We only need to run this code below once -- it will apply when attempting to solve for the other two bands.
sentinel_2_bands <- read.csv("Sentinel 2 SRF.csv", row.names = 1, header = TRUE)

# We will also create an empty tibble to store our convolved values for each group.
# As with the above, this only needs to be done once, and not for when solving for bands 2 and 3:
sentinel_2_convolution <- tibble(
  spectrum_id = character(),
  band_name = character(),
  convolved_value = numeric()
)

# We're now going to filter the Sentinel 2 bands tibble to only the wavelength region we are interested in.
# This is 646 nm, to 684 nm.
sentinel_band_4 <- sentinel_2_bands[rownames(sentinel_2_bands) >= 646 &
                                     rownames(sentinel_2_bands) <= 684, ]


#And from this, we take just the spectral response function for Band 4, which has the column name 'B4'
sentinel_srf_band_4 <- sentinel_band_4$B4

#Let's do the same for the svc_tidy_data tibble we created in the Basic Task.
wavelength_range_band_4 <- svc_tidy_data %>%
  filter(wavelength >= 646, wavelength <= 684)

# Now we conduct the convolution. Recall that this is a ratio:
# The denominator is just the trapezoidal integration of the spectral response function of the band itself:
denominator <- trapz(sentinel_srf_band_4)

#While the numerator is the trapezoidal integration of the product of the reflectance and the SRF of the band:
band_4_results <- wavelength_range_band_4 %>%
  group_by(spectrum_id) %>%
  summarise(
    convolved_value = trapz(mean * sentinel_srf_band_4) / denominator,
    .groups = 'drop'
  ) %>%
  mutate(band_name = "Band 4 - Red") %>%
  select(spectrum_id, band_name, convolved_value)

# Now lets add this to the sentinel_2_convolution tibble we created earlier
sentinel_2_convolution <- bind_rows(sentinel_2_convolution, band_4_results)

#The task now is to repeat the process for Bands 2 and 3.
#Effectively, you need to know the wavelength range of those bands, and modify the code above, giving appropraite names to the variables.
#Remember, we do not need to reinitalize the empty tibble "sentinel_2_convolution", or read in the Sentinel 2 SRF.csv


