#We are going to use the "psr_tidy_data" tibble as the dataset for determining NDVI.
#First off, remember to load your libraries. You'll need to load the tidyverse library:

library(tidyverse)

#Then, we're going to create an empty data frame
#It will take the spectrum_id - i.e., the groups - of the psr_tidy_data as rows.

indices <- psr_tidy_data %>%
  distinct(spectrum_id)

#The code above only needs to be run once. It does not need to be ran again when calculating the indices in the tasks.

#Recall that NDVI is the ratio of:
#1. The difference in the reflectance at 842 nm and 665 nm
#2. The sum of the reflectances at 842 nm ad 665 nm

#Let's analyse the code below in detail:
#A new variable, ndvi, is called - this will hold the NDVI values for each spectrum_id
#We use the tidyverse's dplyr library to limit our wavelength rows to 665 nm and 842 nm
#We then select from those wavelength rows the spectrum_id and the "mean_smoothed" values, which are the reflectances
#We conduct a pivot of the data...
#...which then allows for the calculation of NDVI itself: NDVI = (nm_842 - nm_665) / (nm_842 + nm_665)
#We then take the spectrum_id and NDVI values...
#...and in a seperate line of code, join that NDVI value to the indices tibble we created before.

ndvi <- psr_tidy_data %>%
  dplyr::filter(wavelength %in% c(665, 842)) %>%
  select(spectrum_id, wavelength, mean_smoothed) %>%
  pivot_wider(names_from = wavelength,
              names_prefix = "nm_",
              values_from = mean_smoothed) %>%
  mutate(NDVI = (nm_842 - nm_665) / (nm_842 + nm_665)) %>%
  select(spectrum_id, NDVI)


indices <- indices %>%
  left_join(ndvi, by = "spectrum_id")

#We can now inspect the values with:

head(indices)

#Note that NDVI values in range between -1 and 1. A value close to 1 suggests very healthy vegetation.
#Judging the values only on the reflectance spectra, _R, which of the plants is more healthy, based on NDVI?

#Use the NDVI calculation code above as the basis to calculate CRI, PSRI, and CAI.
#Look at your worksheet to find out which wavelength values to use, and what arithmetic operations to utilize.

#CRI
cri <- psr_tidy_data %>%
  dplyr::filter...
