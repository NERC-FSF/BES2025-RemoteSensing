#In this final task, we'll walk through how to conduct principal component analysis for spectral data.
#First, we'll load in the tidyverse and broom libraries
library(tidyverse)
library(broom)

#Then, we'll install two extra libraries which help with plotting PCA results
install.packages("ggfortify")
library(ggfortify)

install.packages("factoextra")
library(factoextra)


#We're going to read in the ASD data again, but this time, we're going to export to a tibble every individual spectra.
#We won't do any grouping -- this is important, as we want to see how our individual spectra cluster together!

tutorial4_path <- file.path("Tutorial Four Data/")
asd_files <- list.files(tutorial4_path, pattern = "\\.asd$",
                        full.names = TRUE)
asd_data <- reader(asd_files, instrument_type = "asd", data_type = "reflectance")
asd_data

asd_data <- apply_to_collection(asd_data, jump_correct,
                                splices = c(1000, 1800),
                                reference = 2)

asd_tidy_data <- to_tibble(asd_data)


#We're going to assign each of our individual spectra to a land cover type, based on its name
#For example, file names with BES_gravel in their name will be assigned to the "gravel" landcover type.

spectra_tibble <- asd_tidy_data %>%
  mutate(landcover_type = str_extract(spectrum_id, "(?<=BES_)[^_]+"))

#We then morph the spectra_tibble so that it's in a form for the PCA analysis.
spectra_wide <- spectra_tibble %>%
  select(spectrum_id, wavelength, reflectance, landcover_type) %>%
  pivot_wider(
    names_from = wavelength,
    values_from = reflectance,
    names_prefix = "wl_",
    id_cols = c(spectrum_id, landcover_type)
  )

#We initiate a new tibble that will hold the PCA matix:
pca_matrix <- spectra_wide %>%
  select(starts_with("wl_")) %>%
  as.matrix()

#And then run the prcomp function. prcomp is an in built function to R
pca_result <- prcomp(pca_matrix, scale. = TRUE, center = TRUE)

#We then re-input our spectrum_id and landcover type back into
pca_scores <- pca_result$x %>%
  as_tibble() %>%
  bind_cols(spectra_wide %>% select(spectrum_id, landcover_type))

#Now, we can see the results of our PCA.
#What we are interested in is the proportion of variance ascribed to the component.
#PC1 will usually account for between 70% to 90% of all variance
summary(pca_result)


# Let's plot our individual spectra, grouped by landcover type, along two axis - PC1, and PC2.
# We can see that our the indivdiual spectra within landcover types cluster among each.
ggplot(pca_scores, aes(x = PC1, y = PC2, color = landcover_type)) +
  geom_point(alpha = 0.6, size = 1.5) +
  theme_minimal() +
  labs(x = "PC1", y = "PC2",
       title = "PCA of Individual Spectral Measurements",
       color = "landcover Type")


#How would we plot the PC2 vs the PC3 scatter?


#We can also use the factoextra library to plot our PCA analysis.
#The ellipses show the confidence interval per landcover type.
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = spectra_wide$landcover_type,
             palette = c("gravel" = "#5392AD",
                         "sand" = "#E3E36B",
                         "heather" = "#86B565",
                         "soil" = "#BD687B"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             ellipse.level = 0.95,
             pointsize = 1.5,
             alpha.ind = 0.6,
             legend.title = "Landcover Type",
             title = "PCA - Individual Spectra by Landcover Type") +
  theme_minimal()

#What if we want to know which wavelengths are most important for explanining the difference between groups?
#We can take the loading values from the PCA, filter by the first three components, and take the 10 most important wavelengths per PC that contributed to the difference

loadings_df <- pca_result$rotation %>%
  as_tibble(rownames = "wavelength") %>%
  mutate(wavelength = as.numeric(str_remove(wavelength, "wl_"))) %>%
  pivot_longer(starts_with("PC"), names_to = "PC", values_to = "loading")

key_wavelengths <- loadings_df %>%
  dplyr::filter(PC %in% c("PC1", "PC2", "PC3")) %>%
  group_by(PC) %>%
  slice_max(abs(loading), n = 10) %>%
  arrange(PC, desc(abs(loading)))

#We can now inspect which wavelengths can be attributed for the most difference between groups
print(key_wavelengths, n = 20)

#Think on how this could be used in other surveys:
#If you had reflectance values from healthy members of a tree species, and unhealthy members, you could find the wavelength at which you could discriminate between them.

