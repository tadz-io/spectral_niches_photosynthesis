# Vibrational modes of water predict spectral niches for photosynthesis in lakes and oceans
Scripts accompanying figures in:

> Holtrop, T., Huisman, J., Stomp, M., Biersteker, L., Aerts, J., Grébert, T., Partensky, F., Garczarek, L. & Woerd, H.J. Vibrational modes of water predict spectral niches for photosynthesis in lakes and oceans. *Nature Ecology & Evolution* (Accepted)

Datasets will be made availabe on figshare (TBA). Datasets should be downloaded to the `data` folder. 

`plot_figures.R` should be used to recreate Figure 2 in Holtrop et al.  `approx_euphotic_spectra.R` can be used to approximate the spectral irradiance at prespecified depth `EUPH_FRAC` and `csv_path` should point to `./data/irrandiance.csv` or `./data/irradiance_no_harmonics.csv`:

```R
approx_euphotic_spectra('./data/irrandiance.csv', "Ed", quanta=TRUE, EUPH_FRAC=0.01)
```
Example above approximates the spectral *planar* irradiance in quanta at the euphotic depth (1% level). The same calculation but for scalar irradiance:

```R
approx_euphotic_spectra('./data/irrandiance.csv', "Eod", quanta=TRUE, EUPH_FRAC=0.01)
```

Jupyter notebook `spectral_niches.ipynb` should be used to recreate Figure 4 in Holtrop et al.


