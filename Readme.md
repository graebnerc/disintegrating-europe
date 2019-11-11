# Is the Eurozone disintegrating? Macroeconomic divergence, structural polarization, trade and fragility
[Claudius Gräbner](https://claudius-graebner.com), 
[Philipp Heimberger](https://wiiw.ac.at/philipp-heimberger-s-1138.html), 
[Jakob Kapeller](https://jakob-kapeller.org/) and 
[Bernhard Schütz](https://www.jku.at/institut-fuer-die-gesamtanalyse-der-wirtschaft/ueber-uns/team/bernhard-schuetz/)

Here we provide all the data and the code to replicate all empirical exercises in the abovementioned paper, which has been published in the 
*Cambridge Journal of Economics* (DOI: [10.1093/cje/bez059](https://doi.org/10.1093/cje/bez059), Gräbner *et al.* 2019a).

The repository contains all code to replicate all figures in the paper.
While all intermediate data required can be found in the repository, some
raw data has not been deployed because the data is too large. 
However, from the source files the way we process the raw data becomes clear,
and it is easy to use these files to recreate our reduced data if you are
willing to download the raw data from the 
[Atlas of Economic Complexity](https://intl-atlas-downloads.s3.amazonaws.com/index.html).
There is also an accompanying [data publication at the Harvard Dataverse](https://doi.org/10.7910/DVN/1PGDM4),
(Gräbner *et al.* 2019a), which contains all raw data.

## Code structure

The country classification used in the paper is encoded in 
`code/setup_country_classification.R` and used by almost all scripts in this
repository.

The file `code/set_up_harv_hs_trade_data.R` can be used to create the data files
`harv_eci_ranking.csv` and `china_exports.csv` as well as 
`exports_harv_hs_red.csv.bz2`. However, it requires `harv_hs.csv.bz2`, which
is too large and can be downloaded from the Atlas of Economic Complexity or the
accompanying data publication ([Gräbner *et al.* 2019b](https://doi.org/10.7910/DVN/1PGDM4)).

See the source code in `setup_harv_hs_trade.R` to see how the raw data gets
processed. In this repo, only the results of this file needed by other files
are included.

The same is true for the file `code/fig_A1_pci-dist.R`, which produces the
first figure for the supplementary material. It also relies on `harv_hs.csv.bz2`,
which is not included in this repository.

The file `bilat_hs2.R` can be used to create the data file with the same name.
Because of its size, this data file is included only in [Gräbner *et al.* (2019b)](https://doi.org/10.7910/DVN/1PGDM4).

Figure 5 in the paper is created via `code/fig_5_trade-flows-china.R`.
The main functions for this file are defined in `code/bilat_analysis_functions.R`.

The remaining R files produce the figures as indicated in their title and
should be self-contained.

The Mathematica file `fig_6-debt-dynamics.nb` is used to create figures 6 and 10, 
respectively. 
In order to use them you must adjust the working directory in the file.
The data for these files are created in the files `code/fig_6_data.R` 
and `code/fig_10_data.R`, the latter using functions defined in 
`code/fig_10_data_functions.R`.
`code/fig_6_data.R` creates `fig_6_debt_data.csv` by accessing the IMF API 
directly, so it requires access to the internet.
`code/fig_10_data.R` uses `data/exports_harv_hs_red.csv.bz2` to create 
estimates for the measure of technological directedness, which has been 
introduced and discussed in Gräbner *et al.* (2019c).

The resulting data are 
`wls_03_07_vs_10_14.csv`,
`wls_95_99_vs_03_07.csv`, and
`wls_95_99_vs_10_14.csv`, which are then used in `code/fig_6_10.nb`.
Keep in mind that `code/fig_10_data.R` takes quite some time to complete 
its task.


## Structure for the working directory

The code assumes that the working directory is structured as follows:
`working-directory/code/` contains all the R and Mathematica files, 
`working-directory/data/` contains the data and `working-directory/output` 
exists. All figures will be saved in this folder.
The code makes use of the [here package](https://github.com/r-lib/here) 
and thus assumes that an R Project file or a `.here` file is present in the root 
of the working directory.

## References

Gräbner, C., Heimberger, P., Kapeller, J. and Schütz, B. (2019a): 
Is the Eurozone disintegrating? Macroeconomic divergence, structural polarization, trade and fragility, *Cambridge Journal of Economics*, forthcoming. 
DOI: [10.1093/cje/bez059](https://doi.org/10.1093/cje/bez059).

Gräbner, C., Heimberger, P., Kapeller, J. and Schütz, B. (2019b): Replication Data for: Is the Eurozone disintegrating? Macroeconomic divergence, structural polarization, trade and fragility, DOI: [10.7910/DVN/1PGDM4](https://doi.org/10.7910/DVN/1PGDM4), Harvard Dataverse, Version 2.

Gräbner, C., Heimberger, P., Kapeller, J. and Schütz, B. (2019c): 
Structural change in times of increasing openness: assessing path dependency in 
European economic integration, *Journal of Evolutionary Economics*, forthcoming. 
DOI: [10.1007/s00191-019-00639-6](https://doi.org/10.1007/s00191-019-00639-6).