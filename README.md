# 1 Contents
## 1-1 Software
<dl>
	<dt>SYS20250121_SPEEDY-ClimaX_LETKFv1.13</dt>
	<dd>
Porgrams and scripts of ClimaX-LETKF.
	</dd>
</dl>

DOI: 10.5281/zenodo.15029841

## 1-2 Related resources
<dl>
	<dt>weatherbc_v20240703</dt>
	<dd>
WeatherBench data.

Used for input and validation.

DOI: 10.5281/zenodo.15029837
	</dd>
	<dt>SYS20250121_SPEEDY-ClimaX_LETKFv1.13_DATA_min</dt>
	<dd>
Input for or output of ClimaX-LETKF experiments.

They are generated through following processes, so the user doesn't need to use it to run the experiment. They are used for checking the user's products by comparing with them.

DOI: 10.5281/zenodo.15016252
	</dd>
	<dt>PREPBUFR_v02</dt>
	<dd>
Programs and scripts for processing PREPBUFR observation dataset for data assimilation.

DOI: 10.5281/zenodo.15029781
	</dd>
	<dt> PREPBUFR_v02_out_min</dt>
	<dd>
Processed PREPBUFR observation data through observation thinning for data assimilation.

DOI: 10.5281/zenodo.15029720
	</dd>
</dl>

`CLDIR` is the absolute path of this package "SYS20250121_SPEEDY-ClimaX_LETKFv1.13" and `PBDIR` is the absolue path of the package "PREPBUFR_v02".


# 2 How to run ClimaX-LETKF and analyze the output
## 2-1 Preparing WeatherBench data

Download WeatherBench data weatherbc_v20240703.tar.gz from <https://doi.org/10.5281/zenodo.15029837> and execute:

	$ mkdir ${CLDIR}/DATA
	$ tar xvzf weatherbc_v20240703.tar.gz
	$ mv weatherbc_v20240703 ${CLDIR}/DATA/.
	$ cd ${CLDIR}/DATA
	$ ln -s weatherbc_v20240703 weatherbc

## 2-2 Preparing observations

Make observation data for data assimilation following the guide of the observation thinning program for PREPBUFR.

Then, move (or link) the directory of observations as follows:

	$ mkdir ${CLDIR}/DATA/prepbufr_(product name)
	$ mv ${PBDIR}/out/thinning/(product name)/obs ${CLDIR}/DATA/prepbufr_(product name)/.

Product name identifies the assimilated observation data made by observation thinning.

## 2-3 Compiling

	$ cd ${CLDIR}/speedy-climax/300_letkf
	$ singularity run ../share/oneapi-hpckit_latest.sif
	$ sh 200_make_letkf-climax.sh
	$ sh 201_make_obsopeMPI-climax.sh
	$ sh 202_make_ensmean-climax.sh

## 2-4a Running data assimilation cycle experiments

	$ cd ${CLDIR}/speedy-climax/300_letkf/run

### (1) Make initial states

	$ sh 002_init-climax.sh

### <a id="2-4a(2)"></a>(2) Edit main script of experiments

Edit the script `300sk_run_letkf_baseexp_speedy-climax.sh`.

Important parameters are as follows. Note that "XX_MASTER" is a list of "XX". For example, if "XX_MASTER=0 1", then two experiments for "XX=0" and "XX=1" are conducted.
<dl>
	<dt>CLIMAXVER</dt>
	<dd>
Version of ClimaX. "def" is the one where original initialization method is used and "int" is the one where modified method is used.

The details of modification of initialization method is described in Supplement 3 in Takeshima et al. (2025).
	</dd>
	<dt>SETDAS_MASTER</dt>
	<dd>
A list of data assimilation methods.
	</dd>
	<dt>SETPOW_MASTER</dt>
	<dd>
A list of weight parameter of PO-LETKF.
	</dd>
	<dt>SETINF_MASTER</dt>
	<dd>
A list of multiplicative inflation factors.
	</dd>
	<dt>SETRTP_MASTER</dt>
	<dd>
A list of relaxation methods.
	</dd>
	<dt>SETALP_MASTER</dt>
	<dd>
A list of relaxation parameters.
	</dd>
	<dt>SETADD_MASTER</dt>
	<dd>
A list of PO-based additive inflation factors.
	</dd>
	<dt>SETMEM_MASTER</dt>
	<dd>
A list of ensemble sizes.
	</dd>
	<dt>SETHSG_MASTER</dt>
	<dd>
A list of localization scale (km).
	</dd>
	<dt>SETOBS_MASTER</dt>
	<dd>
A list of names of the assimilated observation data.

If `SETOBS_MASTER=XX`, observation data in `${CLDIR}/DATA/XX/obs` are assimilated.
	</dd>
	<dt>SDATE</dt>
	<dd>
Initial date and time.
	</dd>
	<dt>EDATE</dt>
	<dd>
Final date and time.
	</dd>
	<dt>RESTART_MODE</dt>
	<dd>
0 for new experiments and 1 for restarting.
	</dd>
</dl>

Experiment name `$EXP` is as follows:

`V250122_CLIMAX${CLIMAXVER}-${DASNAME}_M${MEM}L${HSG}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}`.

The value of `$DASNAME` corresponds to that of `$DAS`.

### (3) Execute the script

	$ sh 300sk_run_letkf_baseexp_speedy-climax.sh

## 2-5a Validating data assimilation cycle experiment results

	$ cd ${CLDIR}/speedy-climax/211_val-climax

### (1) Make data

Edit the script `000_calc_dacycle.sh` and execute it.

Its important parameters are same to those of the script `300sk_run_letkf_baseexp_speedy-climax.sh` in [2-4a (3)](#2-4a(3)).

Then, text files such as `gnu_000/anal_sprd_time_prepbufr_${EXP}_glb.txt` are generated.

### (2) Draw figures

Edit the GNU file `gscript_010_RTPX.gnu` and execute the script `010_mkfig_times.sh`.

Then, the figures are generated in `fig_010_time`, which is given in the GNU file.


## 2-4b Running Forecast experiments

	$ cd ${CLDIR}/speedy-climax/300_letkf/run

### (1) Make initial states

	$ sh 002_init-climax.sh

### (2) Edit main script of experiments

Edit the script `999sk_run_letkf_baseexp_speedy-climax_fcst.sh`.

Most parameters are common to those of `300sk_run_letkf_baseexp_speedy-climax.sh`, but the user must set the value of `$EXTFCST_STEP`, which gives the number of time steps of the forecast lead time.

If `EXTFCST_STEP=20`, then the lead time is 120 h (6 h x 20).

### (3) Execute the script

	$ sh 999sk_run_letkf_baseexp_speedy-climax_fcst.sh

## 2-5b Validating forecast experiment results

	$ cd ${CLDIR}/speedy-climax/211_val-climax

### (1) Make data

Edit the script `001_calc-forecast.bash` and execute it.

Statistical values such as RMSE and ensemble spread are computed and saved in the directory `gnu_011`.

File name of RMSE of an experiment is `prepbufr_CLIMAX${CLIMAXVER}-${DASNAME}_M$${MEM}L${HSG}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}_SMP${datetime_bgn}-${datetime_end}_fcst_glb_RMSE.txt`.

That of ensemble spread is similar but the string `RMSE` is replaced with `SPRD`.

### (2) Draw figures

Edit the GNU file `gscript_011.gnu` and execute `011_mkfig_fcst.sh`.

Then, the figures are generated in `fig_011_fcst`, which is given in the GNU file.
