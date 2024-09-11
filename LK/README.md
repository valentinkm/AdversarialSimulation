# Individual Study and Thesis by Leonard Kosanke

This subdirectory contains all files from my [individual simulations](https://github.com/lkosanke/AdversarialSimulation/blob/main/LK/run_simulations.R), their [postprocessing](https://github.com/lkosanke/AdversarialSimulation/blob/main/LK/postprocess_results.R), the analysis of their [results](https://github.com/lkosanke/AdversarialSimulation/blob/main/LK/results.pdf) and my [master thesis](https://github.com/lkosanke/AdversarialSimulation/blob/main/LK/thesis.pdf).

All of these can be reproduced as described below.

## Reproducing this project

To reproduce any parts of this project, first follow these steps:

1. **Preperation**
   - Ensure you have admin rights on your account. This is necessary for some of the installations (e.g RStudio, LaTex packages).

2. **Install LaTeX and R**:
   - Ensure that recent versions of LaTeX, R, RStudio and Quarto are installed on your system.
  
3. **Download or clone the repository**
   - The repository root is here: <https://github.com/lkosanke/AdversarialSimulation>.
   - Download or clone the repository.

4. **Open folder and set working directory**
   - In RStudio, navigate to the repository folder.
   - set the working directory at the root.
   
5. **Install all relevant dependencies**:
   - Navigate to the LK subfolder.
   - Open the install_dependencies.R file.
   - Navigate to the Terminal.
   - Depending on your OS, run the following 2 commands in the terminal:
     
   - *Windows*
     ```sh
     tlmgr update --self
     ```
     
     ```sh
     tlmgr install collection-latexrecommended libertine pdfpages lualatex-math luatexbase titling pdfx luatex85 colorprofiles multirow float pgf
     ```

   - *MacOS or Linux*
     ```sh
     sudo tlmgr update --self
     ```

     ```sh
     sudo tlmgr install collection-latexrecommended libertine pdfpages lualatex-math luatexbase titling pdfx luatex85 colorprofiles multirow float pgf
     ```

     Watch the output for password prompts. If one appears, type in your PC password (you won't see the letters when typing) and press Enter.
     
   - Navigate back to the LK subdirectory folder.
   - Run the file install_dependencies.R

6. **Reproduce what you want to reproduce!**:
   - See the specific reproduction guides below

### Specific reproduction guides
Next to all individual scripts and markdowns, you can reproduce the entire simulation study.

#### Reproducing the simulation study

1. After succesfully installing all dependencies, you can run the run_simulations.R file to run all 6 simulation scripts in a parallelised manner.


#### Reproducing the results analysis (only)

In the simulation, the calculation of confidence intervals was off for all studies. 
Thus, the raw results had to be reprocessed, for the calculations to be correct.
This is explained in detail in the results.pdf file.
If you want to reproduce this postprocessing as well, start with the first step. Otherwise, you can jump to step 2.

1. Run the postprocess_results.R file.

2. Run the results.qmd file.

3. For reproduction of the final pdf with quarto, run Render in the top panel of Rstudio in the results.qmd file.

#### Reproducing my thesis document (only)

1. Run the thesis.qmd file

2. For reproduction of the final pdf with quarto, run Render in the top panel of Rstudio in the thesis.qmd file.

#### Reproducing my simulation protocol (only)

1. Run the simulation_protocol.qmd file

2. For reproduction of the final pdf with quarto, run Render in the top panel of Rstudio in the simulation_protocol.qmd file.

