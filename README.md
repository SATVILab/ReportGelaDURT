# ReportGelaDURT

This project builds a `bookdown` HTML document that contains the figures for the paper [Gela (2022) - Effects of BCG vaccination on donor unrestricted T cells in two prospective cohort studies](https://doi.org/10.1016/j.ebiom.2022.103839). 

It relies on data processed in [`DataTidyGelaDURT`](https://github.com/SATVILab/DataTidyGelaDURT).

## Reproducing analyses

### Using VS Code and Docker

Using Linux (WSL or "bare"), with git, Docker and VS Code with the `Remote Extensions Pack` installed and set up, do the following:

1. Clone this repository using `git clone https://github.com/ReportGelaDURT`. 

2. Create the environment variable `DirProjectGelaDURT`, and set this equal to the directory to which you would save all non-code materials related to this project. If you are only running this part of the analysis (i.e. not also running `DataTidyGelaDURT`), then all this does is set where the figures are saved to.

To make this permanent, you could add this to the `~/.bashrc` file as follows:
`export DirProjectGelaDURT=<path_to_project`.

For example:
`export DirProjectGelaDURT=~/projects/ProjectGelaDRT`. 

- Notes:
  - The base name of the path (e.g. `ProjectGelaDURT` in `~/projects/ProjectGelaDURT`) does not need to be `ProjectGelaDURT` but can be anything (e.g. `"xyz"`).
  - This directory need not exist before starting the container.

3. Open the directory `ReportGelaDURT` as a workspace in VS Code.

4. Click `Control Panel >> Remote-Containers: Rebuild and reopen in container`. Wait for container to build.

Note that the `devcontainer.json` file assumes that the logged-in user has user ID `1000`.

5. Start `R` and run `renv::restore()`. Restart `R`.

6. Run `bookdown::render_book()`. 

The book will be built to `./_book`, and the figures will be saved to `$ProjectGelaDURT/Output/figures/manuscript`. 


## Without Docker

Ensure that you have `R` version `4.1.3` installed.

1. Clone this repository using `git clone https://github.com/ReportGelaDURT`.

2. Open `R` (version `4.1.3`) and run `renv::restore()`. Restart `R`.

Note that if you are using RStudio and have installed `R` version `4.1.3`, you can open R4.1.3 in RStudio by clicking `Tools >> Global Options >> General >> Change...` (where `Change...` is next to a path underneath `R version:`), choosing an option ending in `R-4.1.3`, clicking `OK`, clicking `OK` again and restarting RStudio (not just R!).

3. Run `bookdown::render_book()`.

The book will be built to `./_book`.
