# MOREshiny

This repository contains the Dockerized environment of [MORE](https://github.com/BiostatOmics/MORE) (Multi-Omics REgulation) R package. MORE is capable of inferring condition specific multi-modal regulatory networks. The MORE method applies MLRs or PLS to model a target omic expression as a function of experimental variables, such as diseases or treatments, and the potential regulators of that given target feature. The aim is to obtain specific candidate regulators for the biological system under study.

**NOTE:** This is a simplified version of the MORE package, designed for users with fewer experience in **R** programming or **statistics**. It provides an accessible way to run the core functionalities of the package via Docker. However, if the users are experienced in those areas we encourage them to use the extended version of MORE available in the [github](https://github.com/BiostatOmics/MORE). 


## Getting Started

### Prerequisites

-[Docker](https://www.docker.com/) installed in your system

>**Windows Users:** 
> If you are using **Windows** (not macOS or Linux), you must have [WSL (Windows Subsystem for Linux)](https://learn.microsoft.com/en-us/windows/wsl/install) installed and properly set up in order to run Docker containers.  

### Build the Docker Image

Open your terminal and navigate to the folder containing the Dockerfile, then run:

```bash
docker build -t more-shiny .
```

### Run the container

Finally run the docker container by:

```bash
docker build -d -p 3838:3838 more-shiny
```

## Usage

You can find the User Guide for the package in the vignettes folder or access it directly [here](https://github.com/BiostatOmics/MORE/blob/master/vignettes/UsersGuide.pdf). Furthermore, in the same folder we include a full step-by-step tutorial to run MORE (accessible [here](https://github.com/BiostatOmics/MORE/blob/master/vignettes/tutorial.html))


### Citation

If you use MORE, please cite: Aguerralde-Martin, Maider; Clemente-Císcar, Mónica; Conesa, Ana and Tarazona, Sonia. MORE interpretable multi-omic regulatory networks to characterize phenotypes. *Briefings in Bioinformatics*, accepted. DOI: https://doi.org/10.1093/bib/bbaf270.


### License

MORE is developed under the license GPL-2
