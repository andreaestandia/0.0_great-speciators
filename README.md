.
├── data
│   ├── dfs #modified datasets opened in R to avoid merging everytime anything has to be run
│   ├── npoly # n_subspecies/n_polygons intersected between species distribution and biogeographical region. Data produced with scripts found in 'slurm' folder
│   ├── raw 
│   │   └── bamm #info about diversification analysis
│   │       └── trees
│   └── shapefiles #species distribution shapefiles + biogeographic regions shapefiles
│       ├── BOTW.gdb
│       └── wc10
├── notebooks
├── renv
│   ├── library
│   │   └── R-3.6
│   │       └── x86_64-pc-linux-gnu
│   │           ├── renv
│   │           │   ├── doc
│   │           │   ├── help
│   │           │   │   └── figures
│   │           │   ├── html
│   │           │   ├── Meta
│   │           │   ├── R
│   │           │   └── resources
│   │           └── scico
│   │               ├── help
│   │               │   └── figures
│   │               ├── html
│   │               ├── Meta
│   │               └── R
│   └── staging
│       └── 1
│           ├── 00LOCK-rstan
│           │   └── 00new
│           │       └── rstan
│           │           ├── libs
│           │           └── Meta
│           └── rstan
├── reports
│   ├── docs
│   └── plots
├── slurm #scripts to submit batch jobs to SLURM
└── src #source code

