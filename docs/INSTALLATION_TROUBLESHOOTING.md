# Installation troubleshooting
Make sure you have the required dependencies installed
- Dependencies:
    - [LAPACK](https://github.com/Reference-LAPACK/lapack)
    - [BLAS](https://netlib.org/blas/)
    - [GSL](https://www.gnu.org/software/gsl/)

They should be available for most of the popular distros, using default packgage managers for the platform:
- For macOS you can use [Homebrew](https://brew.sh/): run `brew install openblas lapack gsl`
- For Debian-based distros (such as Ubuntu) you could use
`sudo apt-get install libblas-dev liblapack-dev libgsl-dev` ([more info](https://wiki.debian.org/DebianScience/LinearAlgebraLibraries))
- For Arch Linux / distros based on it `pacman -S blas lapack gsl` just solves it
