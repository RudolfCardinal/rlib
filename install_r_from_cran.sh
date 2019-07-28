#!/bin/bash

# https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04

set -e

# Ubuntu 18.04, R 3.5 (e.g. July 2019):

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt update
sudo apt install r-base r-recommended

