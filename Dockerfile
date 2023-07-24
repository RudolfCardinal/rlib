# Dockerfile

FROM asachet/rocker-stan:latest
MAINTAINER "Rudolf Cardinal" rudolf@pobox.com


WORKDIR /cardinal_rlib
COPY . .

WORKDIR /

# Test (from this directory) with:
#
#   docker build -t rudolfcardinal/r_working .
#   docker run -it rudolfcardinal/r_working /bin/bash
#
# and to push to Docker Hub:
#
#   docker login -u "myusername" -p "mypassword" docker.io
#   docker push rudolfcardinal/r_working
