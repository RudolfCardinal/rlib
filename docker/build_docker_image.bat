@echo off

REM Build Docker image for R/Stan, with a consistent name (tag).
REM Also set other convenient environment variables.
REM
REM Rudolf Cardinal, 6 July 2023 onwards.


REM ===========================================================================
REM Constants and paths used elsewhere
REM ===========================================================================

set IMAGE=rudolfcardinal/r_default
set DATA_DIR_HOST=%CD%
set DATA_DIR_DOCKER=/data


REM ===========================================================================
REM Constants and paths used locally
REM ===========================================================================

set THIS_SCRIPT_DIR=%~dp0
set DOCKERFILE=%THIS_SCRIPT_DIR%\Dockerfile
set CONTEXT=%THIS_SCRIPT_DIR%\..


REM ===========================================================================
REM Build an image from the Docker file.
REM ===========================================================================
REM See Bash version for help.

docker build \
    --file "%DOCKERFILE%" \
    --tag "%IMAGE%" \
    "%CONTEXT%"
