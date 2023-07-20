@echo off

REM Run command-line R within our Docker container.
REM See Bash version for help.

REM Build Docker image (if necessary) and set environment variables:
set THIS_DIR=%~dp0
call "%THIS_DIR%\build_docker_image.bat"

REM Run R:
docker run ^
    --volume="%DATA_DIR_HOST%":"%DATA_DIR_DOCKER%":rw ^
    -it ^
    --rm ^
    "%IMAGE%" ^
    R
