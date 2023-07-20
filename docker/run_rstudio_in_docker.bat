@echo off

REM ===========================================================================
REM Constants
REM ===========================================================================

set DOCKER_PORT=8787
set HOST_PORT=8787


REM ===========================================================================
REM Ask for a password
REM ===========================================================================

set /p RSTUDIO_PASSWORD="Enter a new password for RStudio: "
set /p PASSWORD2="Re-enter the password: "
if not RSTUDIO_PASSWORD==PASSWORD2 (
    echo Password did not match. Exiting.
    exot 1
)


REM ===========================================================================
REM Set up environment variables and Docker image
REM ===========================================================================

set THIS_DIR=%~dp0
call "%THIS_DIR%\build_docker_image.bat"


REM ===========================================================================
REM Run RStudio as a web service from the image, mounting our data directory
REM ===========================================================================
REM See Bash version for help.

set SEP================================================================================
REM No quotes required: "set SEP equal to a bunch of equal signs"
echo %SEP%
echo Once launched, browse to http://127.0.0.1:%HOST_PORT%
echo - Username = rstudio
echo - Password = %RSTUDIO_PASSWORD%
echo - The host directory %DATA_DIR_HOST% will be mounted as %DATA_DIR_DOCKER%
echo Press Ctrl-C to terminate Docker when you've finished.
echo %SEP%

docker run ^
    --volume="%DATA_DIR_HOST%":"%DATA_DIR_DOCKER%":rw ^
    -it ^
    --rm ^
    -p %HOST_PORT%:%DOCKER_PORT% ^
    -e PASSWORD="%RSTUDIO_PASSWORD%" ^
    "%IMAGE%"
