#!/usr/bin/env python3

"""
Run commands (or the RStudio web interface) in our Docker environment for R.

Rudolf Cardinal, 6 July 2023 onwards.
"""

# =============================================================================
# Imports
# =============================================================================
# Use only the standard library.

import argparse
from dataclasses import dataclass
from getpass import getpass
import logging
from os import getcwd, pardir
from os.path import abspath, commonpath, dirname, exists, join, relpath
import subprocess
from typing import Dict, List, Union

log = logging.getLogger(__name__)


# =============================================================================
# Constants
# =============================================================================

IMAGE = "rudolfcardinal/r_default:r4.3.1-stan2.26.21-1"
# Not possible to set the tag in the Dockerfile.

DOCKER_RSTUDIO_PORT = 8787  # default TCP/IP port for RStudio (do not change)
DEFAULT_HOST_RSTUDIO_PORT = 8787
SEP = "=" * 79
DOCKER_CMD = "docker"


# =============================================================================
# Paths
# =============================================================================

THIS_SCRIPT_DIR = dirname(abspath(__file__))
DOCKERFILE = join(THIS_SCRIPT_DIR, "Dockerfile")
CONTEXT = abspath(join(THIS_SCRIPT_DIR, pardir))

DEFAULT_DOCKER_DATA_DIR = "/data"


# =============================================================================
# Helper functions
# =============================================================================


@dataclass
class VolumeMount:
    host_dir: str
    docker_dir: str
    rw: bool = False  # read/write, not read only?

    def switch(self) -> str:
        assert (
            ":" not in self.host_dir
        ), f"Host directory should not contain ':' but is {self.host_dir}"
        assert (
            ":" not in self.docker_dir
        ), f"Docker directory should not contain ':' but is {self.docker_dir}"
        assert self.docker_dir.startswith(
            "/"
        ), f"Docker directory should start with '/' but is {self.docker_dir}"
        flag = "rw" if self.rw else "ro"
        return f"--volume={self.host_dir}:{self.docker_dir}:{flag}"

    def description(self) -> str:
        flag = "read/write" if self.rw else "read only"
        return (
            f"Mounting host directory {self.host_dir!r} as "
            f"{self.docker_dir!r} inside Docker ({flag})"
        )


def announce(lines: Union[str, List[str]]) -> None:
    """
    Write something obviously to the screen.
    """
    if isinstance(lines, str):
        lines = [lines]
    print(SEP)
    for line in lines:
        print(line)
    print(SEP)


def run(cmdargs: List[str]) -> None:
    """
    Run a command.
    """
    log.debug(cmdargs)
    subprocess.check_call(cmdargs)


# =============================================================================
# Build Docker container
# =============================================================================


def docker_build() -> None:
    """
    Build our Docker environment (if needed).
    """
    announce("Ensuring Docker container is built.")
    cmdargs = [
        DOCKER_CMD,
        "build",
        # Build a Docker image if necessary. It's slow the first time but then
        # very quick thereafter (it won't rebuild unless the Dockerfile
        # changes).
        "--file",
        DOCKERFILE,
        # Specifies a named Docker file. It's optional because our Docker file
        # is the default of "Dockerfile", but never mind.
        "--tag",
        IMAGE,
        # Give the image this image (or optionally, name:tag).
        CONTEXT
        # The context is the top-level directory used for building the Docker
        # image. All files used by COPY must be within the context.
    ]
    run(cmdargs)


def docker_run(
    cmd: Union[str, List[str]] = None,
    interactive: bool = True,
    rm: bool = True,
    mounts: List[VolumeMount] = None,
    envvars: Dict[str, str] = None,
    ports_docker_to_host: Dict[int, int] = None,
) -> None:
    """
    Run a command in the Docker environment.
    """
    cmd = cmd or []
    if isinstance(cmd, str):
        cmd = [cmd]
    mounts = mounts or []
    envvars = envvars or {}
    ports_docker_to_host = ports_docker_to_host or {}

    cmdargs = [
        DOCKER_CMD,
        "run"
        # Run a command.
    ]
    for mount in mounts:
        cmdargs.append(mount.switch())
        log.info(mount.description())
    for var, value in envvars.items():
        # Set an environment variable.
        cmdargs += ["-e", f"{var}={value}"]
    for docker_port, host_port in ports_docker_to_host.items():
        # Publish container's DOCKER_PORT so it can be seen on the host via
        # HOST_PORT. Synonym is "--publish".
        cmdargs += ["-p", f"{host_port}:{docker_port}"]
    if interactive:
        # Interact with user
        cmdargs.append("-it")  # or "--interactive", "--tty"
    if rm:
        # Remove container afterwards (stops hard disk clogging up).
        cmdargs.append("--rm")
    cmdargs.append(IMAGE)  # Image to run with
    cmdargs += cmd
    # If the command is missing, the image's default command is run.
    run(cmdargs)


# =============================================================================
# Command-line entry point
# =============================================================================


def main() -> None:
    """
    Command-line entry point.
    """
    # -------------------------------------------------------------------------
    # Arguments
    # -------------------------------------------------------------------------
    cmd_bash = "bash"
    cmd_r = "R"
    cmd_rscript = "Rscript"
    cmd_rstudio = "rstudio"

    logging.basicConfig(level=logging.DEBUG)
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=f"""Quick launcher for our Docker R container.

Commands:

- {cmd_bash}
    Launch a bash shell within the Docker container, interactively.

- {cmd_r}
    Launch R within the Docker container, interactively.

- {cmd_rscript} SCRIPT [ARGS...]
    Launch an R script (via the Rscript tool), with optional arguments.

- {cmd_rstudio}
    Launch RStudio as a web service.
""",
    )
    parser.add_argument(
        "command", choices=[cmd_bash, cmd_r, cmd_rscript, cmd_rstudio]
    )
    parser.add_argument(
        "--hostdata",
        help="Host data directory to be mounted. Default is the current "
        "working directory.",
        default=abspath(getcwd()),
    )
    parser.add_argument(
        "--dockerdata",
        help=f"Data directory as seen from Docker. "
        f"Default is {DEFAULT_DOCKER_DATA_DIR!r}.",
        default=DEFAULT_DOCKER_DATA_DIR,
    )
    parser.add_argument(
        "--rw",
        dest="rw",
        help="Make data directory read/write within Docker (the default).",
        action="store_true",
        default=True,
    )
    parser.add_argument(
        "--ro",
        dest="rw",
        help="Make data directory read-only within Docker.",
        action="store_false",
    )
    parser.add_argument(
        "--port",
        help=f"RStudio TCP/IP port number on the host. "
        f"Default: {DEFAULT_HOST_RSTUDIO_PORT}.",
        type=int,
        default=DEFAULT_HOST_RSTUDIO_PORT,
    )
    parser.add_argument(
        "--password",
        help="Password for RStudio (username is 'rstudio'). "
        "If not specified, will be prompted for.",
    )
    parser.add_argument(
        "scriptargs",
        nargs="*",
        help=f"Script name (and any other arguments) for {cmd_rscript} "
        f"command.",
    )
    args = parser.parse_args()

    # -------------------------------------------------------------------------
    # Ensure Docker container is built, and execute command.
    # -------------------------------------------------------------------------

    docker_build()

    datamount = VolumeMount(
        host_dir=abspath(args.hostdata),
        docker_dir=args.dockerdata,
        rw=args.rw,
    )
    mounts = [datamount]

    if args.command == cmd_bash:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Bash
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        announce("Running Bash shell.")
        docker_run("bash", mounts=mounts)
    elif args.command == cmd_r:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # R
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        announce("Starting R inside Docker container.")
        docker_run(
            ["bash", "-c", f"cd {args.dockerdata!r} && R"], mounts=mounts
        )
    elif args.command == cmd_rscript:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # RScript
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if not args.scriptargs:
            raise ValueError("No script specified")
        host_script = abspath(args.scriptargs[0])
        otherargs = args.scriptargs[1:]
        if not exists(host_script):
            raise ValueError(f"No such script: {host_script!r}")
        if commonpath([datamount.host_dir, host_script]) != datamount.host_dir:
            raise ValueError(
                f"Script {host_script!r} is not within host data directory "
                f"{datamount.host_dir!r}"
            )
        docker_script = join(
            datamount.docker_dir, relpath(host_script, datamount.host_dir)
        )
        textargs = " ".join([repr(docker_script)] + otherargs)
        announce(
            [
                f"Running R script: {host_script}",
                "Arguments: " + " ".join(otherargs),
            ]
        )
        docker_run(
            ["bash", "-c", f"cd {args.dockerdata!r} && Rscript " + textargs],
            mounts=mounts,
        )
    elif args.command == cmd_rstudio:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # RStudio
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        pw = args.password
        if not pw:
            done = False
            while not done:
                pw = getpass("Enter a password for RStudio: ")
                if not pw:
                    print("- Please enter a non-blank password")
                else:
                    pw2 = getpass("Re-enter the password: ")
                    if pw2 != pw:
                        print("- Password mismatch.")
                    else:
                        done = True
        announce(
            [
                f"Once launched, browse to http://127.0.0.1:{args.port}",
                "- Username = rstudio",
                f"- Password = {pw}",
                "Press Ctrl-C to terminate Docker when you've finished.",
            ]
        )
        docker_run(
            mounts=mounts,
            envvars=dict(PASSWORD=pw),
            ports_docker_to_host={DOCKER_RSTUDIO_PORT: args.port},
        )
        # No command: the rocker/verse image runs RStudio as its default
        # command.
    else:
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Anything else is a bug.
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        raise RuntimeError("Unknown command")


if __name__ == "__main__":
    main()
