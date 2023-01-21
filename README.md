# Asteroids
## Description
An Asteroids pseudo-clone made in Haskell.

## Installation

### Windows
* Download and install the [Haskell Stack](https://www.haskell.org/downloads/#stack).
* Open up a command prompt and run `stack build --file-watch --exec=asteroids-exe` in the root directory of the repository.

### Linux
From a clean Ubuntu install:

* To install curl, run `sudo apt install curl` if necessary.
* Install the Haskell Stack with `curl -sSL https://get.haskellstack.org/ | sh`.
* Run `sudo apt-get install freeglut3-dev libxrandr-dev libxinerama-dev libxcursor-dev libxxf86vm-dev libxi-dev` to install necessary libraries.
* In the root directory of the repository, run `stack build --exec=asteroids-exe` to build and launch the program.

Some of these may be omitted if the corresponding libraries are already installed.

## Screen capture
![Demo](demo.gif)

(The above gif can be choppy depending on the browser used. [Here](https://www.youtube.com/watch?v=2_-TJjFk4VI) is a YouTube video.)

## Disclaimer
Run at your own risk, program is fair-use etc.