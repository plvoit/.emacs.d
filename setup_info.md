Config File for Emacs on local PC

To run it on cluster:
The WORKON variable in init.el needs to be changed to the location of the
conda environments

If wanted, change the inital folder location on startup

for elpy to work it might be necessary to install virtualenv either by
pip install virtualenv

or sudo apt-install virtualenv

Choosing an conda environment with pyvenv-workon apparently has to to happen before opening the
console (starting the python process)