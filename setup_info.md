Config File for Emacs on local PC

When there is a problem finding and installing packages, there is usually the
message that authentification with a key did not work. Then the keyring
has to be updated via:
gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com --receive-keys 645357D2883A0966
where the last number is the number of the key you see in the message

To run it on cluster:
The WORKON variable in init.el needs to be changed to the location of the
conda environments

If wanted, change the inital folder location on startup


#For Pyenv, if needed
or sudo apt-install virtualenv

or this:
sudo apt install python3-venv


Choosing an conda environment with pyvenv-workon apparently has to to happen before opening the
console (starting the python process)

