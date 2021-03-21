Title: Installing and Using PIP on Synology DSM
Date: 2020-05-18
Modified: 2021-03-20
Category: Synology DSM
Cover: /extra/synology-logo.png

Python3 can be easily installed on Synology DSM through Synology Installation Center but `pip` installation is skipped. The first method to install pip is to bootstrap the pip installer into an existing Python installation. The `ensurepip` package is aimed for it. It's available starting from Python version 3.4. As all pip components are a part of Python package, the internet connection is not required to install pip. The second method installs `pip` manually and it does request internet connection.


The method #1 is based on Synology DSM 6.2.2-24922 Update 4 and Python version is 3.5.1. Python 3.5.1 `bin` folder is `/volume1/@appstore/py3k/usr/local/bin`. Python 3.5.1 `lib` is located in `/volume1/@appstore/py3k/usr/local/lib/python3.5` folder.

The method #2 is tried with Synology DSM 6.2.3-25426 Update 3 and Python version is 3.8.2. Python 3.8.2 `bin` folder is `/volume1/@appstore/py3k/usr/local/bin`. Python 3.8.2 `lib` is located in `/volume1/@appstore/py3k/usr/local/lib/python3.8` folder.

## 1. Validate Python3 installation and version.

    :::bash
    python3 -V

## 2. Install pip

Run pip installation with admin privilege. A running Synology user has to belong to the administrator group. There are 2 options to proceed: (1) run commands as `sudo` or (2) switch to root with `sudo -i`.

    :::bash
    sudo python3 -m ensurepip

If you receive the message: */usr/local/bin/python3: No module named ensurepip*, it means the `ensurepip` package is not available and you have to go with method #2.

* Download `get-pip.py` package.

        :::bash
        wget https://bootstrap.pypa.io/get-pip.py -O /tmp/get-pip.py

* Install `get-pip.py` package.

        :::bash
        sudo python3 /tmp/get-pip.py

## 3. Upgrade pip to the latest version.

Skip this step if it has been used the method #2.

You might need to add an option to limit pip installation version if your Python version is old one and reached end-of-life. For example, Python 3.5 is not supported anymore and the added option is `"pip < 21.0"`.

    :::bash
    sudo python3 -m pip install --upgrade pip

if your Python version is reached end-of-life.

    :::bash
    sudo python3 -m pip install --upgrade "pip < 21.0"

## 4. Validate pip installation and version.
    :::bash
    python3 -m pip -V

## 5. Install a package, for example, requests.
    :::bash
    sudo python3 -m pip install requests

## Resources
* [Bootstrapping the pip installer](https://docs.python.org/3/library/ensurepip.html#module-ensurepip)
