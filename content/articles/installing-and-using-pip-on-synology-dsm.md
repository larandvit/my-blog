Title: Installing and Using PIP on Synology DSM
Date: 2020-05-18
Category: Synology DSM
Cover: /extra/synology-logo.png

Python3 can be easily installed on Synology DSM through Synology Installation Center but `pip` installation is skipped. One of the methods to install pip is to bootstrap the pip installer into an existing Python installation. The `ensurepip` package is aimed for it. It's available starting from Python version 3.4. As all pip components are a part of Python package, the internet connection is not required to install pip.

The article is based on Synology DSM 6.2.2-24922 Update 4 and Python version is 3.5.1.

Python 3.5.1 `bin` folder is `/volume1/@appstore/py3k/usr/local/bin`.

Python 3.5.1 `lib` is located in `/volume1/@appstore/py3k/usr/local/lib/python3.5` folder.

## 1. Validate Python3 installation and version.

    :::bash
    python3 -V

## 2. Install pip

Run pip installation with admin privilege. A running Synology user has to belong to the administrator group. There are 2 options to proceed: (1) run commands as `sudo` or (2) switch to root with `sudo -i`.

    :::bash
    sudo python3 -m ensurepip

## 3. Upgrade pip to the latest version.

    :::bash
    sudo python3 -m pip install --upgrade pip

## 4. Validate pip installation and version.
    :::bash
    python3 -m pip -V

## 5. Install a package, for example, requests.
    :::bash
    sudo python3 -m pip install requests

## Resources
* [Bootstrapping the pip installer](https://docs.python.org/3/library/ensurepip.html#module-ensurepip)
