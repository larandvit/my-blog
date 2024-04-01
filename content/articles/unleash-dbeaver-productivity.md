Title: Unleash DBeaver Productivity
Date: 2024-03-31
Category: DBeaver
Cover: /extra/dbeaver-logo.png

[DBeaver Community](https://dbeaver.io/) open-source version is lack of some features which available in Eclipse Integrated Environment (IDE). Those features improve productivity of database tool adding extra functionality. Variety of Eclipse plug-ins make IDE powerful and attractive for users. Also, some features might not work as expected in DBeaver. The article describes how to transform open-source tool into professional one suitable to solve many challenging tasks and simplify setup and usage of the tool.

The sample is based on DBeaver version 24.0.1 in CentOS 7.

## Available Plug-ins

DBeaver is supplied with a list of plug-in installations tested with the current version of the tool. To see those installations, run **Install New Software...** menu item in **Help** menu. You can install any plug-ins what you are interested in.

![DBeaver Install New Software]({static}/images/unleash-dbeaver-productivity/dbeaver-install-new-software.png)</br></br>

## Eclipse Marketplace

This is the firts plug-in to start with. It will simplify installation and management of plug-ins. Select **Eclipse 2023-12 - https://download.eclipse.org/releases/2023-12/** in **Work with** and filter by **marketplace**.

![Eclipse Marketplace]({static}/images/unleash-dbeaver-productivity/eclipse-marketplace-plugin.png)</br></br>

After installation, **Help** menu contains **Eclipse Marketplace...** menu item.

![DBeaver Eclipse Marketplace]({static}/images/unleash-dbeaver-productivity/dbeaver-eclipse-marketplace.png)</br></br>

## PyDev Python Plug-in

Eclipse Marketplace allows easy search and installation of plug-ins. For example, PyDev is a plug-in to do Python development. Open **Eclipse Marketplace** and type in **python** in **Find** field. It will bring a list of plug-ins related to Python. Select PyDev to install it.

![PyDev Python Plug-in]({static}/images/unleash-dbeaver-productivity/pydev-python-plugin.png)</br></br>

To start working with the plug-in, add PyDev perspective to DBeaver. Click **Open Perspective** button in right top corner and select PyDev.

![DBeaver Open PyDev Perspective]({static}/images/unleash-dbeaver-productivity/dbeaver-open-pydev-perspective.png)</br></br>

## Install DBeaver Supplied Development Tools

If you are looking for development tools, you can use DBeaver supplied ones. Those development tools are part of Eclipse 2023-12 version. To see those installations, run **Install New Software...** menu item in **Help** menu, then select **Eclipse 2023-12 - https://download.eclipse.org/releases/2023-12/** in **Work with**. Scroll to **Programming Languages**.

![Eclipse Programming Languages]({static}/images/unleash-dbeaver-productivity/eclipse-programming-languages.png)</br></br>

## Install Source Controls

DBeaver can use both git and SVN source controls. To install git, you can use both **Install New Software** and **Eclipse Marketplace**. SVN can be only installed from **Eclipse Marketplace**.

After installation, it is better to use **Resources** perspective rather than **DBeaver** one as DBeaver does not work correctly with source controls.

![DBeaver Open Resources Perspective]({static}/images/unleash-dbeaver-productivity/dbeaver-open-resources-perspective.png)</br></br>

SVN plug-in might not work properly with DBeaver. If you receive **File is out of date** error, follow instructions in [DBeaver/Eclipse "File is out of date" SVN Error]({filename}/articles/dbeaver_eclipse_file_is_out_of_date_svn_error.md) article.

## Resources

* [Eclipse Marketplace](https://marketplace.eclipse.org/)
* [DBeaver Optional extensions](https://dbeaver.com/docs/dbeaver/Optional-extensions/)
