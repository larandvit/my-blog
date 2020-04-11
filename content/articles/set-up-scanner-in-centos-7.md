Title: Set Up Scanner in CentOS 7
Date: 2020-01-16
Category: Linux
Cover: /extra/centos-logo.png

As a scanner software can be used [Paperwork](https://openpaper.work/). It's open source software which is available in both Windows and Unix. CentOS 7 includes Paperwork scanner software as a part of the distribution repository.

Make sure that your scanner driver installed in CentOS. This is a sample of Brother printer/scanner driver installation instructions from the official website.

![Install DCP-7065DN scanner driver]({static}/images/set-up-scanner-in-centos7/install-dcp-7065dn-scanner-driver.png)</br></br>

Install Scanner Access Now Easy (SANE) application programming interface that provides standardized access to any raster image scanner hardware.

```bash
sudo yum install sane-backends
```
</br>

Enable SANE connection required for scanning.

```bash
sudo sh -c "echo 127.0.0.1 >> /etc/sane.d/saned.conf"
```
</br>

Set up SANE service to start automatically and run the service.

```bash
sudo systemctl enable saned.socket
sudo systemctl start saned.socket
```
</br>

Open **Application Installer** from **System Tools** menu and search for **paperwork** in search bar.

![Search for paperwork installation]({static}/images/set-up-scanner-in-centos7/search-paperwork-installation.png)</br></br>

Install Paperwork.

![Paperwork installation]({static}/images/set-up-scanner-in-centos7/paperwork-installation.png)</br></br>

After completion it will be available in **Office** menu.

![Paperwork menu item in Office menu]({static}/images/set-up-scanner-in-centos7/paperwork-in-office-menu.png)</br></br>

Run Paperwork and open **Setting** from menu

![Paperwork Setting menu item]({static}/images/set-up-scanner-in-centos7/paperwork-settings-menu.png)</br></br>

Validate that **Device**, **Default source**, and **Resolution** populated properly as per your scanner.

![Paperwork installed properly]({static}/images/set-up-scanner-in-centos7/peparwork-installed-propertly.png)</br></br>


