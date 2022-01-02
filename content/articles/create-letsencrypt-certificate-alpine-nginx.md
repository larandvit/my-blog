Title: Create Let's Encrypt Certificate in Alpine for Nginx
Date: 2021-09-20
Modified: 2022-01-01
Category: Linux, Nginx, Security
Cover: /extra/alpine-logo.png

Securing Web sites with HTTPS protocol gives many advantages along with increasing rankings in search engine optimization (SEO). There are many Certificate Authority which provides SSL/TLS certificates. Some of them are free. [Let's Encrypt](https://letsencrypt.org/) is a reputable non-profit certificate authority providing certificates at no charge. Let's Encrypt is supported by [Certbot](https://certbot.eff.org/) software making a certificate creation in easy steps.

The sample is based on Alpine version 3.14.2 with Python 3.9.5 installed. Root user is used to run all commands below.

## 1. Install Python3 and Pip

Python is needed to run Certbot and install NGINX plugin.

    :::bash
    apk add --update python3 py3-pip

## 2. Install Certbot

    :::bash
    apk add certbot

## 3. Install NGINX plugin

    :::bash
    pip install certbot-nginx

## 4. Generate certificate

    :::bash
    certbot --nginx

   Follow instructions to create a new certificate.

## 5. Renew certificate interactively

    :::bash
    certbot renew

## 6. Renew certificate automatically

  * Validate if crontab service is running.

        :::bash
        rc-service --list | grep -i crond

    Output.

        :::text
        crond

  * If not running, run and enable crontab service.

        :::bash
        rc-service crond start && rc-update add crond

  * Create script to automatically renew certificate.

    The location is **/etc/periodic/daily/renew_letsencrypt**.

        :::bash
        #!/bin/sh

        python3 -c 'import random; import time; time.sleep(random.random() * 3600)' && sudo certbot renew -q

    The script runs the certificate renewal every day between 2:00am - 3:00am. The certificate will be renewed in case if it is left less than 30 days until expiration date.

  * Make the script executable.

        :::bash
        chmod a+x /etc/periodic/daily/renew_letsencrypt

  * Validate the script.

        :::bash
        run-parts --test /etc/periodic/daily

    Output.

        :::text
        /etc/periodic/daily/renew_letsencrypt

### 7. Monthly Upgrade certbot 

It is important to keep Certbot software up-to-date.

  * Create a script with **/etc/periodic/monthly/upgrade_certbot** name.

        :::bash
        #!/bin/sh

        pip3 install --upgrade certbot-nginx

  * Make the script executable.

        :::bash
        chmod a+x /etc/periodic/monthly/upgrade_certbot

  * Validate the script.

        :::bash
        run-parts --test /etc/periodic/monthly

    Output.

        :::text
        /etc/periodic/monthly/upgrade_certbot

## Resources

* [Nginx on Other Linux (pip)](https://certbot.eff.org/instructions?ws=nginx&os=pip)
* [Issue using certbot with nginx](https://stackoverflow.com/questions/53223914/issue-using-certbot-with-nginx)
* [crontab guru](https://crontab.guru)
