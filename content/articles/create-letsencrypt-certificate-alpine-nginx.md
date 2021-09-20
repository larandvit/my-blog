Title: Create Let's Encrypt Certificate in Alpine for Nginx
Date: 2021-09-20
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

## Resources

* [Nginx on Other Linux (pip)](https://certbot.eff.org/lets-encrypt/otherpip-nginx)
* [Issue using certbot with nginx](https://stackoverflow.com/questions/53223914/issue-using-certbot-with-nginx)
