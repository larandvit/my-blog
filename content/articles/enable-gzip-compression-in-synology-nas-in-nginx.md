Title: Enable gzip Compression in Synology NAS in Nginx
Date: 2020-03-21
Category: Synology NAS, gzip, Synology DSM, Nginx

Enabling gzip compression for your website can be done in Synology DiskStation Manager (DSM). HTTP traffic is already compressed with gzip as default. HTTPS protocol needs to be enabled explicitly. DSM manager is doing it in an easy step without rebooting Synology NAS. Also, DSM manager includes text/javascript and text/css MIME types additional to text/html one.

## 1. Validate Compression

### 1.1 Firefox Browses

* Open your website in Firefox browser 

* Click **Shift-F12** to open **Developer Tools** panels.

    ![Firefox Developer Tools]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-developer-tools-panels.png)</br></br>

* Go to **Network** panel

    ![Firefox Developer Tools Network Panel]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-network-panel.png)</br></br>

* Click **Reload** button in the panel or **Ctrl-F5** buttons to initiate your website data load.

    ![Firefox Developer Tools Network Panel Loaded Data]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-website-loaded-data.png)</br></br>

* Filter resources which you are interested in. Those resources are: **HTML**, **CSS**, and **JS**, for example, **HTML** website root page.

    ![Firefox Developer Tools Network Panel Filtered Recources]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-network-panel-filter-html.png)</br></br>

* Click on a resource to see compression and other resource properties, for example, **HTML** resource.
    
    Clear cache.

    ![Firefox Developer Tools Network Panel Compression Property]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-clear-cache.png)</br></br>

    Gzip compression is set up.

    ![Firefox Developer Tools Network Panel Compression Property]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/firefox-html-resource-compression.png)</br></br>

### 1.2 Chrome Browser

The steps to get information are very similar to Firefox. To open Chrome DevTools, press **Command+Option+C** (Mac) or **Ctrl+Shift+C** (Windows, Linux, Chrome OS).

![Chrome DevTools Network Panel Compression Property]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/chrome-html-resource-compression.png)</br></br>

## 2. Enable gzip Compression in DSM

* Open **Control Panel** in Synology DiskStation Manager.

    ![Synology NAS DSM Control Panel]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/synology-diskstation-control-panel.png)</br></br>

* Go to **Security** settings.

    ![Synology NAS DSM Control Panel Security Settings]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/synology-diskstation-security.png)</br></br>

* Select **Advanced** tab and flag **Enable HTTP Compression**.

    ![Synology NAS DSM Control Panel Security Settings Advanced Tab]({static}/images/enable-gzip-compression-in-synology-nas-in-nginx/synology-diskstation-security-enable-http-compression.png)</br></br>

* Apply the setting pressing **Save** button.

## 3. Include Different MIME Types

Compression is applied to HTML MIME type as default. Also, compression is applied to CSS and JS in Synology DSM. If compression is missing for CSS and JS resources, it can be set up.

The sample is based on DSM 6.2.2 operation system.

* Use ssh client to access your Synology NAS with a user which has administrative permission. It can be PuTTY tool in Windows or terminal with `ssh` command in Unix. 

* Switch to root user

        :::bash
        sudo su -

* Back up the current moustache template

        :::bash
        cp /usr/syno/share/nginx/WWWService.mustache /usr/syno/share/nginx/WWWService.mustache.bak

* Open the moustache template for editing

        :::bash
        vi /usr/syno/share/nginx/WWWService.mustache

* Add a line after `gzip on;` command to both HTTP and HTTPS server sections

        :::text
        gzip_types text/javascript text/css;


    The final content should be

        :::text
        server {
            listen 80 default_server{{#reuseport}} reuseport{{/reuseport}};
            listen [::]:80 default_server{{#reuseport}} reuseport{{/reuseport}};

            gzip on;
            gzip_types text/javascript text/css;

            {{> /usr/syno/share/nginx/WWW_Main}}

            location ~ ^/$ {
                rewrite / http://$host:{{DSM.port}}/ redirect;
            }
        }

        server {
            listen 443 default_server ssl{{#reuseport}} reuseport{{/reuseport}};
            listen [::]:443 default_server ssl{{#reuseport}} reuseport{{/reuseport}};
            {{#DSM.https.compression}}
            gzip on;
            gzip_types text/javascript text/css;
            {{/DSM.https.compression}}

            {{> /usr/syno/share/nginx/WWW_Main}}

            location ~ ^/$ {
                rewrite / https://$host:{{DSM.ssl.port}}/ redirect;
            }
        }

* Restart the Nginx web server to apply the changes

        :::bash
        synoservicecfg --restart nginx
