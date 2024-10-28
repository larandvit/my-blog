Title: Install Self-hosted Prefect Server
Date: 2024-10-27
Category: Prefect, Nginx
Cover: /extra/prefect-logo.png

[Prefect](https://www.prefect.io/) product can be deployed as a cloud or a core version. The difference between those versions is in Prefect server. In case of cloud which is paid version, server is located in cloud and it is ready to be used. On the contrary, core version is open source one and Prefect server is self-hosted. Python SDK client communicates with Prefect self-hosted server. Users are responsible for set up and configuration of the Prefect server.

As Prefect self-hosted server is configured to be local one, to access it from outside, it requests a Web server with reverse proxy and enabled Web socket.

The sample is based on Prefect server 3.0.10 installed in Ubuntu 22.04.5 LTS.


##1. Install Nginx Web server

Before installation, update the list of available packages and their versions stored in the system's package index.

    :::bash
    sudo apt update
    sudo apt install nginx

##2. Validate Nginx

After installation, start the Web server and open a landing page.

    :::bash
    sudo service nginx restart

Open browser and type your server URL, for example, `example.com`. The output is below.

    :::text
    Welcome to nginx!
    If you see this page, the nginx web server is successfully installed and working. Further configuration is required.

    For online documentation and support please refer to nginx.org.
    Commercial support is available at nginx.com.

    Thank you for using nginx.

##3. Configure reverse proxy and enable Web socket

Unlink default configuration.

    :::bash
    sudo unlink /etc/nginx/sites-enabled/default

Create a new configuration file.

    :::bash
    nano /etc/nginx/sites-available/reverse-proxy.conf

Copy the setup

    :::text
    server {
        listen 8080;
        server_name example.com;

        location / {
            proxy_pass http://127.0.0.1:4200;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
    }

Promote the new configuration.

    :::bash
    sudo ln -s /etc/nginx/sites-available/reverse-proxy.conf /etc/nginx/sites-enabled/reverse-proxy.conf

Test new configuration

    :::bash
    sudo service nginx configtest

Web server setup is ready and the server can be started.

    :::bash
    sudo service nginx restart

##4. Install SQLite

Validate if SQLite already installed.

    :::bash
    sqlite3 --version

If not installed.

    :::bash
    sudo apt install sqlite3

Validate SQLite installation.

    :::bash
    sqlite3 --version

##5. Install Python virtual environment

It is isolated Python environment to run Prefect. The folder is `.venv` in the current folder.

    :::bash
    python3 -m venv .venv

Activate virtual environment. It has to be done each time when hosting Prefect machine is restarted.

    :::bash
    source .venv/bin/activate

##6. Install Prefect server

Upgrade pip version.

    :::bash
    python3 -m pip install --upgrade pip

Prefect installation.

    :::bash
    pip install -U prefect

Validate Prefect.

    :::bash
    prefect version

##7. Prefect server configuration

SQLite setup. Define PREFECT_HOME environment variable or replace `${PREFECT_HOME}` with real folder.

    :::bash
    prefect config set PREFECT_API_DATABASE_CONNECTION_URL='sqlite+aiosqlite:///${PREFECT_HOME}/prefect.db'
    prefect config set PREFECT_API_DATABASE_ECHO='False'
    prefect config set PREFECT_API_DATABASE_MIGRATE_ON_START='True'
    prefect config set PREFECT_API_DATABASE_PASSWORD='None'

Prefect API

    :::bash
    prefect config set PREFECT_API_URL=http://example.com:8080/api

Validate setup

    :::bash
    prefect profile inspect


##8. Start Prefect server

    :::bash
    prefect server start

##9. Access to Prefect UI from another computer

Type in URL in your Internet browser: http://example.com:8080/.

## Resources
* [Host Prefect server](https://docs-3.prefect.io/3.0/manage/self-host)
* [How to Configure an NGINX Reverse Proxy](https://www.hostinger.com/tutorials/how-to-set-up-nginx-reverse-proxy/)
* [How to Configure NGINX to Proxy WebSockets](https://vsys.host/how-to/how-to-configure-nginx-to-proxy-websockets)
