Title: Install Docker CE Edition in CentOS/RHEL 7
Date: 2020-10-18
Category: Docker, Linux
Cover: /extra/docker-logo.png

Installing Docker CE in CentOS/RHEL 7 is a trivial process but it is not true in all cases. Specially, when we deal with open source products, maintaining and patching might be an issue for doing it in timely manner. Community forums and other sources are our life savers to find workarounds for any encountering roadblocks.


##1. Remove old version

    :::bash
    yum remove docker \
               docker-client \
               docker-client-latest \
               docker-common \
               docker-latest \
               docker-latest-logrotate \
               docker-logrotate \
               docker-engine

##2. Set up Docker repository

* Install package manager.

        :::bash
        yum install -y yum-utils

* Add the Docker repository to the repository database.

        :::bash
        yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo

    If an error has been received, it has to be applied a workaround. As per version 7.9 on Oct 6, 2020, the repository path is broken and the path needs to be adjusted manually with the command.

        :::bash
        yum-config-manager --setopt="docker-ce-stable.baseurl=https://download.docker.com/linux/centos/7/x86_64/stable" --save

##3. Install the latest version of Docker CE

    :::bash
    yum install docker-ce docker-ce-cli containerd.io

##4. Enable and run docker daemon

    :::bash
    systemctl enable docker
    systemctl start docker

##5. Validate installation

* Docker service status

        :::bash
        systemctl status docker

    Output

        :::text
        ● docker.service - Docker Application Container Engine
           Loaded: loaded (/usr/lib/systemd/system/docker.service; enabled; vendor preset: disabled)
           Active: active (running) since Tue 2020-10-06 10:08:03 EDT; 1 weeks 5 days ago

* Run the sample image

        :::bash
        docker run -it centos echo Hello-World

    Output

        :::text
        Hello-World
       
## Resources

* [Install Docker Engine on CentOS](https://docs.docker.com/engine/install/centos/)
* [Docker CE Stable - x86_64 Repo not available : HTTPS Error 404 - Not Found](https://forums.docker.com/t/docker-ce-stable-x86-64-repo-not-available-https-error-404-not-found-https-download-docker-com-linux-centos-7server-x86-64-stable-repodata-repomd-xml/98965/4)
* [CentOS 7/RHEL 7 installations broken where $releasever is '7Server'](https://github.com/docker/for-linux/issues/1111)


