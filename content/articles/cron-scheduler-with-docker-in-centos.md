Title: Cron Scheduler with Docker Container in CentOS/RHEL 7
Date: 2020-02-18
Category: Docker, Container, Cron, Crontab, CentOS, Linux, RHEL
Cover: /extra/docker-logo.png

Using cron with the official CentOS Docker image requests activating systemd, keeping container running, and opening a Docker container in privileged mode. CentOS Docker Hub image includes the description of the setup which should be done to activate systemd and keep a container going. The missing is information how to install and set up cron. This article provides a summary of steps and a functioning sample of Dockerfile. Some customization is needed to implement  your cron project.

## Dockerfile

The Dockerfile can be used as a template to design your file. After line #15, you can add your commands to install any packages. You need to replace line #20 with your time zone. Finally, line #22 shows how to add a scheduled job to crontab file.

    :::docker
    ROM centos:7
    
    ENV container docker
        
    RUN (cd /lib/systemd/system/sysinit.target.wants/; for i in *; do [ $i == \
    systemd-tmpfiles-setup.service ] || rm -f $i; done); \
    rm -f /lib/systemd/system/multi-user.target.wants/*;\
    rm -f /etc/systemd/system/*.wants/*;\
    rm -f /lib/systemd/system/local-fs.target.wants/*; \
    rm -f /lib/systemd/system/sockets.target.wants/*udev*; \
    rm -f /lib/systemd/system/sockets.target.wants/*initctl*; \
    rm -f /lib/systemd/system/basic.target.wants/*;\
    rm -f /lib/systemd/system/anaconda.target.wants/*;
    
    VOLUME [ "/sys/fs/cgroup" ]
    
    RUN yum install -y cronie && yum clean all
    
    RUN rm -rf /etc/localtime
    RUN ln -s /usr/share/zoneinfo/America/Toronto /etc/localtime
    
    RUN crontab -l | { cat; echo "25 04 * * sun,mon,tue python3 /app/do_maintenance.py"; } | crontab -
    
    CMD ["/usr/sbin/init"]

### Dockerfile logical parts

* Activating systemd.

        :::docker
        RUN (cd /lib/systemd/system/sysinit.target.wants/; for i in *; do [ $i == \
        systemd-tmpfiles-setup.service ] || rm -f $i; done); \
        rm -f /lib/systemd/system/multi-user.target.wants/*;\
        rm -f /etc/systemd/system/*.wants/*;\
        rm -f /lib/systemd/system/local-fs.target.wants/*; \
        rm -f /lib/systemd/system/sockets.target.wants/*udev*; \
        rm -f /lib/systemd/system/sockets.target.wants/*initctl*; \
        rm -f /lib/systemd/system/basic.target.wants/*;\
        rm -f /lib/systemd/system/anaconda.target.wants/*;
        
        VOLUME [ "/sys/fs/cgroup" ]

* Install cron.

        ::docker
        RUN yum install -y cronie && yum clean all

* Set up your time zone.

        ::docker
        RUN rm -rf /etc/localtime
        RUN ln -s /usr/share/zoneinfo/America/Toronto /etc/localtime

* Add a job in crontab file.

        ::docker
        RUN crontab -l | { cat; echo "25 04 * * sun,mon,tue python3 /app/do_maintenance.py"; } | crontab -

* Keep container running.

        ::docker
        CMD ["/usr/sbin/init"]

## Build conatiner

The image name is c7-cron and it's designated as local one.

    :::bash
    docker build --rm -t local/c7-cron .

## Run container
 
To create and start a container, use the command.

    :::bash
    docker run --privileged --name=parking –v /sys/fs/cgroup:/sys/fs/cgroup:ro -d local/c7-cron

To access the container in a terminal, run the command.

    :::bash
    docker exec -it parking /bin/bash
