Title: Hubitat Sensor Logging Visualization in Synology NAS
Date: 2022-11-10
Category: Hubitat, Synology DSM
Cover: /extra/hubitat-logo.png

Sensor logging visualization is not available in Hubitat as the standard functionality. There are some ways to do it. The easiest implementation of the functionality is based on InfluxDB with Grafana. Those products are open-source and they are aimed to build logging visualization. The linking piece to Hubitat is InfluxDB Logger application for Hubitat. Also, it is required a box to install and run InfluxDB and Grafana. If Synology NAS with decent configuration is in place, it might be used to install and run InfluxDB and Grafana. Synology NAS Docker handles InfluxDB and Grafana perfectly for the case. The design is outlined and it is time to follow the steps to accomplish the task of implementation of the functionality.

##1. Install Docker in Synology NAS from Package Center

![Install Docker in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/install-docker-in-synology-nas.jpg)</br></br>

##2. Create Subfolder with **grafana** Name in **docker** Folder

![Create grafana Subfolder in docker Folder in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/create-grafana-subfolder-in-docker-folder.jpg)</br></br>

##3. Grant Access for Grafana User to New **grafana** Folder

Grafana image contains a user with 472 user ID. To make the grafana folder persistent, user with 472 ID must be an owner of the folder.

  * Enable SSH service

    ![Enable SSH Service in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/enable-ssh-in-synology-nas.jpg)</br></br>

  * If needed, set security level to Low in Advanced Settings

    ![Set Security to Low in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/set-security-to-low-in-synology-nas.jpg)</br></br>

  * Open ssh tool, for example, PuTTY, connect to Synology NAS, and run the command

    ![PuTTY]({static}/images/hubitat-sensor-logging-visualization-synology-nas/putty.jpg)</br></br>

        :::bash
        sudo chown -R 472:472 /volume1/docker/grafana

    Validate

        :::bash
        ls -l /volume1/docker/
        drwxrwxrwx+ 1  472  472 0 Oct 29 21:54 grafana

##4. Download Latest Grafana Image in Docker

![Download Latest Grafana Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/download-latest-grafana-image-in-synology-nas.jpg)</br></br>

##5. Configure Downloaded Grafana Image

![Configure Downloaded Grafana Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/configure-downloaded-grafana-image.jpg)</br></br>

  * Change name to Grafana
   
    ![Change Name to Grafana for Docker Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/change-name-to-grafana-for-docker-image.jpg)</br></br>

  * Enable auto-start in Advanced Settings

    ![Enable Auto-start in Advanced Settings in Docker Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/enable-auto-start-in-advanced-settings-docker-image.jpg)</br></br>

  * Map **grafana** folder to **/var/lib/grafana** Docker image folder in Advanced Settings

    ![Map grafana Folder to Docker Image Folder in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/map-grafana-folder-to-docker-image-folder.jpg)</br></br>

  * Assign a Grafana port in Advanced Settings

    It has to be unoccupied port.

    ![Assign Grafana Port in Advanced Settings in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/assign-grafana-port-in-advanced-settings.jpg)</br></br>

##6. Run Grafana container

![Run Grafana Container in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/run-grafana-container.jpg)</br></br>

##7. Navigate to Grafana Login Page

User name and password is **admin**. Type **[your Synology NAS address]:[Grafana port as per topic #5]** in browser.

![Navigate to Grafana Login Page in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/navigate-to-grafana-login-page.jpg)</br></br>

##8. Download latest 1.x InfluxDB Image in Docker

![Download Latest 1.x InfluxDB Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/download-latest-1x-influxdb-image-in-synology-nas.jpg)</br></br>

![Choose InfluxDB Docker Tag in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/choose-influxdb-docker-tag-in-synology-nas.jpg)</br></br>

##9. Configure Downloaded InfluxDB Image

  * Change name to InfluxDB

    ![Change Name to InfluxDB for Docker Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/change-name-to-influxdb-for-docker-image.jpg)</br></br>

  * Enable auto-start in Advanced Settings

    ![Enable Auto-start in Advanced Settings for InfluxDB in Docker Image in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/enable-auto-start-in-advanced-settings-for-influxdb-in-docker-image.jpg)</br></br>

  * Create InfluxDB folder in docker folder on Volume Tab of Advanced Settings

    The folders are **influxdb/var/lib/influxdb**.

    ![Create InfluxDB Folders in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/create-influxdb-folders.jpg)</br></br>

    ![Created InfluxDB Folders in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/created-influxdb-folders.jpg)</br></br>

  * Map InfluxDB folders to **/var/lib/influxdb** Docker image folder in Advanced Settings

    ![Map InfluxDB Folder to Docker Image Folder in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/map-influxdb-folder-to-docker-image-folder.jpg)</br></br>

  * Make sure that Grafana and InfluxDB have the same network

    ![Grafana and InfluxDB Have Same Network in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/grafana-and-influxdb-have-same-network.jpg)</br></br>

  * Assign InfluxDB port in Advanced Settings

    ![Assign Local InfluxDB Port in Advanced Settings in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/assign-local-influxdb-port-in-advanced-settings.jpg)</br></br>

  * Add environment variables in Advanced Settings

      * DOCKER_INFLUXDB_INIT_MODE -> setup
      * DOCKER_INFLUXDB_INIT_USERNAME -> my-user
      * DOCKER_INFLUXDB_INIT_PASSWORD -> my-password
      * DOCKER_INFLUXDB_INIT_ORG -> my-org
      * DOCKER_INFLUXDB_INIT_BUCKET -> my-bucket

    ![Add InfluxDB Environment Variables in Advanced Settings in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/add-influxdb-environment-variables-in-advanced-settings.jpg)</br></br>

  * InfluxDB Docker image final step

    ![InfluxDB Docker Image Final Step in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/influxdb-docker-image-final-step.jpg)</br></br>

##11. Create Hubitat database

  * Open InfluxDB container

    ![Open InfluxDB Container in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/open-influxdb-container.jpg)</br></br>

  * Go to Terminal

    ![Go to InfluxDB Container Terminal in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/go-to-influxdb-container-terminal.jpg)</br></br>

  * Click Create button and then select bash

    ![Create New Terminal in InfluxDB Container in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/create-new-terminal-in-influxdb-container.jpg)</br></br>

  * Run influx command

    ![Run influx Command in InfluxDB Docker Container in Synology NAS]({static}/images/hubitat-sensor-logging-visualization-synology-nas/run-influx-command-in-influxdb-docker-container.jpg)</br></br>

  * Create Hubitat database

        :::bash
        > create database Hubitat

  * Validate database creation

        :::bash
        > show databases
        name: databases
        name
        ----
        _internal
        Hubitat

  * Exit from InfluxDB

        :::bash
        > exit 
        root@InfluxDB:/#

##12. Add Grafana Panel to view Habitat Data

  * Open Grafana home page entering **[your Synology NAS address]:[Grafana port as per topic #5]**

    ![Grafana Home Page]({static}/images/hubitat-sensor-logging-visualization-synology-nas/grafana-home-page.jpg)</br></br>

  * Add data source
    
    ![Add Grafana Data Source]({static}/images/hubitat-sensor-logging-visualization-synology-nas/add-grafana-data-source.jpg)</br></br>

  * Find InfluxDB data source
   
    ![Find InfluxDB Data Source]({static}/images/hubitat-sensor-logging-visualization-synology-nas/find-influxdb-data-source.jpg)</br></br>

  * Replace data source name, URL, and Database

    ![Replace Name, URL, and Database in Grafana Data Source]({static}/images/hubitat-sensor-logging-visualization-synology-nas/replace-name-url-database-in-grafana-data-source.jpg)</br></br>

  * Click Save & Test to get message datasource is working

    ![Grafana Datasource Is Working]({static}/images/hubitat-sensor-logging-visualization-synology-nas/grafana-datasource-is-working.jpg)</br></br>

  * Create new dashboard

    ![Create New Grafana Dashboard]({static}/images/hubitat-sensor-logging-visualization-synology-nas/create-new-grafana-dashboard.jpg)</br></br>

  * Add new panel

    ![Add New Panel to Grafana Dashboard]({static}/images/hubitat-sensor-logging-visualization-synology-nas/add-new-panel-to-grafana-dashboard.jpg)</br></br>

  * Create query to retrieve data

    ![Create Grafana Query to Retrieve Data]({static}/images/hubitat-sensor-logging-visualization-synology-nas/create-grafana-query-to-retrieve-data.jpg)</br></br>

## Resources
* [Hubitat + InfluxDB + Grafana How to Ubuntu Linux](https://community.hubitat.com/t/hubitat-influxdb-grafana-howto-ubuntu-linux/97798)
* [Synology NAS monitoring](https://github.com/wbenny/synology-nas-monitoring)
* [How to Install Grafana on a Synology NAS](https://www.wundertech.net/how-to-install-grafana-on-a-synology-nas/)

