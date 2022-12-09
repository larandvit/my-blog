Title: Hide Passwords and Sensitive Data in Trino Configuration Files
Date: 2020-10-17
Modified: 2022-12-08
Category: Trino, Security
Cover: /extra/trino-logo.png

Trino configuration files contain passwords and sensitive data in plain text. Corporation security policies are not tolerant with it. That kind of information must be hidden from users who are not authorized to have access to it. As a rule of thumb, DevOps and security teams are responsible to set up and maintain the part of configuration with sensitive information. It can be achieved by implementing environment variables to supply passwords and sensitive data in Trino configuration files. A Trino property value is replaced with a name of an environment variable. 

The simplest method is to populate environment variables manually or with a shell script before starting of Trino. This functionality is fully supported by Trino. Also, it is possible to read data from a secure storage and assign values to environment variables. Both ways are shown in the article. As a storage, it is used a text file with limited access, but it can be replaced with another type of a storage.

The samples are based on [Starburst](https://www.starburstdata.com/) 393-e open-source distribution and RHEL 7 Linux distribution. There are two different cases depending on what Trino installation is used RPM or tar.

##Manually populated environment variable

  1. Add environment variable in Linux
     
        :::bash
        export MYSQL_SERVER_PASSWORD=password123

  2. Use the variable in MySql catalog configuration file

        :::ini
        connector.name=mysql
        connection-url=jdbc:mysql://example.net:3306
        connection-user=root
        connection-password=${ENV:MYSQL_SERVER_PASSWORD} 

##Environment variable extracted from text file

### RPM installation

  1. Create `ini` file with secrets

     The file contains entries in format `name=value`, for example, `MYSQL_SERVER_PASSWORD=password123`.

        :::bash
        nano /root/trino_secrets.ini

  2. Load sensitive information from `ini` file

     Read the environment variable in `/etc/starburst/env.sh` Trino configuration file.

        :::bash
        export MYSQL_SERVER_PASSWORD=$(awk -F "=" '/MYSQL_SERVER_PASSWORD/ {print $2}' /root/trino_secrets.ini)

  3. Deploy the setup files to a coordinator and/or workers 

     * /root/trino_secrets.ini
     * /etc/starburst/env.sh

  4. Limit access to `ini` file on each Trino node

     Make `root` as an owner of the file and remove everybody else from accessing the file.

        :::bash
        chown root:root /root/trino_secrets.ini
        chmod g+rw,u+rw,o-rwx /root/trino_secrets.ini

  5. Use the variable in MySql catalog configuration file

        :::ini
        connector.name=mysql
        connection-url=jdbc:mysql://example.net:3306
        connection-user=root
        connection-password=${ENV:MYSQL_SERVER_PASSWORD}

### tar installation

  1. Create `sh`file with secrets

        :::bash
        sudo /trino/trino_secrets.sh

    Each entry is an environment variable export. Surround variable values with single quotes (`'`) to mitigate any special characters.

        :::ini
        export MYSQL_SERVER_PASSWORD='password123'

  2. Limit access to the secret file to a user running Trino service, for example, starburst.

        :::bash
        sudo chown -R starburst:starburst /trino
        sudo chmod -R o-rxw /trino

  3. Run the secret file in Trino launcher script

     Add `. /trino/trino_secrets.sh` command before `exec "$(dirname "$0")/launcher.py" "$@"` line.

     Open Trino launcher script if Trino has been installed in `/usr/lib/starburst` folder.

        :::bash
        sudo nano /usr/lib/starburst/bin/launcher

     Make changes

        :::bash
        . /trino/trino_secrets.sh
        exec "$(dirname "$0")/launcher.py" "$@"

  4. Use the variable in MySql catalog configuration file

        :::ini
        connector.name=mysql
        connection-url=jdbc:mysql://example.net:3306
        connection-user=root
        connection-password=${ENV:MYSQL_SERVER_PASSWORD}


## Resources

* [Secrets](https://docs.starburst.io/latest/security/secrets.html)
