Title: Hide Passwords and Sensitive Data in Presto Configuration Files
Date: 2020-10-17
Modified: 2022-11-24
Category: Trino, Security
Cover: /extra/trino-logo.png

Presto configuration files contain passwords and sensitive data in plain text. Corporation security policies are not tolerant with it. That kind of information has to be hidden from users who are not authorized to have access to it. As a rule of thumb, DevOps and security teams are responsible to set up and maintain the part of configuration with sensitive information. It can be achieved by implementing environment variables to supply passwords and sensitive data in Presto configuration files. A Presto property value can be replaced with a name of an environment variable. Populating of environment variables are done during starting of Presto service. Sensitive information can be loaded from different sources, for example, a file located in a secure place outside of Presto.

The sample is based on [Starburst](https://www.starburstdata.com/) 343-e open source distribution with RPM installation and RHEL 7 Linux distribution. Sensitive information is stored in a file.

##1. Create ini file with secrets

The file contains entries in format `name=value`, for example, `MYSQL_SERVER_PASSWORD=password123`.

    :::bash
    nano /root/presto_secrets.ini

##2. Load sensitive information from ini file

Presto runs `/etc/presto/env.sh` file to initiate additional setup. Add an environment variable to the file.

    :::bash
    export MYSQL_SERVER_PASSWORD=$(awk -F "=" '/MYSQL_SERVER_PASSWORD/ {print $2}' /root/presto_secrets.ini)

##3. Deploy setup files to a coordinator and/or workers 

* /root/presto_secrets.ini
* /etc/presto/env.sh

##4. Limit access to ini file on each Presto node

Make `root` as an owner of the file and remove everybody else from accessing the file.

    :::bash
    chown root:root /root/presto_secrets.ini
    chmod g+rw,u+rw,o-rwx /root/presto_secrets.ini

##5. Adjust Presto service file

The file location is `/etc/rc.d/init.d/presto`. Add `--preserve-env` after `sudo -u $SERVICE_USER`.

    :::bash
    start () {
        echo "Starting ${SERVICE_NAME} "
        if [ -z "${JAVA_HOME}" ]
        then
            echo "Warning: No value found for \$JAVA_HOME. Default Java will be used." >&2
            sudo -u $SERVICE_USER --preserve-env /usr/lib/presto/bin/launcher start "${CONFIGURATION[@]}"
        else
            sudo -u $SERVICE_USER --preserve-env PATH=${JAVA_HOME}/bin:$PATH /usr/lib/presto/bin/launcher start "${CONFIGURATION[@]}"
        fi
        return $?
    }

##6. Restart Presto cluster

##7. Usage samples

MySQL Server connector file.

    :::ini
    connector.name=mysql
    connection-url=jdbc:mysql://example.net:3306
    connection-user=root
    connection-password=${ENV:MYSQL_SERVER_PASSWORD}

## Resources

* [Presto Secrets](https://prestosql.io/docs/current/security/secrets.html)
* [Presto Slack channels](https://prestosql.slack.com)
