Title: Connect DBeaver to Trino with HTTPS Protocol
Date: 2020-08-25
Modified: 2022-12-10
Category: Trino, DBeaver
Cover: /extra/dbeaver-logo.png

Communication between [Trino](https://trino.io/) and a client can be secured with [HTTPS protocol](http-server.https.keystore.path). `config.properties` Trino setup file located in `/etc/trino` folder includes the information necessary for establishing a connection to DBeaver. Java Keystore file contains a public key which is required to connect DBeaver to Trino cluster secured with SSL/TLS. Also, it is possible to disable SSL verification.


## Setup

1. This step can be skipped if SSL verification is disabled (option 2 below) in a connection. Find a file defined in `http-server.https.keystore.path` variable. The file can be in two formats: Java keystore (`.jks`) or certificate (`.pem`). Java keystore works for DBeaver. Certificate [can be converted into Java keystore]({filename}/articles/convert-pem-file-format-java-keystore.md) as well. It is recommended to [remove private key from Java keystore]({filename}/articles/remove-private-key-java-keystore.md). Place the file in any location where accessible by DBeaver . It might be `C:\DBeaver` DBeaver root folder. 

2. Create Trino connection.

    ![DBeaver Trino Connection Wizard]({static}/images/connect-dbeaver-presto-https-protocol/dbeaver-trino-connection-wizard.jpg)</br></br>

3. Replace Host with your Trino coordinator FQDN and port with your Trino HTTPS one, and add user name on **Main** tab.

    ![Trino Connection Main Tab]({static}/images/connect-dbeaver-presto-https-protocol/trino-connection-main-tab.jpg)</br></br>

4. Switch to **Driver properties** tab.

    ![Trino Connection Driver Properties Tab]({static}/images/connect-dbeaver-presto-https-protocol/trino-connection-driver-properties-tab.jpg)</br></br>

5. Change `SSL` property to `true` on **Driver properties** tab.

    ![Change SSL Property to true on Driver Properties Tab]({static}/images/connect-dbeaver-presto-https-protocol/change-ssl-property-true.jpg)</br></br>

6. Add properties on **Driver properties** tab.

    **Option 1 with SSL verification**

    ![Trino Connection Driver Properties Tab with Added Properties]({static}/images/connect-dbeaver-presto-https-protocol/trino-connection-driver-properties-tab-with-properties.jpg)</br></br>

    User properties

    * `SSLKeyStorePath`: path to Java Keystore file, for example, `C:\DBeaver\java_keystore_file.jks`.
    * `SSLKeyStorePassword`: Java Keystore file password

    **Option 2 without SSL verification**

    ![Trino Connection Driver Properties Tab with Added Properties]({static}/images/connect-dbeaver-presto-https-protocol/trino-connection-driver-properties-tab-with-properties2.jpg)</br></br>

    User properties

    * `SSLVerification`: NONE

7. Test connection.

    Success

    ![Test Trino Connection Success]({static}/images/connect-dbeaver-presto-https-protocol/test-trino-connection-success.jpg)</br></br>

    Failure

    ![Test Trino Connection Failure]({static}/images/connect-dbeaver-presto-https-protocol/test-trino-connection-failure.jpg)</br></br>

## Resources

* [Trino JDBC driver](https://trino.io/docs/current/installation/jdbc.html)
