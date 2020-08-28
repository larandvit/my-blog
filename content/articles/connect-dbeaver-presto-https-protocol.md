Title: Connect DBeaver to Presto with HTTPS Protocol
Date: 2020-08-25
Modified: 2020-08-27
Category: Presto, DBeaver
Cover: /extra/dbeaver-logo.png

Communication between [Presto](https://prestodb.io/) and a client can be secured with [HTTPS protocol](https://prestodb.io/docs/current/security/internal-communication.html). `config.properties` Presto setup file located in `/etc/presto` folder includes the information necessary for establishing a connection to DBeaver. Java Keystore file contains a public key which is required to connect DBeaver to Presto cluster secured with SSL/TLS. 

The sample uses encrypted with SSL/TLS [Starburst](https://www.starburstdata.com/) Presto cluster.

## Setup

1. Find a file defined in `http-server.https.keystore.path` variable. The file can be in two formats: Java keystore (`.jks`) or certificate (`.pem`). Java keystore works for DBeaver. Certificate can be converted into Java keystore as well. It is recommended to remove private key from Java key store to remove security issue. Place the file in any location where DBeaver installed. It might be DBeaver root or `C:\Users\[Windows user name]\AppData\Roaming\DBeaverData` folder. 

2. Create PrestoSQL connection.

    ![DBeaver PrestoSQL Connection Wizard]({static}/images/connect-dbeaver-presto-https-protocol/dbeaver-prestosql-connection-wizard.png)</br></br>

3. Replace Host with your Presto coordinator FQDN and port with your Presto HTTPS one, and add user name on `Main` tab.

    ![PrestoSQL Connection Main Tab]({static}/images/connect-dbeaver-presto-https-protocol/prestosql-connection-main-tab.png)</br></br>

4. Add properties on `Driver properties` tab.

    ![PrestoSQL Connection Driver Properties Tab]({static}/images/connect-dbeaver-presto-https-protocol/prestosql-connection-driver-properties-tab.png)</br></br>

    ![PrestoSQL Connection Driver Properties Tab with Added Properties]({static}/images/connect-dbeaver-presto-https-protocol/prestosql-connection-driver-properties-tab-with-properties.png)</br></br>

    Properties

    * `SSL`: true
    * `SSLKeyStorePath`: path to Java Keystore file, for example, C:/Users/sample/AppData/Roaming/DBeaverData. **Make sure to use forward slash**.
    * `SSLKeyStorePassword`: Java Keystore file password.

5. Test connection.

    Success

    ![Test PrestoSQL Connection Success]({static}/images/connect-dbeaver-presto-https-protocol/test-prestosql-connection-success.png)</br></br>

    Failure

    ![Test PrestoSQL Connection Failure]({static}/images/connect-dbeaver-presto-https-protocol/test-prestosql-connection-failure.png)</br></br>

## Resources

* [JDBC Driver](https://prestosql.io/docs/current/installation/jdbc.html)
