Title: Connect DBeaver to Presto with HTTPS Protocol
Date: 2020-08-25
Category: Presto, DBeaver
Cover: /extra/dbeaver-logo.png

Communication between [Presto](https://prestodb.io/) nodes can be secured with [HTTPS protocol](https://prestodb.io/docs/current/security/internal-communication.html). Java Keystore file containing public/private keys along with the key password are required to connect DBeaver to Presto cluster secured with SSL/TLS. `config.properties` Presto setup file located in `/etc/presto` folder includes the information for establishing a connection.

The sample uses encrypted with SSL/TLS [Starburst](https://www.starburstdata.com/) Presto cluster.

## Setup

1. Take `keystore.jks` Java Keystore file located in `/etc/presto` or another folder and place it in any location where DBeaver installed. It might be DBeaver root or `C:\Users\[Windows user name]\AppData\Roaming\DBeaverData` folder.

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
