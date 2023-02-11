Title: Use HTTPS Protocol in Trino Server with PKCS 12 Store
Date: 2023-02-10
Category: Trino
Cover: /extra/trino-logo.png

Trino documentation includes description of access to Trino cluster secured by HTTPS protocol with load balancer, PEM and Java KeyStore files. PKCS #12 file is another option to implement HTTPS protocol. It is a binary format for storing a certificate chain and private key in a single, encryptable file with .p12 or .pfx extensions. The file format is always encrypted by a password. Depending on tools to generate a file, PKCS #12 file might be an intermediate one before getting a PEM file. It means one step can be skipped in creating of private keys.

## 1. Validate PKCS #12 file

    :::bash
    openssl pkcs12 -info -in coordinator_key.pfx

##2. Place file on coordinator

  The location of the file can be any accessible by a Trino coordinator server process.

##3. Add file to config.properties file

  The configuration is deployable only to a coordinator. 

    :::ini
    http-server.https.enabled=true
    http-server.https.port=8443
    http-server.https.keystore.path=/path_to_file/coordinator_key.pfx
    http-server.https.keystore.key=password_here

##4. Restart coordinator

 To apply configuration, a cluster has to be restarted.

##5. Validate configuration

 Open Web UI or run Trino CLI.

##6. Extract public certificate

  In some cases, you might be interested in getting a public certificate. Also, it is an intermediate file to generate key store on the next step.

  Option 1

    :::bash
    openssl pkcs12 -in coordinator_key.pfx -clcerts -nokeys -out public_certificate.crt 

  Open 2

  Follow steps in [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

##7. Create keystore with public certificate

  It is an optional step. A certificate might be used in tools or Java applications to access to a Trino cluster.

  Make sure that a keystore file does not exist. The key store requests a password.

    :::bash
    keytool -import -trustcacerts -file public_certificate.crt -alias mytrinocluster -keystore public_certificate.ks -storepass mypassword

##8. Validate keystore

    :::bash
    keytool -v -list -keystore public_certificate.ks -storepass mypassword

## Resources
* [Trino TLS and HTTPS](https://trino.io/docs/current/security/tls.html)
* [Securing Presto with Dain](https://trino.io/blog/2020/08/13/training-security.html)
* [Extracting the certificate and keys from a .pfx file](https://www.ibm.com/docs/en/arl/9.7?topic=certification-extracting-certificate-keys-from-pfx-file)
* [Extra: add PEM to keystore](https://trino.io/docs/current/security/inspect-jks.html#extra-add-pem-to-keystore)
