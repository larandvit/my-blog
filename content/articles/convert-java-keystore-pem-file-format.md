Title: Convert Java Keystore to PEM File Format
Date: 2020-12-12
Category: Security
Cover: /extra/security-logo.jpg

Java keystore (JKS) file includes public certificates and cryptography keys. It is secured by a password and used in java applications. Other consumers of public certificates and cryptography keys, for example, tools or software libraries might not accept JKS format. In that case, Java keystore file can be converted into different formats.

PEM is widely used format which can contain certificates and private keys as well. Conversion between those formats is done with multistep process. `keytool` is one tool to convert formats. The tool is a part of JDK or JRE. The second tool is `openssl`.

1. List certificates in a source JKS repository

        :::bash
        keytool -list -v -keystore source_keystore.jks

    Alias identifies certificates and keys.

2. Convert JKS format into intermediate PKCS #12 one.

        :::bash
        keytool -importkeystore -alias <alias from previous step> -srckeystore source_keystore.jks -destkeystore intermediate.p12 -srcstoretype jks -deststoretype pkcs12 -srcstorepass <source keystore password> -deststorepass 123456 -destkeypass 123456

    * `alias` parameter can be omitted if there is only 1 entry. 
    * `deststorepass` and `destkeypass` can be have any values but they have to match to `password` value on the next step.

    Output

        :::text
        Importing keystore source_keystore.jks to intermediate.p12...

3. Create final not encrypted PEM file.
   
    Option #1. One file containing both a certificate and a cryptography key.

        :::bash
        openssl pkcs12 -in intermediate.p12 -nodes -out output.pem -password pass:123456

    Output

        :::text
        MAC verified OK

    Option #2. Two files with a certificate and a cryptography key.

    * Certificate

            :::bash
            openssl pkcs12 -in intermediate.p12 -nokeys -out output.crt -password pass:123456

    * Key

            :::bash
            openssl pkcs12 -in coordinator_keystore.p12 -nocerts -nodes -out output.key -password pass:123456

    Output

        :::text
        MAC verified OK

