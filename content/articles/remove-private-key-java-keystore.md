Title: Remove Private Key from Java Keystore
Date: 2020-08-30
Category: Security
Cover: /extra/security-logo.jpg

Java keystore (JKS) is a file secured by a password. Java keystore repository can contain public key certificates with corresponding private keys. The keys are used to encrypt communication over network. If JKS file is distributed, private keys should be removed from the repository. `keytool` tool is aimed to manipulate JKS repository. The tool is a part of JDK or JRE.

1. List certificates in a source JKS repository

        :::bash
        keytool -list -v -keystore source_keystore.jks

2. Export a public key certificate from a JKS file.

    Public key certificate is exported in binary format.

        :::bash
        keytool -export -alias <your certificate name> -keystore source_keystore.jks -file <export file name, for example, public.der> -storepass <source JKS password>

3. Import the public key certificate into a new JKS file.
   
    If JKS file doesn't exist, it will be created.

        :::bash
        keytool -import -trustcacerts -alias <your certificate name> -file <export file name, for example, public.der> -keystore destination_keystore.jks -storepass <destination JKS password>

4. List certificates in a destination JKS repository.

        :::bash
        keytool -list -v -keystore destination_keystore.jks
