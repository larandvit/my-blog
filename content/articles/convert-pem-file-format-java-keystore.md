Title: Convert PEM File Format to Java Keystore
Date: 2022-09-03
Category: Security
Cover: /extra/security-logo.jpg

There are two major ways to convert PEM file into Java keystore one. The fist way is very easy to use a tool which is capable of direct conversion of those files. As a tool, for example, you can use Oracle Endeca Key Importer utility. It is a part of a commercial solution. 
The second way is not trivial as the first one as it requests multiple steps, but you can use publicly available tools.

The sample is based on a PEM certificate including only public key. The commands are run in Red Hat Linux 7.9.

1. Create empty java key store

    a) Create key story with dummy certificate

        :::bash
        keytool -genkeypair -alias dummy -storepass changeit -keypass changeit -keystore my_java_keystore.jks -dname "CN=Developer, OU=Department, O=Company, L=City, ST=State, C=CA"

    Store and key passwords are **changeit**. The output file is **my_java_keystore.jks**.

    b) Delete dummy certificate

        :::bash
        keytool -delete -alias dummy -storepass changeit -keystore my_java_keystore.jks

    c) Validate the key store

        :::bash
        keytool -list -keystore my_java_keystore.jks -storepass changeit

    Output

        :::bash
        Keystore type: PKCS12
        Keystore provider: SUN

        Your keystore contains 0 entries

2. Add PEM certificate

    a) Add public key

        :::bash
        keytool -import -file your_certificate.pem -alias your_alias -keystore my_java_keystore.jks -deststorepass changeit

    Your PEM file with a public key is **your_certificate.pem**. Replace **your_alias** name with yours.

    b) Validate the key store

        :::bash
        keytool -list -keystore my_java_keystore.jks -storepass changeit

    Output

        :::bash
        Keystore type: PKCS12
        Keystore provider: SUN

        Your keystore contains 1 entry

        your_alias, Sep 3, 2022, trustedCertEntry,
        Certificate fingerprint (SHA-256): 00:F2:DC:06:5C:9C:96:7A:47:C6:C6:27:EC:A9:70:F9:85:9E:74:79:3D:BE:27:FC:0F:9E:F4:1A:CC:B3:D8:5B

Resources

* [How to create an empty java trust store](https://stackoverflow.com/questions/37994315/how-to-create-an-empty-java-trust-store)

