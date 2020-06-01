Title: Create Keytab for Kerberos Authentication in Linux
Date: 2020-05-31
Category: Kerberos
Cover: /extra/kerberos-logo.png

Keytab stands for key table. It is a file which stores one or more Kerberos principals with corresponding encrypted keys. Encrypted keys are generated based on user passwords. It allows to secure storing of passwords and authenticate users without entering of passwords. The current version of the Kerberos protocol is 5.

The article is sampled in CentOS / RHEL distribution.

## 1. Validate that Kerberos 5 client is installed

Kerberos 5 client is installed as default. There are two components.

    :::bash
    yum list installed | grep 'krb5-workstation\|krb5-libs'

Output

    :::text
    krb5-libs.x86_64                        1.15.1-46.el7                  @base
    krb5-workstation.x86_64                 1.15.1-46.el7                  @base

Kerberos 5 client installation

    :::bash
    sudo yum install krb5-workstation krb5-libs

## 2. Create a folder to store keytab file

    :::bash
    mkdir ~/kerberos

## 3. Create keytab file

The tool to generate keytab file is interactive one and you need to type in the commands.

    :::text
    ktutil
    ktutil: addent -password -p username@SAMPLE.COM -k 1 -e RC4-HMAC 
    Password for username@SAMPLE.COM: 
    ktutil: wkt /home/username/kerberos/username.keytab 
    ktutil: l
    slot KVNO Principal
    ---- ---- --------------------------------------------------------------------- 
       1    1                                                   username@SAMPLE.COM 
    ktutil: exit

## 4. Validate keytab file

    :::bash
    klist -e -k -t ~/kerberos/username.keytab
