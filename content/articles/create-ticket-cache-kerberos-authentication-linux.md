Title: Create Ticket Cache File for Kerberos Authentication in Linux
Date: 2020-06-02
Category: Kerberos
Cover: /extra/kerberos-logo.png

Kerberos credentials can be stored in Kerberos ticket cache. They are valid for relatively short period of time. The period can be a session or a specified timeframe. A Kerberos ticket cache contains a service and a client principal names, lifetime indicators, flags, and the credential itself. Kerberos 5 client is aimed to generate a ticket cache file.

The article is based on CentOS / RHEL distribution.

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

## 2. Create a folder to store ticket cache file

    :::bash
    mkdir ~/kerberos

## 3. Add `KRB5CCNAME` variable

The variable defines the location of a Kerberos ticket cache file.

* Open `.bashrc` file.

        :::bash
        nano ~/.bashrc

* Add the variable `export` command.

        :::bash
        export KRB5CCNAME=/home/username/kerberos/krb5cc_username

* Reboot your computer to make it effective.

* Validate `KRB5CCNAME` variable.

        :::bash
        export | grep KRB5CCNAME

## 4. Create ticket cache file

    :::bash
    kinit -c /home/username/kerberos/krb5cc_username username@SAMPLE.COM -l 10h

-c means the location of the ticket cache

-l states lifetime of the ticket cache

## 4. Validate ticket cache file

    :::bash
    klist -c /home/username/kerberos/krb5cc_username

## 5. Configuration file

`krb5.conf` is a configuration file to tune up Kerberos ticket cache creation. The default location is `/etc` but `KRB5_CONFIG` environmental variable can overwrite the location of the configuration file.

Our interest is mainly 2 sections: `[libdefaults]`and `[realms]`.

    :::ini
    [libdefaults]
    default_realm = SAMPLE.COM
    ticket_lifetime = 24h
    renew_lifetime = 7d
    forwardable = true

    [realms]
    SAMPLE.COM = {
    kdc = server1.sample.com
    }
