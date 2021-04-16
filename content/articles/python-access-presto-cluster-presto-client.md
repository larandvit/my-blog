Title: Python Access to Presto Cluster with Presto Client
Date: 2020-12-13
Modified: 2021-04-15
Category: Python, Presto
Cover: /extra/python-logo.png

[Presto](https://prestosql.io/) access is represented by many Python libraries among those are [Dropbox/PyHive](https://github.com/dropbox/PyHive), [prestosql/presto-python-client](https://github.com/prestosql/presto-python-client), [prestodb/presto-python-client](https://github.com/prestodb/presto-python-client), and [easydatawarehousing/prestoclient](https://github.com/easydatawarehousing/prestoclient). Mostly of libraries use [Python DB-API](https://www.python.org/dev/peps/pep-0249/) interface to access Presto which uniforms commands.

[Python Access to Presto Cluster with PyHive]({filename}/articles/python-access-presto-cluster.md) article describes Dropbox/PyHive library usage.

prestosql/presto-python-client library is actively supported by Presto developers. The sample is run with Python 3 in Windows.

## 1. Install Presto client library

Linux.

    :::bash
    sudo pip3 install presto-client

Windows.

    :::bash
    pip install presto-client

## 2. Include requested libraries

    :::python
    import presto

## 3. Establish connection

   * Access to Presto cluster without password.

        :::python
        conn = presto.dbapi.connect(host='localhost',
                                    port=8080,
                                    catalog='system',
                                    schema='runtime')

   * Presto cluster is secured by password but skip SSL verification. This case might be used during development stage.

        :::python
        conn = presto.dbapi.connect(host='localhost',
                                    port=443,
                                    http_scheme='https',
                                    catalog='system',
                                    schema='runtime',
                                    auth=presto.auth.BasicAuthentication('<user name>', '<password>'),
                                    verify=False)

   * Presto cluster is secured by password.
      
      Option #1. Follow instructions in [Convert Java Keystore to PEM File Format]({filename}/articles/convert-java-keystore-pem-file-format.md) article to create `presto.crt` file. The file contains Presto SSL public certificate converted from Java keystore file.

      Option #2. Extract `presto.crt` certificate from Internet Browser. Follow [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

        :::python
        conn = presto.dbapi.connect(host='localhost',
                                    port=443,
                                    http_scheme='https',
                                    catalog='system',
                                    schema='runtime',
                                    auth=presto.auth.BasicAuthentication('<user name>', '<password>'),
                                    verify='presto.crt')


## 4. Create cursor

    :::python
    cur = conn.cursor()


## 5. Retrieve data

    :::python
    cur.execute('SELECT * FROM nodes')
    for row in cur.fetchall():
        print(row)

## 6. Improvements

To disable insecure warnings during https requests if `verify=False`, add the code in `import` section.

    :::python
    import urllib3
    urllib3.disable_warnings()

## 7. Troubleshooting

In case of getting `ssl.SSLError: [SSL: CERTIFICATE_VERIFY_FAILED] certificate verify failed (_ssl.c:777)` error, check your certificate expiration date. The date has to be valid.

## Resources
* [SSL Cert Verification](https://2.python-requests.org/en/master/user/advanced/#ssl-cert-verification)
