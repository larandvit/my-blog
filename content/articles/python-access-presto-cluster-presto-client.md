Title: Python Access to Trino Cluster with Trino Client
Date: 2020-12-13
Modified: 2022-12-20
Category: Python, Trino
Cover: /extra/python-logo.png

[Trino](https://trino.io/) access is represented by many Python libraries among those are [Dropbox/PyHive](https://github.com/dropbox/PyHive), [trinodb/trino-python-client](https://github.com/trinodb/trino-python-client), [PySpark](https://spark.apache.org/), and [PyJDBC](https://github.com/mkleehammer/pyodbc). Mostly of libraries use [Python DB-API](https://www.python.org/dev/peps/pep-0249/) interface to access Trino which uniforms commands.

trinodb/trino-python-client library is actively supported by Trino community and native to Trino. An alternative option to access Trino is described in [Python Access to Trino Cluster with PyHive]({filename}/articles/python-access-presto-cluster.md) in article.

The sample is run with Python 3.8 in Windows with [Starburst](https://www.starburst.io/) distribution.

## 1. Install Trino client library

**Linux**

With sudo access.

    :::bash
    sudo pip3 install trino

**Windows**

Run as administrator.

    :::bash
    pip install trino

Also, it's possible to install the library without elevated privileges in Linux and Windows.

    :::bash
    pip3 install --user trino

## 2. Include requested libraries

    :::python
    import trino

## 3. Establish connection

   * Access to Trino cluster without password.

        :::python
        conn = trino.dbapi.connect(host='localhost',
                                   port=8080,
                                   catalog='system',
                                   schema='runtime')

   * Trino cluster is secured by password but skip SSL verification. This case might be used during development stage.

        :::python
        conn = trino.dbapi.connect(host='localhost',
                                   port=443,
                                   http_scheme='https',
                                   catalog='system',
                                   schema='runtime',
                                   auth=trino.auth.BasicAuthentication('<user name>', '<password>'),
                                   verify=False)

   * Trino cluster is secured by password.
      
      Option #1. Follow instructions in [Convert Java Keystore to PEM File Format]({filename}/articles/convert-java-keystore-pem-file-format.md) article to create `trino.pem` file. The file contains Trino SSL public certificate converted from Java keystore file.

      Option #2. Extract `trino.pem` certificate from Internet Browser. Follow [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

        :::python
        conn = trino.dbapi.connect(host='localhost',
                                   port=443,
                                   http_scheme='https',
                                   catalog='system',
                                   schema='runtime',
                                   auth=trino.auth.BasicAuthentication('<user name>', '<password>'),
                                   verify='trino.pem')


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
