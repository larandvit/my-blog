Title: Python Access to Trino Cluster with PyHive
Date: 2020-11-30
Modified: 2022-12-18
Category: Python, Trino
Cover: /extra/python-logo.png

[Trino](https://trino.io/) access is represented by many Python libraries among those are [Dropbox/PyHive](https://github.com/dropbox/PyHive), [trinodb/trino-python-client](https://github.com/trinodb/trino-python-client), and [easydatawarehousing/prestoclient](https://github.com/easydatawarehousing/prestoclient). Mostly of libraries use [Python DB-API](https://www.python.org/dev/peps/pep-0249/) interface to access Trino which uniforms commands.

[Python Access to Trino Cluster with Trino Client]({filename}/articles/python-access-presto-cluster-presto-client.md) article describes Trino client library usage.

Dropbox/PyHive library is universal one as it can be used to access Hive or Trino. The sample is run with Python 3 in Windows.

## 1. Install PyHive library

Linux.

    :::bash
    sudo pip3 install 'pyhive[trino]'

Windows. Run as administrator.

    :::bash
    pip install 'pyhive[trino]'

It installs only Trino interface.

## 2. Include requested libraries

   * Access to Trino cluster without password.

        :::python
        from pyhive import trino

   * Trino cluster is secured by password.

        :::python
        from pyhive import trino
        from requests.auth import HTTPBasicAuth

## 3. Establish connection

   * Access to Trino cluster without password.

        :::python
        conn = trino.connect(host='localhost',
                             port=8080,
                             catalog='system',
                             schema='runtime')

   * Trino cluster is secured by password but skip SSL verification. This case might be used during development stage.

        :::python
        conn = trino.connect(host='localhost',
                             port=443,
                             protocol='https',
                             catalog='system',
                             schema='runtime',
                             requests_kwargs={'auth': HTTPBasicAuth('<user name>', '<password>'),
                                              'verify':False})

   * Trino cluster is secured by password.
      
      Option #1. Follow instructions in [Convert Java Keystore to PEM File Format]({filename}/articles/convert-java-keystore-pem-file-format.md) article to create `trino.pem` file. The file contains Trino SSL public certificate converted from Java keystore file.

      Option #2. Extract `trino.pem` certificate from Internet Browser. Follow [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

        :::python
        conn = trino.connect(host='localhost',
                             port=443,
                             protocol='https',
                             catalog='system',
                             schema='runtime',
                             requests_kwargs={'auth': HTTPBasicAuth('<user name>', '<password>'),
                                              'verify':'trino.pem'})


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
