Title: Python Access to Presto Cluster
Date: 2020-11-30
Modified: 2020-12-02
Category: Python, Presto
Cover: /extra/python-logo.png

[Presto](https://prestosql.io/) access is represented by many Python libraries among those are [Dropbox/PyHive](https://github.com/dropbox/PyHive), [prestosql/presto-python-client](https://github.com/prestosql/presto-python-client), [prestodb/presto-python-client](https://github.com/prestodb/presto-python-client), and [easydatawarehousing/prestoclient](https://github.com/easydatawarehousing/prestoclient). Some of libraries use [Python DB-API](https://www.python.org/dev/peps/pep-0249/) interface to access Presto. They have limitations to verify SSL certificates for HTTPS requests. SSL certificates must be unencrypted. Because of this limitation, verifying of the SSL certificate might be ignored in connection to Presto.

Dropbox/PyHive library is universal one as it can be used to access Hive or Presto. prestosql/presto-python-client library is actively supported by Presto developers.

Python Dropbox/PyHive library is picked up to demonstrate how to access to Presto cluster. The sample is run in Python 3 in Windows.

## 1. Install PyHive library

Linux.

    :::bash
    sudo pip3 install 'pyhive[presto]'

Windows. Run as administrator.

    :::bash
    pip install 'pyhive[presto]'

It installs only Presto interface.

## 2. Include requested libraries

Access to Presto cluster without password.

    :::python
    from pyhive import presto

Presto cluster is secured by password.

    :::python
    from pyhive import presto
    from requests.auth import HTTPBasicAuth

## 3. Establish connection

Access to Presto cluster without password.

    :::python
    conn = presto.connect(host='localhost',
                          port=8080,
                          catalog='system',
                          schema='information_schema')

Presto cluster is secured by password.

    :::python
    conn = presto.connect(host='localhost',
                          port=443,
                          protocol='https',
                          catalog='system',
                          schema='information_schema',
                          requests_kwargs={'auth': HTTPBasicAuth('username', 'userpassword'),'verify':False})

## 4. Create cursor

    :::python
    cur = conn.cursor()


## 5. Retrieve data

    :::python
    cur.execute('SELECT column_name FROM columns LIMIT 10')
    for row in cur.fetchall():
        print(row[0])

## 6. Improvements

To disable insecure warnings during https requests, add the code in `import` section.

    :::python
    import urllib3
    urllib3.disable_warnings()

## Resources
* [SSL Cert Verification](https://2.python-requests.org/en/master/user/advanced/#ssl-cert-verification)
