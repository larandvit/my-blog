Title: Access MinIO Secured by SSL/TLS with MinIO Python SDK
Date: 2021-03-06
Category: MinIO, Python
Cover: /extra/python-logo.png

Enterprise level products are secured with SSL/TLS protocol on top of HTTP. It enforces encrypted communications between a Web server and a client. To access those data, a client is supposed to obtain a SSL certificate. [MinIO](https://min.io/) supports both secured and unsecured access to object storage. [MinIO Python SDK](https://docs.min.io/docs/python-client-quickstart-guide.html) has been developed as a native library to MinIO and it works with Amazon S3 compatible Cloud Storage as well. 

There are some modifications to access MinIO storage secured by HTTPS. Prerequisite for each method is a SSL certificate. It can received from your company infrastructure team or it might be extracted from internet browser as per [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

The samples are developed with Python 3.6.4 and MinIO SDK 7.0.2 in Windows 10. They show a list of buckets in a MinIO cluster.

## Option 1

    :::python
    import os

    from minio import Minio

    if __name__ == '__main__':
    
        os.environ['SSL_CERT_FILE'] = r'C:\temp\sample.crt'
    
        client = Minio('minio.sample.com:9000',
                       access_key='<accress keyu>',
                       secret_key='secret key',
                       secure=True)
    
        bucket_list = client.list_buckets()

        for bucket in bucket_list:
            print(bucket.name)

## Option 2

    :::python
    from minio import Minio
    import urllib3
    from datetime import timedelta

    if __name__ == '__main__':
    
        timeout = timedelta(minutes=5).seconds
    
        http_client = urllib3.PoolManager(
        timeout=urllib3.util.Timeout(connect=timeout, read=timeout),
                maxsize=10,
                cert_reqs='CERT_REQUIRED',
                ca_certs=r'C:\temp\sample.crt',
                retries=urllib3.Retry(
                    total=5,
                    backoff_factor=0.2,
                    status_forcelist=[500, 502, 503, 504]
                )
        )
    
        client = Minio('minio.sample.com:9000',
                       access_key='<access key>',
                       secret_key='secret key',
                       secure=True,
                       http_client=http_client)
    
        bucket_list = client.list_buckets()

        for bucket in bucket_list:
            print(bucket.name)

## Option 3

Add `SSL_CERT_FILE` environment variable.

1. Press Win+R combination, and then type in `SystemPropertiesAdvanced` to open **System Properties**

     ![Windows 10 System Properties]({static}/images/access-minio-secured-ssl-minio-python-sdk/windows-10-system-properties.jpg)</br></br>

2. Click **Environment Variable**

     ![Windows 10 environment variables]({static}/images/access-minio-secured-ssl-minio-python-sdk/windows-10-environment-variables.jpg)</br></br>

3. It can be a **User** or **System** variable

     ![Windows 10 new user variable]({static}/images/access-minio-secured-ssl-minio-python-sdk/windows-10-new-user-variable.jpg)</br></br>

Restart your Integrated Development Environment (IDE) or command line window to pick up the new variable. If it does not work, restart your computer.

    :::python
    from minio import Minio

    if __name__ == '__main__':
    
        client = Minio('minio.sample.com:9000',
                       access_key='<secret key>',
                       secret_key='secret key',
                       secure=True)
    
        bucket_list = client.list_buckets()

        for bucket in bucket_list:
            print(bucket.name)

## Resources

* [MinIO Python SDK for Amazon S3 Compatible Cloud Storage](https://docs.min.io/docs/python-client-quickstart-guide.html) documentation
* [MinIO Python SDK](https://github.com/minio/minio-py) GitHub repository
