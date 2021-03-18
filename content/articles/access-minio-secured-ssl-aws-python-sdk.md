Title: Access MinIO Secured by SSL/TLS with AWS SDK for Python (Boto3)
Date: 2021-03-17
Category: MinIO, Python
Cover: /extra/python-logo.png

[AWS SDK for Python](https://boto3.amazonaws.com/v1/documentation/api/latest/index.html) can be used with many AWS services including Amazon Simple Service (Amazon S3). Also, the SDK is capable to access S3 compatible storages such as [MinIO](https://min.io/). While [MinIO Python SDK](https://docs.min.io/docs/python-client-quickstart-guide.html) is a native library for MinIO access, there are some cases when AWS SDK for Python can be used as alternative. If MinIO access is secured by SSL/TLS protocol, SSL certificate is requested or insecure connection can be made as a workaround. 

SSL certificate can be received from your company infrastructure team or it might be extracted from internet browser as per [Export TLS/SSL Server Certificate from Internet Browser]({filename}/articles/export-tls-ssl-server-certificate-from-internet-browser.md) article.

Discover [Access MinIO Secured by SSL/TLS with MinIO Python SDK]({filename}/articles/access-minio-secured-ssl-minio-python-sdk.md) article if you decide to access to MinIO with [MinIO Python SDK](https://docs.min.io/docs/python-client-quickstart-guide.html).

The samples are developed with Python 3.6.4 and AWS SDK for Python (Boto3) 1.17.27 in Windows 10. Those samples show lists of objects in buckets in a MinIO cluster.

## Secure connection

    :::python
    import boto3
    from botocore.exceptions import ClientError

    if __name__ == '__main__':
    
        certificate_path = 'sample.pem'
    
        clientArgs = {
            'aws_access_key_id': '<access key>',
            'aws_secret_access_key': 'secret key',
            'endpoint_url': 'https://minio.sample.com:9000',
            'verify': certificate_path
        }

        client = boto3.resource("s3", **clientArgs)

        try:
            print('Retrieving buckets...')
            print()
            
            for bucket in client.buckets.all():
                bucket_name = bucket.name
                print('Bucket name: {}'.format(bucket_name))
            
                objects = client.Bucket(bucket_name).objects.all()
          
                for obj in objects:
                    object_name = obj.key
                
                    print('Object name: {}'.format(object_name))
                
                print()
            
        except ClientError as err:
            print("Error: {}".format(err)) 

## Insecure connection

    :::python
    import boto3
    from botocore.exceptions import ClientError

    import urllib3
    urllib3.disable_warnings()

    if __name__ == '__main__':
    
        clientArgs = {
            'aws_access_key_id': '<access key>',
            'aws_secret_access_key': 'secret key',
            'endpoint_url': 'https://minio.sample.com:9000',
            'verify': False
        }

        client = boto3.resource("s3", **clientArgs)

        try:
            print('Retrieving buckets...')
            print()
            
            for bucket in client.buckets.all():
                bucket_name = bucket.name
                print('Bucket name: {}'.format(bucket_name))
            
                objects = client.Bucket(bucket_name).objects.all()
          
                for obj in objects:
                    object_name = obj.key
                
                    print('Object name: {}'.format(object_name))
                
                print()
            
        except ClientError as err:
            print("Error: {}".format(err)) 

`urllib3` library is aimed to eliminate *C:\Program Files\Python36\lib\site-packages\urllib3\connectionpool.py:1020: InsecureRequestWarning: Unverified HTTPS request is being made to host 'minio.sample.com'. Adding certificate verification is strongly advised. See: https://urllib3.readthedocs.io/en/latest/advanced-usage.html#ssl-warnings InsecureRequestWarning* message.

## Resources

* [Boto3 documentation](https://boto3.amazonaws.com/v1/documentation/api/latest/index.html)
* [Boto3 - The AWS SDK for Python](https://github.com/boto/boto3) GitHub repository
