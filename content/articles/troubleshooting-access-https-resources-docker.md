Title: Troubleshooting Access to HTTP/HTTPS Resources in Docker
Date: 2021-07-27
Category: Docker
Cover: /extra/docker-logo.png

As containerization is trending now, you might encounter some issues with setting up software to access HTTP/HTTPS resource in Docker. For example, if you are installing a [Hive Standalone Metastore for Trino]({filename}/articles/standalone-hive-metastore-presto-docker.md) in Docker, Hive metasrore might need access to MinIO storage and PostgreSQL database. The access to MinIO can be done with HTTP or secured HTTP protocols. The same technic is applicable to other software used in Docker.

## 1. Make sure that container is running

    :::bash
    docker container ls

## 2. Connect to container

* If a container is running already

        :::bash
        docker exec -it [container name] bash

* if a container is not running yet

        :::bash
        docker run -it [container name] bash

## 3. Access to host from Docker

    :::bash
    ping sample.org

Output

    :::text
    PING sample.org (64.99.80.121) 56(84) bytes of data.
    64 bytes from realnames.com (64.99.80.121): icmp_seq=1 ttl=243 time=71.4 ms
    64 bytes from realnames.com (64.99.80.121): icmp_seq=2 ttl=243 time=79.8 ms
    64 bytes from realnames.com (64.99.80.121): icmp_seq=3 ttl=243 time=61.7 ms

## 4. Run diagnostic in Docker

* Issue to connect to the resource

        :::bash
        curl -v sample.org:883

    Or

        :::bash
        curl --verbose sample.org:883

    Output

        :::text
        * About to connect() to sample.org port 883 (#0)
        *   Trying 64.99.80.121...
        * Connection refused
        * Failed connect to sample.org:883; Connection refused
        * Closing connection 0
        curl: (7) Failed connect to sample.org:883; Connection refused

* Connection is successful for HTTP

        :::bash
        curl -v sample.org

    *Default port is 80.

    Output

        :::text
        * About to connect() to sample.org port 80 (#0)
        *   Trying 64.99.80.121...
        * Connected to sample.org (64.99.80.121) port 80 (#0)
        > GET / HTTP/1.1
        > User-Agent: curl/7.29.0
        > Host: sample.org
        > Accept: */*
        > 
        < HTTP/1.1 200 OK
        < Server: nginx/1.6.2
        < Date: Wed, 28 Jul 2021 02:24:48 GMT
        < Content-Type: text/html; charset=utf-8
        < Transfer-Encoding: chunked
        < Connection: keep-alive
        < Vary: Accept-Encoding
        < X-Frame-Options: SAMEORIGIN
        < X-XSS-Protection: 1; mode=block
        < X-Content-Type-Options: nosniff
        < ETag: W/"3fa3474c0ba2674deb1c00e2999534d1"
        < Cache-Control: max-age=0, private, must-revalidate
        < X-Request-Id: 51daaf75baa3f45054c5516923b8e12c
        < X-Runtime: 0.019993
        < P3P: CP="IDC DSP COR ADM DEVi TAIi PSA PSD IVAi IVDi CONi HIS OUR IND CNT"
        < 
        <!DOCTYPE html>
        <html>
        <head>
        ...
        </body>
        </html>
        * Connection #0 to host sample.org left intact

* Connection is successful for HTTPS

        :::bash
        curl -v https://s3.amazonaws.com

    Output

        :::text
        * About to connect() to s3.amazonaws.com port 443 (#0)
        *   Trying 52.217.38.94...
        * Connected to s3.amazonaws.com (52.217.38.94) port 443 (#0)
        * Initializing NSS with certpath: sql:/etc/pki/nssdb
        *   CAfile: /etc/pki/tls/certs/ca-bundle.crt
          CApath: none
        * SSL connection using TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256
        * Server certificate:
        * 	subject: CN=s3.amazonaws.com,O="Amazon.com, Inc.",L=Seattle,ST=Washington,C=US
        * 	start date: Jun 23 00:00:00 2021 GMT
        * 	expire date: Jul 24 23:59:59 2022 GMT
        * 	common name: s3.amazonaws.com
        * 	issuer: CN=DigiCert Baltimore CA-2 G2,OU=www.digicert.com,O=DigiCert Inc,C=US
        > GET / HTTP/1.1
        > User-Agent: curl/7.29.0
        > Host: s3.amazonaws.com
        > Accept: */*
        > 
        < HTTP/1.1 307 Temporary Redirect
        < x-amz-id-2: fRm7Lvqpofrkn0IElv2QHsE+4J34eVigkg13gSB9oiBUbVwVWm9APdQ5pudCaAPn/DbK+xXH9/s=
        < x-amz-request-id: B3340YVNDXF5C17X
        < Date: Wed, 28 Jul 2021 02:32:41 GMT
        < Location: https://aws.amazon.com/s3/
        < Server: AmazonS3
        < Content-Length: 0
        < 
        * Connection #0 to host s3.amazonaws.com left intact

* Connection is successful in case of not entering requested login credentials

        :::bash
        curl -v https://storage.googleapis.com

    Output

        :::text
        * About to connect() to storage.googleapis.com port 443 (#0)
        *   Trying 172.217.164.208...
        * Connected to storage.googleapis.com (172.217.164.208) port 443 (#0)
        * Initializing NSS with certpath: sql:/etc/pki/nssdb
        *   CAfile: /etc/pki/tls/certs/ca-bundle.crt
          CApath: none
        * SSL connection using TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256
        * Server certificate:
        * 	subject: CN=*.storage.googleapis.com,O=Google LLC,L=Mountain View,ST=California,C=US
        * 	start date: Jun 28 03:04:25 2021 GMT
        * 	expire date: Sep 20 03:04:24 2021 GMT
        * 	common name: *.storage.googleapis.com
        * 	issuer: CN=GTS CA 1O1,O=Google Trust Services,C=US
        > GET / HTTP/1.1
        > User-Agent: curl/7.29.0
        > Host: storage.googleapis.com
        > Accept: */*
        > 
        < HTTP/1.1 400 Bad Request
        < X-GUploader-UploadID: ADPycdvqEm57jFatvBvI0Up9BvFXb0C9rZkiTP4NAX4izSnzcu-Van59elCfXihl9_BxQ-9RS9Iic4hW6-AOUzxtV5gHCkNgtA
        < Content-Type: application/xml; charset=UTF-8
        < Content-Length: 181
        < Date: Wed, 28 Jul 2021 02:38:17 GMT
        < Expires: Wed, 28 Jul 2021 02:38:17 GMT
        < Cache-Control: private, max-age=0
        < Server: UploadServer
        < Alt-Svc: h3=":443"; ma=2592000,h3-29=":443"; ma=2592000,h3-T051=":443"; ma=2592000,h3-Q050=":443"; ma=2592000,h3-Q046=":443"; ma=2592000,h3-Q043=":443"; ma=2592000,quic=":443"; ma=2592000; v="46,43"
        < 
        * Connection #0 to host storage.googleapis.com left intact
        <?xml version='1.0' encoding='UTF-8'?><Error><Code>MissingSecurityHeader</Code><Message>Your request was missing a required header.</Message><Details>Authorization</Details></Error>

## Alternative diagnostic tool

    :::bash
    wget sample.org

## Resources
   * [curl.1 the man page](https://curl.se/docs/manpage.html)
   * [Docker documentation](https://docs.docker.com/)
