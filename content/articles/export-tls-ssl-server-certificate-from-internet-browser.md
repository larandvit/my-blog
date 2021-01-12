Title: Export TLS/SSL Server Certificate from Internet Browser
Date: 2021-01-10
Modified: 2021-01-11
Category: Security
Cover: /extra/security-logo.jpg

To securely browse the Web, HTTPS protocol is established. The secure protocol requests public certificate which is freely transmitted from a Web site to a client. The public certificate can be exported from Internet browser in Privacy-Enhanced Mail (PEM) format. If a certificate is exported from PKCS #12 or other formats, it includes Bag Attributes. Bag Attributes can be recreated from an Internet browser certificate information as well and they might be requested in some tools or software libraries to access a Web site in safely manner.

A sample of a PEM certificate.

    :::text
    -----BEGIN CERTIFICATE-----
    MIIFOTCCBCGgAwIBAgISBIAlOcXvyAyEBqj0ULzsgUPOMA0GCSqGSIb3DQEBCwUA
    MDIxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1MZXQncyBFbmNyeXB0MQswCQYDVQQD
    EwJSMzAeFw0yMDEyMjYwMjUwNDFaFw0yMTAzMjYwMjUwNDFaMBoxGDAWBgNVBAMT
    D3RlY2hqb2dnaW5nLmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
    AMxMmigFj121UY1CWskO/3BB/xYaGYKWVjdcBeyRGTacnq7pXxngC8O3Q7hCIhFF
    zyWFQH+XWTobFtbkMi1xW+igHXZ4bPkI84PsfxjsXH8jTam2n7naJ1NW2XBuw4Bj
    WTvSdPti5LJsK7sbb0VpHbsH2q+WhPtjxxep0e+ZZCWl6BGK1/wRH588Y2ECH1RQ
    uLPgyTsuXcqlm3Wygfr7ikyfMu00aVuhPXlkob65VknYR+AvktYe4UEr0+QazGP4
    9J9gvqYBLK2SrgA8FlWjnarE0/ouyPdzeSwA9iyhNKiffAN2HkLig+Sy8/K6L2zh
    QZcGr4Z7XUKBmBWbqXHyY7MCAwEAAaOCAl8wggJbMA4GA1UdDwEB/wQEAwIFoDAd
    BgNVHSUEFjAUBggrBgEFBQcDAQYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNV
    HQ4EFgQUT5d3Rw4gZGWvSw5zb21jRv40gSkwHwYDVR0jBBgwFoAUFC6zF7dYVsuu
    UAlA5h+vnYsUwsYwVQYIKwYBBQUHAQEESTBHMCEGCCsGAQUFBzABhhVodHRwOi8v
    cjMuby5sZW5jci5vcmcwIgYIKwYBBQUHMAKGFmh0dHA6Ly9yMy5pLmxlbmNyLm9y
    Zy8wLwYDVR0RBCgwJoIPdGVjaGpvZ2dpbmcuY29tghN3d3cudGVjaGpvZ2dpbmcu
    Y29tMEwGA1UdIARFMEMwCAYGZ4EMAQIBMDcGCysGAQQBgt8TAQEBMCgwJgYIKwYB
    BQUHAgEWGmh0dHA6Ly9jcHMubGV0c2VuY3J5cHQub3JnMIIBBAYKKwYBBAHWeQIE
    AgSB9QSB8gDwAHYAXNxDkv7mq0VEsV6a1FbmEDf71fpH3KFzlLJe5vbHDsoAAAF2
    nSt67wAABAMARzBFAiEAi+1Q1m/Oxyp5cB2POMz9utvZGq9KSg4/OarZA6vtM1kC
    IGdvKThenxh7v2LRfgmlUcWcxRq16q4ZB78OSF8WSbz2AHYAfT7y+I//iFVoJMLA
    yp5SiXkrxQ54CX8uapdomX4i8NcAAAF2nSt7CwAABAMARzBFAiBbaSk5tCZgx7bC
    UIXJVayOIA9Nsif654YSGtWa+DdwoQIhAPo3YId+rVlUHgf6/ucJsZd45Hj3mxb1
    DQOVHM0hdIT+MA0GCSqGSIb3DQEBCwUAA4IBAQB5XTUi0NQhPPBXdakL/YPoH/50
    BmC6iadIx1g6bqW+QcPWmqOqLm1ZyCgYpliJdCX9Q6uHXVjDJNep4w31+v5Sh3YQ
    hiwAzZk4m5hJ/ZbWOHatgqew7jApi9cfJ++lHXPC8C0oY4bNOvbU9x5ygSbpzsCn
    nqQH74SSvn49UtZZ/a2gSt+854daHPRqVHwnV7FhTttpeuSyEmkc3ocpZ2uxTgFC
    CAOcGrAzYIwfxzsTv7l3+L+a921vjei6KpLLw0z+DvzWAeMNRbEUL2IqD7EN+5Zm
    SNEXHPbcvGKXBueNoAzCQFmHoh9wUy2mPN2/wmHxfGgt+U6ykDGU5FIiKVtO
    -----END CERTIFICATE-----

A sample of a PEM certificate with Bag Attributes.

    :::text
    Bag Attributes
        friendlyName: techjogging.com
        localKeyID: 53 64 6A 61 20 33 36 30 39 36 35 38 38 33 31 31 31 32 
    subject=/CN=techjogging.com
    issuer=/C=US/O=Let's Encrypt/CN=R3
    -----BEGIN CERTIFICATE-----
    MIIFOTCCBCGgAwIBAgISBIAlOcXvyAyEBqj0ULzsgUPOMA0GCSqGSIb3DQEBCwUA
    ...
    SNEXHPbcvGKXBueNoAzCQFmHoh9wUy2mPN2/wmHxfGgt+U6ykDGU5FIiKVtO
    -----END CERTIFICATE-----

## FireFox browser

* Click the lock (padlock) icon in the address bar.

    ![FireFox padlock icon]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/firefox-padlock-icon.jpg)</br></br>

* Click **Show connection details** button.

    ![FireFox show connection details]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/firefox-show-connection-details.jpg)</br></br>

* Select **More Information** menu item.

    ![FireFox more information]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/firefox-more-information.jpg)</br></br>

* Click **View Certificate** on **Security** tab.

    ![FireFox view certificate]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/firefox-view-certificate.jpg)</br></br>

* Finally, click **PEM (cert)** link in Miscellaneous section.

    ![FireFox download PEM]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/firefox-download-pem.jpg)</br></br>

## Google Chrome browser

* Click the lock (padlock) icon in the address bar.

    ![Chrome padlock icon]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/chrome-padlock-icon.jpg)</br></br>

* Select **Certificate** menu item.

    ![Chrome certificate]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/chrome-certificate.jpg)</br></br>

* Click **Copy to file...** on **Details** tab.

    ![Chrome certificate details]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/chrome-certificate-details.jpg)</br></br>

* Follow **Certificate Export Wizard** steps. Select **Base-64 encoded X.509 (.CER)** format.

    ![Chrome file export format]({static}/images/export-tls-ssl-server-certificate-from-internet-browser/chrome-file-export-format.jpg)</br></br>

## Microsoft Edge browser

Export steps are the same as for Google Chrome browser.

## Microsoft Internet Explorer browser

Export steps are the same as for Google Chrome browser. The padlock icon is located on the right side of the address bar.
