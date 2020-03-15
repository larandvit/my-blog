Title: Create keytab File for Kerberos Authentication in Windows
Date: 2020-03-15
Category: Kerberos, Windows, Authentication, Keytab

Windows has a limited set of tools to create a keytab file. There are a couple of tools for this purpose. One tool is the Windows Server built-in utility **ktpass**. It can be only run on a Windows Server. Another tool is **ktab** which can be used on any Windows computer. **ktab** tool is a part of Java installation.

## 1. ktpass

There are some restrict requirements to run the tool. It must be run on either a member server or a domain controller of the Active Directory domain. Windows Server operating system such as Windows Server 2008, 2012, or 2016 are supported. When running ktpass.exe, Windows Command Prompt must be run with **Run as administrator** option.

    :::bash
    ktpass -princ [Windows user name]@[Realm name] -pass [Password] -crypto [Encryption type] -ptype [Principle type] -kvno [Key version number] -out [Keytab file path]

* [Windows user name] - mywindowsname
* [Real name] - SAMPLE.COM
* [Password] - mywindowsname user password
* [Encryption type] - RC4-HMAC-NT
* [Principle type] - KRB5\_NT_PRINCIPAL which is Kerberos protocol 5
* [Key version number] - 0
* [Keytab file path] - c:\kerberos\keytabname.keytab

Available encryption types as per [RFC 3961, section 8](https://tools.ietf.org/html/rfc3961#section-8).

<table style="border-collapse: collapse; border: 1px solid black;">
<tr style="border: 1px solid black;"><th style="padding:5px;border: 1px solid black;">Encryption Type</th><th style="padding:5px;border: 1px solid black;">Code</th><th style="padding:5px;border: 1px solid black;">Section or Comment</th></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des-cbc-crc</td><td style="padding:5px;border: 1px solid black;">1</td><td style="padding:5px;border: 1px solid black;">6.2.3</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des-cbc-md4</td><td style="padding:5px;border: 1px solid black;">2</td><td style="padding:5px;border: 1px solid black;">6.2.2</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des-cbc-md5</td><td style="padding:5px;border: 1px solid black;">3</td><td style="padding:5px;border: 1px solid black;">6.2.1</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">[reserved]</td><td style="padding:5px;border: 1px solid black;">4</td><td style="padding:5px;border: 1px solid black;"></td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des3-cbc-md5</td><td style="padding:5px;border: 1px solid black;">5</td><td style="padding:5px;border: 1px solid black;"></td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">[reserved]</td><td style="padding:5px;border: 1px solid black;">6</td><td style="padding:5px;border: 1px solid black;"></td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des3-cbc-sha1</td><td style="padding:5px;border: 1px solid black;">7</td><td style="padding:5px;border: 1px solid black;"></td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">dsaWithSHA1-CmsOID</td><td style="padding:5px;border: 1px solid black;">9</td><td style="padding:5px;border: 1px solid black;">(pkinit)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">md5WithRSAEncryption-CmsOID</td><td style="padding:5px;border: 1px solid black;">10</td><td style="padding:5px;border: 1px solid black;">(pkinit)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">sha1WithRSAEncryption-CmsOID </td><td style="padding:5px;border: 1px solid black;">11</td><td style="padding:5px;border: 1px solid black;">(pkinit)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">rc2CBC-EnvOID</td><td style="padding:5px;border: 1px solid black;">12</td><td style="padding:5px;border: 1px solid black;">(pkinit)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">rsaEncryption-EnvOID</td><td style="padding:5px;border: 1px solid black;">13</td><td style="padding:5px;border: 1px solid black;">(pkinit from PKCS#1 v1.5)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">rsaES-OAEP-ENV-OID</td><td style="padding:5px;border: 1px solid black;">14</td><td style="padding:5px;border: 1px solid black;">(pkinit from PKCS#1 v2.0)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des-ede3-cbc-Env-OID</td><td style="padding:5px;border: 1px solid black;">15</td><td style="padding:5px;border: 1px solid black;">(pkinit)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">des3-cbc-sha1-kd</td><td style="padding:5px;border: 1px solid black;">16</td><td style="padding:5px;border: 1px solid black;">6.3</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">aes128-cts-hmac-sha1-96</td><td style="padding:5px;border: 1px solid black;">17</td><td style="padding:5px;border: 1px solid black;">[KRB5-AES]</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">aes256-cts-hmac-sha1-96</td><td style="padding:5px;border: 1px solid black;">18</td><td style="padding:5px;border: 1px solid black;">[KRB5-AES]</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">rc4-hmac</td><td style="padding:5px;border: 1px solid black;">23</td><td style="padding:5px;border: 1px solid black;">(Microsoft)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">rc4-hmac-exp</td><td style="padding:5px;border: 1px solid black;">24</td><td style="padding:5px;border: 1px solid black;">(Microsoft)</td></tr>
<tr style="border: 1px solid black;"><td style="padding:5px;border: 1px solid black;">subkey-keymaterial</td><td style="padding:5px;border: 1px solid black;">65</td><td style="padding:5px;border: 1px solid black;">(opaque; PacketCable)</td></tr>
</table><br>

For example,

* Windows account 

   Account properties dialog contains the next options for Kerberos authentication. 

  ![Windows account properties Kerberos authentication options]({static}/images/create_keytab-file-for-kerberos-authentication-in-windows/windows-account-properties-kerberos-authentication-options.png)</br></br>

## 2. ktab

It requests to install Java JRE or SDK or open source equivalent, for example, OpenJDK. The tool has a limited set of options.

It can't be defined encryption and principle types. It will be used Kerberos protocol 5 and it will be created multiple encryption types.

    :::bash
    ktab -a [Windows user name]@[Realm name] [Password] -n [Key version number] -k [Keytab file path]

List all encryption types stored in a keytab file

    :::bash
    ktab -l -e -k [Keytab file path]

If multiple encryption types are not accepted in authentication process, it can be left one encryption type and the rest can be deleted. 

    :::bash
    ktab -d [Windows user name]@[Realm name] -f -e [Number of encryption type] -k [Keytab file path]

* [Number of encryption type] - 16. As per RFC 3961, section 8.
