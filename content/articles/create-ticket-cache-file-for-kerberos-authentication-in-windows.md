Title: Create Ticket Cache File for Kerberos Authentication in Windows
Date: 2020-05-10
Category: Kerberos
Cover: /extra/kerberos-logo.png

Kerberos ticket cache is one of the options to utilize Kerberos authentication in Windows. Another option is to use [Kerberos keytab file]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md). Kerberos ticket cache can be transparently consumed by many tools, whereas Kerberos keytab requests additional setup to plug in to tools. 

Kerberos ticket cache file default location and name are `C:\Users\windowsuser\krb5cc_windowsuser` and mostly tools recognizes it.

There are some tools and techniques to generate a ticket cache file.

## 1. Kinit Java tool

Make sure that Java JRE or SDK or open source equivalent, for example, [OpenJDK](https://openjdk.java.net/) is installed. Run `kinit` tool located in **C:\Program Files\Java\jre[version]\bin** folder. The folder name depends on JRE or SDK or 32 or 64 bit edition. It's assumed java 8 is installed in C:\Program Files\Java\jre1.8.0_192 folder.

  1. If Kerberos ticket cache is created for a user currently logged in to a Windows computer

        :::bash
        "C:\Program Files\Java\jre1.8.0_192\kinit"

     Output
 
        :::text
        Password for windowsuser@SAMPLE.COM:
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser

  2. If Kerberos ticket cache is created for a different user from currently logged in to a Windows computer

        :::bash
        "C:\Program Files\Java\jre1.8.0_192\kinit" windowsuser@SAMPLE.COM

     Output

        :::text
        Password for windowsuser@SAMPLE.COM:
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser

  3. Utilize Kerberos keytab file with kerberized Windows service account provided by your administrator.

        :::bash
        "C:\Program Files\Java\jre1.8.0_192\kinit" servicewindowsaccount@SAMPLE.COM -k -t C:\keytabfolder\keytabname.keytab

     Output

        :::text
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser
        
The created cache file can be validated with `klist` command

    :::bash
    "C:\Program Files\Java\jre1.8.0_192\klist"

Output

    :::text
    Credentials cache: C:\Users\windowsuser\krb5cc_windowsuser
    
    Default principal: windowsuser@SAMPLE.COM, 1 entry found.
    
    [1]  Service Principal:  krbtgt/SAMPLE.COM@SAMPLE.COM
         Valid starting:     Mar 26, 2020 21:35:00
         Expires:            Mar 27, 2020 07:35:00
  
## 2.MIT Kerberos software

MIT Kerberos can be loaded from [MIT Kerberos Distribution Page](https://web.mit.edu/kerberos/dist/). It includes command line and GUI tools. Because of coming from Unix environment, it doesn't understand the default location and the location should be explicitly  stated.

  1. If Kerberos ticket cache is created for a user currently logged in to a Windows computer

        :::bash
        "C:\Program Files\MIT\Kerberos\bin\kinit" -c C:\Users\windowsuser\krb5cc_windowsuser

     No output.

  2. If Kerberos ticket cache is created for a different user from currently logged in to a Windows computer

        :::bash 
        "C:\Program Files\MIT\Kerberos\bin\kinit" -c C:\Users\windowsuser\krb5cc_windowsuser windowsuser@SAMPLE.COM

     No output.

  3. Utilize Kerberos keytab file with kerberized Windows service account provided by your administrator.

        :::bash
        "C:\Program Files\MIT\Kerberos\bin\kinit" -k -t C:\keytabfolder\keytabname.keytab -c C:\Users\windowsuser\krb5cc_windowsuser servicewindowsaccount@SAMPLE.COM

    No output.

The created cache file can be validated with `klist` command

    :::bash
    "C:\Program Files\MIT\Kerberos\bin\klist" -c C:\Users\windowsuser\krb5cc_windowsuser

Output

    :::text
    Ticket cache: FILE:C:\Users\windowsuser\krb5cc_windowsuser
    Default principal: windowsuser@SAMPLE.COM

    Valid starting     Expires            Service principal
    05/09/20 22:39:22  05/10/20 08:39:22  krbtgt/krbtgt/SAMPLE.COM@SAMPLE.COM
            renew until 05/10/20 22:39:22

It can be applied some options to customize ticket cache, for example, `-r renewable_life`.

**MIT Kerberos Ticket Manager** is GUI tool. It can be run from Windows Start menu or from desktop or `C:\Program Files\MIT\Kerberos\bin\MIT Kerberos.exe`.

  1. Set up 'KRB5CCNAME' environment variable

     * Open **System Properties** entering `sysdm.cpl` in Windows Start

        ![Run System Properties]({static}/images/create-ticket-cache-file-for-kerberos-authentication-in-windows/entering-sysdm.cpl.png)</br></br>

     * Go to **Advanced** tab and click **Environment Variables...**
  
        ![System Properties]({static}/images/create-ticket-cache-file-for-kerberos-authentication-in-windows/system-properties.png)</br></br>

     * Add a new **System Variable**. Name: `KRB5CCNAME` and value: `C:\Users\windowsuser\krb5cc_windowsuser`.

        ![New system variable]({static}/images/create-ticket-cache-file-for-kerberos-authentication-in-windows/new-system-variable.png)</br></br>

     * Reboot computer to make it in effect.

  2. Run **MIT Kerberos Ticket Manager** 

     * Click **Get Ticket** and enter **Principal** and **Password**. Also, you can customize ticket properties.

        ![MIT Kerberos Get Ticket]({static}/images/create-ticket-cache-file-for-kerberos-authentication-in-windows/mit-kerberos-get-ticket.png)</br></br>

  3. Validate ticket location in **Credential Cache** column or `C:\Users\windowsuser\krb5cc_windowsuser` file.
