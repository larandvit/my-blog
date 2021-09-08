Title: Access SQL Server with Trusted Connection in Linux with Python Using Kerberos Keytab
Date: 2021-09-07
Category: Python, MS SQL Server, Kerberos
Cover: /extra/python-logo.png

When supplying credentials in plain text in Python applications and tools are a concern of the security policy in your company, Kerberos keytab might be a relief. Kerberos keytab hides sensitive information and it serves as a password to authenticate access to different resources. In other words, a keytab is a password replacement.

The most common use case is a service account when an application utilizes a special account. All users who run an application are authenticated with the same service account. It is convenient way to access, for example, data located on SQL Server.

The sample code is developed in CentOS 7 with Python 3 and pyodbc library. 

## Prerequisites

   * Install Kerberos client.

        :::bash
        sudo yum install krb5-workstation krb5-libs

   * Install ODBC on non MS Windows platforms.

        :::bash
        sudo yum install unixODBC-devel

   * Install Microsoft ODBC driver for SQL Server.
      
      The sample uses ODBC driver version 13 but the latest version is 17. 

        :::bash
        sudo rpm -i https://packages.microsoft.com/rhel/7/prod/msodbcsql-13.1.9.2-1.x86_64.rpm

      If you decide for the latest ODBC driver, get one from [Download ODBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server).

   * Install Python pyodbc library

        :::bash
        pip install pyodbc

   * Create a Kerberos keytab
     
      Follow steps in [Create Keytab for Kerberos Authentication in Linux]({filename}/articles/create-keytab-file-kerberos-authentication-linux.md) or [Create Keytab for Kerberos Authentication in Windows]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md).

## Sample

    import pyodbc
    import os
    
    def createKerberosTicket(user_full_name, keytab_path):
        
        error_code = os.system(f'kinit {user_full_name} -k -t {keytab_path}')
        if (error_code != 0):
            message = 'kinit error: {}'.format(error_code)
            print(message)
            
        return error_code
    
    def sqlserver_connection():
        return pyodbc.connect("Driver={ODBC Driver 13 for SQL Server};Server=" + 'sqlserver.sample.com' + \
                                    ";Database=" + 'master' + \
                                    ";Trusted_Connection=yes")
        
    if __name__ == '__main__':
        
        user_name = 'sampleuser'
        keytab_path = f'/home/sampleuser/{user_name}.keytab'
        domain_name = 'SAMPLE.COM'
        user_full_name = f'{user_name}@{domain_name}'
            
        createKerberosTicket(user_full_name, keytab_path)
        
        cur = sqlserver_connection().cursor()
                
        sql_text = 'SELECT @@VERSION'
        cur.execute(sql_text)
        row = cur.fetchone()
        
        print(row[0])

## Troubleshooting

* Error message

        :::text
        pyodbc.OperationalError: ('HYT00', '[HYT00] [unixODBC][Microsoft][ODBC Driver 13 for SQL Server]Login timeout expired (0) (SQLDriverConnect)')

    One of the reasons is your SQL Server name. Make sure that it is used Fully Qualified Domain Name (FQDN), for example, `sqlserver.sample.com`.

* Check Kerberos ticket(s) created by an application. Open terminal and run commands.

        :::bash
        klist

    Or valid ticket(s).

        :::bash
        klist -s

* Missing Kerberos ticket cache file variable.

    It might be requested to create `KRB5CCNAME` variable with location and name of Kerberos ticket cache file. See more details in [Create Ticket Cache File for Kerberos Authentication in Linux ]({filename}/articles/create-ticket-cache-kerberos-authentication-linux.md) article.

* Missing krb5.conf Kerberos configuration file. 
  
    The file contains default realm and Kerberos ticket settings. See more details in [Create Ticket Cache File for Kerberos Authentication in Linux ]({filename}/articles/create-ticket-cache-kerberos-authentication-linux.md) article.

## Resources

* [pyodbc](https://github.com/mkleehammer/pyodbc)
* [kinit - MIT Kerberos Documentation](https://web.mit.edu/kerberos/krb5-1.12/doc/user/user_commands/kinit.html)
* [klist - MIT Kerberos Documentation](https://web.mit.edu/kerberos/krb5-1.12/doc/user/user_commands/klist.html)
* [Microsoft ODBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server)
* [Create Keytab for Kerberos Authentication in Linux]({filename}/articles/create-keytab-file-kerberos-authentication-linux.md)
* [Create Keytab for Kerberos Authentication in Windows]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md)
* [Create Ticket Cache File for Kerberos Authentication in Linux ]({filename}/articles/create-ticket-cache-kerberos-authentication-linux.md)
