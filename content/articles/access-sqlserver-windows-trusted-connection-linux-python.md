Title: Access SQL Server with Windows Trusted Connection in Linux with Python
Date: 2021-08-15
Modified: 2021-08-16
Category: Python, MS SQL Server
Cover: /extra/python-logo.png

Integration of Windows and Linux environments is common in many companies. One of the scenarios is when Windows Active Directory serves for user authentication and Linux servers or containers are used for running applications. Also, MS SQL Server is utilized for storing data. 
There are some ways to connect to MS SQL Server in that case. 

The first method is the traditional one to access to a SQL Server data with a SQL Server account. It is an easy way which does not request any special efforts. The second way is more complicated with using a kerberized Active Directory account and Windows trusted connection to SQL Server. We need to create a Kerberos keytab which contains Windows service account credentials and generate a Kerberos ticket based on the Kerberos keytab. This method might not work if we want to know who access our SQL Server data as each user is going to connect to SQL Server with the same Windows service account. 

Let's try to supply our applications with the current user Windows credentials for accessing of SQL Server data. Our issue is that Python data access libraries are fed with SQL Server accounts or access data with Windows trusted connection. Windows trusted connection works perfectly in Windows but Linux environment behaves differently.

To achieve access to SQL Server with user Windows credentials, we follow the steps.

1. Receive Windows user name and password.
3. Create a Kerberos ticket.
4. Access data with Windows trusted connection.

You need to think how to pass user credentials in your application pipeline safely without compromising it. It might be many moving parts with possibility of revealing those credentials.

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

## Sample

    :::python
    import pyodbc
        
    import subprocess
    import getpass
    
    def create_kerberos_ticket(user_name, domain_name, user_password):
        
        ssh = subprocess.Popen(["kinit", f'{user_name}@{domain_name}'],
                            stdin =subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            universal_newlines=True,
                            bufsize=0)
     
        ssh.stdin.write(f"{user_password}\n")
        ssh.stdin.write("exit\n")
        ssh.stdin.close()
        
    def sqlserver_connection():
        return pyodbc.connect('Driver={ODBC Driver 13 for SQL Server}' + \
                              ';Server=' + 'sqlserver.sample.com' + \
                              ';Database=' + 'sample_database' + \
                              ';Trusted_Connection=yes')
    
    if __name__ == '__main__':
        
        user_name = 'sampleuser'
        user_password = getpass.getpass(f'Enter {user_name} Windows password: ')
        domain_name = 'SAMPLE.COM'
            
        create_kerberos_ticket(user_name, domain_name, user_password)
        
        cur = sqlserver_connection().cursor()
                
        sql_text = 'SELECT TOP 1 * ' \
                   'from sample_table'
                   
        cur.execute(sql_text)
        row = cur.fetchone()
        
        print(row)

## Troubleshooting

* Error message

        :::text
        pyodbc.Error: ('HY000', '[HY000] [unixODBC][Microsoft][ODBC Driver 13 for SQL Server]SSPI Provider: No Kerberos credentials available (default cache: FILE:/tmp/krb5cc_1000) (851968) (SQLDriverConnect)')

    One of the reasons is incorrect password. In that case, a Kerberos ticket is not created.

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
* [How to Execute Shell Commands with Python](https://janakiev.com/blog/python-shell-commands/)
* [Microsoft ODBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/odbc/microsoft-odbc-driver-for-sql-server)
* [Create Ticket Cache File for Kerberos Authentication in Linux ]({filename}/articles/create-ticket-cache-kerberos-authentication-linux.md)
