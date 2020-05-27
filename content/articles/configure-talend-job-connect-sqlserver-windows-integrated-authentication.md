Title: Configure Talend Job to Connect to SQL Server with Windows Integrated Authentication
Date: 2020-05-27
Category: Talend
Cover: /extra/talend-logo.png

The main way to access MS SQL Server in Talend is SQL Server authentication when user name and password must be supplied. SQL Server credentials are compromised as password is in plain text. In some cases, this is only a way to connect to SQL Server. There is another option to use Windows Integrated authentication. It's safe way because Talend jobs don't contain either user name or password. Credentials are provided by Windows Operation System or another service in background. Windows Integrated authentication is common in Windows environment as it's used Active Directory.

## 1. Download `jTDS`  [SQL Server and Sybase JDBC driver](https://sourceforge.net/projects/jtds/)

## 2. Extract `ntlmauth.dll` library from the driver package. 

As Talend is 64 bit now, it has to be 64 bit library as well.

    :::text
    jtds-1.3.1-dist
    │
    └───x64
        └───SSO
             ntlmauth.dll

## 3. Place the extracted library to Talend root folder

![ntlmauth.dll library in Talend root folder]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/library-in-talend-folder.png)</br></br>

## 4. Set Up tMSSqlInput Talend Component

There are 2 options. The first one is to set up tMSSqlInput component directly and another one uses tMSSqlInput component in conjunction with database connection.

Option #1.

* tMSSqlInput component settings. Leave Username and password empty with double quotes. The advanced settings has to include `IntegratedSecurity=true` in double quotes.

![tMSSqlInput Talend component basic settings]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/component-basic-settings-direct-tmssqlinput.png)</br></br>

![tMSSqlInput Talend component advanced settings]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/component-advanced-settings-direct-tmssqlinput.png)</br></br>

Option #2

* Create a new database connection. Leave Username and password blank. Add `IntegratedSecurity=true` to Additional parameters.

![New Talend database connection]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/database-connection.png)</br></br>

* MSSqlInput component settings with the database connection.

![tMSSqlInput Talend component basic settings]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/component-basic-settings-tmssqlinput-with-dbconnection.png)</br></br>

![tMSSqlInput Talend component advanced settings]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/component-advanced-settings-tmssqlinput-with-dbconnection.png)</br></br>

## 5. Testing

* Create 2 jobs. Each job contains 2 components: (1) tMSSqlInput and (2) tJavaRow.

![Talend job to test Windows Integrated authentication]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/talend-job-direct-tmssqlinput-component.png)</br></br>

![Talend job to test Windows Integrated authentication]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/talend-job-tmssqlinput-component-with-dbconnection.png)</br></br>

* Change settings of tMSSqlInput component as per **Set Up tMSSqlInput Talend Component** accordingly.

* Change settings of tJavaRow component as below.

![tJavaRow Talend component settings]({static}/images/configure-talend-job-connect-sqlserver-windows-integrated-authentication/tjavarow-component.png)</br></br>






