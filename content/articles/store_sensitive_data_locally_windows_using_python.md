Title: Store Sensitive Data Locally in Windows Using Python
Date: 2021-12-14
Category: Python, Security
Cover: /extra/python-logo.png

Some applications request to key in sensitive information each time when you run it. For example, a password to access MySql server. You can store it in a file or in a database, but it might violate your company security policy or it might add a level of complexity. Majority of Operation Systems (OS) include a secure storage for this purpose.

Credentials Manager is Windows implementation of the secure storage. It has a key which identifies a set of credentials. Each set contains two fields: username and password. It does not mean that you are supposed to store your credentials. You can store any sensitive data there.

Python keyring library is a portable library applicable across multiple OS. To store or retrieve your sensitive data, you need to supply service name or key and username or your stored value name.

The samples is developed in Python 3.9 with keyring library version 23.1.0.

## Installation

Run this command with Command Prompt in Run as Administrator mode.
    
    :::bash
    pip install keyring
    
The command below does not request admin privileges.
    
    :::bash
    pip install keyring --user
    
## Store and retrieve credentials

    :::python
    import getpass

    import keyring
    from keyring.backends import Windows
    keyring.set_keyring(Windows.WinVaultKeyring())

    app_name = 'sample'
    user_name = getpass.getuser()

    user_password =  keyring.get_password(app_name, user_name)

    if not user_password:
        user_password = getpass.getpass(f'Enter {user_name} Windows password: ')
        keyring.set_password(app_name, user_name, user_password)
        
    print(f'"{user_name}" user password for "{app_name}" application is "{user_password}".')

    
## Delete credentials

    :::python
    import getpass

    import keyring
    from keyring.backends import Windows
    keyring.set_keyring(Windows.WinVaultKeyring())

    app_name = 'sample'
    user_name = getpass.getuser()

    try:
        keyring.delete_password(app_name, user_name)
    except keyring.errors.PasswordDeleteError:
        pass

    print(f'"{user_name}" user password for "{app_name}" application has been deleted.')

## Resources
* [keyring](https://pypi.org/project/keyring/)
