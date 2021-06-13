Title: Create SSH Key in DBeaver CE
Date: 2021-06-12
Category: DBeaver, Security
Cover: /extra/dbeaver-logo.png

SSH keys are widely used to secure access to sensitive information. They provide both relatively easy implementation and better security than passwords. Generating of SSH keys can be done with different tools. In some cases, the process is complicated and requests manual interaction. DBeaver Community Edition (CE) utilizes SSH keys, for example, access GitHub repositories. As DBeaver is Eclipse based Integrated Development Environment (IDE), it includes functionality to generate a pair of SSH keys and set it up for usage. It is a transparent and trivial process.

The article uses DBeaver 21.1.0 CE installed in Windows.

## 1. Go to Window menu

![DBeaver Preferences in Windows menu]({static}/images/create-ssh-key-dbeaver-ce/dbeaver-windows-menu-preferences.jpg)</br></br>

## 2. Open Preferences menu item and find SSH2

![DBeaver SSH2 in Preferences]({static}/images/create-ssh-key-dbeaver-ce/dbeaver-preferences-ssh2.jpg)</br></br>

## 3. Switch to Key Management tab and click Generate RSA Key

![DBeaver Generate SSH key pair]({static}/images/create-ssh-key-dbeaver-ce/dbeaver-generate-copy-public-key.jpg)</br></br>

## 4. Enter Passphrase and Confirm passphrase

## 5. Click Save Private Key button

## 6. Store generated public key for further distribution, for example, create a file with the key.

## 7. Click Close and Apply to finish it
