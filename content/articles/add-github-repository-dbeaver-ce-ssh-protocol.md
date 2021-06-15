Title: Add GitHub Repository to DBeaver CE Secured with SSH Protocol
Date: 2021-06-14
Category: DBeaver
Cover: /extra/dbeaver-logo.png

GitHub is a good addition to DBeaver Community Edition (CE). It allows to store scripts and other files in source control. Even with some limitations in DBeaver CE, GitHub full functionality can be utilized. There are varieties of ways connecting GitHub to DBeaver CE. SSH protocol provides with enhanced security and seamlessly integrated with DBeaver CE. In some cases, it can work when HTTPS one might be problematic.

The article uses DBeaver 21.1.0 CE installed in Windows.

## 1. Prerequisite. Create SSH key

Follow [Create SSH Key in DBeaver CE]({filename}/articles/create-ssh-key-dbeaver-ce.md) article.

## 2. Prerequisite. Install DBeaver Git extension

Follow [Install DBeaver CE Git Extension]({filename}/articles/install-dbeaver-ce-git-extension.md) article.

## 3. Open File menu and select Import menu item

![DBeaver Import Menu Item]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-menu-item.jpg)</br></br>

## 4. Expand Git node and select any item, for example, Projects from Git

![DBeaver Import Wizard Select]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-wizard-select.jpg)</br></br>

## 5. Select Clone URI

![DBeaver Import Wizard Select Repository Source]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-wizard-select-repository-source.jpg)</br></br>

## 6. Go to your repository in GitHub and copy SSH URL

![GitHub Repository Copy SSH URL]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/github-copy-ssh-url.jpg)</br></br>

## 7. Return back to DBeaver and click Next

## 8. Paste SSH URL to URI field. The rest of fields will be filled out automatically

Leave **Password** field empty.

![DBeaver Import Wizard Source Git Repository]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-wizard-source-git-repository.jpg)</br></br>

## 9. Accept and store key

![DBeaver Import Wizard Accept Store Key]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-wizard-accept-store-key.jpg)</br></br>

## 10. Confirm creating of know_hosts file

![DBeaver Import Wizard Confirm Adding to known_hosts File]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-wizard-confirm-known-hosts-file.jpg)</br></br>

## 11. Key in passphrase

![DBeaver Import Wizard Enter Keyphrase]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-enter-keyphrase.jpg)</br></br>

## 12. Select branch to import

![DBeaver Import Wizard Import Branch Selection]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-branch-selection.jpg)</br></br>

## 13. Pick up a destination directory or accept default one

![DBeaver Import Wizard Local Destination]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-local-destination.jpg)</br></br>

## 14. Select a wizard for importing your project

A general project should work for many cases.

![DBeaver Import Wizard Importing Projects]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-select-wizard-importing-projects.jpg)</br></br>

## 15. Final confirmation

![DBeaver Import Wizard Final Confirmation]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-import-final-confirmation.jpg)</br></br>

## 16. Switch to Projects tab to see the imported project

![DBeaver Switch to Projects Tab]({static}/images/add-github-repository-dbeaver-ce-ssh-protocol/dbeaver-switch-to-projects-tab.jpg)</br></br>


