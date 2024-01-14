Title: DBeaver/Eclipse "File is out of date" SVN Error
Date: 2024-01-13
Category: DBeaver, Git
Cover: /extra/dbeaver-logo.png

DBeaver is Eclipse based Integrated Development Environment (IDE). It can be used with varieties if plug-ins. Subversion (SVN) is one of the plug-ins with source control functionality. It is integrated seamlessly with DBeaver, but Eclipse marketplace is supplied with old version of SVN library. It causes `File is out of date` error in SVN commits. Errors might be different nature and they have different remedies.

## Samples of errors

* Sample #1

        :::text
        svn: E160024: Commit failed (details follow):
        svn: E160024: File or directory '...' is out of date; try updating
        svn: E160024: resource out of date; try updating
        svn: E175002: CHECKOUT of '...': 409 Conflict (...)

* Sample #2

        :::text
        svn commit has countered a problem org.apache.subversion.javahl.ClientException: 
        svn: E155011: Commit failed (details follow): svn: E155011: File is out of date

## Update to Header remedy

Update your Eclipse project to header.

![Update Eclipse project to header]({static}/images/dbeaver_eclipse_file_is_out_of_date_svn_error/update-eclipse-project-to-header.jpg)</br></br>

## Upgrade SVNKit and JavaHL libraries remedy

SVN plug-in installation from Eclipse Market place is [Subversive - SVN Team Provider](https://marketplace.eclipse.org/content/subversive-svn-team-provider). "Subversive - SVN Team Provider" [https://download.eclipse.org/technology/subversive/4.3/release/latest/](https://download.eclipse.org/technology/subversive/4.8/release/latest/) contains 1.10.1 old version of `SVNKit` library. Installing at least version 1.10.6 library might solve the issue. The updated version of library is [http://eclipse.svnkit.com/1.10.x/](http://eclipse.svnkit.com/1.10.x/). Also, it can be upgraded `JavaHL` library. 

Select **Install New Software** from **Help**.

![Upgrade SVNKit library]({static}/images/dbeaver_eclipse_file_is_out_of_date_svn_error/upgrade-svnkit-library.jpg)</br></br>

## Change library to JavaHL remedy

SVN plug-in installation from Eclipse Market place is [Subversive - SVN Team Provider](https://subclipse.github.io/updates/subclipse/4.3.x/). There are 2 libraries included: `SVNKit` or `JavaHL`. You can select `JavaHL` library in **Preferencies**.

![DBeaver/Eclipse SVN libraries]({static}/images/dbeaver_eclipse_file_is_out_of_date_svn_error/dbeaver-eclipse-svn-libraries.jpg)</br></br>

## Resources

* [Stack overflow](https://stackoverflow.com/questions/87950/how-do-you-overcome-the-svn-out-of-date-error)
* [svn: E155011: Commit failed](https://github.com/subclipse/subclipse/issues/237)
* [Eclipse Update Site](https://svnkit.com/download.php)
