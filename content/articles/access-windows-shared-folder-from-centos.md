Title: Access Windows 10 Shared Folder from RHEL/CentOS 7
Date: 2020-08-16
Category: Windows, Linux
Cover: /extra/centos-logo.png

Heterogeneous networks are common now. Those networks include computers with Windows and Unix Operation Systems (OS). Exchange files between those Operation Systems is a trivial task. It can be done with command line interface (CLI) or Graphic User Interface (GUI). Both methods give the same results but it depends on usage, user technical level, and so on which method is suitable.

## Access with CLI

1. Install Samba client and other related libraries on your Linux computer.

        :::bash
        sudo yum -y install samba-client samba-common cifs-utils

2. Create mount point

        :::bash
        sudo mkdir -p /mnt/F_drive

3. Check connection to Windows server.

        :::bash
        smbclient -L //window_server -U user_name

4. Access Windows shared folder

        :::bash
        sudo mount.cifs //window_server/F_shared_drive /mnt/F_drive -o rw,username=user,file_mode=0777,dir_mode=0777

    Parameters

    * rw - read write access
    * ro - read only access
    * file_mode=0777,dir_mode=0777 - Linux access to shared folder. The sample grants full access to everybody.<br><br>

5. Troubleshooting

    * **Invalid argument** error

            :::text
            mount error(22): Invalid argument
            Refer to the mount.cifs(8) manual page (e.g. man mount.cifs)
    
        Possible issue is missing shared folder in UNC: `//window_server` rather than `//window_server/F_shared_drive`.

    * **Device or resource busy** error

            :::text
            mount error(16): Device or resource busy
            Refer to the mount.cifs(8) manual page (e.g. man mount.cifs)

        Possible issue is that your mount is used. You need to unmount your mount point, for example, 

            :::bash
            sudo umount /mnt/F_drive

    * **Could not resolve address** error

            :::text
            mount error: could not resolve address for window_server: Unknown error

        Possible issue is shared folder computer name. You need to replace name with IP address, for example, replace `//window_server` with `192.168.0.2`.

## Access with GUI

`Files` is a default file manager in GNOME desktop and it has embedded Samba client.

1. Open `Files`, go to `Other Locations`, and type in your shared folder path, for example, //window_server/F_shared_drive

    ![CentOS Files file manager Other Locations]({static}/images/access-windows-shared-folder-from-centos/files-other-locations-form.png)</br></br>

2. Fill out final connection form

    ![CentOS Files file manager Other Locations]({static}/images/access-windows-shared-folder-from-centos/files-final-connection-form.png)</br></br>

3. Troubleshooting

    In case if receiving `Unable to access location` error, it might be some reasons.

    1) Address can not be resolved and you need to use IP address rather than computer name, for example, `192.168.0.2`.<br>
    2) It is used the SMB version 1 protocol in Files file manager. You might need to try one of the options below.

       a) Update your computer 

        :::bash
        sudo yum update

       b) Downgrade SMB protocol to version 1 in you your Windows server.

       * Open `Windows Explorer` and key in `Control Panel\Programs` in address bar

          ![Windows Explorer Control Panel Program]({static}/images/access-windows-shared-folder-from-centos/windows-explorer-control-panel-programs.png)</br></br>

       * Go to `Turn Windows features on or off`

          ![Turn Windows features on or off]({static}/images/access-windows-shared-folder-from-centos/turn-windows-features-on-off.png)</br></br>

       * Check `SMB 1.0/CIFS Server` setting

          ![Windows SMB file sharing support]({static}/images/access-windows-shared-folder-from-centos/windows-smb-file-sharing-support.png)</br></br>

## Resources

* [Simple file manager for GNOME](https://wiki.gnome.org/action/show/Apps/Files?action=show&redirect=Apps%2FNautilus)
