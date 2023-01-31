Title: Expand Logical Volume in CentOS/RHEL 7
Date: 2020-01-29
Modified: 2023-01-30
Category: Linux
Cover: /extra/centos-logo.png

As Linux systems have root file systems on a logical volume, it can be used Logical Volume Management (LVM) to resize the volume. The exercise of logical volume expanding is completed in case of adding an extra disk to a physical system or having a pool of storage in a virtual environment.

1. Run **fdisk** or **gdisk** partition tool. **gdisk** is used if the partition layout is GPT otherwise **fdisk** has to be used. **gdisk** will make your system unbootable if you don't have GPT partition. Both tools work identically when a new partiton is created.

    How to figure out if GPT partition is present in your system? Assiming that **sda** is device with available space, run **gdisk /dev/sda** command.

    This screen shows that your partition is GPT.

        ::text
        $ gdisk /dev/sda

        GPT fdisk (gdisk) version 0.8.10

        Partition table scan:
          MBR: protective
          BSD: not present
          APM: not present
          GPT: present

        Found valid GPT with protective MBR; using GPT.

    In case of MBR partition your screen is.

        ::text
        $ gdisk /dev/sda

        GPT fdisk (gdisk) version 0.8.10

        Partition table scan:
          MBR: MBR only
          BSD: not present
          APM: not present
          GPT: not present

        ***************************************************************
        Found invalid GPT and valid MBR; converting MBR to GPT format
        in memory. THIS OPERATION IS POTENTIALLY DESTRUCTIVE! Exit by
        typing 'q' if you don't want to convert your MBR partitions
        to GPT format!
        ***************************************************************

2. Create a new logical volume partition. Enter **8E00** partition code.

        ::text
        Command (? for help): n

        Partition number (6-128, default 6): 
        First sector (31457280-52428766, default = 31457280) or {+-}size{KMGTP}: 
        Last sector (31457280-52428766, default = 52428766) or {+-}size{KMGTP}: 
        Current type is 'Linux filesystem'
        Hex code or GUID (L to show codes, Enter = 8300): 8E00
        Changed type of partition to 'Linux LVM'

    If you have received the message.

        ::text
        No free sectors available

    You need to fix the pointer running commands.

    a) Run expert mode.

        ::text
        Command (? for help): x

    b) Fix the pointer.

        ::text
        Expert command (? for help): e
        Relocating backup data structures to the end of the disk

    c) Return to main menu.

        ::text
        Expert command (? for help): m
        
    d) Validate available space, for example.

        ::text
        Command (? for help): p
        Total free space is 52428800 sectors (25.0 GiB)

    e) Rerun to create a new logical volume partition command. Enter **8E00** partition code.
   
        ::text
        Command (? for help): n

        Partition number (6-128, default 6): 
        First sector (31457280-52428766, default = 31457280) or {+-}size{KMGTP}: 
        Last sector (31457280-52428766, default = 52428766) or {+-}size{KMGTP}: 
        Current type is 'Linux filesystem'
        Hex code or GUID (L to show codes, Enter = 8300): 8E00
        Changed type of partition to 'Linux LVM'

3. Validate your new partition

        :::text
        Command (? for help): p

    The partion name must be `Linux LVM`. If not, change the type running change a partition's type code.

        :::text
        Command (? for help): t
        Partition number (1-3): 3
        Hex code or GUID (L to show codes, Enter = 8300): 8E00

4. Apply changes.

        ::text
        Command (? for help): w

        Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING
        PARTITIONS!!

        Do you want to proceed? (Y/N): Y
        OK; writing new GUID partition table (GPT) to /dev/sda.
        Warning: The kernel is still using the old partition table.
        The new table will be used at the next reboot.
        The operation has completed successfully.

5. Notify the operation system about changes in the partition tables.

        ::text
        $ partprobe

6. Validate the new created partition. It can be used either fdisk or gdisk partition tool

        ::text
        $ fdisk -l /dev/sda

        WARNING: fdisk GPT support is currently new, and therefore in an experimental phase. Use at your own discretion.

        Disk /dev/sda: 26.8 GB, 26843545600 bytes, 52428800 sectors
        Units = sectors of 1 * 512 = 512 bytes
        Sector size (logical/physical): 512 bytes / 512 bytes
        I/O size (minimum/optimal): 512 bytes / 512 bytes
        Disk label type: gpt
        Disk identifier: 71DD2E79-BD1C-4713-9880-22664C87E57B

        #         Start          End    Size  Type            Name
        1         2048      2099199      1G  Linux filesyste Linux filesystem
        2      2099200     16777215      7G  Linux LVM       Linux LVM
        3     16777216     20971519      2G  Linux LVM       Linux LVM
        4     20971520     31457279      5G  Linux LVM       Linux LVM
        5           34         2047   1007K  BIOS boot       BIOS boot partition
        6     31457280     52428766     10G  Linux LVM       Linux LVM

7. Find out what logical groups/volumes are available.

        ::text
        $ lvs

          LV   VG     Attr       LSize   Pool Origin Data%  Meta%  Move Log Cpy%Sync Convert
          root centos -wi-ao---- <13.19g
          swap centos -wi-ao---- 820.00m

8. Our intertest is to add space to the root file system. The logical volume path is **centos/root**. In case of RHEL, it might be **rhel/root**.

9. Create a physical volume.

        ::text
        $ pvcreate /dev/sda6

        WARNING: ext4 signature detected on /dev/sda6 at offset 1080. Wipe it? [y/n]: y
        Wiping ext4 signature on /dev/sda6.
        Physical volume "/dev/sda6" successfully created.

10. Extend **centos** volume group.

        ::text
        $ vgextend centos /dev/sda6
         
        Volume group "centos" successfully extended

11. Figure out exact free space in PE. The field name is **Free  PE / Size** and the value in the sample is **2559**

        ::text
        $ vgdisplay

        --- Volume group ---
        VG Name               centos
        System ID             
        Format                lvm2
        Metadata Areas        4
        Metadata Sequence No  8
        VG Access             read/write
        VG Status             resizable
        MAX LV                0
        Cur LV                2
        Open LV               2
        Max PV                0
        Cur PV                4
        Act PV                4
        VG Size               23.98 GiB
        PE Size               4.00 MiB
        Total PE              6140
        Alloc PE / Size       3581 / <13.99 GiB
        Free  PE / Size       2559 / <10.00 GiB
        VG UUID               ZPaYGz-7hbZ-2H6y-RS9W-x13x-2K81-pXCsA3

12. Extend **centos/root** logical volume

        ::text
        $ lvextend -l+2559 centos/root
        
        Size of logical volume centos/root changed from <13.19 GiB (3376 extents) to 23.18 GiB (5935 extents).
        Logical volume centos/root successfully resized.

13. XFS file system may be grown while mounted using the xfs_growfs command.

        ::text
        $ xfs_growfs /dev/centos/root

        meta-data=/dev/mapper/centos-root isize=512    agcount=9, agsize=406016 blks
                 =                       sectsz=512   attr=2, projid32bit=1
                 =                       crc=1        finobt=0 spinodes=0
        data     =                       bsize=4096   blocks=3457024, imaxpct=25
                 =                       sunit=0      swidth=0 blks
        naming   =version 2              bsize=4096   ascii-ci=0 ftype=1
        log      =internal               bsize=4096   blocks=2560, version=2
                 =                       sectsz=512   sunit=0 blks, lazy-count=1
        realtime =none                   extsz=4096   blocks=0, rtextents=0
        data blocks changed from 3457024 to 6077440
