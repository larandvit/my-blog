Title: Clean Up USB Flash, SSD, Hard Drives
Date: 2020-01-11
Modified: 2020-10-03
Category: Windows
Cover: /extra/microsoft-windows-logo.png

When a drive has been used in Operation System (OS) different from Windows, for example, Linux, it can get usable in Windows. **Disk Management** Windows GUI tool can't handle those disks but **DiskPart** Windows tool can help clean up and re-partition a drive. The tool is similar to **fdisk** Linux one with comprehensive functionality.

## 1. Run the tool typing `diskpart` from Windows Command Prompt or Windows Start

    :::batch
    Microsoft DiskPart version 10.0.19041.1

    Copyright (C) Microsoft Corporation.
    On computer: MAINCOMPUTER

    DISKPART>

## 2. Identify an index of a drive
   
    :::batch
    DISKPART> LIST DISK
    
      Disk ###  Status         Size     Free     Dyn  Gpt
      --------  -------------  -------  -------  ---  ---
      Disk 0    Online          931 GB  1024 KB        *
      Disk 1    Online          953 GB  1024 KB        *
      Disk 2    Online         1863 GB  1863 GB

## 3. Select a drive

   Based on size, the drive index is 2.

    :::batch
    DISKPART> SELECT DISK 2

    Disk 2 is now the selected disk.

## 4. Clean up drive

   Remove all partitions from the drive.

    :::batch
    DISKPART> CLEAN

    DiskPart succeeded in cleaning the disk.

   if additionally, the drive's contents has to be removed securely. It will take time as it will write to every sector of the disk.

    :::batch
    DISKPART> CLEAN ALL

    DiskPart succeeded in cleaning the disk.

## 5. Create a new primary partition

    :::batch
    DISKPART> CREATE PARTITION PRIMARY

    DiskPart succeeded in creating the specified partition.

## 6. Format the drive

   A list of file systems is FAT, FAT32, NTFS, exFAT. `quick` option allows to complete it very fast

    :::batch
    DISKPART> FORMAT fs=exFAT quick

      100 percent completed

    DiskPart successfully formatted the volume.

## 7. Assign a letter to access the drive in Windows

    :::batch
    DISKPART> ASSIGN

    DiskPart successfully assigned the drive letter or mount point.

## 8. Close DiskPart tool

    :::batch
    DISKPART> EXIT
