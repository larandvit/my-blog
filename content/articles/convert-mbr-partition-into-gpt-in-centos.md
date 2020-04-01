Title: Convert MBR Partition into GPT in CentOS/RHEL 7
Date: 2020-01-28
Category: Linux, MBR, GPT, CentOS, Partition, RHEL
Cover: /extra/centos-logo.png

Master Boot Record (MBR) partitioned disks are replaced with newer GUID Partition Table (GPT) standard but MBR is still used widely as a default format. GPT layout for the partition tables has a lot of benefits comparing with MBR one. Along with supporting significantly larger size of disks, it introduces faster and more stable booting. GPT requests to support Unified Extensible Firmware Interface (UEFI) boot.

1. Switch to root user.

        ::bash
        $ sudo su - 

2. Run GPT partition tool. Assuming that **sda** disk is bootable and will be converted into GPT.

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

3. Make sure that there is enough space before the first partition to support a boot partition. **2048** value for the first sector confirms that GPT can be applied to MBR.

        ::text
        Command (? for help): p
    
        Disk /dev/sda: 52428800 sectors, 25.0 GiB
        Logical sector size: 512 bytes
        Disk identifier (GUID): 71DD2E79-BD1C-4713-9880-22664C87E57B
        Partition table holds up to 128 entries
        First usable sector is 34, last usable sector is 52428766
        Partitions will be aligned on 2048-sector boundaries
        Total free space is 20973501 sectors (10.0 GiB)

        Number  Start (sector)    End (sector)  Size       Code  Name
           1            2048         2099199   1024.0 MiB  8300  Linux filesystem
           2         2099200        16777215   7.0 GiB     8E00  Linux LVM
           3        16777216        20971519   2.0 GiB     8E00  Linux LVM
           4        20971520        31457279   5.0 GiB     8E00  Linux LVM

4. Create a new bootable partition. As the first sector, enter **34** and the last sector is **2047**. Partition code is **ef02**.

        ::text
        Command (? for help): n

        Partition number (5-128, default 5): 
        First sector (34-52428766, default = 31457280) or {+-}size{KMGTP}: 34
        Last sector (34-2047, default = 2047) or {+-}size{KMGTP}: 
        Current type is 'Linux filesystem'
        Hex code or GUID (L to show codes, Enter = 8300): ef02
        Changed type of partition to 'BIOS boot partition'

5. Save changes.
    
        ::text
        Command (? for help): w

        Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING
        PARTITIONS!!

        Do you want to proceed? (Y/N): Y
        OK; writing new GUID partition table (GPT) to /dev/sda.
        Warning: The kernel is still using the old partition table.
        The new table will be used at the next reboot.
        The operation has completed successfully.

6. Notify the operation system about changes. It eliminates rebooting of the system.

        ::text
        $ partprobe

7. Install GRUB on the new bootable partition.

        ::text
        $ grub2-install /dev/sda

        Installing for i386-pc platform.
        Installation finished. No error reported.

8. Validate the conversion.

        ::text
        $ gdisk /dev/sda

        GPT fdisk (gdisk) version 0.8.10

        Partition table scan:
          MBR: protective
          BSD: not present
          APM: not present
          GPT: present

        Found valid GPT with protective MBR; using GPT.
