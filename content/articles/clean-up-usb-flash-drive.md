Title: Clean Up USB Flash Drive
Date: 2020-01-11
Category: Windows Tools
Cover: /extra/microsoft-windows-logo.png

When a USB flash drive is used in Linux or as an ISO image is recorded to it, USB drive might be is not usable in Windows. **diskpart** Windows tool can be used to clean up and re-partition USB flash drive.

The tool can be run from Windows Command Prompt typing `diskpart`

![Command Prompt window]({static}/images/clean-up-usb-flash-drive/command-prompt-diskpart-run.png)</br></br>

The first step is to identify an index of our USB drive. Run `LIST DISK` command

![diskpart LIST DISK]({static}/images/clean-up-usb-flash-drive/diskpart-list-disk.png)</br></br>

Based on size, our flash drive index is 2 and the next step is to select our drive. Run `SELECT DISK`

![diskpart SELECT DISK]({static}/images/clean-up-usb-flash-drive/diskpart-select-disk.png)</br></br>

Now it's time to remove all content from the drive including any partitions. Run `CLEAN` command

![diskpart CLEAN]({static}/images/clean-up-usb-flash-drive/diskpart-clean.png)</br></br>

After making our flash drive empty, a new primary partition is created. Run `CREATE PRIMARY PARTITION` command

![diskpart CREATE PRIMARY PARTITION]({static}/images/clean-up-usb-flash-drive/diskpart-create-primary-partition.png)</br></br>

Next step is to format flash drive. A list of file systems is FAT, FAT32, NTFS, exFAT. `quick` option allows to complete it very fast. Run `FORMAT` command

![diskpart FORMAT]({static}/images/clean-up-usb-flash-drive/diskpart-format.png)</br></br>

Final step is to assign a letter to access the flash drive in Windows. Run `ASSIGN` command

![diskpart ASSIGN]({static}/images/clean-up-usb-flash-drive/diskpart-assign.png)</br></br>

Close diskpart tool. Run `EXIT`

![diskpart EXIT]({static}/images/clean-up-usb-flash-drive/diskpart-exit.png)</br></br>
