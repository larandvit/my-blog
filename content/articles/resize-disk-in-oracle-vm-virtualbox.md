Title: Resize Disk in Oracle VM VirtualBox
Date: 2020-01-30
Category: Virtualization, VM, VirtualBox
Cover: /extra/virtualbox-logo.png

Virtualization is flexible in terms of using resources. A virtual machine can be built with minimal assigned resources and later, they can be added when it's needed. Oracle VM VirtualBox allows allocation more space to an existing disk. This procedure is common for all virtualized operation systems. It's similar to replacing your old hard drive with new one which is larger size.

1. Run Oracle VM VirtualBox Manager and open settings of a virtual machine. It can be observed that the disk of the virtual machine is run out of space.

    ![Virtual machine original disk settings]({static}/images/resize-disk-in-oracle-vm-virtualbox/original-disk-size.png)</br></br>

2. Open Virtual Media Manager

    ![Open Virtual Manager]({static}/images/resize-disk-in-oracle-vm-virtualbox/open-virtual-media-manager.png)</br></br>

3. Select disk to add more space

    ![Add space to drive]({static}/images/resize-disk-in-oracle-vm-virtualbox/expand-drive-size.png)</br></br>

4. After applying the change, open settings of the virtual machine again to validate it

    ![New disk size]({static}/images/resize-disk-in-oracle-vm-virtualbox/new-disk-size.png)</br></br>

5. Next steps depend on your operation system. You need to run your virtual machine and consume added space. Follow [article]({filename}/articles/expand-logical-volume-in-centos.md) to set it up for CentOS/RHEL.
