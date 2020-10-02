Title: Set Up NVMe Raid on Z370/Z390 Motherboards
Date: 2020-08-10
Modified: 2020-10-01
Category: Hardware
Cover: /extra/intel-logo.png

Z370 and Z390 Intel chipsets was released back in 2017 and 2018 years and they still are mainstream now. There are many motherboards based on Z370 and Z390 chipsets containing 2 or more M.2 sockets. Those motherboards might accommodate NVMe SSD storages with RAID 0 or 1 built on Intel® Rapid Storage Technology.

[ASRock Z370 Pro4](https://www.asrock.com/mb/Intel/Z370%20Pro4/index.asp) and [ASUS Prime Z390-A](https://www.asus.com/ca-en/Motherboards/PRIME-Z390-A/) motherboards are used in the article to build RAID 1 on [WD Blue SN550 1TB](https://www.westerndigital.com/products/internal-drives/wd-blue-nvme-ssd) and [Intel 660P 1TB](https://www.intel.ca/content/www/ca/en/products/memory-storage/solid-state-drives/consumer-ssds/6-series/ssd-660p-series/660p-series-1-tb-m-2-80mm-3d2.html) NVMe SSDs.

The performnce of NVMe SSDs can be observed in [NVMe RAID Performance on Z390 Chipset Motherboard]({filename}/articles/nvme-raid-performance-z390-chipset-motherboards.md) article.

Those two motherboards and NVMe SSDs work unstable in bootable RAID 1 configuration. It seems the chipsets are not aimed to be used in that role. After installing Windows 10 on RAID 1, both motherboards are constantly doing two steps: rebuilding RAID and crashing Windows.

![Windows 10 DPC_WATCHDOG VIOLATION crash]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/windows-crash.jpg)</br></br>

There are 2 ways to fix it. The first way is to use [ASUS HYPER M.2 X16 CARD V2](https://www.asus.com/ca-en/Motherboard-Accessories/HYPER-M-2-X16-CARD-V2/) adapter. It is only applicable to ASUS motherboard as ASRock motherboard is lack of bifurcation. The second way is to make your motherboard cooler running CPU and motherboard fans on 80% or more of their capacity. Also, it can be used non-bootable RAID.

Intel 660P NVMe is only supported by ASUS HYPER M.2 X16 CARD V2 adapter as the adapter accepts Intel NVMe SSDs for Z370 and Z390 chipsets. WD Blue SN550 is recognized but RAID can't be built and it works unstable as well.

## Installing RAID on ASRock Z370 Pro4 motherboard

1. Enable RAID in BIOS.
    * Enter Advanced menu.
    * Click Storage Configuration menu.
    * Switch SATA Operation Mode option to Raid.
    * Enable remapping of installed PCIE SSD M.2 slots.
    * Save BIOS setting.
    * Exit BIOS setup.
    * Reboot computer.

    ![ASRock motherboard create Raid]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-enable-raid.png)</br></br>

2. Create a RAID volume.
    * Enter Advanced menu.
    * Click Intel Rapid Storage Technology. This item is available only if Raid mode is enabled.

    ![ASRock motherboard Intel Rapid Storage Technology]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-rapid-storage-technology.png)</br></br>

    * Create RAID.

    ![ASRock motherboard create volume]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-create-volume.png)</br></br>

After the RAID setup completed, Windows 10 can be installed. Windows 10 does not request any additional drivers as BIOS handles everything transparently.

## Installing Raid on ASUS Prime Z390-A motherboard

The easiest way to set up Raid is EZ Tuning Wizard. It takes care of all settings.

1. Run EZ Tuning Wizard from BIOS landing screen.

    ![ASUS motherboard EZ Tuning Wizard]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-ez-tuning-wizard.png)</br></br>

2. Select PCIE mode

    ![ASUS motherboard PCIE Raid Mode]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-pcie-selection.png)</br></br>

3. Follow wizard.

After the RAID setup completed, Windows 10 can be installed. Windows 10 does not request any additional drivers as BIOS handles everything transparently.

## Installing RAID with ASUS HYPER M.2 X16 CARD V2 adapter

1. Enable the adapter in BIOS

    Pay attention to information how many NVMe SSDs can be used in the adapter and what slots can be occupied. In the sample below, M.2_1, M.2_3, and M.2_4 are usable slots and it can be maximum 3 NVMe SSDs.

    ![ASUS Enable HYPER M.2 X16 CARD V2]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-enable-hyper-m.2-x16-card-v2.jpg)</br></br>

2. Switch to RAID mode in SATA Mode Selection

    ![ASUS RAID SATA mode]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-sata-mode-raid.jpg)</br></br>

3. Reboot computer

4. Create a RAID volume.
    * Enter Advanced menu.
    * Click Intel Rapid Storage Technology. This item is available only if RAID mode is enabled.
    * Create RAID.

    ![ASUS motherboard create volume]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-create-raid-volume.jpg)</br></br>

5. Switch Launch CSM to Disabled in Boot menu

    ![ASUS launch CSM disabled]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-launch-csm-disabled.jpg)</br></br>

After the RAID setup completed, Windows 10 can be installed. Windows 10 requests drivers and they can downloaded from [the motherboard support page](https://www.asus.com/ca-en/Motherboards/PRIME-Z390-A/HelpDesk_Download/) and located in SATA section. Extract drivers and write to USB with Windows 10 installation.

