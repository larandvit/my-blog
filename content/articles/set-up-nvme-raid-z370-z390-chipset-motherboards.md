Title: Set Up NVMe Raid on Z370/Z390 Motherboards
Date: 2020-08-10
Category: Hardware
Cover: /extra/intel-logo.png

Z370 and Z390 Intel chipsets was released back in 2017 and 2018 years and they still are mainstream now. There are many motherboards based on Z370 and Z390 chipsets containing 2 or more M.2 sockets. Those motherboards might accommodate NVMe SSD storages with Raid 0 or 1 built on Intel® Rapid Storage Technology.

[ASRock Z370 Pro4](https://www.asrock.com/mb/Intel/Z370%20Pro4/index.asp) and [ASUS Prime Z390-A](https://www.asus.com/ca-en/Motherboards/PRIME-Z390-A/) motherboards are used in the article to build Raid 1 on [WD Blue SN550 1TB](https://www.westerndigital.com/products/internal-drives/wd-blue-nvme-ssd) NVMe SSDs.

Those two motherboards and WD Blue SN550 NVMe work unstable in Raid configuration. It seems the chipsets are not aimed to be used in Raid with NVMe SSDs. After installing Windows 10, both motherboards are constantly doing two steps: rebuilding Raid and crashing Windows. ASRock support was communicated and a new motherboard was RMAed. ASUS motherboard states that NVMe Raid can be created with addition of ASUS Hyper M.2 X16 Card V2 adapter.

![Windows 10 DPC_WATCHDOG VIOLATION crash]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/windows-crash.jpg)</br></br>

There are 2 ways to fix it. The first way is to use ASUS Hyper M.2 X16 Card V2 adapter. It is only applicable to ASUS motherboard as ASRock motherboard is lack of bifurcation. The second way is to make your motherboard cooler running CPU and motherboard fans on 80% or more of their capacity.

## Installing Raid on ASRock Z370 Pro4 motherboard

1. Enable Raid in BIOS.
    * Enter Advanced menu.
    * Click Storage Configuration menu.
    * Switch SATA Operation Mode option to Raid.
    * Enable remapping of installed PCIE SSD M.2 slots.
    * Save BIOS setting.
    * Exit BIOS setup.
    * Reboot computer.

    ![ASRock motherboard create Raid]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-enable-raid.png)</br></br>

2. Create a Raid volume.
    * Enter Advanced menu.
    * Click Intel Rapid Storage Technology. This item is available only if Raid mode is enabled.

    ![ASRock motherboard Intel Rapid Storage Technology]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-rapid-storage-technology.png)</br></br>

    * Create Raid.

    ![ASRock motherboard create volume]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asrock-create-volume.png)</br></br>

After the Raid setup completed, Windows 10 can be installed. Windows 10 does not request any additional drivers as BIOS handles everything transparently.

## Installing Raid on ASUS Prime Z390-A motherboard

The easiest way to set up Raid is EZ Tuning Wizard. It takes care of all settings.

1. Run EZ Tuning Wizard from BIOS landing screen.

    ![ASUS motherboard EZ Tuning Wizard]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-ez-tuning-wizard.png)</br></br>

2. Select PCIE mode

    ![ASUS motherboard PCIE Raid Mode]({static}/images/set-up-nvme-raid-z370-z390-chipset-motherboards/asus-pcie-selection.png)</br></br>

3. Follow wizard.

After the Raid setup completed, Windows 10 can be installed. Windows 10 does not request any additional drivers as BIOS handles everything transparently.
