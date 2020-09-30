Title: NVMe RAID Performance on Z390 Chipset Motherboard
Date: 2020-09-30
Category: Hardware
Cover: /extra/intel-logo.png

NVMe SSD RAIDs are getting more popular in our days. It is an interesting question - what hardware to use to build more efficient RAID in terms of performance. Z390 chipset motherboard might include 2 or 3 NVMe slots for building of Raid 0 or 1 based on Intel® Rapid Storage Technology. Also, if a motherboard supports bifurcation, it can be used NVMe PCIe expansion cards. Some NVMe card don't request bifurcation but they are more expensive.

Performance tests have been done on [ASUS Prime Z390-A](https://www.asus.com/ca-en/Motherboards/PRIME-Z390-A/) motherboard with [WD Blue SN550 1TB](https://www.westerndigital.com/products/internal-drives/wd-blue-nvme-ssd) and [Intel 660P 1TB](https://www.intel.ca/content/www/ca/en/products/memory-storage/solid-state-drives/consumer-ssds/6-series/ssd-660p-series/660p-series-1-tb-m-2-80mm-3d2.html) NVMe SSDs used in RAID 1.

WD Blue SN550 NVMes have been installed in onboard slots whereas Intel 660P has been set up in [ASUS HYPER M.2 X16 CARD V2](https://www.asus.com/ca-en/Motherboard-Accessories/HYPER-M-2-X16-CARD-V2/) card.

## AS SSD Benchmark 1GB data

   * Intel 660P

     ![Intel 660P Performance 1GB Data]({static}/images/nvme-raid-performance-z390-chipset-motherboards/intel-660p-performance-1gb-data.jpg)</br></br>

   * WD Blue SN550

     ![Western Digital Blue SN550 Performance 1GB Data]({static}/images/nvme-raid-performance-z390-chipset-motherboards/western-digital-blue-sn550-performance-1gb-data.jpg)</br></br>

## AS SSD Benchmark 10GB data

   * Intel 660P

     ![Intel 660P Performance 10GB Data]({static}/images/nvme-raid-performance-z390-chipset-motherboards/intel-660p-performance-10gb-data.jpg)</br></br>

   * WD Blue SN550

     ![Western Digital Blue SN550 Performance 10GB Data]({static}/images/nvme-raid-performance-z390-chipset-motherboards/western-digital-blue-sn550-performance-10gb-data.jpg)</br></br>

## AS SSD Benchmark 1GB data with BitLocker encrypted

   * Intel 660P

     ![Intel 660P Performance 1GB Data BitLocker Encrypted]({static}/images/nvme-raid-performance-z390-chipset-motherboards/intel-660p-performance-1gb-data-bitlocker-encrypted.jpg)</br></br>

   * WD Blue SN550

     ![Western Digital Blue SN550 Performance 1GB Data BitLocker Encrypted]({static}/images/nvme-raid-performance-z390-chipset-motherboards/western-digital-blue-sn550-performance-1gb-data-bitlocker-encrypted.jpg)</br></br>

## AS SSD Benchmark 10GB data with BitLocker encrypted

   * Intel 660P

     ![Intel 660P Performance 10GB Data BitLocker Encrypted]({static}/images/nvme-raid-performance-z390-chipset-motherboards/intel-660p-performance-10gb-data-bitlocker-encrypted.jpg)</br></br>

   * WD Blue SN550

     ![Western Digital Blue SN550 Performance 10GB Data BitLocker Encrypted]({static}/images/nvme-raid-performance-z390-chipset-motherboards/western-digital-blue-sn550-performance-10gb-data-bitlocker-encrypted.jpg)</br></br>

## Conclusion

In spite of better performance in specification, Western Digital NVMe RAID is behind of Intel one in many tests. HYPER M.2 X16 card boosts performance of Intel RAID because of directly interfacing with the CPU's existing PCIe lanes. Also, Intel RAID is better than Western Digital one in all writing tests. Western Digital RAID overperforms Intel one in 10GB data chunks because Intel NVMe buffer is out of game. Copying a big data from or to Intel RAID is an issue as it starts throttling. The speed is dropping significantly and it gets comparable with existing hard drives.




