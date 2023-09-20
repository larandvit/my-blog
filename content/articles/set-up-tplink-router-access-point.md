Title: Set Up TP-Link Router as Access Point
Date: 2023-09-19
Category: Networking
Cover: /extra/networking-logo.png

There are many articles on how to set up an old TP-Link router as an access point, for example, [How to configure the TP-Link wireless router as Access Point](https://www.tp-link.com/ca/support/faq/1384/) by TP-Link. It's relatively easy to do with Operation Mode menu available in your router, but a case without the Operation Mode menu requests some extra steps. This case will be covered. The existing documentation, as mentioned before, might have an issue that you can't access your router after switching it to an access point. It creates difficulties to make changes to any settings and as a result, a router reset should be done to fix it.

TL-WDR3600 router will be configured as an access point below. Archer C80 router is a main router used in the same network.

1. Log into the router’s web management page.

2. Change IP address of the router on **LAN** page of **Network** menu. 

    IP address is inside of the main router’s DHCP range, for example, **192.168.1.105** IP address can be assigned if your DHCP IP address pool is **192.168.1.100 - 192.168.1.199**. After making the change, the router restart is requested.

    ![Change TP-Link Router IP Address]({static}/images/set-up-tplink-router-access-point/change-router-ip-address.jpg)</br></br>

3. Set up wireless 2.4GHz and/or 5GHz connections in the router.

    ![Wireless Settings 2.4GHz]({static}/images/set-up-tplink-router-access-point/wifi24_connection.jpg)</br></br>

    ![Wireless Settings 5GHz]({static}/images/set-up-tplink-router-access-point/wifi5_connection.jpg)</br></br>

4. Disable DHCP server.

    ![Disable DHCP Server]({static}/images/set-up-tplink-router-access-point/disable-dhcp.jpg)</br></br>

5. Restart the router.

6. Reserve the assigned IP address on the main router.

    It is requested to eliminate assigning of the access point IP address to another device. 

    Find out the access point MAC address.

    ![Access Point IP Address]({static}/images/set-up-tplink-router-access-point/access-point-ip-address.jpg)</br></br>

    Reserve the IP address.

    ![Reserve IP Address on Main Router]({static}/images/set-up-tplink-router-access-point/reserve-ip-address.jpg)</br></br>


