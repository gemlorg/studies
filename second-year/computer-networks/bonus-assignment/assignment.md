To complete the solution for the given network task, we need to address the following parts:

1. **Logical Connection of Routers**:

   - Draw the logical connection between routers R11, R12, R13, and R14.
   - Assign interface names and IP addresses.
   - Ensure the solution is consistent with the provided traceroute outputs.

2. **Routing Table of Router R12**:

   - Provide a fragment of the routing table for R12 that enables the communication shown.

3. **Traceroute Results**:

   - Write the expected results of the traceroute commands from eagle.zad3sik.edu.pl and kestrel.zad3sik.edu.pl to pigeon.zad3sik.edu.pl.

4. **Zone File for zad3sik.edu.pl**:
   - Create a zone file containing all the name-to-address mappings appearing in the task and the solution.

### Solution Steps:

#### 1. Logical Connection of Routers

The provided traceroute output helps in visualizing the logical topology. Here's the logical connection of the routers:

```
Pigeon <-> R11 <-> R12 <-> R13 <-> Eagle
                       |
                       +-> R14 <-> Kestrel
```

Assigning interface names and IP addresses:

- **R11**:
  - Interface to Pigeon: `eth0`, IP: `207.13.56.91`
  - Interface to R12: `eth1`, IP: `209.85.127.99`
- **R12**:
  - Interface to R11: `eth0`, IP: `209.85.127.99`
  - Interface to R13: `eth1`, IP: `64.157.174.44`
  - Interface to R14: `eth2`, IP: `72.12.47.61`
- **R13**:
  - Interface to R12: `eth0`, IP: `64.157.174.44`
  - Interface to Eagle: `eth1`, IP: `25.3.143.12`
- **R14**:
  - Interface to R12: `eth0`, IP: `72.12.47.61`
  - Interface to Kestrel: `eth1`, IP: `193.19.88.91`

#### 2. Routing Table for R12

A fragment of the routing table for R12 to support the given communication:

```
Destination     Gateway         Genmask         Flags Metric Ref    Use Iface
25.3.143.12     64.157.174.44   255.255.255.255 U     0      0        0 eth1
193.19.88.91    72.12.47.61     255.255.255.255 U     0      0        0 eth2
207.13.56.91    209.85.127.99   255.255.255.255 U     0      0        0 eth0
```

#### 3. Traceroute Results

Assuming packets follow the same path in reverse:

- **From eagle.zad3sik.edu.pl to pigeon.zad3sik.edu.pl**:

  ```
  traceroute to pigeon.zad3sik.edu.pl (222.67.1.27), 30 hops max, 60 byte packets
    1  R13.zad3sik.edu.pl (64.157.174.44)
    2  R12.zad3sik.edu.pl (209.85.127.99)
    3  R11.zad3sik.edu.pl (207.13.56.91)
    4  * * *
    5  * * *
    ...
    30  * * *
  ```

- **From kestrel.zad3sik.edu.pl to pigeon.zad3sik.edu.pl**:
  ```
  traceroute to pigeon.zad3sik.edu.pl (222.67.1.27), 30 hops max, 60 byte packets
    1  R14.zad3sik.edu.pl (72.12.47.61)
    2  R12.zad3sik.edu.pl (209.85.127.99)
    3  R11.zad3sik.edu.pl (207.13.56.91)
    4  * * *
    5  * * *
    ...
    30  * * *
  ```

#### 4. Zone File for zad3sik.edu.pl

Creating a zone file to include all mappings:

```
$TTL 3600
@    IN  SOA ns1.zad3sik.edu.pl. admin.zad3sik.edu.pl. (
        2021072301 ; Serial
        3600       ; Refresh
        1800       ; Retry
        1209600    ; Expire
        3600 )     ; Minimum TTL

     IN  NS  ns1.zad3sik.edu.pl.
     IN  NS  ns2.zad3sik.edu.pl.

ns1  IN  A   142.27.28.98
ns2  IN  A   142.27.28.99

pigeon    IN  A  222.67.1.27
eagle     IN  A  25.3.143.12
kestrel   IN  A  193.19.88.91

R11  IN  A   207.13.56.91
R12  IN  A   209.85.127.99
R13  IN  A   64.157.174.44
R14  IN  A   72.12.47.61
```

### PDF Submission

This solution should be compiled into a PDF document with your name and submitted to Moodle.

**Example PDF Contents**:

1. Logical diagram of routers with interface names and IPs.
2. Routing table for R12.
3. Traceroute results from eagle and kestrel to pigeon.
4. Zone file for `zad3sik.edu.pl`.

Include all the above in a well-organized PDF document for submission.
