Title: COBOL Signed BINARY Data Type
Date: 2022-09-18
Category: COBOL
Cover: /extra/cobol-logo.png

Signed integers are represented 0 in the leftmost bit for positive values and 1 for negative ones.

One of the usages of COBOL data type information is to develop a tool to convert COBOL data into or from another format, for example, ASCII. [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) is a tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text.

## Storage Description

**BINARY 1 (1 byte)**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 7</th>
            <th class="text-center">Bit 6</th>
            <th class="text-center">Bit 5</th>
            <th class="text-center">Bit 4</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Sign</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
        </tr>
     </tbody>
</table>

**BINARY 2 (2 bytes)**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 15</th>
            <th class="text-center">Bit 14</th>
            <th class="text-center">Bit 13</th>
            <th class="text-center">...</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Sign</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td class="text-center">...</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
        </tr>
     </tbody>
</table>

**BINARY 4 (4 bytes)**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 31</th>
            <th class="text-center">Bit 30</th>
            <th class="text-center">Bit 29</th>
            <th class="text-center">...</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Sign</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td class="text-center">...</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
        </tr>
     </tbody>
</table>

**BINARY 8 (8 bytes)**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 63</th>
            <th class="text-center">Bit 62</th>
            <th class="text-center">Bit 61</th>
            <th class="text-center">...</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Sign</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td class="text-center">...</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
            <td>Bit value</td>
        </tr>
     </tbody>
</table>

## Sample 1

**35,791** value

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Byte 4</th>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>0000 0000</td>
            <td>0000 0000</td>
            <td>1000 1011</td>
            <td>1100 1111</td>
        </tr>
     </tbody>
</table>

**Explanation**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 31</th>
            <th class="text-center">...</th>
            <th class="text-center">Bit 16</th>
            <th class="text-center">Bit 15</th>
            <th class="text-center">Bit 14</th>
            <th class="text-center">Bit 13</th>
            <th class="text-center">Bit 12</th>
            <th class="text-center">Bit 11</th>
            <th class="text-center">Bit 10</th>
            <th class="text-center">Bit 9</th>
            <th class="text-center">Bit 8</th>
            <th class="text-center">Bit 7</th>
            <th class="text-center">Bit 6</th>
            <th class="text-center">Bit 5</th>
            <th class="text-center">Bit 4</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>0</td>
            <td class="text-center">...</td>
            <td>0</td>
            <td>32768</td>
            <td>0</td>
            <td>0</td>
            <td>0</td>
            <td>2048</td>
            <td>0</td>
            <td>512</td>
            <td>256</td>
            <td>128</td>
            <td>64</td>
            <td>0</td>
            <td>0</td>
            <td>8</td>
            <td>4</td>
            <td>2</td>
            <td>1</td>
        </tr>
    </tbody>
</table>

**35791 = 32768 + 2048 + 512 + 256 + 128 + 64 + 8 + 4 + 2 + 1**

## Sample 2

**-35,791** value

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Byte 4</th>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1111 1111</td>
            <td>1111 1111</td>
            <td>0111 0100</td>
            <td>0011 0001</td>
        </tr>
     </tbody>
</table>

**Explanation**

<table class="table table-condensed table-bordered">
    <thead>
        <tr>
            <th class="text-center">Bit 31</th>
            <th class="text-center">...</th>
            <th class="text-center">Bit 15</th>
            <th class="text-center">Bit 14</th>
            <th class="text-center">Bit 13</th>
            <th class="text-center">Bit 12</th>
            <th class="text-center">Bit 11</th>
            <th class="text-center">Bit 10</th>
            <th class="text-center">Bit 9</th>
            <th class="text-center">Bit 8</th>
            <th class="text-center">Bit 7</th>
            <th class="text-center">Bit 6</th>
            <th class="text-center">Bit 5</th>
            <th class="text-center">Bit 4</th>
            <th class="text-center">Bit 3</th>
            <th class="text-center">Bit 2</th>
            <th class="text-center">Bit 1</th>
            <th class="text-center">Bit 0</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>2147483648</td>
            <td class="text-center">...</td>
            <td>0</td>
            <td>16384</td>
            <td>8192</td>
            <td>4096</td>
            <td>0</td>
            <td>1024</td>
            <td>0</td>
            <td>0</td>
            <td>0</td>
            <td>0</td>
            <td>32</td>
            <td>16</td>
            <td>0</td>
            <td>0</td>
            <td>0</td>
            <td>1</td>
        </tr>
    </tbody>
</table>

**-35791 = -([maximum of unsigned 4 byte integer: 4294967295] - [2147483648...16384+8192+4096+1024+32+16+1] + 1)**

## COBOL Representation Samples

* 01 VAR-BINARY PIC S9(5) USAGE COMP.
* 01 VAR-BINARY PIC S9(05) USAGE COMP.
* 01 VAR-BINARY PIC S9(5) COMP.
* 01 VAR-BINARY PIC S9(5) USAGE COMP.
