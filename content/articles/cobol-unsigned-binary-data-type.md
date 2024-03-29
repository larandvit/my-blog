Title: COBOL Unsigned BINARY Data Type
Date: 2022-09-20
Modified: 2022-10-17
Category: COBOL
Cover: /extra/cobol-logo.png

This group of values are integer unsigned data type. They can be from 1 to 8 bytes with maximum range from 0 to 18446744073709551615. Unsigned integers don't reserve a bit for a sign and they occupy entire storage with value bits. The summary of unsigned BINARY data type can be found in [COBOL Data Types]({filename}/articles/cobol-data-types.md) article.

## Storage Description

**BINARY 1 (1 byte)**

<table class="table table-condensed table-bordered" style="border-width: 3px">
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
            <td>Bit value</td>
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

<table class="table table-condensed table-bordered" style="border-width: 3px">
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
            <td>Bit value</td>
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

<table class="table table-condensed table-bordered" style="border-width: 3px">
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
            <td>Bit value</td>
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

<table class="table table-condensed table-bordered" style="border-width: 3px">
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
            <td>Bit value</td>
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

**255** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
             <td>1111 1111</td>
        </tr>
     </tbody>
</table>

**Explanation**

<table class="table table-condensed table-bordered" style="border-width: 3px">
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
            <td>128</td>
            <td>64</td>
            <td>32</td>
            <td>16</td>
            <td>8</td>
            <td>4</td>
            <td>2</td>
            <td>1</td>
        </tr>
     </tbody>
</table>

**255 = 128 + 64 + 32 + 16 + 8 + 4 + 2 + 1**

## Sample 2

**35,791** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1000 1011</td>
            <td>1100 1111</td>
        </tr>kuY%ta25F
     </tbody>
</table>

**Explanation**

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
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

## COBOL Representation Samples

    :::cobol
    01 VAR-BINARY PIC 9(5) USAGE COMP.
    01 VAR-BINARY PIC 9(05) USAGE COMP.
    01 VAR-BINARY PIC 9(5) COMP.
    01 VAR-BINARY PIC 9(5) USAGE COMP.

## Resources
* [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text
