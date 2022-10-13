Title: COBOL Decimal Data Type
Date: 2022-10-12
Category: COBOL
Cover: /extra/cobol-logo.png

Decimal data type is called packed because it stores two digits in each byte. An exception is low-order byte which contains one digit in the leftmost portion and the sign (positive or negative) in the rightmost portion. 
Positive numbers are represented by hexadecimal F and negative numbers by hexadecimal D. 

It requests less storage than binary or display numeric types. The summary of decimal data type can be found in [COBOL Data Types]({filename}/articles/cobol-data-types.md) article.

## Storage Description

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Digit-Digit</td>
            <td>Digit-Digit</td>
            <td>Digit-Sign</td>
        </tr>
     </tbody>
</table>

## Sample 1

**35,791** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>0011-0101</td>
            <td>0111-1001</td>
            <td>0001-1111</td>
        </tr>
     </tbody>
</table>

## Sample 2

**-35,791** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>0011-0101</td>
            <td>0111-1001</td>
            <td>0001-1101</td>
        </tr>
     </tbody>
</table>

## COBOL Representation Samples

* 01 VAR-PK-DECIMAL PIC 9(5) USAGE COMP-3.
* 01 VAR-PK-DECIMAL PIC 9(05) USAGE COMP-3.
* 01 VAR-PK-DECIMAL PIC 9(5) COMP-3.
* 01 VAR-PK-DECIMAL PIC S9(5) USAGE COMP-3.
* 01 VAR-PK-DECIMAL PIC 9(5)V9(2) USAGE COMP-3.
* 01 VAR-PK-DECIMAL PIC 9(5)V9(2) COMP-3.
* 01 VAR-PK-DECIMAL PIC S9(5)V9(2) COMP-3.

## Resources
* [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text
