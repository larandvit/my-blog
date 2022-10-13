Title: COBOL Display Decimal Data Type
Date: 2022-10-12
Category: COBOL
Cover: /extra/cobol-logo.png

Display decimal data type another name is zoned decimal. Each byte is separated into 2 parts. The first 4 bits is a zone and the second 4 bits is a value, thus one byte can contain only one digit. The low byte zone includes a sign. Positive numbers are represented by hexadecimal F and negative numbers by hexadecimal D. The summary of decimal data type can be found in [COBOL Data Types]({filename}/articles/cobol-data-types.md) article.

## Storage Description

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 5</th>
            <th class="text-center">Byte 4</th>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Zone-Digit</td>
            <td>Zone-Digit</td>
            <td>Zone-Digit</td>
            <td>Zone-Digit</td>
            <td>Sign-Digit</td>
        </tr>
     </tbody>
</table>

## Sample 1

**35,791** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 5</th>
            <th class="text-center">Byte 4</th>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1111-0011</td>
            <td>1111-0101</td>
            <td>1111-0111</td>
            <td>1111-1001</td>
            <td>1111-0001</td>
        </tr>
     </tbody>
</table>

## Sample 2

**-35,791** value

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Byte 5</th>
            <th class="text-center">Byte 4</th>
            <th class="text-center">Byte 3</th>
            <th class="text-center">Byte 2</th>
            <th class="text-center">Byte 1</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>1111-0011</td>
            <td>1111-0101</td>
            <td>1111-0111</td>
            <td>1111-1001</td>
            <td>1101-0001</td>
        </tr>
     </tbody>
</table>

## COBOL Representation

### Generic Formula

    :::cobol
    USAGE DISPLAY SIGN [IS] {TRAILING | LEADING} [SEPARATE [CHARACTER]]

### Samples

    :::cobol
    05 VAR-ZN-DECIMAL PIC 9(5).
    05 VAR-ZN-DECIMAL PIC 9(5)V9(2) DISPLAY.
    05 VAR-ZN-DECIMAL PIC 9(5) USAGE DISPLAY.
    05 VAR-ZN-DECIMAL PIC 9(5)V9(2) USAGE DISPLAY.
    05 VAR-ZN-DECIMAL PIC 9(5) USAGE DISPLAY SIGN TRAILING SEPARATE.
    05 VAR-ZN-DECIMAL PIC S9(5).
    05 VAR-ZN-DECIMAL PIC S9(5)V9(3) DISPLAY.

## Resources
* [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text
