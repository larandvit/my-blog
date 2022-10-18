Title: COBOL Character Data Type
Date: 2022-09-24
Modified: 2022-10-17
Category: COBOL
Cover: /extra/cobol-logo.png

Characters include letters, numbers, special characters, and other attributes. There are 3 groups of character data type: (1) alpha with letters: A-Z, a-z, and space, (2) numeric characters: 0-9, and (3) any characters. Characters are organized in code pages. Each code page combines a set of characters with a special designation, for example, it can be a region-specific code page. Each character in a code page has a numeric position.

The summary of character data type can be found in [COBOL Data Types]({filename}/articles/cobol-data-types.md) article.

## Storage Description

The character data type represents characters converted from a source code page to the output one. Source code pages are a set of character tables supported by COBOL, for example, ibm037 coding is the most widespread one.

## Sample 1

**ibm037** code page

The code page is 1 byte. It contains 255 characters.

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Converted to uft-8 bytes</th>
            <th class="text-center">H</th>
            <th class="text-center">e</th>
            <th class="text-center">l</th>
            <th class="text-center">l</th>
            <th class="text-center">o</th>
            <th class="text-center"> </th>
            <th class="text-center">e</th>
            <th class="text-center">v</th>
            <th class="text-center">e</th>
            <th class="text-center">r</th>
            <th class="text-center">y</th>
            <th class="text-center">b</th>
            <th class="text-center">o</th>
            <th class="text-center">d</th>
            <th class="text-center">y</th>
            <th class="text-center">!</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>hex ibm037 bytes</td>
            <td>c8</td>
            <td>85</td>
            <td>93</td>
            <td>93</td>
            <td>96</td>
            <td>40</td>
            <td>85</td>
            <td>a5</td>
            <td>85</td>
            <td>99</td>
            <td>a8</td>
            <td>82</td>
            <td>96</td>
            <td>84</td>
            <td>a8</td>
            <td>5a</td>
        </tr>
     </tbody>
</table>

## Sample 2

**ibm935 (Chinese)** code page

The code page is 2 bytes. It contains 65535 characters.

English characters

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Converted to uft-8 bytes</th>
            <th class="text-center">H</th>
            <th class="text-center">e</th>
            <th class="text-center">l</th>
            <th class="text-center">l</th>
            <th class="text-center">o</th>
            <th class="text-center"> </th>
            <th class="text-center">e</th>
            <th class="text-center">v</th>
            <th class="text-center">e</th>
            <th class="text-center">r</th>
            <th class="text-center">y</th>
            <th class="text-center">b</th>
            <th class="text-center">o</th>
            <th class="text-center">d</th>
            <th class="text-center">y</th>
            <th class="text-center">!</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>hex ibm935 bytes</td>
            <td>c8</td>
            <td>85</td>
            <td>93</td>
            <td>93</td>
            <td>96</td>
            <td>40</td>
            <td>85</td>
            <td>a5</td>
            <td>85</td>
            <td>99</td>
            <td>a8</td>
            <td>82</td>
            <td>96</td>
            <td>84</td>
            <td>a8</td>
            <td>5a</td>
        </tr>
     </tbody>
</table>

Hello everybody! in Chinese

<table class="table table-condensed table-bordered" style="border-width: 3px">
    <thead>
        <tr>
            <th class="text-center">Converted to uft-8 bytes</th>
            <th class="text-center">Special byte to mark the beginning of a sequence of multi-byte codes</th>
            <th class="text-center">大</th>
            <th class="text-center">家</th>
            <th class="text-center">好</th>
            <th class="text-center">Special byte to mark the end of a sequence of multi-byte codes</th>
            <th class="text-center">!</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>hex ibm935 bytes</td>
            <td>0e</td>
            <td>4af2</td>
            <td>4ed1</td>
            <td>4dc2</td>
            <td>0f</td>
            <td>5a</td>
        </tr>
     </tbody>
</table>

## COBOL Representation Samples

#### Alpha Characters

    :::cobol
    01 VAR-CHARACTER PIC A(5).
    01 VAR-CHARACTER PIC A(05).
    01 VAR-CHARACTER PIC AAAAA.

#### Numeric Characters

    :::cobol
    01 VAR-CHARACTER PIC 9(5).
    01 VAR-CHARACTER PIC 9(05).
    01 VAR-CHARACTER PIC 99999.

#### Any Characters

    :::cobol
    01 VAR-CHARACTER PIC X(5).
    01 VAR-CHARACTER PIC X(05).
    01 VAR-CHARACTER PIC XXXXX.

## Resources
* [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text
