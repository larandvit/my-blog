Title: COBOL Float Data Type
Date: 2022-10-17
Category: COBOL
Cover: /extra/cobol-logo.png

Float data type is aimed to store floating-point numbers. Depending on values, float data type can be 4- or 8-byte size. There are two parts in floating-point numbers: (1) mantissa and (2) exponent. To get a floating-point value, the mantissa is multiplied by 10 raised to the power of the exponent.

The summary of float data type can be found in [COBOL Data Types]({filename}/articles/cobol-data-types.md) article.

## Storage Description

### Sample

**35791** value

3.5791 is the mantissa and 4 is the exponent.

    :::text
    3.5791 * (10 ** 4) = 35791

### FLOAT 4 bytes

It contains 8 digits in mantissa and it's called single floating point data type. The leftmost 8 bits stores exponent and the remaining 24 bits stores mantissa.

### FLOAT 8 bytes

It contains 16 digits in mantissa and it's called double floating point data type. The leftmost 12 bits stores exponent and the remaining 52 bits stores mantissa.

## COBOL Representation

### Samples 4 bytes

    :::cobol
    01 VAR-FLOAT PIC 9(5) USAGE COMP-1.
    01 VAR-FLOAT PIC 9(5) COMP-1.
    01 VAR-FLOAT PIC 9(5)V9(2) COMP-1.

### Samples 8 bytes

    :::cobol
    01 VAR-FLOAT PIC 9(05) USAGE COMP-2.
    01 VAR-FLOAT PIC S9(5) USAGE COMP-2.
    01 VAR-FLOAT PIC 9(5)V9(2) USAGE COMP-2.
    01 VAR-FLOAT PIC S9(5)V9(2) COMP-2.

## Resources
* [ebcdic-parser](https://github.com/larandvit/ebcdic-parser) tool for converting of mainframe EBCDIC data into Unicode ASCII delimited text
* [FLOAT(n)](https://www.ibm.com/docs/en/informix-servers/14.10?topic=types-floatn)
