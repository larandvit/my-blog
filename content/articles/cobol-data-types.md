Title: COBOL Data Types
Date: 2022-09-15
Category: COBOL
Cover: /extra/cobol-logo.png

The article is served as a quick information about COBOL data types without diving into details how data is organized in code. There are 4 major groups of data types in COBOL: (1) alphabetic, (2) alphanumeric, (3) numeric, and (4) decimal. Numeric and decimal data types can include a sign with values '+' or '-'.


<table class="table table-condensed table-bordered table-hover">
    <thead>
        <tr>
            <th>COBOL Type</th>
            <th>COBOL Representation</th>
            <th>Range</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Signed BINARY 1 byte</td>
            <td>PIC S9 to S9(3) COMP</td>
            <td>-128 to 128</td>
        </tr>
       <tr>
            <td>Signed BINARY 2 bytes</td>
            <td>PIC S9(4) to S9(5) COMP</td>
            <td>-32768 to 32767</td>
        </tr>
       <tr>
            <td>Signed BINARY 4 bytes</td>
            <td>PIC S9(6) to S9(10) COMP</td>
            <td>-2147483648 to 2147483647</td>
        </tr>
       <tr>
            <td>Signed BINARY 8 bytes</td>
            <td>PIC S9(11) to S9(19) COMP</td>
            <td>-9223372036854775808 to 9223372036854775807</td>
        </tr>
       <tr>
            <td>Unsigned BINARY 1 byte</td>
            <td>PIC 9 to 9(3) COMP</td>
            <td>0 to 255</td>
        </tr>
       <tr>
            <td>Unsigned BINARY 2 bytes</td>
            <td>PIC 9(4) to 9(5) COMP</td>
            <td>0 to 65535</td>
        </tr>
       <tr>
            <td>Unsigned BINARY 4 bytes</td>
            <td>PIC 9(6) to 9(10) COMP</td>
            <td>0 to 4294967295</td>
        </tr>
       <tr>
            <td>Unsigned BINARY 8 bytes</td>
            <td>PIC 9(11) to 9(20) COMP</td>
            <td>0 to 18446744073709551615</td>
        </tr>
       <tr>
            <td>FLOAT 4 bytes</td>
            <td>PIC S9(p)V9(s) COMP-1</td>
            <td>-3.4028235E+38 to -1.1754944E-38, 0.0E+0, +1.1754944E-38 to +3.4028235E+38</td>
        </tr>
       <tr>
            <td>FLOAT 8 bytes</td>
            <td>PIC S9(p)V9(s) COMP-2</td>
            <td>-1.797693134862315E+308 to -2.225073858507201E-308, 0.0E+0, +2.225073858507201E-308 to +1.797693134862315E+308</td>
        </tr>
       <tr>
            <td>Alpha CHARACTER (A-Z, a-z, and space)</td>
            <td>PIC A(n)</td>
            <td>N/A</td>
        </tr>
       <tr>
            <td>Any CHARACTER</td>
            <td>PIC X(n)</td>
            <td>N/A</td>
        </tr>
       <tr>
            <td>Numeric CHARACTER (numbers 0-9)</td>
            <td>PIC 9(n)</td>
            <td>N/A</td>
        </tr>
       <tr>
            <td>DECIMAL</td>
            <td>PIC S9(p)V9(s) COMP-3</td>
            <td>N/A</td>
        </tr>
       <tr>
            <td>DISPLAY NUMERIC</td>
            <td>PIC S9(p)V9(s)</td>
            <td>N/A</td>
        </tr>
    </tbody>
</table>

## Resources
* [Data Types and Data Formats](https://www.ibm.com/docs/en/i/7.2?topic=definitions-data-types-data-formats)
