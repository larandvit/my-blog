Title: Extract Number in Microsoft Access/Excel
Date: 2020-01-18
Category: Microsoft Office
Tags: microsoft, access, excel, regexp

Microsoft Access and Excel don't include any string functions to extract a number from a string. It should be created a custom function to complete this task. Regular Expressions are a good option to deal with manipulation of text data. Microsoft Access and Excel are lacking of support of regular expressions but they allow to utilize third party libraries. A library of our interest is Microsoft VBScript Regular Expressions 5.5 one.

1. Open your Microsoft Access database or Excel spreadsheet, then go to VBA Editor pressing Alt-F11 combination.

    **Microsoft Access**

    ![Microsoft Access VBA Editor]({static}/images/extract-number-in-access-excel/access-vba-editor.png)</br></br>

    **Microsoft Excel**

    ![Microsoft Access VBA Editor]({static}/images/extract-number-in-access-excel/excel-vba-editor.png)</br></br>

2. Create a new module calling context menu on the root node of the project tree.

    **Microsoft Access**

    ![Microsoft Access create new module]({static}/images/extract-number-in-access-excel/access-create-new-module.png)</br></br>
    ![Microsoft Access created module]({static}/images/extract-number-in-access-excel/access-module-created.png)</br></br>

    **Microsoft Excel**

    ![Microsoft Access create new module]({static}/images/extract-number-in-access-excel/excel-create-new-module.png)</br></br>
    ![Microsoft Access created new module]({static}/images/extract-number-in-access-excel/excel-module-created.png)</br></br>

3. Copy and paste the function below to the new created module.

        :::VBScript
        Function ExtractNumber(textValue)
         
            Dim re As Object
            Set re = CreateObject("vbscript.RegExp")
        
            re.Pattern = "[^\d]"
            re.Global = True
       
            ExtractNumber = re.Replace(textValue, "")
        
        End Function

    This code uses late binding to the library. This method is not preferable but it reduces number of steps to implement the solution.
    
4. Close VBA Editor.

5. Create a table and a query in Microsoft Access and a column of values in Microsoft Excel to test the function.

     **Microsoft Access**

    ![Microsoft Access query designer]({static}/images/extract-number-in-access-excel/access-query-designer.png)</br></br>
    ![Microsoft Access function in list]({static}/images/extract-number-in-access-excel/access-function-in-list.png)</br></br>
    ![Microsoft Access added function]({static}/images/extract-number-in-access-excel/access-added-function.png)</br></br>
    ![Microsoft Access final result]({static}/images/extract-number-in-access-excel/access-final-result.png)</br></br>

    **Microsoft Excel**

    ![Microsoft Access function in list]({static}/images/extract-number-in-access-excel/excel-function-in-list.png)</br></br>
    ![Microsoft Access create statement]({static}/images/extract-number-in-access-excel/excel-create-statement.png)</br></br>
    ![Microsoft Access final result]({static}/images/extract-number-in-access-excel/excel-final-result.png)</br></br>
