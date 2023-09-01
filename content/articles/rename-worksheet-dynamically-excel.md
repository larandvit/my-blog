Title: Rename Worksheet Dynamically in Excel
Date: 2023-08-31
Category: Microsoft Excel
Cover: /extra/microsoft-excel-logo.png

Microsoft Excel events can be used to rename a worksheet based on a value in a cell. Depending on what kind of manipulations are done with a cell driving a worksheet name, it needs to pick up a specific event to trigger renaming.

Good candidates for events are.

* SelectionChange
* Calculate
* Change

The best choice is `Change` worksheet event as it is triggered by any changes in a cell.

Let's consider a case when a cell is located in the same worksheet which we are going to rename. The cell is A1. A new workbook contains only 1 worksheet. The sample is developed in Microsoft Office Processional Plus 2016.

1. Open your Excel spreadsheet, then go to VBA Editor pressing Alt-F11 combination.

    ![Microsoft Excel VBA Editor]({static}/images/rename-worksheet-dynamically-excel/microsoft-excel-vba-editor.jpg)</br></br>

2. Add `Change` event procedure selecting `Worksheet` in the first dropdown list and `Change` in the second one.

    ![Microsoft Excel Change Event]({static}/images/rename-worksheet-dynamically-excel/microsoft-excel-change-event.jpg)</br></br>

3. Add code to the event.

        :::text
        Set CellWithNewWorksheetname = Range("A1")
        If CellWithNewWorksheetname = "" Then Exit Sub
        Worksheets(1).Name = CellWithNewWorksheetname

    The event procedure is.

    ![Microsoft Excel Change Event Code]({static}/images/rename-worksheet-dynamically-excel/microsoft-excel-change-event-code.jpg)</br></br>

4. Return back to Excel and enter value in `A1` cell.

    ![Microsoft Excel Rename Worksheet]({static}/images/rename-worksheet-dynamically-excel/microsoft-excel-rename-worksheet.jpg)</br></br>
