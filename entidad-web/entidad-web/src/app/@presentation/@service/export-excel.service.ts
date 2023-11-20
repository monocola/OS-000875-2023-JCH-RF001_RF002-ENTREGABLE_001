import { Injectable } from '@angular/core';
import * as Excel from 'exceljs/dist/exceljs';
import * as fs from 'file-saver';
import moment from 'moment';

// import * as logo from './mylogo.js';

@Injectable({
  providedIn: 'root',
})
export class ExportExcelService {
  constructor() {}

  exportExcel(excelData: ExportExcelModel) {
    // Title, Header & Data
    const title = excelData.title;
    const header = excelData.headers;

    excelData.data = excelData.data.map((value) => {
      let datanew = {};
      excelData.keys.forEach((key: string) => {
        datanew[key] = value[key];
      });
      return datanew;
    });

    let data = [];
    excelData.data.forEach((row: any) => {
      data.push(Object.values(row));
    });

    // Create a workbook with a worksheet
    let workbook = new Excel.Workbook();
    let worksheet = workbook.addWorksheet('data');

    // Add Row and formatting
    worksheet.mergeCells('C1', 'F4');
    let titleRow = worksheet.getCell('C1');
    titleRow.value = title;
    titleRow.font = {
      name: 'Calibri',
      size: 16,
      underline: 'single',
      bold: true,
      color: { argb: '0085A3' },
    };
    titleRow.alignment = { vertical: 'middle', horizontal: 'center' };

    // Date
    worksheet.mergeCells('G1:H4');
    let date = moment().format('DD/MM/YYYY');
    let dateCell = worksheet.getCell('G1');
    dateCell.value = date;
    dateCell.font = {
      name: 'Calibri',
      size: 12,
      bold: true,
    };
    dateCell.alignment = { vertical: 'middle', horizontal: 'center' };

    // Add Image
    // let myLogoImage = workbook.addImage({
    //   base64: logo.imgBase64,
    //   extension: 'png',
    // });
    worksheet.mergeCells('A1:B4');
    // worksheet.addImage(myLogoImage, 'A1:B4');

    // Blank Row
    worksheet.addRow([]);

    // Adding Header Row
    let headerRow = worksheet.addRow(header);
    headerRow.eachCell((cell, number) => {
      cell.fill = {
        type: 'pattern',
        pattern: 'solid',
        fgColor: { argb: '0D88BC' },
        bgColor: { argb: '' },
      };
      cell.font = {
        bold: true,
        color: { argb: 'FFFFFF' },
        size: 12,
      };
    });

    // Adding Data with Conditional Formatting
    data.forEach((rowData) => {
      worksheet.addRow(rowData);
    });
    // worksheet.getColumn(3).width = 20;
    worksheet.addRow([]);

    for (let i = 0; i < worksheet.columns.length; i += 1) {
      let dataMax = 0;
      const column = worksheet.columns[i];
      for (let j = 1; j < column.values.length; j += 1) {
        if (j !== headerRow.number) {
          if (column.values[j] && column.values[j] != null) {
            const columnLength = column.values[j].length;
            if (columnLength > dataMax) {
              dataMax = columnLength;
            }
          }
        }
      }
      column.width = dataMax < 15 ? 15 : dataMax + 2;
    }

    // Generate & Save Excel File
    workbook.xlsx.writeBuffer().then((file) => {
      let blob = new Blob([file], {
        type:
          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
      });
      fs.saveAs(blob, title + '.xlsx');
    });
  }
}
export class ExportExcelModel {
  title: string;
  data: any[];
  headers: string[];
  keys: string[];
}
