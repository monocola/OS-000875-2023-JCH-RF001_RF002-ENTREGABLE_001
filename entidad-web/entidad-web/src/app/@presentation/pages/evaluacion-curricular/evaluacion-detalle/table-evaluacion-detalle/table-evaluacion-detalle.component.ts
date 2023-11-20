import { AfterViewInit, Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel, ExportExcelService } from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-table-evaluacion-detalle',
  templateUrl: './table-evaluacion-detalle.component.html',
  styleUrls: ['./table-evaluacion-detalle.component.scss']
})
export class TableEvaluacionDetalleComponent implements OnInit, AfterViewInit {

  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() dataExport = new ExportExcelModel();
  tableDataSource = new MatTableDataSource([]);
  @Input() pageIndex: number = 0;
  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }
  @Input() tamanio: number = 0;
  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() pageEvent: EventEmitter<any> = new EventEmitter<any>();
  @Output() pdfAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() aprobarAction: EventEmitter<any> = new EventEmitter<any>();

  displayedColumns: string[];
  fontSize = 'fs-13';
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];


  constructor(
    private exportExcelService: ExportExcelService
  ) { }

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [...columnNames,'Redereci','Acciones'];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.sort = this.matSort;
  }

  setTableDataSource(data: any) {
    this.tableDataSource = new MatTableDataSource<any>(data);
    this.tableDataSource.sort = this.matSort;
  }


  exportData() {
    const datos = this.tableDataSource.filteredData;
    this.dataExport.title = this.title;
    from(datos)
      .pipe(
        map((value) => {
          let datanew = {};
          for (const key of this.dataExport.keys) {
            datanew[key] = value[key];
          }
          return datanew;
        })
      )
      .pipe(toArray())
      .subscribe((value) => {
        this.dataExport.data = value;
        this.exportExcelService.exportExcel(this.dataExport);
      });
  }

  sortTable(sortParameters: Sort) {
    this.sort.emit(sortParameters);
  }

  emitPdfElement(row: any) {
    this.pdfAction.emit(row);
  }

  emitEditAction(row: any) {
    this.editAction.emit(row);
  }

}
