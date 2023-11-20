import {
  AfterViewInit,
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  ViewChild,
} from '@angular/core';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { TableColumn } from './table-column';
import { MatPaginatorIntl } from '@angular/material/paginator';
import { getDutchPaginatorIntl } from './paginator-translate';

@Component({
  selector: 'material-table-checkbox',
  templateUrl: './material-table-checkbox.component.html',
  styleUrls: ['./material-table-checkbox.component.scss'],
  providers: [
    { provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }  // Here
  ]
})
export class MaterialTableCheckboxComponent implements OnInit, AfterViewInit {
  public tableDataSource = new MatTableDataSource([]);
  public displayedColumns: string[];
  @ViewChild(MatPaginator, { static: false })
  matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() isPageable = false;
  @Input() isSortable = false;
  @Input() isFilterable = false;
  @Input() tableColumns: TableColumn[];
  @Input() fontSize = 'fs-14';
  @Input() rowHeight = '38px';
  @Input() acciones = false;
  @Input() showTitle = false;
  @Input() title: string = '';

  @Input() btnImageEnabled: boolean = false;
  @Input() imageTitle: string = "";
  @Input() btnViewEnabled: boolean = false;
  @Input() viewTitle: string = "";
  @Input() btnEditEnabled: boolean = false;
  @Input() editTitle: string = '';
  @Input() btnDeleteEnabled: boolean = false;
  @Input() deleteTitle: string = '';
  @Input() btnCopyEnabled: boolean = false;
  @Input() copyTitle: string = '';

  @Input() rowActionIcon: string;
  @Input() paginationSizes: number[] = [5, 10, 15];
  @Input() defaultPageSize = this.paginationSizes[1];

  @Input() dataExport = new ExportExcelModel();
  @Input() showDownloadButton = true;

  @Input() cantidadList: number;


  @Input() descripcion: string;
  @Input() flagSeleccion: any;
  @Input() preguntaId: number;

  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() viewAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() copyAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() imageAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() checkItemAction: EventEmitter<any> = new EventEmitter<any>();

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService) { }

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    if (this.acciones) {
      this.displayedColumns = [...columnNames, 'Acciones'];
    } else {
      this.displayedColumns = columnNames;
    }
  }

  ngAfterViewInit(): void {
    this.tableDataSource.paginator = this.matPaginator;
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

  setTableDataSource(data: any) {
    this.tableDataSource = new MatTableDataSource<any>(data);
    this.tableDataSource.paginator = this.matPaginator;
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    sortParameters.active = this.tableColumns.find(
      (column) => column.name === sortParameters.active
    ).dataKey;
    this.sort.emit(sortParameters);
  }

  emitEditAction(row: any) {
    this.editAction.emit(row);
  }

  emitDeleteElement(row: any) {
    this.deleteAction.emit(row);
  }

  emitCopyElement(row: any) {
    this.copyAction.emit(row);
  }

  emitViewAction(row: any) {
    this.viewAction.emit(row);
  }

  emitImageAction(row: any) {
    this.imageAction.emit(row);
  }

  emitCheckItem(row: any, checkvalue: any) {
    row.flagSeleccion = checkvalue.currentTarget.checked;
    this.checkItemAction.emit(row);
  }

  validateImage(row: any) {
    if (typeof row === 'object' && 'img' in row) {
      if (row.img !== null) {
        return true;
      } else {
        return false;
      }
    }
  }
}
