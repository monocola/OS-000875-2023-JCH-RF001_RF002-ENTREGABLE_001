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
import { Const } from '../../../@data/services/const';

@Component({
  selector: 'serv-talento-table',
  templateUrl: './material-table.component.html',
  styleUrls: ['./material-table.component.scss'],
})
export class MaterialTableComponent implements OnInit, AfterViewInit {
  EST_PERFILES_POR_REVISAR: number;
  EST_PERFILES_REVISADO: number;
  accionesTitle: string;
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
  @Input() isPerfiles: boolean = false;

  @Input() btnImageEnabled: boolean = false;
  @Input() imageTitle: string = '';
  @Input() btnViewEnabled: boolean = false;
  @Input() viewTitle: string = '';
  @Input() btnPdfEnabled: boolean = false;
  @Input() pdfTitle: string = '';
  @Input() btnEditEnabled: boolean = false;
  @Input() editTitle: string = '';
  @Input() btnDeleteEnabled: boolean = false;
  @Input() deleteTitle: string = '';
  @Input() btnCopyEnabled: boolean = false;
  @Input() copyTitle: string = '';
  @Input() btnVirtualEnabled: boolean = false;
  @Input() virtualTitle: string = '';
  @Input() btnPresencialEnabled: boolean = false;
  @Input() presencialTitle: string = '';
  @Input() btnEmailEnabled: boolean = false;
  @Input() emaillTitle: string = '';
  @Input() btnShowPdf: boolean = false;

  @Input() rowActionIcon: string;
  @Input() paginationSizes: number[] = [5, 10, 15];
  @Input() defaultPageSize = this.paginationSizes[1];

  @Input() dataExport = new ExportExcelModel();
  @Input() showDownloadButton = true;

  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() viewAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() copyAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() pdfAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() imageAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() virtualAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() presencialAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() emailAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() checkAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() showpdfAction: EventEmitter<any> = new EventEmitter<any>();

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService) {}

  ngOnInit(): void {
    this.EST_PERFILES_POR_REVISAR = Const.EST_PERFILES_POR_REVISAR;
    this.EST_PERFILES_REVISADO = Const.EST_PERFILES_REVISADO;

    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    if (this.acciones) {
      this.displayedColumns = [...columnNames, 'Acciones'];
    } else {
      this.displayedColumns = columnNames;
    }

    let accionesCount = 0;
    if (this.btnImageEnabled) accionesCount++;
    if (this.btnViewEnabled) accionesCount++;
    if (this.btnPdfEnabled) accionesCount++;
    if (this.btnEditEnabled) accionesCount++;
    if (this.btnDeleteEnabled) accionesCount++;
    if (this.btnCopyEnabled) accionesCount++;
    if (this.btnShowPdf) accionesCount++;
    this.accionesTitle = accionesCount === 1 ? 'ACCIÓN' : 'ACCIONES';
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

  emitPdfElement(row: any) {
    this.pdfAction.emit(row);
  }

  emitViewAction(row: any) {
    this.viewAction.emit(row);
  }

  emitImageAction(row: any) {
    this.imageAction.emit(row);
  }

  emitVirtualAction(row: any) {
    this.virtualAction.emit(row);
  }

  emitPresencialAction(row: any) {
    this.presencialAction.emit(row);
  }

  emitEmailAction(row: any) {
    this.emailAction.emit(row);
  }

  emitCheckAction(row: any) {
    this.checkAction.emit(row);
  }

  emitPdfAction(row: any) {
    this.showpdfAction.emit(row);
  }
  
  validateSendEmail(row: any) {
    if (typeof row === 'object' && 'validador' in row) {
      if (row.validador) {
        return true;
      } else {
        return false;
      }
    }
  }

  enabledEmail(row: any) {
    if (typeof row === 'object' && 'toSend' in row) {
      if (row.toSend) {
        return true;
      } else {
        return false;
      }
    }
  }

  validateView(row: any) {
    if (typeof row === 'object' && 'toView' in row) {
      if (row.toView) {
        return true;
      } else {
        return false;
      }
    } else {
      return true;
    }
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

  validateExamen(row: any, id: number) {
    if (typeof row === 'object' && 'modalidadId' in row) {
      if (row.modalidadId === id) {
        return true;
      } else {
        return false;
      }
    }
  }


}
