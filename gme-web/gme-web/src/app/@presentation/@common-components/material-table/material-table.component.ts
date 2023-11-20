import {
  AfterViewInit,
  Component, ElementRef,
  EventEmitter,
  Input,
  OnInit,
  Output, QueryList,
  ViewChild, ViewChildren
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
import { SelectionModel } from '@angular/cdk/collections';

@Component({
  selector: 'gme-web-table',
  templateUrl: './material-table.component.html',
  styleUrls: ['./material-table.component.scss'],
})
export class MaterialTableComponent implements OnInit, AfterViewInit {
  accionesTitle: string;
  public tableDataSource = new MatTableDataSource([]);
  public displayedColumns: string[];
  @ViewChild(MatPaginator, { static: false })
  matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;
  @ViewChild('headerCheck', { static: false, read: ElementRef }) headerCheck: ElementRef;
  @ViewChildren('itemCheck') itemChecks: QueryList<ElementRef>;

  @Input() holderText = 'Buscar por ..';
  @Input() isPageable = false;
  @Input() isSortable = false;
  @Input() isFilterable = false;
  @Input() tableColumns: TableColumn[];
  @Input() fontSize = 'fs-14';
  @Input() rowHeight = '38px';
  @Input() acciones = false;
  @Input() showTitle = false;
  @Input() title: string = '';
  @Input() nombreTabla: string = '';

  @Input() btnImageEnabled: boolean = false;
  @Input() imageTitle: string = '';
  @Input() btnViewEnabled: boolean = false;
  @Input() viewTitle: string = '';
  @Input() btnEditEnabled: boolean = false;
  @Input() editTitle: string = '';
  @Input() btnDeleteEnabled: boolean = false;

  @Input() btnDisabledKey: string = null;
  @Input() btnShow: boolean = false;

  @Input() deleteTitle: string = '';
  @Input() btnCopyEnabled: boolean = false;
  @Input() copyTitle: string = '';
  @Input() btnVirtualEnabled: boolean = false;
  @Input() virtualTitle: string = '';
  @Input() btnPresencialEnabled: boolean = false;
  @Input() presencialTitle: string = '';
  @Input() btnEmailEnabled: boolean = false;
  @Input() emaillTitle: string = '';

  @Input() rowActionIcon: string;
  @Input() paginationSizes: number[] = [20, 40, 60];
  @Input() defaultPageSize = this.paginationSizes[1];

  @Input() dataExport = new ExportExcelModel();
  @Input() showDownloadButton = true;

  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() viewAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() copyAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() imageAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() virtualAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() presencialAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() emailAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() onCheckedItem: EventEmitter<any> = new EventEmitter<any>();
  @Output() onCheckedAll: EventEmitter<any> = new EventEmitter<any>();

  @Input() custom1BtnShow: boolean = false;
  @Output() custom1BtnOnClick: EventEmitter<any> = new EventEmitter<any>();
  @Input() custom1BtnIcon: string = '';
  @Input() custom1BtnPack: string = 'entweb';
  @Input() custom1BtnTitle: string = '';
  @Input() custom1BtnClass: string = '';

  @Input() custom2BtnDisabledKey: string = null;
  @Input() custom2BtnShow: boolean = false;
  @Output() custom2BtnOnClick: EventEmitter<any> = new EventEmitter<any>();
  @Input() custom2BtnIcon: string = '';
  @Input() custom2BtnTitle: string = '';
  @Input() custom2BtnClass: string = '';

  @Output() custom3BtnOnClick: EventEmitter<any> = new EventEmitter<any>();
  @Input() customBtnDeleteDisabledKey: string = null;

  @Input() selectsRows: boolean = false;
  defaultSelectRows: any[] = [];
  @Input() keyValueSelect: string = "keyselect";

  selectedItems: boolean[] = [];
  public selection = new SelectionModel<any>(true, this.defaultSelectRows);

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }


  enabledCustomBtnDisable(row: any) {
    if (typeof row === 'object' && this.customBtnDeleteDisabledKey != null && this.customBtnDeleteDisabledKey in row) {
      return !!row[this.customBtnDeleteDisabledKey];
    }
  }

  constructor(private exportExcelService: ExportExcelService) {}

  ngOnInit(): void {
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
    if (this.btnEditEnabled) accionesCount++;
    if (this.btnDeleteEnabled) accionesCount++;
    if (this.btnCopyEnabled) accionesCount++;
    this.accionesTitle = accionesCount === 1 ? 'ACCIÓN' : 'Acciones';
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

  emitVirtualAction(row: any) {
    this.virtualAction.emit(row);
  }

  emitPresencialAction(row: any) {
    this.presencialAction.emit(row);
  }

  emitEmailAction(row: any) {
    this.emailAction.emit(row);
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

  selectAll(event: PointerEvent) {
    const input: any = event.target;
    this.selectedItems = this.selectedItems.map(x => input.checked);
    this.itemChecks.forEach(x => x.nativeElement.checked = input.checked);
    this.onCheckedAll.emit(input.checked);
  }

  selectItem(event: PointerEvent, index: number) {
    const input: any = event.target;
    this.selectedItems[index] = input.checked;
    if (!input.checked) {
      this.headerCheck.nativeElement.checked = false;
    } else {
      this.headerCheck.nativeElement.checked = !this.itemChecks.some(x => !x.nativeElement.checked);
    }
    this.onCheckedItem.emit({checked: input.checked, index: index, data: this.tableDataSource.data[index]});
  }

  clickCustom1(row: any) {
    this.custom1BtnOnClick.emit(row);
  }

  clickCustom2(row: any) {
    this.custom2BtnOnClick.emit(row);
  }

  enabledCustomBtn2(row: any) {
    if (typeof row === 'object' && this.custom2BtnDisabledKey != null && this.custom2BtnDisabledKey in row) {
      return !!row[this.custom2BtnDisabledKey];
    }
  }

  clickCustom3() {
    this.custom3BtnOnClick.emit(this.selection);
  }


}
