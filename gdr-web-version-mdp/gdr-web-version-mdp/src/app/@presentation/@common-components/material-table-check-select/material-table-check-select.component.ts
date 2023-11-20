import { AfterViewInit, Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorIntl } from '@angular/material/paginator';
import { getDutchPaginatorIntl } from '../material-table-checkbox/paginator-translate';
import { MatTableDataSource } from '@angular/material/table';
import { MatSort, Sort } from '@angular/material/sort';
import { ExportExcelModel, ExportExcelService } from '../../@service/export-excel.service';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ThemePalette } from '@angular/material/core';
import {SelectionModel} from '@angular/cdk/collections';
import { TableColumn2 } from './table-column';

@Component({
  selector: 'serv-talento-table-check-select',
  templateUrl: './material-table-check-select.component.html',
  styleUrls: ['./material-table-check-select.component.scss'],
  providers: [
    { provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }  // Here
  ]
})
export class MaterialTableCheckSelectComponent implements OnInit, AfterViewInit {

  selection = new SelectionModel<any[]>(true, []);
  accionesTitle: string;
  public tableDataSource = new MatTableDataSource([]);
  public displayedColumns: string[];
  @ViewChild(MatPaginator, { static: false })
  matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() holderText = 'Buscar por ..';
  @Input() isPageable = false;
  @Input() isSortable = false;
  @Input() isFilterable = false;
  @Input() tableColumns: TableColumn2[];
  @Input() fontSize = 'fs-14';
  @Input() rowHeight = '38px';
  @Input() acciones = false;
  @Input() showTitle = false;
  @Input() title: string = '';

  @Input() cboList: any[];
  @Input() frm: FormGroup;
  @Input() selectSeg: any;
  @Input() flagCombo1: boolean = false;
  @Input() flagCheck: boolean = false;
  @Input() allComplete: boolean = false;

  @Input() btnImageEnabled: boolean = false;
  @Input() imageTitle: string = '';
  @Input() btnViewEnabled: boolean = false;
  @Input() viewTitle: string = '';
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

  @Output() onChangeAction: EventEmitter<any> = new EventEmitter<any>();

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(
    private exportExcelService: ExportExcelService,
    private fb: FormBuilder
  ) {
    this.frm = this.fb.group({
      segmento: ''
    });
  }

  get f() {
    return this.frm.controls;
  }

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn2) => tableColumn.name
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

  someComplete(): boolean {
    if (this.task.subtasks == null) {
      return false;
    }
    return this.task.subtasks.filter(t => t.completed).length > 0 && !this.allComplete;
  }

  setAll(completed: boolean) {
    this.allComplete = completed;
    if (this.task.subtasks == null) {
      return;
    }
    this.task.subtasks.forEach(t => t.completed = completed);
  }

  updateAllComplete() {
    this.allComplete = this.task.subtasks != null && this.task.subtasks.every(t => t.completed);
  }

  task: Task = {
    name: 'Indeterminate',
    completed: false,
    color: 'primary',
    subtasks: [
      {name: 'Primary', completed: false, color: 'primary'},
      {name: 'Accent', completed: false, color: 'accent'},
      {name: 'Warn', completed: false, color: 'warn'}
    ]
  };

  emitOnChange(events, item) {
    const body = {events, item};
    this.onChangeAction.emit(body);
  }

}

export interface Task {
  name: string;
  completed: boolean;
  color: ThemePalette;
  subtasks?: Task[];
}
