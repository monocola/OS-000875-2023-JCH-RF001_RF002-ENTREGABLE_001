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
import { TableColumn } from '../../../@common-components/material-table/table-column';
import {
  ExportExcelModel,
  ExportExcelService,
} from '../../../@service/export-excel.service';
import { MatTableDataSource } from '@angular/material/table';
import { Const } from '../../../../@data/services/const';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-table-seguimiento',
  templateUrl: './table-seguimiento.component.html',
  styleUrls: ['./table-seguimiento.component.scss'],
})
export class TableSeguimientoComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() dataExport = new ExportExcelModel();
  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() procesoAction: EventEmitter<any> = new EventEmitter<any>();

  acciones = true;
  @Input() paginationSizes: number[] = [5, 10, 15];
  @Input() defaultPageSize = this.paginationSizes[0];
  @Input() tamanio: number = 0;
  @Input() pageIndex: number = 0;
  @Output() pageEvent: EventEmitter<any> = new EventEmitter<any>();

  @Output() openConvocatoriaPostulante = new EventEmitter();

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  fontSize = 'fs-13';

  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(
    private exportExcelService: ExportExcelService,
    private authRepository: AuthenticationRepository
  ) {}
  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [
      ...columnNames,
      'nombrePostulante',
      'fechaPostulante',
      'valid',
      'REQMIN',
      'Estado',
      // 'Acciones',
    ];
  }

  ngAfterViewInit(): void {
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

  checkEstado(e) {}

  setTableDataSource(data: any) {
    this.tableDataSource = new MatTableDataSource<any>(data);
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    let columns: boolean = true;
    if (
      sortParameters.active === 'estado' ||
      sortParameters.active === 'rnssc' ||
      sortParameters.active === 'reqMinimo'
    ) {
      columns = false;
    }
    if (columns) {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  openConvocatoriaPostul(row) {
    this.openConvocatoriaPostulante.emit(row);
  }
}
