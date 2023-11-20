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
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { Const } from 'src/app/@data/services/const';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-tabla-contratos',
  templateUrl: './tabla-contratos.component.html',
  styleUrls: ['./tabla-contratos.component.scss'],
})
export class TablaContratosComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() dataExport = new ExportExcelModel();
  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  @Output() procesoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() subirAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() verAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() descargarAction: EventEmitter<any> = new EventEmitter<any>();

  acciones = true;
  paginationSizes: number[] = [20, 50, 100];
  defaultPageSize = this.paginationSizes[0];
  @Input() tamanio: number = 0;
  @Input() pageIndex: number = 0;
  @Output() pageEvent: EventEmitter<any> = new EventEmitter<any>();

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  fontSize = 'fs-13';

  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService) {}
  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [
      ...columnNames,
      'Estado',
      // 'fecPublicacion',
      'Acciones',
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

  setTableDataSource(data: any) {
    this.tableDataSource = new MatTableDataSource<any>(data);
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    if (sortParameters.active !== 'estado') {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  showProceso(codigoEtapa: string) {
    return (
      codigoEtapa === this.const.EST_CONTRATO_CREADO ||
      codigoEtapa === this.const.EST_CONTRATO_PENDIENTE
    );
  }

  showVer(codigoEtapa: string) {
    return (
      codigoEtapa === this.const.EST_CONTRATO_SUSCRITO ||
      codigoEtapa === this.const.EST_CONTRATO_ANULADO
    );
  }
}
