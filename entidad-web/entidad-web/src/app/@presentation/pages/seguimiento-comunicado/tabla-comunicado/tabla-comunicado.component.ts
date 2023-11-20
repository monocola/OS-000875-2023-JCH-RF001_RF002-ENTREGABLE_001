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
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-tabla-comunicado',
  templateUrl: './tabla-comunicado.component.html',
  styleUrls: ['./tabla-comunicado.component.scss'],
})
export class TablaComunicadoComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() dataExport = new ExportExcelModel();
  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  @Output() enviarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() procesoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() observadoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() verAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editarcomunicadoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() showpdfAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();

  acciones = true;
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];
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
    if (sortParameters.active !== 'estado') {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  showEnviar(codigoEtapa: string) {
    return codigoEtapa === this.const.EST_COMUNI_PROCESO;
  }

  showProceso(codigoEtapa: string) {
    return (
      codigoEtapa === this.const.EST_COMUNI_PROCESO &&
      (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad())
    );
  }

  showObservado(codigoEtapa: string) {
    return (
      codigoEtapa === this.const.EST_COMUNI_OBSERVADO &&
      (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad())
    );
  }

  showVer(codigoEtapa: string) {
    if (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad()) {
      return codigoEtapa === this.const.EST_COMUNI_POR_REVISAR;
    } else {
      return (
        codigoEtapa === this.const.EST_COMUNI_OBSERVADO ||
        codigoEtapa === this.const.EST_COMUNI_APROBADO
      );
    }
  }

  showComunicado(codigoEtapa: string) {
    return (
      codigoEtapa === this.const.EST_COMUNI_POR_REVISAR &&
      (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad())
    );
  }

  showPDF(codigoEtapa: string) {
    return codigoEtapa !== this.const.EST_COMUNI_PROCESO;
  }

  showDelete(codigoEtapa: string) {
    return (
      (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad()) &&
      codigoEtapa === this.const.EST_COMUNI_PROCESO
    );
  }
}
