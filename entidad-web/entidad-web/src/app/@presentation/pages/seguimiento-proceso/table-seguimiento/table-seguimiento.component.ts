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

  @Output() comunicadoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() cronogramaAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() procesoAction: EventEmitter<any> = new EventEmitter<any>();

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
    this.displayedColumns = [...columnNames, 'Estado', 'Acciones'];
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
    this.sort.emit(sortParameters);
  }

  showPublicar(codigoEtapa: string) {
    return (
      (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad()) &&
      codigoEtapa === this.const.ETA_BASE_REVISADO
    );
  }

  showRevisarObs(codigoEtapa: string) {
    return (
      (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad()) &&
      codigoEtapa === this.const.ETA_BASE_OBSERVADO
    );
  }

  showRevisar(codigoEtapa: string) {
    let etapasAccept = [
      this.const.ETA_BASE_POR_REVISAR,
      this.const.ETA_BASE_REVISADO,
      this.const.ETA_BASE_POR_PUBLICAR,
    ];
    return (
      (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad()) && etapasAccept.includes(codigoEtapa)
    );
  }

  showVer(codigoEtapa: string) {
    let etapasAccept: string[] = [];
    if (this.authRepository.isGestor()) {
      etapasAccept.push(this.const.ETA_BASE_ELIMINADO);
      etapasAccept.push(this.const.ETA_BASE_POR_REVISAR);
      etapasAccept.push(this.const.ETA_BASE_REVISADO);
      etapasAccept.push(this.const.ETA_BASE_POR_PUBLICAR);
      etapasAccept.push(this.const.ETA_BASE_PUBLICADO);
    } else {
      etapasAccept.push(this.const.ETA_BASE_OBSERVADO);
      etapasAccept.push(this.const.ETA_BASE_ELIMINADO);
      etapasAccept.push(this.const.ETA_BASE_PUBLICADO);
      etapasAccept.push(this.const.ETA_BASE_PROCESO);
    }
    return etapasAccept.includes(codigoEtapa);
  }

  showEditar(codigoEtapa: string) {
    return (
      (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad()) &&
      codigoEtapa === this.const.ETA_BASE_PROCESO
    );
  }

  showPDF(codigoEtapa: string) {
    return codigoEtapa !== this.const.ETA_BASE_ELIMINADO;
  }
  showEliminar(codigoEtapa: string) {
    if (this.authRepository.isGestor()) {
      return codigoEtapa === this.const.ETA_BASE_PROCESO;
    } else {
      let etapasAccept = [
        this.const.ETA_BASE_PUBLICADO,
        this.const.ETA_BASE_ELIMINADO,
        this.const.ETA_BASE_PROCESO,
      ];
      return !etapasAccept.includes(codigoEtapa);
    }
  }
}
