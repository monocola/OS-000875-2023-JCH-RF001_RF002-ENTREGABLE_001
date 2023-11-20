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
  selector: 'serv-talento-tabla-base',
  templateUrl: './tabla-base.component.html',
  styleUrls: ['./tabla-base.component.scss'],
})
export class TablaBaseComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() dataExport = new ExportExcelModel();
  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  @Output() publicarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() revisarObsAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() revisarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() verAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() editarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() showpdfAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();


  acciones = true;
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];

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
    this.tableDataSource.paginator = this.matPaginator;
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
    this.tableDataSource.paginator = this.matPaginator;
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    if (sortParameters.active !== 'codigoEtapa') {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  emitDeleteElement(row: any) {
    this.deleteAction.emit(row);
  }

  showPublicar(codigoEtapa: string) {
    return (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad()) && codigoEtapa === this.const.ETA_BASE_REVISADO;
  }

  showRevisarObs(codigoEtapa: string) {
    return (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad())  && codigoEtapa === this.const.ETA_BASE_OBSERVADO;
  }

  showRevisar(codigoEtapa: string) {
    let etapasAccept = [this.const.ETA_BASE_POR_REVISAR, this.const.ETA_BASE_REVISADO]; // , this.const.ETA_BASE_POR_PUBLICAR
    return (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad()) && etapasAccept.includes(codigoEtapa);
  }

  showVer(codigoEtapa: string) {
    let etapasAccept: string[] = [];
    if (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad() ) {
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
    return (this.authRepository.isGestor() || this.authRepository.isSuperAdminEntidad()) && codigoEtapa === this.const.ETA_BASE_PROCESO;
  }

  showPDF(codigoEtapa: string) {
    return codigoEtapa !== this.const.ETA_BASE_ELIMINADO;
  }
  showEliminar(codigoEtapa: string) {
    if (this.authRepository.isGestor()) {
      return codigoEtapa === this.const.ETA_BASE_PROCESO;
    } else {
      let etapasAccept = [this.const.ETA_BASE_PUBLICADO, this.const.ETA_BASE_ELIMINADO, this.const.ETA_BASE_PROCESO];
      return !etapasAccept.includes(codigoEtapa);
    }
  }
}
