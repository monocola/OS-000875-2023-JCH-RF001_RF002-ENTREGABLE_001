import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  Output,
  SimpleChanges,
} from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { PageEvent } from '@angular/material/paginator';
import {
  NbSortDirection,
  NbSortRequest,
  NbTreeGridDataSource,
  NbTreeGridDataSourceBuilder,
} from '@nebular/theme';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalCreationComponent } from '../modal-creation/modal-creation.component';

@Component({
  selector: 'serv-talento-entidad-section',
  templateUrl: './entidad-section.component.html',
  styleUrls: ['./entidad-section.component.scss'],
})
export class EntidadSectionComponent implements OnChanges {
  @Input() cabecerasMaestra = [];
  @Input() changeValue = null;

  @Output() changeDetected = new EventEmitter();

  length = 0;
  pageSize = 5;
  pageSizeOptions: number[] = [5, 10, 25];
  arrayBuilded = [];
  searchMode = false;

  dataToEdit = null;

  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

  combo = new FormControl('');

  dataSource: NbTreeGridDataSource<any>;
  data: any[] = [];
  dataToRender: any[] = [];

  customColumn = 'id';
  defaultColumns = ['descripcion', 'descripcionCorta', 'sigla', 'referencia'];
  defaultColumnsNames = [
    'TABLA MAESTRA',
    'NOMBRE CORTO',
    'SIGLA',
    'DESCRIPCIÓN',
  ];
  customColumn2 = 'ACCIONES';
  customColumn3 = 'ESTADO';

  allColumns = [
    this.customColumn,
    ...this.defaultColumns,
    this.customColumn3,
    this.customColumn2,
  ];
  dataExport: ExportExcelModel;

  showBtnRegister = false;

  constructor(
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>,
    private exportExcelService: ExportExcelService,
    private maestraEntidadRepository: MaestraEntidadRepository,
    private toastService: ToastService,
    private dialog: MatDialog
  ) {
    this.dataExport = this.getDataExport();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (!changes.changeValue?.firstChange) {
      if (this.searchMode) {
        this.search();
      }
    }
  }

  search() {
    this.searchMode = true;
    const idCabecera = this.combo.value === '-' ? '' : this.combo.value;
    this.maestraEntidadRepository
      .getMaestraDetalleEntidad(idCabecera)
      .subscribe((res) => {
        const aux = res.slice(0);
        this.data = res.slice(0);
        this.length = this.data.length;
        this.dataToRender = aux.splice(0, this.pageSize);
        this.arrayBuilded = this.transformArray(this.dataToRender);
        this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
      });
  }

  openCreateModal(createMode: boolean = true) {
    const creacionModal = this.dialog.open(ModalCreationComponent, {
      data: {
        tablaMaestra: this.cabecerasMaestra.filter(
          (item) => item.maeCabeceraId === this.combo.value
        )[0],
        dataToEdit: !createMode ? this.dataToEdit : null,
      },
    });

    creacionModal.afterClosed().subscribe((res) => {
      if (res) {
        if (this.searchMode) {
          this.search();
        }
      }
      this.dataToEdit = null;
    });
  }

  //  ----------------------- Tabla ------------------------

  changeComboCabecera() {
    if (!this.combo.value) {
      this.searchMode = false;
    }
    this.showBtnRegister =
      this.cabecerasMaestra.filter(
        (item) => item.maeCabeceraId === this.combo.value
      )[0].soloServir === '0'
        ? true
        : false;
  }

  getDataEvent(e: PageEvent) {
    this.pageSize = e.pageSize;
    const aux = this.data.slice(0);
    this.dataToRender = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    this.arrayBuilded = this.transformArray(this.dataToRender);
    this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
  }

  transformArray(array) {
    const arrayAux = array.slice(0);
    const aux = arrayAux.map((item) => {
      item.id = item.maeCabeceraId;
      const children = [];
      // Detalles Servir
      item.listMaestraDetalles.map((detalle) => {
        detalle.id = detalle.maeDetalleId;
        detalle.entidad = false;
        detalle.active = detalle.configuracionId
          ? detalle.estadoConfiguracion === '1'
            ? true
            : false
          : false;
        detalle.estadoConfiguracionTexto = detalle.configuracionId
          ? detalle.estadoConfiguracion === '1'
            ? 'ACTIVO'
            : 'INACTIVO'
          : 'INACTIVO';
        if (detalle.estadoConfiguracion === '1') {
          children.push({
            data: detalle,
          });
        }
      });
      
      if (item.listaMaestraDetalleEntidads) {
        item.listaMaestraDetalleEntidads.forEach((detEntidad) => {
          detEntidad.id = detEntidad.configuracionId;
          detEntidad.entidad = true;
          detEntidad.active = detEntidad.configuracionId
            ? detEntidad.estadoConfiguracion === '1'
              ? true
              : false
            : false;
          detEntidad.estadoConfiguracionTexto = detEntidad.configuracionId
            ? detEntidad.estadoConfiguracion === '1'
              ? 'ACTIVO'
              : 'INACTIVO'
            : 'INACTIVO';
          children.push({
            data: detEntidad,
          });
        });
      }

      if (children.length !== 0) {
        return {
          data: item,
          children: children,
          expanded: true,
        };
      } else {
        return null;
      }
    });

    return aux.filter((item) => item !== null);
  }

  getSortDirection(column: string): NbSortDirection {
    if (this.sortColumn === column) {
      return this.sortDirection;
    }
    return NbSortDirection.NONE;
  }

  getShowOn(index: number) {
    const minWithForMultipleColumns = 400;
    const nextColumnStep = 100;
    return minWithForMultipleColumns + nextColumnStep * index;
  }

  updateSort(sortRequest: NbSortRequest): void {
    this.sortColumn = sortRequest.column;
    this.sortDirection = sortRequest.direction;
  }

  editAction(row) {
    this.dataToEdit = row.data;
    this.openCreateModal(false);
  }

  getAllJson(data, array) {
    if (
      data.estadoRegistro === '1' ||
      (data.estadoRegistro === '0' && data.entidad === true)
    ) {
      array.push(data);
    }
    if (data.listMaestraDetalles?.length > 0) {
      data.listMaestraDetalles.map((d) => this.getAllJson(d, array));
    }
    if (data.listaMaestraDetalleEntidads?.length > 0) {
      data.listaMaestraDetalleEntidads.map((d) => this.getAllJson(d, array));
    }
    return array;
  }

  exportData() {
    const aux = this.data.slice(0);
    let arrayToBuild = [];
    aux.forEach((r) => {
      this.getAllJson(r, arrayToBuild);
      arrayToBuild.push({ id: '', descripcion: '' });
    });
    this.dataExport.data = arrayToBuild;
    this.exportExcelService.exportExcel(this.dataExport);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de tablas maestras entidad';
    model.headers = [
      '#ID',
      'TABLA MAESTRA / CAMPOS',
      'NOMBRE CORTO',
      'SIGLA',
      'DESCRIPCIÓN',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'descripcion',
      'descripcionCorta',
      'sigla',
      'referencia',
      'estadoConfiguracionTexto',
    ];
    return model;
  }

  checkedChange(event, row) {
    const data = row.data;
    this.maestraEntidadRepository
      .actualizaConfigMaestraDetalle(data.configuracionId, event)
      .subscribe((res) => {
        if (res) {
          this.changeDetected.emit(true);
          this.search();
          this.toastService.showToast(
            'Se actualizó el estado de la maestra asignada',
            'success'
          );
        }
      });
  }
}
