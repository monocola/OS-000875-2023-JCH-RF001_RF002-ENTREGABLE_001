import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  Output,
  SimpleChanges,
} from '@angular/core';
import { FormControl } from '@angular/forms';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

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

@Component({
  selector: 'serv-talento-servir-section',
  templateUrl: './servir-section.component.html',
  styleUrls: ['./servir-section.component.scss'],
})
export class ServirSectionComponent implements OnChanges {
  @Input() cabecerasMaestra = [];
  @Input() changeValue = null;

  @Output() changeDetected = new EventEmitter();

  combo = new FormControl('');
  arrayBuilded = [];
  searchMode = false;

  length = 0;
  pageSize = 5;
  pageSizeOptions: number[] = [5, 10, 25];
  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

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

  constructor(
    private maestraService: MaestraRepository,
    private authService: AuthenticationRepository,
    private exportExcelService: ExportExcelService,
    private maestraEntidadRepository: MaestraEntidadRepository,
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>,
    private toastService: ToastService
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

  changeComboCabecera() {
    if (!this.combo.value) {
      this.searchMode = false;
    }
  }

  search() {
    this.searchMode = true;
    const idCabecera = this.combo.value === '-' ? '' : this.combo.value;
    const user = this.authService.getCurrentUserValue;
    this.maestraService
      .getMaestraListDetail(idCabecera, user.entidadId)
      .subscribe((res) => {
        res = res.filter((item) => item.listMaestraDetalles?.length > 0);
        const aux = res.slice(0);
        this.data = res.slice(0);
        this.length = this.data.length;
        this.dataToRender = aux.splice(0, this.pageSize);
        this.arrayBuilded = this.transformArray(this.dataToRender);
        this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
      });
  }

  transformArray(array) {
    const arrayAux = array.slice(0);
    const aux = arrayAux.map((item) => {
      item.id = item.maeCabeceraId;
      const children = [];
      item.listMaestraDetalles.map((detalle) => {
        detalle.id = detalle.maeDetalleId;
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
        if (detalle.estadoRegistro === '1') {
          children.push({
            data: detalle,
          });
        }
      });
      // delete item['listMaestraDetalles'];
      return {
        data: item,
        children: children,
        expanded: true,
      };
    });

    return aux;
  }

  getDataEvent(e: PageEvent) {
    this.pageSize = e.pageSize;
    const aux = this.data.slice(0);
    this.dataToRender = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    this.arrayBuilded = this.transformArray(this.dataToRender);
    this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
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

  editAction(action) {
    // this.organigramaRepository.setOrganoOrUnidadFromChart(action.data.restOfData);
    // this.router.navigateByUrl('pages/organigrama/configuracion');
  }

  getAllJson(data, array) {
    array.push(data);
    if (data.listMaestraDetalles?.length > 0) {
      data.listMaestraDetalles.map((d) => this.getAllJson(d, array));
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

  checkedChange(event, row) {
    const data = row.data;
    if (data.configuracionId) {
      this.maestraEntidadRepository
        .actualizaConfigMaestraDetalle(data.configuracionId, event)
        .subscribe((res) => {
          if (res) {
            this.toastService.showToast(
              'Se actualizó el estado de la maestra asignada',
              'success'
            );
            this.search();
            this.changeDetected.emit(true);
          }
        });
    } else {
      this.maestraEntidadRepository
        .asignaMaestraDetalle(data.maeCabeceraId, data.maeDetalleId)
        .subscribe((res) => {
          if (res) {
            this.toastService.showToast(
              'La maestra se ha asignado correctamente',
              'success'
            );
            this.search();
            this.changeDetected.emit(true);
          }
        });
    }
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de tablas maestras servir';
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
}
