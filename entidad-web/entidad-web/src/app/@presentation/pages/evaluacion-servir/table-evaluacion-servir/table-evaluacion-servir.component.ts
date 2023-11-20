import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  Output,
  SimpleChanges,
} from '@angular/core';
import { PageEvent } from '@angular/material/paginator';
import {
  NbSortDirection,
  NbSortRequest,
  NbTreeGridDataSource,
  NbTreeGridDataSourceBuilder,
  NbTreeGridPresentationNode,
} from '@nebular/theme';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-table-evaluacion-servir',
  templateUrl: './table-evaluacion-servir.component.html',
  styleUrls: ['./table-evaluacion-servir.component.scss'],
})
export class TableEvaluacionServirComponent implements OnChanges {
  @Input() data = [];
  @Input() servirMode = true;
  @Output() editEmitter = new EventEmitter();
  @Output() deleteEmmiter = new EventEmitter();
  @Output() jerarquiasReady = new EventEmitter();

  length = 0;
  pageSize = 5;
  pageSizeOptions: number[] = [5, 10, 25];
  arrayBuilded = [];

  allRows = [];

  dataSource: NbTreeGridDataSource<any>;
  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

  dataToRender: any[] = [];

  // customColumn = 'id';
  defaultColumns = ['orden', 'peso', 'pmin', 'pmax'];
  defaultColumnsNames = ['ORDEN', 'PESO%', 'P. MÍNIMO', 'P.MÁXIMO'];
  customColumn2 = 'ACCIONES';
  customColumn4 = 'DESCRIPCIÓN';

  jerarquiasExistentes = [];

  // allColumns = [this.customColumn4, this.customColumn, ...this.defaultColumns, this.customColumn3, this.customColumn2];
  allColumns = [this.customColumn4, ...this.defaultColumns, this.customColumn2];
  dataExport: ExportExcelModel;

  constructor(
    private exportExcelService: ExportExcelService,
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>
  ) {
    this.dataExport = this.getDataExport();
  }

  ngOnChanges(changes: SimpleChanges): void {
    this.jerarquiasExistentes = [];
    this.jerarquiasReady.emit(this.jerarquiasExistentes);
    if (changes.data?.currentValue?.length > 0) {
      this.jerarquiasExistentes = [];
      this.jerarquiasReady.emit(this.jerarquiasExistentes);
      const aux = this.data.slice(0);
      this.allRows = this.data.slice(0);
      this.length = aux.length;
      this.dataToRender = aux.splice(0, this.pageSize);
      this.arrayBuilded = this.transformJSON(this.dataToRender);
      this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
    }
  }

  transformJSON(listaRegimenes) {
    const aux = listaRegimenes.map((regimen) => this.setPrimerNivel(regimen));
    return aux;
  }

  setPrimerNivel(regimen) {
    return {
      data: {
        id: regimen.codigoNivel1,
        descripcion: regimen.descripcionCorta,
        restOfData: regimen,
      },
      children: this.setSegundoNivel(regimen),
      expanded: true,
    };
  }

  setSegundoNivel(regimen) {
    const aux =
      regimen.listaDetalleModalidad?.map((modalidad) =>
        this.setSegundoNivelHelper(regimen, modalidad)
      ) || [];
    return aux;
  }

  setSegundoNivelHelper(regimen, modalidad) {
    return {
      data: {
        id: modalidad.codNivel2,
        descripcion: modalidad.descripcionCorta,
        restOfData: { regimen, modalidad },
      },
      children: this.setTercerNivel(modalidad, regimen),
      expanded: true,
    };
  }

  setTercerNivel(modalidad, regimen) {
    const aux = modalidad.listaTipo.map((tipo) =>
      this.setTercerNivelHelper(regimen, modalidad, tipo)
    );
    this.getJerarquias(aux);
    return aux;
  }

  getJerarquias(array: any[]) {
    array.forEach((tipo) =>
      this.jerarquiasExistentes.push({
        modalidad: tipo.data.restOfData.modalidad.descripcionCorta,
        modalidadId: tipo.data.restOfData.modalidad.codNivel2,
        orden: (this.jerarquiasExistentes.length || 0) + 1,
        regimen: tipo.data.restOfData.regimen.descripcionCorta,
        regimenId: tipo.data.restOfData.regimen.codigoNivel1,
        tipo: tipo.data.restOfData.tipo.descripcionCorta,
        tipoId: tipo.data.restOfData.tipo.codigoNivel3,
        existe: true,
        settings: {
          disableDelete: true,
        },
      })
    );
    this.jerarquiasReady.emit(this.jerarquiasExistentes);
  }

  setTercerNivelHelper(regimen, modalidad, tipo) {
    return {
      data: {
        id: tipo.maeDetalleId,
        descripcion: tipo.descripcionCorta,
        restOfData: { regimen, modalidad, tipo },
      },
      children: this.setEvaluaciones(regimen, modalidad, tipo),
    };
  }

  setEvaluaciones(regimen, modalidad, tipo) {
    tipo.listaEvaluacion.sort((a, b) =>
      a.orden > b.orden ? 1 : b.orden > a.orden ? -1 : 0
    );
    const aux = tipo.listaEvaluacion.map((evaluacion) =>
      this.setEvaluacionesHelper(regimen, modalidad, tipo, evaluacion)
    );
    return aux;
  }

  setEvaluacionesHelper(regimen, modalidad, tipo, eva) {
    return {
      data: {
        id: eva.evaluacionId,
        descripcion: eva.detalleEvaluacion,
        orden: eva.orden,
        peso: eva.peso.toString(),
        pmin: eva.puntajeMinimo.toString(),
        pmax: eva.puntajeMaximo.toString(),
        estado: eva.estado,
        restOfData: { regimen, modalidad, tipo, eva },
      },
    };
  }

  getDataEvent(e: PageEvent) {
    this.pageSize = e.pageSize;
    const aux = this.data.slice(0);
    this.dataToRender = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    this.arrayBuilded = this.transformJSON(this.dataToRender);
    this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
  }

  updateSort(sortRequest: NbSortRequest): void {
    this.sortColumn = sortRequest.column;
    this.sortDirection = sortRequest.direction;
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

  editAction(row: NbTreeGridPresentationNode<any>) {
    this.editEmitter.emit(row);
  }

  deleteAction(row: NbTreeGridPresentationNode<any>) {
    this.deleteEmmiter.emit(row);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de evaluaciones';
    model.headers = [
      'Regimen',
      'ID Regimen',
      'Modalidad de acceso',
      'ID Modalidad de acceso',
      'Tipo de acceso',
      'ID Tipo de acceso',
      'ID Jerarquia',
      'ID Jerarquia Eva.',
      'Nombre Evaluacion',
      'Orden',
      'Peso',
      'Puntaje mínimo',
      'Puntaje máximo',
    ];
    model.keys = [
      'regimen',
      'idRegimen',
      'modalidad',
      'idModalidad',
      'tipoModalidad',
      'idTipoModalidad',
      'idJerarquia',
      'idJerarquiaEvaluacion',
      'nombreEvaluacion',
      'ordenEvaluacion',
      'peso',
      'puntajeMinimo',
      'puntajeMaximo',
    ];
    return model;
  }

  exportData() {
    const aux = this.allRows.slice(0);
    const arrayBuilded = this.transformJSON(aux).slice(0);

    const onlyEvaluations = [];
    this.getOnlyEvaluations(arrayBuilded, onlyEvaluations);
    const evaluacionesFormateadas = [];
    onlyEvaluations.forEach((eva) => {
      if (eva) {
        evaluacionesFormateadas.push({
          idJerarquia: eva.restOfData.eva.jerarquiaId,
          idJerarquiaEvaluacion: eva.restOfData.eva.evaluacionId,
          nombreEvaluacion: eva.descripcion,
          ordenEvaluacion: eva.orden,
          peso: eva.peso,
          puntajeMinimo: eva.pmin,
          puntajeMaximo: eva.pmax,
          estadoEvaluacion: eva.estado,
          tipoModalidad: eva.restOfData.tipo.descripcionCorta,
          idTipoModalidad: eva.restOfData.tipo.codigoNivel3,
          modalidad: eva.restOfData.modalidad.descripcionCorta,
          idModalidad: eva.restOfData.modalidad.codNivel2,
          regimen: eva.restOfData.regimen.descripcionCorta,
          idRegimen: eva.restOfData.regimen.codigoNivel1,
        });
      } else {
        evaluacionesFormateadas.push({
          nombreEvaluacion: null,
          ordenEvaluacion: null,
          puntajeMinimo: null,
          puntajeMaximo: null,
          estadoEvaluacion: null,
          tipoModalidad: null,
          peso: null,
          idTipoModalidad: null,
          modalidad: null,
          idModalidad: null,
          regimen: null,
          idRegimen: null,
        });
      }
    });
    this.dataExport.data = evaluacionesFormateadas;
    this.exportExcelService.exportExcel(this.dataExport);
  }

  getOnlyEvaluations(array, onlyEvaluations) {
    array.map((item) => {
      if (item.children) {
        this.getOnlyEvaluations(item.children, onlyEvaluations);
        if (item.data.restOfData.tipo) {
          onlyEvaluations.push(null);
        }
      } else {
        onlyEvaluations.push(item.data);
      }
    });
  }
}
