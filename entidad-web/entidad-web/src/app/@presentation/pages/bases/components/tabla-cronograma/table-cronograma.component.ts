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
import moment from 'moment';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-table-cronograma',
  templateUrl: './table-cronograma.component.html',
  styleUrls: ['./table-cronograma.component.scss'],
})
export class TableCronogramaComponent implements OnChanges {
  @Input() data: any[] = [];
  @Input() title: string = 'Lista de actividades del Cronograma';
  @Input() showEdit: boolean = true;
  @Input() showDelete: boolean = true;
  @Input() usePaddingBody: boolean = true;
  @Input() showAcciones: boolean = true;

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
  defaultColumns = ['actividad', 'responsable', 'periodo'];
  defaultColumnsNames = ['DESCRIPCION', 'RESPONSABLE', 'PERIODO'];
  customColumn2 = 'ACCIONES';
  customColumn4 = 'DESCRIPCIÓN';

  actividades = [];

  allColumns = [this.customColumn4, ...this.defaultColumns, this.customColumn2];
  dataExport: ExportExcelModel;

  constructor(
    private exportExcelService: ExportExcelService,
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>
  ) {
    this.dataExport = this.getDataExport();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (this.showAcciones === true) {
      this.allColumns = [this.customColumn4, ...this.defaultColumns, this.customColumn2];
    } else {
      this.allColumns = [this.customColumn4, ...this.defaultColumns];
    }

    this.actividades = [];
    this.jerarquiasReady.emit(this.actividades);
    if (changes.data?.currentValue?.length > 0) {
      this.actividades = [];
      this.jerarquiasReady.emit(this.actividades);
      const aux = this.data.slice(0);
      this.allRows = this.data.slice(0);
      this.length = aux.length;
      this.dataToRender = aux.splice(0, this.pageSize);
      this.arrayBuilded = this.transformJSON(this.dataToRender);
      this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
    }
  }

  transformJSON(listaEtapas) {
    const aux = listaEtapas.map((etapa) => this.setPrimerNivel(etapa));
    return aux;
  }

  setPrimerNivel(etapa) {
    return {
      data: {
        id: etapa.cronogramaId,
        descripcion: etapa.descripcion,
        periodo:
          moment(etapa.periodoIni).format('DD-MM-YYYY') +
          ' - ' +
          moment(etapa.periodoFin).format('DD-MM-YYYY'),
        restOfData: etapa,
      },
      children: this.setSegundoNivel(etapa),
      expanded: true,
    };
  }

  setSegundoNivel(etapa) {
    etapa.actividadDTOList.sort((a, b) => {
      return +moment(a.fechaIni) - +moment(b.fechaIni);
    });
    const aux =
      etapa.actividadDTOList?.map((actividad) =>
        this.setSegundoNivelHelper(etapa, actividad)
      ) || [];
    return aux;
  }

  setSegundoNivelHelper(etapa, actividad) {
    return {
      data: {
        id: actividad.actividadId,
        actividad: actividad.descripcion,
        responsable: actividad.responsable,
        periodo:
          moment(actividad.fechaIni + ' ' + actividad.horaIni).format(
            'DD-MM-YYYY HH:mm A'
          ) +
          ' - ' +
          moment(actividad.fechaFin + ' ' + actividad.horaFin).format(
            'DD-MM-YYYY HH:mm A'
          ),
        restOfData: { etapa, actividad },
      },
      expanded: true,
    };
  }

  getDataEvent(e: PageEvent) {
    // this.pageSize = e.pageSize;
    // const aux = this.data.slice(0);
    // this.dataToRender = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    // this.arrayBuilded = this.transformArray(this.dataToRender);
    // this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
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
    const minWithForMultipleColumns = 200;
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
    model.title = 'Lista de actividades Cronograma';
    model.headers = ['Etapa', 'Actividad', 'Responsable', 'Periodo'];
    model.keys = ['etapa', 'actividad', 'responsable', 'periodo'];
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
          etapa: eva.restOfData.etapa.descripcion,
          actividad: eva.restOfData.actividad.descripcion,
          responsable: eva.restOfData.actividad.responsable,
          periodo:
            moment(
              eva.restOfData.actividad.fechaIni +
              ' ' +
              eva.restOfData.actividad.horaIni
            ).format('DD-MM-YYYY HH:mm A') +
            ' - ' +
            moment(
              eva.restOfData.actividad.fechaFin +
              ' ' +
              eva.restOfData.actividad.horaFin
            ).format('DD-MM-YYYY HH:mm A'),
        });
      } else {
        evaluacionesFormateadas.push({
          etapa: null,
          actividad: null,
          responsable: null,
          periodo: null,
        });
      }
    });

    this.dataExport.data = evaluacionesFormateadas;
    this.exportExcelService.exportExcel(this.dataExport);
  }

  getOnlyEvaluations(array: any[], onlyEvaluations: any) {
    array.forEach((item: any) => {
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

  validarDeleteVisible(row: any) {
    let returned: boolean = true;

    const etapa = this.data.find(
      (item: any) => item.etapaId === row.data.restOfData.etapa.etapaId
    );

    if (etapa && etapa.actividadDTOList.length <= 1) {
      returned = false;
    }

    return returned;
  }
}
