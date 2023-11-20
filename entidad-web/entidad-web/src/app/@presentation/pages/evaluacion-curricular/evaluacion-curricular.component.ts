import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl } from '@angular/forms';
import { TableColumn } from '../../../@presentation/@common-components/material-table/table-column';
import { Router } from '@angular/router';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Utils } from 'src/app/utils/utils';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';

@Component({
  selector: 'serv-talento-evaluacion-curricular',
  templateUrl: './evaluacion-curricular.component.html',
  styleUrls: ['./evaluacion-curricular.component.scss'],
})
export class EvaluacionCurricularComponent implements OnInit {
  filterForm: FormGroup;

  conocimientoTableColumns: TableColumn[];
  data: any[] = [];

  estados_autocomplete: any[] = [];
  convocatorias_autocomplete: any[] = [];
  _convocatorias_autocomplete: any[] = [];
  regimen_autocomplete: any[] = [];
  _regimen_autocomplete: any[] = [];
  idBase: any;

  constructor(
    private router: Router,
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private srvSeleccionRepository: SeleccionServirRepository,
    private authenticationRepository: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.initializeFilterForm();
    this.initializeColumns();
    this.listarAutocomplete();
    this.listarCombocatorias();
  }

  initializeFilterForm() {
    this.filterForm = new FormGroup({
      convocatoria: new FormControl(''),
      regimen: new FormControl(''),
      rangoFechas: new FormControl(''),
      estado: new FormControl(''),
    });
  }

  initializeColumns(): void {
    this.conocimientoTableColumns = [
      {
        name: '#',
        dataKey: 'index',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CÓDIGO DE CONVOCATORIA',
        dataKey: 'convocatoria',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'RÉGIMEN',
        dataKey: 'regimen',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'CANT PERFILES',
        dataKey: 'cantidadPerfiles',
        position: 'center',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'INICIO CONVOCATORIA',
        dataKey: 'fechaInicioConvocatoria',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'FIN CONVOCATORIA',
        dataKey: 'fechaFinConvocatoria',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'ESTADO',
        dataKey: 'desEstado',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  listarAutocomplete(): void {
    this.evaluacionCurricularService
      .getEstadosConvocatoria()
      .toPromise()
      .then((res: any) => {
        this.estados_autocomplete = res;
      });

    this.srvSeleccionRepository
      .getConvocatoriasEntidad(
        this.authenticationRepository.getCurrentUserValue.entidadId
      )
      .toPromise()
      .then((res: any) => {
        this._convocatorias_autocomplete = res;
        this.convocatorias_autocomplete = res;
      });

    this.srvSeleccionRepository
      .getRegimenEntidad(
        this.authenticationRepository.getCurrentUserValue.entidadId
      )
      .toPromise()
      .then((res: any) => {
        this._regimen_autocomplete = res;
        this.regimen_autocomplete = res;
      });
  }

  listarCombocatorias(): void {
    const request: any = {
      trace: {
        traceId: 'string',
      },
      payload: this.get_payload(),
    };

    this.evaluacionCurricularService
      .getEvaluacionFiltro(
        this.authenticationRepository.getCurrentUserValue.entidadId,
        request
      )
      .subscribe((res: any[]) => {
        this.data = res;
        this.data.forEach((item: any, index: number) => {
          item.index = index + 1;
          item.fechaInicioConvocatoria =
            item.fechaInicioConvocatoria == null
              ? '-'
              : Utils.formatDateTimeFechaString(
                  item.fechaInicioConvocatoria,
                  'DD/MM/yyyy'
                );
          item.fechaFinConvocatoria =
            item.fechaFinConvocatoria == null
              ? '-'
              : Utils.formatDateTimeFechaString(
                  item.fechaFinConvocatoria,
                  'DD/MM/YYYY'
                );
        });
      });
  }

  get_payload() {
    let fechaInicio: string = null;
    let fechaFin: string = null;

    if (this.filterForm.controls['rangoFechas'].value !== null) {
      fechaInicio = this.filterForm.controls['rangoFechas'].value.start;
      fechaFin = this.filterForm.controls['rangoFechas'].value.end;
    }

    let returned: any = {
      codigo: this.filterForm.value.convocatoria,
      idRegimen: this.getIdArray(
        this.regimen_autocomplete,
        'desRegimen',
        this.filterForm.value.regimen,
        'idRegimen'
      ),
      fechaInicio: this.getFechaFormat(fechaInicio),
      fechaFin: this.getFechaFormat(fechaFin),
      estado: this.getIdArray(
        this.estados_autocomplete,
        'sigla',
        this.filterForm.value.estado,
        'maeDetalleId'
      ),
    };

    return returned;
  }

  getFechaFormat(data: any) {
    if (data === null || data === undefined) {
      return null;
    }

    const datetime = Utils.formatFechaString(data, 'DD/MM/YYYY');

    return datetime;
  }

  getIdArray(array: any[], search_key: string, value: string, key: string) {
    let ID: number = null;

    array.forEach((regimen: any) => {
      if (regimen[search_key] === value) {
        ID = regimen[key];
      }
    });

    return ID;
  }

  limpiar(): void {
    this.filterForm.reset();
    this.filterForm.controls['convocatoria'].setValue('');
    this.filterForm.controls['regimen'].setValue('');
    this.filterForm.controls['rangoFechas'].setValue('');
    this.filterForm.controls['estado'].setValue('');
    this.buscar();
  }

  buscar(): void {
    this.listarCombocatorias();
  }

  viewDetailEvaluacion(event: any): void {
    localStorage.setItem('selectedEvaluacion', JSON.stringify(event));
    this.router.navigateByUrl('pages/evaluacion-curricular/evaluacion-detalle');
  }

  onInputChangeAutocomplete(event: any, key: string) {
    const search_text = this.filterForm.controls[key].value;

    this.convocatorias_autocomplete = this._convocatorias_autocomplete;
    this.regimen_autocomplete = this._regimen_autocomplete;

    if (key === 'convocatoria') {
      this.convocatorias_autocomplete = this.convocatorias_autocomplete.filter(
        (item: any) => {
          if (item.codigoConvocatoria !== null) {
            return (
              item.codigoConvocatoria
                .toLowerCase()
                .indexOf(search_text.toLowerCase()) > -1
            );
          }
          return false;
        }
      );
    } else if (key === 'regimen') {
      this.regimen_autocomplete = this.regimen_autocomplete.filter(
        (item: any) => {
          if (item.desRegimen !== null) {
            return (
              item.desRegimen.toLowerCase().indexOf(search_text.toLowerCase()) >
              -1
            );
          }
          return false;
        }
      );
    }
  }

  getDataExportEvaluaciones(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista completa de evaluaciones';
    model.headers = [
      '#',
      'CÓDIGO DE CONVOCATORIA',
      'RÉGIMEN',
      'CANT PERFILES',
      'INICIO CONVOCATORIA',
      'FIN CONVOCATORIA',
      'ESTADO',
    ];
    model.keys = [
      'index',
      'convocatoria',
      'regimen',
      'cantidadPerfiles',
      'fechaInicioConvocatoria',
      'fechaFinConvocatoria',
      'desEstado',
    ];

    return model;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }
}
