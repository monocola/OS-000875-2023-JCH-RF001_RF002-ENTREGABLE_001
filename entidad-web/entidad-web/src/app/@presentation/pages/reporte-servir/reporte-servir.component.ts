import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { EvaluacionesServirService } from 'src/app/@data/services/evaluaciones-servir.service';
import { forkJoin } from 'rxjs';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReportesRepository } from 'src/app/@domain/repository/reportes.repository';
import { EntidadRepository } from 'src/app/@domain/repository/entidad.repository';
import moment from 'moment';

@Component({
  selector: 'serv-talento-reporte-servir',
  templateUrl: './reporte-servir.component.html',
  styleUrls: ['./reporte-servir.component.scss'],
})
export class ReporteServirComponent implements OnInit {
  visibleFilters: boolean = true;
  form: FormGroup;

  entidades: any[] = [];
  regimenes: any[] = [];
  etapas: any[] = [];
  departamentos: any[] = [];
  estados: any[] = [];
  roles: any[] = [
    {
      rolId: 1,
      nombreRol: 'Gestor',
    },
    {
      rolId: 2,
      nombreRol: 'Coordinador',
    },
  ];
  responsables: any[] = [];

  columns: TableColumn[];
  data: any[] = [];
  resumen: any = {};
  page: number = 1;
  size: number = 20;

  cantPos: number = 0;
  departamentoNombre: string = 'Todos';
  regimenNombre: string = 'Todos';

  fechaIni: any = null;
  fechaFin: any = null;

  constructor(
    private entidadService: EntidadService,
    private evaluacionServirService: EvaluacionesServirService,
    private maestraService: MaestraService,
    private srvParameterRepository: ParameterRepository,
    private srvReportesRepository: ReportesRepository,
    private entidadRepository: EntidadRepository
  ) {}

  ngOnInit(): void {
    this.form = new FormGroup({
      entidad: new FormControl(null),
      regimen: new FormControl(null),
      periodo: new FormControl(''),
      etapa: new FormControl(null),
      departamento: new FormControl(null),
      estado: new FormControl(null),
      rol: new FormControl(null),
      responsable: new FormControl(null),
    });

    this.initializeColumns();
    this.loadCombox();
    this.filter();
  }

  loadCombox() {
    const getEntidades = this.entidadService.getEntidades();
    const evaluacionServirService = this.evaluacionServirService.getRegimenesServir(
      'TBL_REGIMEN'
    );
    const getMaestraDetalleByCod = this.maestraService.getMaestraDetalleByCod(
      'TIP_ETA_PRO'
    );
    const getDepartamento = this.srvParameterRepository.getDepartamento();
    const getEstadoConvocatoria = this.srvReportesRepository.getEstadoConvocatoria();
    const getRoles = this.srvReportesRepository.getRoles();

    forkJoin([
      getEntidades,
      evaluacionServirService,
      getMaestraDetalleByCod,
      getDepartamento,
      getEstadoConvocatoria,
      getRoles,
    ]).subscribe((results) => {
      this.entidades = results[0];
      this.regimenes = results[1];
      this.etapas = results[2];
      this.departamentos = results[3];
      this.estados = results[4];
      // this.roles = results [5];
    });
  }

  get f() {
    return this.form.controls;
  }

  initializeColumns() {
    this.columns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Reportes';
    model.headers = [
      '#',
      'ENTIDAD',
      'CÓDIGO',
      'CRONOGRAMA',
      'RÉGIMEN',
      'VACANT.',
      'GESTOR ORH',
      'COORDINADOR',
      'POSTUL.',
      'ETAPA',
      'PERFILES',
      'EST. CONV.',
    ];
    model.keys = [
      'correlativo',
      'entidad',
      'codConvocatoria',
      'cronograma',
      'regimen',
      'perfiles_vacantes',
      'gestor',
      'coordinador',
      'perfiles_postulantes',
      'etapa',
      'perfiles_length',
      'estado',
    ];
    return model;
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
  }

  filter() {
    let formValue = this.form.getRawValue();

    this.fechaIni = null;
    this.fechaFin = null;

    if (formValue.periodo && formValue.periodo.start) {
      this.fechaIni = moment(formValue.periodo.start).format('yyyy[-]MM[-]DD');
    }

    if (formValue.periodo && formValue.periodo.end) {
      this.fechaFin = moment(formValue.periodo.end).format('yyyy[-]MM[-]DD');
    }

    const request: any = {
      fechaIni: this.fechaIni,
      fechaFin: this.fechaFin,
      entidadId: formValue.entidad,
      regimenId: formValue.regimen,
      etapaId: formValue.etapa,
      estadoId: formValue.estado,
      departamentoId: formValue.departamento,
      rol: formValue.rol,
      responsableId: formValue.responsable,
    };
    if (formValue.departamento !== null) {
      this.departamentoSelected(formValue.departamento);
    }
    if (formValue.regimen !== null) {
      this.regimenSelected(formValue.regimen);
    }
    this.srvReportesRepository
      .getReporteServir(request)
      .toPromise()
      .then((res: any) => {
        this.data = res;
        this.data.forEach((item, index, array) => {
          array[index].correlativo = index + 1;
          array[index].estadoId = Number(array[index].estadoId);
          array[index].perfiles_length = array[index].perfiles.length;
          array[index].perfiles_vacantes = this.getVacantesPostulantes(
            array[index].perfiles,
            'vacantes'
          );
          array[index].perfiles_postulantes = this.getVacantesPostulantes(
            array[index].perfiles,
            'postulantes'
          );
        });
        this.getCantidadPostulantes(res);
      });
  }

  getRelativeDate(date: string) {
    if (date === null || date === undefined) {
      return '';
    }

    return moment(date).format('ll');
  }

  getCantidadPostulantes(list: any[]) {
    this.cantPos = 0;
    list.forEach((cov: any) => {
      cov.perfiles.forEach((perfil: any) => {
        this.cantPos += perfil.postulantes;
      });
    });
  }

  getVacantesPostulantes(perfiles: any[], object: string) {
    if (perfiles === null || perfiles === undefined) {
      return 0;
    }

    let cantidad: number = 0;
    perfiles.forEach((perfil: any) => {
      cantidad += perfil[object];
    });

    return cantidad;
  }

  reset() {
    this.form.reset();
    this.departamentoNombre = 'Todos';
    this.regimenNombre = 'Todos';
    this.filter();
  }

  departamentoSelected(ubigeoId: any) {
    this.departamentoNombre = this.departamentos.find(
      (x) => x.ubigeoId === ubigeoId
    ).nombre;
  }

  regimenSelected(regId: any) {
    console.log(regId);
    console.log(this.regimenes);
    this.regimenNombre = this.regimenes.find(
      (x) => x.codProg === regId
    ).descripcion;
  }

  entidadSelected(event: any) {
    this.entidadRepository
      .getUsuariosEntidad(event)
      .toPromise()
      .then((res: any) => {
        this.responsables = res;
        this.responsables.forEach((item, index, array) => {
          array[index].fullName =
            array[index].nombres +
            ' ' +
            array[index].apellidoPaterno +
            ' ' +
            array[index].apellidoMaterno;
        });
      });
  }
}
