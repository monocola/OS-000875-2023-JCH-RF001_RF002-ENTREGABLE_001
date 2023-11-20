import { Component, OnInit } from '@angular/core';
import { ComboitemModel } from '../../../@data/model/generic/comboitem.model';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { NbPosition, NbTrigger } from '@nebular/theme';
import { SeguimientoRepository } from '../../../@domain/repository/seguimiento.repository';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../@common-components/toast';
import { ModalContratosComponent } from './modal-contratos/modal-contratos.component';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { SeguimientoComunicadoService } from '../seguimiento-comunicado/seguimiento-comunicado.service';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { ModalRedamComponent } from './modal-redam/modal-redam.component';
import { ExportExcelModel } from '../../@service/export-excel.service';
import moment from 'moment';
import { NotasComponent } from './notas/notas.component';
import { Const } from '../../../@data/services/const';
@Component({
  selector: 'serv-talento-seguimiento-eleccion',
  templateUrl: './seguimiento-eleccion.component.html',
  styleUrls: ['./seguimiento-eleccion.component.scss'],
})
export class SeguimientoEleccionComponent implements OnInit {
  popHint: NbTrigger = NbTrigger.HINT;
  pospopRigth: NbPosition = NbPosition.END;
  today: moment.Moment = moment();
  fechaInicio: moment.Moment = moment();
  fechaFin: moment.Moment = moment();
  progressPorcent = 0;
  grupo: ComboitemModel[] = [];
  evaluacion: ComboitemModel[] = [];
  perfiles: ComboitemModel[] = [];
  opcionesRedam: ComboitemModel[] = [
    {
      value: 1,
      description: 'INSCRITO',
    },
    {
      value: 0,
      description: 'NO INSCRITO',
    },
  ];
  dataTipoContrato: any = [];
  estados: ComboitemModel[] = [
    {
      value: 1,
      description: 'CALIFICA',
    },
    {
      value: 0,
      description: 'NO CALIFICA',
    },
  ];
  filterForm: FormGroup = null;
  showCompleateFilter: boolean = false;
  columns: TableColumn[];
  data: any[] = [];

  page: number = 0;
  size: number = 50;
  total: number = 0;
  envio: boolean = false;

  isFiltro: boolean = false;
  filterValue: string = '';

  filtros = {
    perfil: 0,
    fecIni: '',
    fecFin: '',
    estado: 0,
    rnssc: 0,
    reqmin: 0,
  };

  vacantes: number;
  califica: number;
  nocalifica: number;
  convocatoria: string;

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    if (this.isFiltro) {
      this.buscador();
    } else {
      this.getData();
    }
  }
  entidadId: number;
  baseId: number;

  constructor(
    private fb: FormBuilder,
    public router: Router,
    private seguimientoRepository: SeguimientoRepository,
    public dialog: MatDialog,
    private toast: ToastService,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    public helperService: SeguimientoComunicadoService,
    private seleccionService: SeleccionServirRepository
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.entidadId = JSON.parse(sessionStorage.getItem('persona')).entidadId;
    this.baseId = this.g?.baseId.value;

    if (!this.helperService.formConvocatoria)
      this.helperService.initializeForm();
    this.today = moment();
    this.convocatoria = this.g.nomConvocatoria.value;
    this.fechaInicio = moment('01/07/2021', 'DD/MM/YYYY');
    this.fechaFin = moment('15/07/2021', 'DD/MM/YYYY');
    this.progressPorcent =
      (this.today.diff(this.fechaInicio, 'days') * 100) /
      this.fechaFin.diff(this.fechaInicio, 'days');

    if (this.g.baseId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
      return;
    }

    this.evaluacionConocimientosService
      .comboPerfiles(this.g.baseId.value)
      .subscribe((res) => {
        this.perfiles = res;
      });

    /*this.seguimientoRepository
      .getFiltroMaestra('VAL_REDAM')
      .subscribe((items) => {
        this.opcionesRedam = items;
      });

    this.seguimientoRepository
      .getFiltroMaestra('EST_ETAPAS')
      .subscribe((items) => (this.estados = items));*/
    this.columns = [
      {
        name: 'PERFIL l GRUPO',
        dataKey: 'perfil',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'DATOS PERSONALES DEL POSTULANTE',
        dataKey: 'postulante',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'FECHA DE POSTULACIÓN',
        dataKey: 'fechaPostulacion',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      /*       {
        name: 'RESULTADO',
        dataKey: 'etapa',
        position: 'left',
        isSortable: true,
        width: '7%',
      },
      {
        name: 'REDAM',
        dataKey: 'redam',
        position: 'left',
        isSortable: true,
        width: '5%',
      }, */
    ];

    this.getTipoContrato();
  }

  getTipoContrato() {
    this.seleccionService
      .getTipoContrato(this.g.regimen.value)
      .subscribe((res) => {
        this.dataTipoContrato = res;
      });
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      perfil: 0,
      periodo: ['', Validators.required],
      redam: null,
      estado: null,
    });
  }

  get f() {
    return this.filterForm.controls;
  }

  get g() {
    return this.helperService.formConvocatoria?.controls;
  }

  limpiar() {
    this.initializeForm();
    if (!this.isFiltro) {
      this.buscar();
    }
  }

  buscar() {
    if (!this.showCompleateFilter) {
      this.showCompleateFilter = true;
    }
    this.isFiltro = false;
    this.filtros.perfil = this.f.perfil.value;
    this.filtros.estado = this.f.estado.value;
    this.filtros.rnssc = this.f.redam.value;
    this.filtros.fecIni =
      this.f.periodo.value.start === undefined
        ? ''
        : moment(this.f.periodo.value.start).format('DD/MM/yyyy');
    this.filtros.fecFin =
      this.f.periodo.value.end === undefined
        ? ''
        : moment(this.f.periodo.value.end).format('DD/MM/yyyy');
    this.page = 0;
    if (this.entidadId != null && this.baseId != null) {
      this.seleccionService.getInterpolacion(this.entidadId, this.baseId)
        .subscribe( (res) => {
          if (res.status.success) {
            this.getData();
          } else {
            this.toast.showToast("Error Inesperado al realizar el calculo de la Interpolación","warning");
          }
        }, (error) => {
          this.toast.showToast("Error Inesperado al realizar el calculo de la Interpolación", 'danger');
          console.log(error.message);
        });
    } else {
      console.log("Volver a cargar los datos de selección");
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
    }
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }
  action(e) {}

  private getData() {
    this.seleccionService
      .getPostulantesByConvocatoria(
        this.g.baseId.value,
        this.filtros.perfil,
        this.filtros.estado,
        this.filtros.rnssc,
        this.filtros.reqmin,
        this.filtros.fecIni,
        this.filtros.fecFin,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.data = res.items;
        this.nocalifica = res.noCalifican;
        this.califica = res.califican;
        this.vacantes = res.vacantes;
        this.total = res.total;
        let count = 1;
        this.envio = res.flagConvenio;
        this.data.forEach((element) => {
          element.id = count;
          element.estado = element.estado != null ? element.estado : this.actualizaEstado(element.flagCalifica);
          element.ordenMerito = 'N° ' + (count++ + this.page * this.size);
          element.estadoPostulante = false;
          element.fechaPostulacion = moment(element.fechaPostulacion).format(
            'DD/MM/yyyy'
          );
          element.postulante =
            ((element.nombres + ' ' + element.apellidos).length > 20
              ? (element.nombres + ' ' + element.apellidos).substring(0, 20) +
                '... '
              : element.nombres + ' ' + element.apellidos + ',') +
            (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
            ' ' +
            element.documento;
          element.postulanteCompleto =
            element.nombres + ' ' + element.apellidos;
          element.documentoCompleto =
            (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
            ' ' +
            element.documento;
          this.agregarBonificacion(element);
        });
      });
  }

  actualizaEstado(flagCalifica: any) {
    if (flagCalifica === Const.EST_EVAL_NOCALIF ) {
      return "NO CALIFICA";
    } else if (flagCalifica === Const.EST_EVAL_CALIF ) {
      return "CALIFICA";
    } else if (flagCalifica === Const.EST_EVAL_DESCALF) {
      return "DESCALIFICADO";
    } else if (flagCalifica === Const.EST_EVAL_NOASIST) {
      return "NO ASISTIO";
    } else {
      return "EN PROCESO";
    }
  }
  enviarContratoConvenio() {
    const modalNivelEducativo = this.dialog.open(ModalContratosComponent, {
      width: '40rem',
      data: {
        listaAptos: this.data,
        regimen: this.g.regimenId.value,
        vacantes: this.vacantes,
        finalistas: this.califica,
        dataTipoContrato: this.dataTipoContrato,
      },
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res) {
        this.envio = true;
      }
    });
  }

  GoToContratosOrConvenios() {
    if (this.g.regimenId.value === '1') {
      this.router.navigateByUrl('pages/generacioncontrato');
    } else {
      this.router.navigateByUrl('pages/generacionconvenio');
    }
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Contratos';
    model.headers = [
      '#',
      'PERFIL',
      'POSTULANTE',
      'DOCUMENTO',
      'FECHA POSTULACION',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'perfil',
      'postulanteCompleto',
      'documentoCompleto',
      'fechaPostulacion',
      'estado',
    ];
    return model;
  }

  dataRedam(e: any) {
    const modalRedam = this.dialog.open(ModalRedamComponent, {
      width: '40rem',
      data: {
        titulo: 'prueba',
        apellidos: e.apellidos,
        nombres: e.nombres,
      },
    });

    modalRedam.afterClosed().subscribe((res) => {
      if (res && res.response) {
        const request: any = {
          trace: {
            traceId: 'string',
          },
          payload: {
            postulanteSelId: e.convocatoriaPostulanteId, // id de convocatoria postulante ID
            flagRedam: e.flagredam === true ? 1 : 0,
            descripcion: res.descripcion,
          },
        };

        this.seleccionService
          .updateFlagRedam(request)
          .toPromise()
          .then(() => {
            this.toast.showToast(
              'Registro exitoso, los cambios impactaron en el estado del Postulante.',
              'success',
              'Atención'
            );
          });
      } else {
        e.flagredam = !e.flagredam;
      }
    });
  }

  verNotas(e: any) {
    this.dialog.open(NotasComponent, {
      data: {
        titulo: 'NOTAS',
        notas: e.otrasEvaluacionesDTOList,
      },
    });
  }

  keyup(event: Event) {
    this.initializeForm();
    this.filterValue = (event.target as HTMLInputElement).value;
    this.page = 0;
    this.buscador();
  }

  buscador() {
    if (this.filterValue.length >= 3) {
      this.isFiltro = true;
      this.seleccionService
        .FiltroEleccion(
          this.g.baseId.value,
          this.filterValue,
          this.page,
          this.size
        )
        .subscribe((res) => {
          this.data = res.items;
          this.total = res.total;
          let count = 1;
          this.data.forEach((element) => {
            element.ordenMerito = 'N° ' + (count++ + this.page * this.size);
            element.estadoPostulante = false;
            element.fechaPostulacion = moment(element.fechaPostulacion).format(
              'DD/MM/yyyy'
            );
            element.postulante =
              ((element.nombres + ' ' + element.apellidos).length > 20
                ? (element.nombres + ' ' + element.apellidos).substring(0, 20) +
                  '... '
                : element.nombres + ' ' + element.apellidos + ',') +
              (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
              ' ' +
              element.documento;
            element.postulanteCompleto =
              element.nombres + ' ' + element.apellidos;
            element.documentoCompleto =
              (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
              ' ' +
              element.documento;
            this.agregarBonificacion(element);
          });
        });
    } else {
      if (this.filterValue.length === 0) {
        this.buscar();
      }
    }
  }

  agregarBonificacion(element) {
    if (element.tieneBono && element.otrasEvaluacionesDTOList.length !== 0) {
      if (element.otrasEvaluacionesDTOList[0].bonificacionDTOList !== 0) {
        element.otrasEvaluacionesDTOList[0].bonificacionDTOList.forEach(
          (item) => {
            let bonificacion = {
              descripcion: 'Bonificación ' + item.tipoBoni,
              notasEval: item.bonificacion + ' % del resultado final',
            };
            element.otrasEvaluacionesDTOList.push(bonificacion);
          }
        );
      }
    }
  }
}
