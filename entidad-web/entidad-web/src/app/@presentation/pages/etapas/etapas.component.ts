import { DialogConfirmRnsscComponent } from './dialog-confirm-rnssc/dialog-confirm-rnssc.component';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { SeguimientoRepository } from '../../../@domain/repository/seguimiento.repository';
import moment from 'moment';
import { Router } from '@angular/router';

import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { SeguimientoComunicadoService } from '../seguimiento-comunicado/seguimiento-comunicado.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { ModalAprobarComponent } from './modal-aprobar/modal-aprobar.component';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../@common-components/toast';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { ComboitemModel } from 'src/app/@data/model/generic/comboitem.model';
import { NbDialogService } from '@nebular/theme';

@Component({
  selector: 'serv-talento-etapas',
  templateUrl: './etapas.component.html',
  styleUrls: ['./etapas.component.scss'],
})
export class EtapasComponent implements OnInit {
  filterForm: FormGroup = null;
  perfiles: [];
  estados = [];
  estadosReclutamiento: ComboitemModel[] = [];
  nombreConvocatoria: string = '';
  rnssc = [];
  reqmin = [];

  showSelect: any = false;

  columns: TableColumn[];
  data: any[] = [];

  page: number = 0;
  size: number = 20;
  total: number = 0;
  filtros = {
    perfil: 0,
    fecIni: '',
    fecFin: '',
    estado: 0,
    rnssc: 0,
    reqmin: 0,
  };
  diaInicio = 'Inicio: ???';
  diaFinal = 'Fin: ???';
  totalDias = 0;
  etapaId;
  progress: any;
  rangePickerStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private seguimientoRepository: SeguimientoRepository,
    public router: Router,

    public helperService: SeguimientoComunicadoService,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private maestraService: MaestraRepository,
    private seleccionService: SeleccionServirRepository,
    private basesService: BasesRepository,
    public dialog: MatDialog,
    private toast: ToastService,
    private evaluacionCurricularService: EvaluacionCurricularRepository,
  ) { }

  ngOnInit(): void {
    if (!this.helperService.formConvocatoria)
      this.helperService.initializeForm();
    this.initializeForm();
    this.initializeColumns();
    this.validarSesion();
    this.getEtapaProgresoReclutamiento();

  }

  validarSesion() {
    if (this.g.baseId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
    } else {
      this.getEstados();
      this.getCombox();
    }
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      persona: '',
      perfil: 0,
      periodo: null,
      estado: 0,
      rnssc: 0,
      reqmin: 0,
    });
  }

  initializeColumns() {
    this.columns = [
      {
        name: '#',
        dataKey: 'id',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'Perfil',
        dataKey: 'perfil',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      // {
      //   name: 'Datos Personales del Postulante',
      //   dataKey: 'postulante',
      //   position: 'left',
      //   isSortable: true,
      //   width: '20%',
      // },
      // {
      //   name: 'Fecha de postulación',
      //   dataKey: 'fechaPostulacion',
      //   position: 'left',
      //   isSortable: true,
      //   width: '10%',
      // },
    ];
  }

  get f() {
    return this.filterForm.controls;
  }

  get g() {
    return this.helperService.formConvocatoria.controls;
  }

  getCombox() {
    this.nombreConvocatoria = this.g.nomConvocatoria.value;
    this.cargarPeriodo();
    this.evaluacionConocimientosService
      .comboPerfiles(this.g.baseId.value)
      .subscribe((res) => {
        this.perfiles = res;
      });

    this.maestraService.getMaestraDetalleByCod('VAL_RNCSS').subscribe((res) => {
      this.rnssc = res;
    });

    this.maestraService
      .getMaestraDetalleByCod('VAL_REQMIN')
      .subscribe((res) => {
        this.reqmin = res;
      });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getData();
  }

  getBuscar() {
    this.showSelect = true;
    this.filtros.perfil = this.f.perfil.value;
    this.filtros.estado = this.f.estado.value;
    this.filtros.rnssc = this.f.rnssc.value;
    this.filtros.reqmin = this.f.reqmin.value;
    let fecIni = '';
    let fecFin = '';
    if (this.f.periodo.value && this.f.periodo.value.start) {
      fecIni = moment(this.f.periodo.value.start).format('DD/MM/yyyy');
    }

    if (this.f.periodo.value && this.f.periodo.value.end) {
      fecFin = moment(this.f.periodo.value.end).format('DD/MM/yyyy');
    }
    this.filtros.fecIni = fecIni;
    this.filtros.fecFin = fecFin;
    this.page = 0;
    this.getData();
  }

  private getData() {
    this.seleccionService
      .GetPostulanteEtapaReclutamiento(
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
        console.log(res);
        this.data = res.items;
        this.total = res.total;
        let count = 1;
        this.data.forEach((element) => {
          element.id = count++ + this.page * this.size;
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
        });
      });
  }


  getEstados() {
    let flag = 'ESTADO_TODOS';
    this.evaluacionCurricularService
      .listarEstadoEvaluacionCurricular(flag)
      .subscribe((res: any) => {
        this.estadosReclutamiento = res;
        console.log(this.estadosReclutamiento);
      });
  }
  cargarPeriodo() {
    this.basesService
      .getListaCronogramasV2(this.g.convocatoriaId.value)
      .subscribe((res) => {
        console.info(res);
        let obj: any;
        for (let index = 0; index < res.length; index++) {
          for (let j = 0; j < this.estados.length; j++) {
            if (res[index].etapaId === this.estados[j].maeDetalleId) {
              this.etapaId = this.estados[j].maeDetalleId;
              if (this.estados[j].codProg === '2') {
                obj = res[index];
                break;
              }
            }
          }
        }

        if (obj !== undefined) {
          let ini = moment(obj.periodoIni).format('YYYY-MM-DD');
          let fin = moment(obj.periodoFin).format('YYYY-MM-DD');
          let total = moment(fin).diff(moment(ini), 'days');
          this.diaInicio =
            'Inicio: ' + moment(obj.periodoIni).format('DD/MM/YYYY');
          this.diaFinal = 'Fin: ' + moment(obj.periodoFin).format('DD/MM/YYYY');
          this.totalDias =
            +moment(Date.now()).diff(moment(ini), 'days') / total;
        }
      });
  }

  verMensaje(e: any) {
    console.log(e);
  }

  limpiar() {
    this.initializeForm();
    this.getBuscar();
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de comunicados';
    model.headers = [
      '#',
      'PERFIL',
      'POSTULANTE',
      'FECHA',
      'VALID. RNSSC',
      'REQ. MIN',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'perfil',
      'postulante',
      'fechaPostulacion',
      'rnssc',
      'reqMinimo',
      'estado',
    ];
    return model;
  }

  openConvocatoriaPostulante(row: any) {
    sessionStorage.setItem("convocatoriaPostulante", JSON.stringify(row));
    console.log("convocatoriaPostulante",row);
    this.router.navigateByUrl('pages/etapas/resultados-postulante');
  }

  getEtapaProgresoReclutamiento() {
    this.seleccionService
      .EtapaProgresoReclutamiento(this.g.baseId.value, this.g.etapaId.value)
      .subscribe((res) => {
        this.progress = res;
        // this.diaInicio = res.fechaInicio;
        // this.diaFinal = res.fechaFin;
        if (this.g.baseId.value > 0) {
          this.basesService
            .getListaCronogramasV2(this.g.baseId.value)
            .subscribe((res1) => {
              let listCrono: any = res1;
              if (listCrono.length > 0) {
                let cronoIni: any = listCrono[0];
                let cronoFin: any = listCrono[listCrono.length -1];
                this.diaInicio = moment(cronoIni.fechaInicio).format("DD/MM/YYYY");
                this.diaFinal = moment(cronoFin.periodoFin).format('DD/MM/YYYY');
              }
            });
        }
      });
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const periodo = this.filterForm.controls['periodo'].value;

    if (this.filterForm.controls['periodo'].errors && periodo === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  validarRnssc() {
    if (confirm("Desea realizar la validación RNSSC?")) {
      this.evaluacionConocimientosService.actualizarFlagSanciones(this.g.baseId.value,this.f.perfil.value).subscribe((mc) => {
        if (mc) {
          this.getBuscar();
          this.toast.showToast('Validación RNSSC realizada con éxito','primary');
        }

      });
    }
  }
}
