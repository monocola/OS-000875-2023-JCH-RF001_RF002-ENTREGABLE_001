import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import FileSaver from 'file-saver';
import moment from 'moment';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from '../../@common-components/toast';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { SeguimientoComunicadoService } from './seguimiento-comunicado.service';

@Component({
  selector: 'serv-talento-seguimiento-comunicado',
  templateUrl: './seguimiento-comunicado.component.html',
  styleUrls: ['./seguimiento-comunicado.component.scss'],
})
export class SeguimientoComunicadoComponent implements OnInit {
  filterForm: FormGroup;
  TableColumns: TableColumn[];

  perfiles = [];

  etapas = [];

  estados = [];

  comunicados = [];

  searchMode = false;
  lista = [];

  page: number = 0;
  size: number = 10;
  total: number = 0;

  const = Const;

  filtros = {
    perfil: '',
    etapa: '',
    fecIni: '',
    fecFin: '',
    estado: '',
    comunicado: '',
  };

  rol: number = 0;

  constructor(
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    private dialog: MatDialog,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    public router: Router,
    public helperService: SeguimientoComunicadoService,
    private seguimientoService: SeguimientoRepository,
    private toast: ToastService,
    private authenticationRepository: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    if (!this.helperService.formConvocatoria)
      this.helperService.initializeForm();
    this.initializeForm();
    this.validarSesion();
    this.initializeColumns();
  }

  get f() {
    return this.filterForm.controls;
  }

  get g() {
    return this.helperService.formConvocatoria.controls;
  }

  validarSesion() {
    if (this.g.convocatoriaId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
    } else {
      this.getCombox();
      this.getBuscar();
    }
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      perfil: '',
      etapa: '',
      fecha: '',
      estado: '',
      comunicado: '',
    });
  }

  getCombox() {
    this.rol = this.authenticationRepository.getCurrentUserValue.rolId;
    this.evaluacionConocimientosService
      .comboPerfiles(this.g.baseId.value)
      .subscribe((res) => {
        this.perfiles = res;
      });

    this.maestraService
      .getMaestraDetalleByCod('TIP_ETA_PRO')
      .subscribe((res) => {
        this.etapas = res;
      });

    this.maestraService
      .getMaestraDetalleByCod('EST_COMUNI')
      .subscribe((res) => {
        this.estados = res;
      });

    this.maestraService.getMaestraDetalleByCod('TIP_COMU').subscribe((res) => {
      this.comunicados = res;
    });
  }

  clear() {
    this.initializeForm();
    this.getBuscar();
  }

  initializeColumns() {
    this.TableColumns = [
      {
        name: '#',
        dataKey: 'comunicadoId',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'ETAPA',
        dataKey: 'etapa',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'TIPO DE COMUNICADO',
        dataKey: 'tipoComunicado',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'PERFIL',
        dataKey: 'nombrePerfil',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'GESTOR',
        dataKey: 'gestor',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'COORDINADOR',
        dataKey: 'coordinador',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'FECHA',
        dataKey: 'fechaPublicacion',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getBandeja();
  }

  getBuscar() {
    this.filtros.perfil = this.f.perfil.value;
    this.filtros.etapa = this.f.etapa.value;
    this.filtros.fecIni =
      this.f.fecha.value.start === undefined
        ? ''
        : moment(this.f.fecha.value.start).format('DD/MM/yyyy');
    this.filtros.fecFin =
      this.f.fecha.value.end === undefined
        ? ''
        : moment(this.f.fecha.value.end).format('DD/MM/yyyy');
    this.filtros.estado = this.f.estado.value;
    this.filtros.comunicado = this.f.comunicado.value;
    this.page = 0;

    this.getBandeja();
  }

  getBandeja() {
    this.seguimientoService
      .getComunicados(
        this.g.convocatoriaId.value,
        +this.filtros.perfil,
        +this.filtros.etapa,
        this.filtros.fecIni,

        this.filtros.fecFin,
        +this.filtros.comunicado,
        +this.filtros.estado,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.lista = res.items;
        this.total = res.total;

        if (this.lista.length !== 0) {
          this.searchMode = true;
        }
      });
  }

  openModalCrearComunicado() {
    let e = {
      baseId: 0,
      id: 0,
      estado: '',
    };
    this.helperService.enviarConvocatoria(
      e,
      this.g.baseId.value,
      this.g.etapaId.value,
      this.g.desEtapa.value,
      this.g.nomConvocatoria.value,
      true
    );
    this.router.navigateByUrl(
      'pages/seguimientoconvocatoria/comunicado/registro'
    );
  }

  showPDF(e) {
    this.seguimientoService.getComunicado(e.comunicadoId).subscribe((res) => {
      this.getBase64(res.url);
    });
  }

  getBase64(url: string) {
    let fileName = url.split('/').pop();

    this.maestraService.downloadBase64(url).subscribe((resp) => {
      let base64String = 'data:application/pdf;base64,' + resp;
      FileSaver.saveAs(base64String, fileName.split('.')[0]);
    });
  }

  ver(e) {
    this.helperService.enviarConvocatoria(
      e,
      this.g.baseId.value,
      this.g.etapaId.value,
      this.g.desEtapa.value,
      this.g.nomConvocatoria.value,
      false
    );
    this.router.navigateByUrl(
      'pages/seguimientoconvocatoria/comunicado/registro'
    );
  }

  eliminar(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar comunicado',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.seguimientoService
          .inactivarComunicado(e.comunicadoId)
          .subscribe((resp) => {
            this.toast.showToast(resp, 'success', 'Atención');
            this.getBandeja();
          });
      }
    });
  }

  enproceso(e) {
    this.helperService.enviarConvocatoria(
      e,
      this.g.baseId.value,
      this.g.etapaId.value,
      this.g.desEtapa.value,
      this.g.nomConvocatoria.value,
      true
    );
    this.router.navigateByUrl(
      'pages/seguimientoconvocatoria/comunicado/registro'
    );
  }

  observado(e) {
    this.helperService.enviarConvocatoria(
      e,
      this.g.baseId.value,
      this.g.etapaId.value,
      this.g.desEtapa.value,
      this.g.nomConvocatoria.value,
      true
    );
    this.router.navigateByUrl(
      'pages/seguimientoconvocatoria/comunicado/registro'
    );
  }

  editar(e) {
    this.helperService.enviarConvocatoria(
      e,
      this.g.baseId.value,
      this.g.etapaId.value,
      this.g.desEtapa.value,
      this.g.nomConvocatoria.value,
      false
    );
    this.router.navigateByUrl(
      'pages/seguimientoconvocatoria/comunicado/registro'
    );
  }

  enviar(e) {
    console.log(e);
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lista);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de comunicados';
    model.headers = [
      '#',
      'ETAPA',
      'TIPO DE COMUNICADO',
      'PERFIL',
      'GESTOR',
      'COORDINADOR',
      'FECHA',
      'ESTADO',
    ];
    model.keys = [
      'comunicadoid',
      'etapa',
      'tipoComunicado',
      'nombrePerfil',
      'gestor',
      'coordinador',
      'fechaPublicacion',
      'estado',
    ];
    return model;
  }
}
