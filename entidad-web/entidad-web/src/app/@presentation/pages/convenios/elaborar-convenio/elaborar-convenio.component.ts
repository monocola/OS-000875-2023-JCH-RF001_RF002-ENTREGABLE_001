import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { ModalConvenioComponent } from '../modal-convenio/modal-convenio.component';
import { ModalConvenioEntidadComponent } from '../modal-convenio-entidad/modal-convenio-entidad.component';
import { ModalDataEntidadComponent } from '..//modal-data-entidad/modal-data-entidad.component';
import { ModalAnularContratoComponent } from '../../contratos/modal-anular-contrato/modal-anular-contrato.component';
import { ToastService } from '../../../@common-components/toast';
import { ListaContratoRepository } from 'src/app/@domain/repository/lista-contrato.repository';
import { ConveniosService } from '../convenios.service';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ModalContratoComponent } from '../../contratos/modal-contrato/modal-contrato.component';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { ModalDescargaComponent } from '../../contratos/modal-descarga/modal-descarga.component';
import FileSaver from 'file-saver';
import moment from 'moment';

@Component({
  selector: 'serv-talento-elaborar-convenio',
  templateUrl: './elaborar-convenio.component.html',
  styleUrls: ['./elaborar-convenio.component.scss'],
})
export class ElaborarConvenioComponent implements OnInit {
  filterForm: FormGroup;
  valor: any = 1;
  regimenes = [{ id: 1, descripcion: 'Valor1' }];
  datosContrato: any = [];
  datosEntidad: any = [];
  datosPostulante: any = [];
  codProEstado: any = '';
  estadoContrato: any = '';
  convenioList: any = [];
  contratoCreado: boolean = false;
  maestraValor: any = [];
  contratoList: any = [];
  contratoId: any = '';
  fechainiPractica: any = '';
  fechafinPractica: any = '';
  fechasubscripcion: any = '';
  fechanacimiento: any = '';

  searchMode = false;
  estados = [];
  perfiles = [];
  textObservacion: string = null;
  title: string = 'Crear';
  fechaVinculacion: any = '';
  fechaSubscripcion: any = '';
  fechaNacimiento: any = '';
  contratos = [];

  page: number = 0;
  size: number = 50;
  total: number = 0;
  filtros: any = {
    tipo: 0,
    regimen: 0,
    perfil: 0,
    estado: 0,
    fecIni: '',
    fecFin: '',
  };

  constructor(
    private fb: FormBuilder,
    public router: Router,
    private dialog: MatDialog,
    private toast: ToastService,
    private ListaContratoService: ListaContratoRepository,
    public helperService: ConveniosService,
    private seguimientoService: SeguimientoRepository,
    private maestraService: MaestraRepository,
  ) { }

  ngOnInit(): void {
    if (!this.helperService.formConvenio) this.helperService.initializeForm();

    this.initializeForm();
    this.getData();
    this.getListContrato();
    this.dataMaestra();
    this.estadoContrato = this.g.estado.value;
    this.codProEstado = this.g.codProEstado.value;
  }

  descargar(e: any) {
    const modal = this.dialog.open(ModalDescargaComponent, {
      data: {
        contratoId: this.g.idContrato.value,
        validador: 2,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        let fileName = e.archivoSuscrito.split('/').pop();

        this.maestraService
          .downloadBase64(e.archivoSuscrito)
          .subscribe((resp) => {
            let base64String = 'data:application/pdf;base64,' + resp;
            FileSaver.saveAs(base64String, fileName.split('.')[0]);
          });
      }
    });
  }

  dataMaestra() {
    this.maestraService.getMaestraDetalleByCodandCodProg(
      'TBL_EST_CONT_CONV',
      2
    ).subscribe((res) => {
      this.maestraValor = res;
    });
  }

  getData() { }
  get g() {
    return this.helperService.formConvenio.controls;
  }
  get f() {
    return this.filterForm.controls;
  }
  initializeForm() {
    this.filterForm = this.fb.group({});
  }




  crearContrato() {

    if (
      this.contratoList.codigoTipoContrato === 'PRE1401'
    ) {

      if (
        this.contratoList.direccionLabores != null &&
        this.contratoList.periodoConvenio != null &&
        this.contratoList.fechaIniPractica != null &&
        this.contratoList.fechaFinPractica != null &&
        this.contratoList.horaIniPractica != null &&
        this.contratoList.horaFinPractica != null &&
        this.contratoList.puestoRepreUni != null &&
        this.contratoList.nombRepreUni != null &&
        this.contratoList.tipoDocRepreUni != null &&
        this.contratoList.nroDocRepreUni != null &&
        this.contratoList.direccionCentroEstudios != null &&
        this.contratoList.rucCentroEstudios != null
      ) {
        this.toast.showToast(
          'Se creó el convenio con éxito.',
          'success',
          'Atención'
        );

        this.contratoCreado = !this.contratoCreado;
        this.ListaContratoService.GuardarConvenio(
          this.g.idContrato.value,
          this.contratoList.direccionLabores,
          this.contratoList.periodoConvenio,
          this.contratoList.fechaIniPractica,
          this.contratoList.fechaFinPractica,
          this.contratoList.horaIniPractica.substring(0, 5),
          this.contratoList.horaFinPractica.substring(0, 5),
          this.contratoList.puestoResponsableOrh,
          this.contratoList.responsableOrh,
          this.contratoList.tipoDocResponsable,
          this.contratoList.nroDocResponsable,
          this.contratoList.puestoRepreUni,
          this.contratoList.nombRepreUni,
          this.contratoList.tipoDocRepreUni,
          this.contratoList.nroDocRepreUni,
          this.contratoList.direccionCentroEstudios,
          this.contratoList.rucCentroEstudios,
          this.maestraValor[0].maeDetalleId
        ).subscribe();

      } else {
        this.toast.showToast(
          'Los campos de Convenio y Entidad deben estar llenos.',
          'danger',
          'Atención'
        );
      }
    } else if (this.contratoList.codigoTipoContrato === 'PRO1401') {

      if (
        this.contratoList.direccionLabores != null &&
        this.contratoList.periodoConvenio != null &&
        this.contratoList.fechaIniPractica != null &&
        this.contratoList.fechaFinPractica != null &&
        this.contratoList.horaIniPractica != null &&
        this.contratoList.horaFinPractica != null
      ) {

        this.toast.showToast(
          'Se creó el contrato con éxito.',
          'success',
          'Atención'
        );

        this.contratoCreado = !this.contratoCreado;
        this.ListaContratoService.GuardarConvenio(
          this.g.idContrato.value,
          this.contratoList.direccionLabores,
          this.contratoList.periodoConvenio,
          this.contratoList.fechaIniPractica,
          this.contratoList.fechaFinPractica,
          this.contratoList.horaIniPractica.substring(0, 5),
          this.contratoList.horaFinPractica.substring(0, 5),
          this.contratoList.puestoResponsableOrh,
          this.contratoList.responsableOrh,
          this.contratoList.tipoDocResponsable,
          this.contratoList.nroDocResponsable,
          this.contratoList.puestoRepreUni,
          this.contratoList.nombRepreUni,
          this.contratoList.tipoDocRepreUni,
          this.contratoList.nroDocRepreUni,
          this.contratoList.direccionCentroEstudios,
          this.contratoList.rucCentroEstudios,
          this.maestraValor[0].maeDetalleId
        ).subscribe();

      } else {
        this.toast.showToast('Los campos del Convenio deben estar llenos.', 'danger', 'Atención'
        );
      }

    }
  }


  getBandeja() {
    this.seguimientoService
      .getContratosOrConvenios(
        this.filtros.tipo,
        this.filtros.regimen,
        this.g.idContrato.value,
        this.filtros.perfil,
        this.filtros.fecIni,
        this.filtros.fecFin,
        this.filtros.estado,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.total = res.total;
        this.contratos = res.items;
        let count: number = 1;
        if (this.contratos.length !== 0) {
          this.searchMode = true;
        }
        this.contratos.forEach((item) => {
          item.id = count++ + this.page * this.size;
          item.postulante =
            ((item.nombres + ' ' + item.apellidos).length > 18
              ? (item.nombres + ' ' + item.apellidos).substring(0, 18) + '... '
              : item.nombres + ' ' + item.apellidos + ' ,') +
            (item.tipoDoc === '1' ? 'DNI ' : ' CE ') +
            item.nroDocumento;
          item.fechaContrato =
            item.fechaContrato !== null
              ? moment(item.fechaContrato).format('DD/MM/YYYY')
              : item.fechaContrato;
        });
      });
  }

  subir() {
    const modal = this.dialog.open(ModalContratoComponent, {
      width: '400px',
      data: {
        titulo: 'Registro de comunicado',
        contratoId: this.g.idContrato.value,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.getBandeja();
        this.router.navigateByUrl('pages/generacionconvenio');
        this.toast.showToast(
          'El convenio fue suscrito con éxito',
          'success',
          ''
        );
      }
    });
  }

  openModalConvenio() {
    const modal = this.dialog.open(ModalConvenioComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de Convenio',
        direccionLabores: this.contratoList.direccionLabores,
        periodoConvenio: this.contratoList.periodoConvenio,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.contratoList.direccionLabores = res.direccionLabores;
        this.contratoList.periodoConvenio = res.periodoConvenio;
        this.contratoList.fechaIniPractica =
          res.fechaIniPractica === undefined
            ? ''
            : moment(res.fechaIniPractica).format('yyyy-MM-DD');
        this.contratoList.fechaFinPractica =
          res.fechaFinPractica === undefined
            ? ''
            : moment(res.fechaFinPractica).format('yyyy-MM-DD');
        this.contratoList.horaIniPractica =
          res.horaIniPractica === undefined
            ? ''
            : moment(res.horaIniPractica).format('hh:mm A');
        this.contratoList.horaFinPractica =
          res.horaFinPractica === undefined
            ? ''
            : moment(res.horaFinPractica).format('hh:mm A');

        this.fechainiPractica =
          res.fechaIniPractica === undefined
            ? ''
            : moment(res.fechaIniPractica).format('DD-MM-yyyy');
        this.fechafinPractica =
          res.fechaFinPractica === undefined
            ? ''
            : moment(res.fechaFinPractica).format('DD-MM-yyyy');
      }
    });
  }

  openModalEntidadConvenio() {
    const modal = this.dialog.open(ModalConvenioEntidadComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de Convenio',
        puestoRepreUni: this.contratoList.puestoRepreUni,
        nombRepreUni: this.contratoList.nombRepreUni,
        nroDocRepreUni: this.contratoList.nroDocRepreUni,
        direccionCentroEstudios: this.contratoList.direccionCentroEstudios,
        rucCentroEstudios: this.contratoList.rucCentroEstudios,
        codigoTipoContrato: this.contratoList.codigoTipoContrato
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.contratoList.puestoRepreUni = res.puestoRepreUni;
        this.contratoList.nombRepreUni = res.nombRepreUni;
        this.contratoList.tipoDocRepreUni = res.TipoDoc;
        this.contratoList.nroDocRepreUni = res.nroDocRepreUni;
        this.contratoList.direccionCentroEstudios = res.direccionCentroEstudios;
        this.contratoList.rucCentroEstudios = res.rucCentroEstudios;
      }
    });
  }

  openModalEntidadData() {
    const modal = this.dialog.open(ModalDataEntidadComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de Entidad',
        puestoResponsableOrh: this.contratoList.puestoResponsableOrh,
        responsableOrh: this.contratoList.responsableOrh,
        nroDocResponsable: this.contratoList.nroDocResponsable,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.contratoList.puestoResponsableOrh = res.puestoResponsableOrh;
        this.contratoList.responsableOrh = res.responsableOrh;
        this.contratoList.tipoDocResponsable = res.TipoDoc;
        this.contratoList.nroDocResponsable = res.nroDocResponsable;
      }
    });
  }

  getListContrato() {
    this.ListaContratoService.getContrato(this.g.idContrato.value).subscribe(
      (res) => {
        this.contratoList = res;

        this.fechainiPractica =
          this.contratoList.fechaIniPractica === null
            ? ''
            : this.contratoList.fechaIniPractica
              .substring(0, 10)
              .split('-')
              .reverse()
              .join('-');
        this.fechafinPractica =
          this.contratoList.fechaFinPractica === null
            ? ''
            : this.contratoList.fechaFinPractica
              .substring(0, 10)
              .split('-')
              .reverse()
              .join('-');
        this.fechasubscripcion =
          this.contratoList.fechaSubscripcion === null
            ? ''
            : this.contratoList.fechaSubscripcion
              .substring(0, 10)
              .split('-')
              .reverse()
              .join('-');
        this.fechanacimiento =
          this.contratoList.fechaNacimiento === null
            ? ''
            : this.contratoList.fechaNacimiento
              .substring(0, 10)
              .split('-')
              .reverse()
              .join('-');
      }
    );
  }

  desestimarContrato() {
    const modal = this.dialog.open(ModalAnularContratoComponent, {
      width: '100px',
      data: {
        titulo: 'Desestimar el Contrato',
        contratoId: this.g.idContrato.value,
        tipoTrabajo: '2',
        baseId: this.contratoList.baseId,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.router.navigateByUrl('pages/generacionconvenio');
        this.toast.showToast(
          'El accesitario se creo de manera exitosa',
          'success',
          ''
        );
      }
    });
  }
}
