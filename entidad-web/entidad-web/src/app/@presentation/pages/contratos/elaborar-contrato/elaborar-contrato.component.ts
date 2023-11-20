import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { ModalObservarComponent } from '../modal-observar/modal-observar.component';
import { ModalEntidadComponent } from '../modal-entidad/modal-entidad.component';
import { ModalPostulanteComponent } from '../modal-postulante/modal-postulante.component';
import { ModalAnularContratoComponent } from '../modal-anular-contrato/modal-anular-contrato.component';
import { ToastService } from '../../../@common-components/toast';
import { ListaContratoRepository } from 'src/app/@domain/repository/lista-contrato.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ContratosService } from '../contratos.service';
import { ModalContratoComponent } from '../modal-contrato/modal-contrato.component';
import { ModalDescargaComponent } from '../modal-descarga/modal-descarga.component';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import moment from 'moment';


@Component({
  selector: 'serv-talento-elaborar-contrato',
  templateUrl: './elaborar-contrato.component.html',
  styleUrls: ['./elaborar-contrato.component.scss']
})
export class ElaborarContratoComponent implements OnInit {
  filterForm: FormGroup;
  valor: any = 1;

  fileName: string = 'Subir documento';
  filepdf: string = '';
  pathpdf: string = '';
  urlpdf: string = null;
  aprobacion: boolean = false;
  uploadfile: boolean = true;
  regimenes = [{ id: 1, descripcion: 'Valor1' }];
  datosContrato: any = [];
  datosEntidad: any = [];
  datosPostulante: any = [];
  contratoCreado: boolean = false;
  codProEstado: any = '';
  estadoContrato: any = '';
  maestraValor: any = [];
  contratoList: any = [];
  contratoActualizar: any = {
    fechaResolucion: '',
    nroResolucion: '',
    nroInforme: '',
    fechaVinculacion: '',
    periodoPrueba: '',
    resolResponOrh: '',
    nroNorma: '',
  };

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
    private seguimientoService: SeguimientoRepository,
    private maestraService: MaestraRepository,
    private ListaContratoService: ListaContratoRepository,
    public helperService: ContratosService,
  ) {

  }

  ngOnInit(): void {
    if (!this.helperService.formContrato)
      this.helperService.initializeForm();

    this.initializeForm();
    this.getListContrato();
    this.estadoContrato = this.g.estado.value;
    this.codProEstado = this.g.codProEstado.value;
    this.dataMaestra();

  }

  descargar(e: any) {
    const modal = this.dialog.open(ModalDescargaComponent, {
      data: {
        contratoId: this.g.idContrato.value,
        validador: 2,
      },
    });
    modal.afterClosed().subscribe((res) => {
      console.log(res);
      /*let fileName = null;
      if (res) {
        let fileName = e.archivoSuscrito.split('/').pop();

        this.maestraService
          .downloadBase64(e.archivoSuscrito)
          .subscribe((resp) => {
            let base64String = 'data:application/pdf;base64,' + resp;
            FileSaver.saveAs(base64String, fileName.split('.')[0]);
          });
      }*/
    });
  }

  dataMaestra() {
    this.maestraService
      .getMaestraDetalleByCodandCodProg('TBL_EST_CONT_CONV', 2)
      .subscribe((res) => {
        this.maestraValor = res;
      });
  }

  get g() {
    return this.helperService.formContrato.controls;
  }

  get f() {
    return this.filterForm.controls;
  }
  initializeForm() {
    this.filterForm = this.fb.group({

    });
  }

  openModalObservar() {
    const modal = this.dialog.open(ModalObservarComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de resolución y contrato',
        nroResolucion: this.contratoList.nroResolucion,
        nroInforme: this.contratoList.nroInforme,
        periodoPrueba: this.contratoList.periodoPrueba,
        codigoTipoContrato: this.contratoList.codigoTipoContrato,
        nroMerito: this.contratoList.nroPosPueMeri
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.contratoList.fechaResolucion = res.fechaResolucion === undefined ? '' : moment(res.fechaResolucion).format('yyyy-MM-DD');
        this.contratoList.nroResolucion = res.Nresolucion;
        this.contratoList.nroInforme = res.informe;
        this.contratoList.fechaVinculacion = res.fechaVinculacion === undefined ? '' : moment(res.fechaVinculacion).format('yyyy-MM-DD');
        this.contratoList.periodoPrueba = res.periodoPrueba;
        this.contratoList.nroPosPueMeri = res.nroMerito;

        this.fechaVinculacion = res.fechaVinculacion === undefined ? '' : moment(res.fechaVinculacion).format('DD-MM-yyyy');

      }
    });
  }

  openModaleEntidad() {
    const modal = this.dialog.open(ModalEntidadComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de resolución y contrato',
        resolResponOrh: this.contratoList.resolResponOrh,
        nroNorma: this.contratoList.nroNorma
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.contratoList.resolResponOrh = res.resolResponOrh;
        this.contratoList.nroNorma = res.nroNorma;

      }
    });
  }

  openModalePostulante() {
    const modal = this.dialog.open(ModalPostulanteComponent, {
      width: '12px',
      data: {
        titulo: 'Editar Datos de resolución y contrato',
      },
    });
    modal.afterClosed().subscribe((res) => {

    });
  }

  editContrato() {
    alert('lol');
  }


  crearContrato() {

    if (
      this.contratoList.codigoTipoContrato === 'SCC30057'
    ) {

      if (
        this.contratoList.nroInforme != null &&
        this.contratoList.fechaVinculacion != null
      ) {
        this.toast.showToast(
          'Se creó el contrato con éxito.',
          'success',
          'Atención'
        );

        this.contratoCreado = !this.contratoCreado;
        this.ListaContratoService.GuardarContrato(
          this.g.idContrato.value,
          this.contratoList.nroResolucion,
          this.contratoList.nroInforme,
          this.contratoList.fechaVinculacion,
          this.contratoList.resolResponOrh,
          this.contratoList.nroNorma,
          this.contratoList.periodoPrueba,
          this.maestraValor[0].maeDetalleId,
          this.contratoList.nroPosPueMeri
        ).subscribe();

      } else {
        this.toast.showToast('Los campos del Contrato deben estar llenos.', 'danger', 'Atención'
        );
      }
    } else {

      if (
        this.contratoList.nroInforme != null &&
        this.contratoList.fechaVinculacion != null &&
        this.contratoList.periodoPrueba != null
      ) {

        this.toast.showToast(
          'Se creó el contrato con éxito.',
          'success',
          'Atención'
        );

        this.contratoCreado = !this.contratoCreado;
        this.ListaContratoService.GuardarContrato(
          this.g.idContrato.value,
          this.contratoList.nroResolucion,
          this.contratoList.nroInforme,
          this.contratoList.fechaVinculacion,
          this.contratoList.resolResponOrh,
          this.contratoList.nroNorma,
          this.contratoList.periodoPrueba,
          this.maestraValor[0].maeDetalleId,
          this.contratoList.nroPosPueMeri
        ).subscribe();

      } else {
        this.toast.showToast('Los campos del Contrato deben estar llenos.', 'danger', 'Atención'
        );
      }

    }
  }

  getListContrato() {
    if (this.g.idContrato.value === 0) {
      this.router.navigateByUrl('/pages/generacioncontrato');
    } else {
      this.ListaContratoService
        .getContrato(
          this.g.idContrato.value
        )
        .subscribe((res) => {
          console.info(res);
          this.contratoList = res;
          this.fechaVinculacion = this.contratoList.fechaVinculacion === null ? '' : this.contratoList.fechaVinculacion.substring(0, 10).split('-').reverse().join('-');
          this.fechaSubscripcion = this.contratoList.fechaSubscripcion === null ? '' : this.contratoList.fechaSubscripcion.substring(0, 10).split('-').reverse().join('-');
          this.fechaNacimiento = this.contratoList.fechaNacimiento === null ? '' : this.contratoList.fechaNacimiento.substring(0, 10).split('-').reverse().join('-');
        });
    }
  }

  desestimarContrato() {
    const modal = this.dialog.open(ModalAnularContratoComponent, {
      width: '115px',
      data: {
        titulo: 'Desestimar el Contrato',
        contratoId: this.g.idContrato.value,
        tipoTrabajo: '1',
        baseId: this.contratoList.baseId,
        postulanteId: this.contratoList.postulanteId
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.router.navigateByUrl('pages/generacioncontrato');
        this.toast.showToast(
          'El accesitario se creo de manera exitosa',
          'success',
          ''
        );
      }
    });
  }

  getBandeja() {
    this.seguimientoService
      .getContratosOrConvenios(
        this.filtros.tipo,
        this.filtros.regimen,
        0,
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
        this.router.navigateByUrl('pages/generacioncontrato');
        this.toast.showToast(
          'El contrato fue suscrito con éxito',
          'success',
          ''
        );
      }
    });
  }



}
