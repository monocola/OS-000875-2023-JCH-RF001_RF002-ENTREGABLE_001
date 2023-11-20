import { Component, HostListener, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { DomSanitizer } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { SeguimientoComunicadoService } from '../seguimiento-comunicado.service';
import { ModalObservarComponent } from '../modal-observar/modal-observar.component';
import { ModalPublicarComponent } from '../modal-publicar/modal-publicar.component';
import { ModalAprobarComponent } from '../modal-aprobar/modal-aprobar.component';
import { base64toFile } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-registro-comunicado',
  templateUrl: './registro-comunicado.component.html',
  styleUrls: ['./registro-comunicado.component.scss'],
})
export class RegistroComunicadoComponent implements OnInit {
  Form: FormGroup;
  message: boolean = false;
  fileName: string = 'Subir documento';
  filepdf: string = '';
  fileMultipart: any;
  pathpdf: string = '';
  urlpdf: string = null;
  aprobacion: boolean = false;
  perfiles = [];
  comunicados = [];
  estados = [];
  comunicado: any;
  textObservacion: string = null;
  title: string = 'Crear comunicado';

  uploadfile: boolean = true;
  editable: boolean = false;
  publicar: boolean = false;

  scrHeight: any;
  scrWidth: any;
  tamStyle: number = 365;

  const = Const;
  rol: number = 0;
  estadoConvocatoriaId: any = null;
  constructor(
    private fb: FormBuilder,
    public router: Router,
    private maestraService: MaestraRepository,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    public helperService: SeguimientoComunicadoService,
    private dialog: MatDialog,
    private seguimientoService: SeguimientoRepository,
    private sanitizer: DomSanitizer,
    private toast: ToastService,
    private authenticationRepository: AuthenticationRepository
  ) {
    this.getScreenSize();
  }

  async ngOnInit() {
    this.rol = this.authenticationRepository.getCurrentUserValue.rolId;
    if (!this.helperService.form) this.helperService.initializeForm();
    this.initializeForm();
    this.validarSesion();
  }

  @HostListener('window:resize', ['$event'])
  getScreenSize(event?) {
    this.scrHeight = window.innerHeight;
    this.scrWidth = window.innerWidth;
    let valor = 290;

    if (this.scrHeight > 880) {
      valor = 390;
    }

    if (this.scrHeight <= 880 && this.scrHeight > 820) {
      valor = 350;
    }

    if (this.scrHeight <= 820 && this.scrHeight > 730) {
      valor = 290;
    }
    if (this.scrHeight <= 730 && this.scrHeight > 660) {
      valor = 350;
    }
    if (this.scrHeight <= 660) {
      valor = 290;
    }
    this.tamStyle = this.scrHeight - valor;
  }

  get f() {
    return this.Form.controls;
  }

  get g() {
    return this.helperService.form.controls;
  }

  get h() {
    return this.helperService.formConvocatoria.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      perfil: '',
      comunicado: ['', Validators.required],
    });
  }

  validarSesion() {
    if (this.h.convocatoriaId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
    } else {
      this.getCombox();
    }
  }

  getCombox() {
    if (this.g.comunicadoId.value !== 0) {
      this.title = 'Editar comunicado';
    }
    this.editable = this.g.editable.value;
    if (this.editable) {
      this.f.perfil.enable();
      this.f.comunicado.enable();
    } else {
      this.f.perfil.disable();
      this.f.comunicado.disable();
    }
    this.getPerfiles();
  }

  getPerfiles() {
    this.evaluacionConocimientosService
      .comboPerfiles(this.g.baseId.value)
      .subscribe((res) => {
        this.perfiles = res;
        this.getTipoComunicados();
      });
  }

  getTipoComunicados() {
    this.maestraService.getMaestraDetalleByCod('TIP_COMU').subscribe((res) => {
      this.comunicados = res;
      this.getEstados();
    });
  }

  getEstados() {
    this.maestraService
      .getMaestraDetalleByCod('EST_COMUNI')
      .subscribe((res) => {
        this.estados = res;
        this.cargarComunicado();
      });
  }

  onFileSelected(event: any) {
    const file: File = event.target.files[0];
    const reqFileName = file.name.split('.')[0];
    const pattern = new RegExp(/^[A-Za-z0-9]+$/g);

    if (!pattern.test(reqFileName)) {
      this.toast.showToast(
        ' El nombre del archivo solo debe contener números y letras.',
        'warning',
        'Atención'
      );
    } else {
      this.fileName = file.name;

      getBase64(file).then((data: string) => {
        this.fileMultipart = event.target.files[0];
        this.filepdf = data.split(',')[1];
        this.pathpdf = 'data:application/pdf;base64,' + this.filepdf;
        this.uploadfile = this.pathpdf === '' ? false : true;
        this.urlpdf = null;
        this.editCambios();
      });
    }
  }

  PDFURL() {
    return this.sanitizer.bypassSecurityTrustResourceUrl(
      this.urlpdf == null ? this.pathpdf : this.urlpdf
    );
  }

  cargarComunicado() {
    if (this.g.comunicadoId.value !== 0) {
      this.seguimientoService
        .getComunicado(this.g.comunicadoId.value)
        .subscribe((res) => {
          this.f.perfil.setValue(res.perfilId);
          this.f.comunicado.setValue(res.tipoComunicadoId);
          this.urlpdf = Const.API_FILE_SERVER + res.url;
          this.pathpdf = 'lleno';
          this.fileName = this.urlpdf.split('/').pop();
          this.comunicado = res;
          this.aprobacion = true;
          this.textObservacion = res.observacion;
          this.maestraService.downloadBase64(res.url).subscribe((resp) => {
            this.filepdf = resp + '';
            this.fileMultipart = base64toFile(
              resp + '',
              this.fileName,
              'application/pdf'
            );
          });
        });
        this.publicar = true
    }
  }

  saveOrUpdate() {
    if (this.g.comunicadoId.value === 0) {
      this.guardar();
    } else {
      this.updateComunicado();
    }
  }
 
  editCambios() {
    this.aprobacion = false;
  }

  guardar() {
    this.Form.markAllAsTouched();
    if (this.Form.valid && this.pathpdf !== '') {
      this.seguimientoService
        .saveComunicado(
          this.h.convocatoriaId.value,
          this.f.perfil.value,
          this.g.etapaId.value,
          this.f.comunicado.value,
          this.fileMultipart,
          this.estados.find((item) => item.codProg === Const.EST_COMUNI_PROCESO)
            .maeDetalleId
        )
        .subscribe((res) => {
          this.toast.showToast(
            'Se registró comunicado.',
            'success',
            'Atención'
          );
          this.comunicado = res;
          this.aprobacion = true;
          this.urlpdf = null;
          if (this.authenticationRepository.isSuperAdminEntidad()) {
            this.aprobacion = false;
            this.publicar = true;
            this.openModalPublicar();
          }
        });
    } else {
      this.uploadfile = this.pathpdf === '' ? false : true;
    }
  } 
  openModalObservar() {
    const modal = this.dialog.open(ModalObservarComponent, {
      width: '12px',
      data: {
        titulo: 'Registro de comunicado',
        text: this.textObservacion,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.textObservacion = res;
        this.message = true;
      }
    });
  }

  openDeleteObservar() {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar a Obervación ',
        bodyText: `Esta seguro que desea eliminar la Observación`,
        rutaImagen: 'assets/images/icons/eliminar.png',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.message = false;
        this.textObservacion = '';
      }
    });
  }

  openSendObservar() {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Enviar a Obervación ',
        bodyText: `Esta seguro que desea enviar la Observación`,
        rutaImagen: 'assets/images/icons/send.png',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.enviarEstadoObservacion();
      }
    });
  }

  enviarEstadoObservacion() {
    const nombreCoordinador = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;

    const idCoordinador = this.authenticationRepository.getCurrentUserValue
      .personaId;

    const rol = this.authenticationRepository.getCurrentUserValue.rolId;

    this.seguimientoService
      .updateEstadoComunicado(
        this.estados.find((item) => item.codProg === Const.EST_COMUNI_OBSERVADO)
          .maeDetalleId,
        this.textObservacion !== null ? this.textObservacion : null,
        this.comunicado.gestorId !== null
          ? this.comunicado.gestorId
          : Const.R_GESTOR_ORH === rol
          ? idCoordinador
          : null,
        Const.R_COORDINADOR === rol ? idCoordinador : null,
        this.comunicado.nombreGestor,
        Const.R_COORDINADOR === rol ? nombreCoordinador : null,
        this.comunicado.comunicadoId,
        this.estadoConvocatoriaId
      )
      .subscribe((res) => {
        this.toast.showToast(
          'Se actualizó el comunicado.',
          'success',
          'Atención'
        );
        this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
      });
  }

  openModalPublicar() {
    const modal = this.dialog.open(ModalPublicarComponent, {
      width: '12px',
      data: {
        titulo: this.f.comunicado.value,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.estadoConvocatoriaId = res;
        this.sendPublicar();
      }
    });
  }

  sendPublicar() {
    const nombreCoordinador = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;

    const idCoordinador = this.authenticationRepository.getCurrentUserValue
      .personaId;

    const rol = this.authenticationRepository.getCurrentUserValue.rolId;

    this.seguimientoService
      .updateEstadoComunicado(
        this.estados.find((item) => item.codProg === Const.EST_COMUNI_PUBLICADO)
          .maeDetalleId,
        this.textObservacion !== null ? this.textObservacion : null,
        this.comunicado.gestorId !== null
          ? this.comunicado.gestorId
          : Const.R_GESTOR_ORH === rol
          ? idCoordinador
          : null,
        Const.R_COORDINADOR === rol ? idCoordinador : null,
        this.comunicado.nombreGestor,
        Const.R_COORDINADOR === rol ? nombreCoordinador : '',
        this.comunicado.comunicadoId,
        this.estadoConvocatoriaId
      )
      .subscribe((res) => {
        this.toast.showToast(
          'Se actualizó el comunicado.',
          'success',
          'Atención'
        );
        this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
      });
  }

  openModalAprobar() {
    const modal = this.dialog.open(ModalAprobarComponent, {
      width: '12px',
      data: {
        titulo: this.f.comunicado.value,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.sendAprobacion();
        // this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
      }
    });
  }

  sendAprobacion() {
    const nombreCoordinador = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;

    const idCoordinador = this.authenticationRepository.getCurrentUserValue
      .personaId;

    const rol = this.authenticationRepository.getCurrentUserValue.rolId;

    this.seguimientoService
      .updateEstadoComunicado(
        this.estados.find((item) => item.codProg === Const.EST_COMUNI_APROBADO)
          .maeDetalleId,
        this.textObservacion !== null ? this.textObservacion : null,
        this.comunicado.gestorId !== null
          ? this.comunicado.gestorId
          : Const.R_GESTOR_ORH === rol
          ? idCoordinador
          : null,
        Const.R_COORDINADOR === rol ? idCoordinador : null,
        this.comunicado.nombreGestor,
        Const.R_COORDINADOR === rol ? nombreCoordinador : '',
        this.comunicado.comunicadoId,
        this.h.estadoConvocatoriaId.value
      )
      .subscribe((res) => {
        this.toast.showToast(
          'Se actualizó el comunicado.',
          'success',
          'Atención'
        );

        this.g.codProgEstado.setValue(Const.EST_COMUNI_APROBADO);
        // this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
        if (this.authenticationRepository.isSuperAdminEntidad()) {
            this.openModalPublicar();
        }
      });
  }

  editObservacion() {
    const modal = this.dialog.open(ModalObservarComponent, {
      width: '12px',
      data: {
        titulo: 'Registro de observaciones',
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.message = true;
      }
    });
  }

  enviar() {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Enviar a aprobación',
        bodyText: `Esta seguro que desea enviar a aprobación el comunicado`,
        rutaImagen: 'assets/images/icons/send.png',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.enviarAprobacion();
      }
    });
  }

  updateComunicado() {
    const nombreCoordinador = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;

    const idCoordinador = this.authenticationRepository.getCurrentUserValue
      .personaId;

    const rol = this.authenticationRepository.getCurrentUserValue.rolId;

    this.seguimientoService
      .updateComunicado(
        this.comunicado.comunicadoId,
        this.comunicado.convocatoriaId,
        this.f.perfil.value,
        this.comunicado.etapaId,
        this.estados.find((item) => item.codProg === Const.EST_COMUNI_PROCESO)
          .maeDetalleId,
        this.f.comunicado.value,
        this.comunicado.gestorId,
        Const.R_GESTOR_ORH === rol ? idCoordinador : null,
        this.comunicado.nombreGestor,
        Const.R_GESTOR_ORH === rol ? nombreCoordinador : '',
        this.fileMultipart
      )
      .subscribe((res) => {
        this.toast.showToast(
          'Se modificó el comunicado.',
          'success',
          'Atención'
        );
        this.aprobacion = true;
        if (this.authenticationRepository.isSuperAdminEntidad()) {
          this.aprobacion = false;
          this.publicar = true;
          this.openModalPublicar();
          }
      });
  }

  enviarAprobacion() {
    const nombreCoordinador = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;

    const idCoordinador = this.authenticationRepository.getCurrentUserValue
      .personaId;

    const rol = this.authenticationRepository.getCurrentUserValue.rolId;

    this.seguimientoService
      .updateComunicado(
        this.comunicado.comunicadoId,
        this.comunicado.convocatoriaId,
        this.f.perfil.value,
        this.comunicado.etapaId === null
          ? this.g.etapaId.value
          : this.comunicado.etapaId,
        this.estados.find(
          (item) => item.codProg === Const.EST_COMUNI_POR_REVISAR
        ).maeDetalleId,
        this.f.comunicado.value,
        this.comunicado.gestorId !== null
          ? this.comunicado.gestorId
          : Const.R_GESTOR_ORH === rol
          ? idCoordinador
          : null,
        Const.R_COORDINADOR === rol ? idCoordinador : null,
        this.comunicado.nombreGestor,
        Const.R_COORDINADOR === rol ? nombreCoordinador : '',
        this.fileMultipart
      )
      .subscribe((res) => {
        this.toast.showToast(
          'Se envió para aprobación el comunicado.',
          'success',
          'Atención'
        );
        if (this.authenticationRepository.isSuperAdminEntidad()) {
          this.openModalAprobar();
        } else {
        this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
        }
      });
  }
 
  cancelar() {
    this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
  }

  estadoConvocatoria(): number {
    let convocatoriaId = this.h.estadoConvocatoriaId.value;
    if (this.f.comunicado.value === Const.TIP_COMUNI_DESIERTA) {
      convocatoriaId = +Const.EST_CONV_DES;
    }
    if (this.f.comunicado.value === Const.TIP_COMUNI_CANCELADA) {
      convocatoriaId = +Const.EST_CONV_CANCEL;
    }
    return convocatoriaId;
  }
}
