import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { FormBuilder, FormGroup } from '@angular/forms';
import { SolicitudExternaRepository } from '../../../../@domain/repository/solicitud.externa.repository';
import { SolicitudExterna } from '../../../../@data/model/SolicitudExterna';
import { forkJoin } from 'rxjs';
import { ToastService } from '../../../@common-components/toast';
import { ResponseRequest } from '../../../../@data/model/reponse-request';
import { MatDialog } from '@angular/material/dialog';
import { ModalEliminarQuestionComponent } from './modal-detalle-eliminar-question/modal-eliminar-question.component';
import { ModalObservarSolicitudComponent } from './modal-observar-solicitud/modal-observar-solicitud.component';
import { Router } from '@angular/router';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';

@Component({
  selector: 'gme-web-detalle-solicitud-ext',
  templateUrl: './detalle-solicitud-ext.component.html',
  styleUrls: ['./detalle-solicitud-ext.component.scss'],
})
export class DetalleSolicitudExtComponent implements OnInit {
  solicitudExtId: number;
  solicitud: SolicitudExterna = null;
  frm: FormGroup = null;
  estadoColor: string;
  obs: any;
  habilitarOpcion: Boolean = false;
  cboNivel: MaestraParametro[] = [];
  cboSector: MaestraParametro[] = [];
  cboTipoEntPub: MaestraParametro[] = [];

  cboNivelObject: MaestraParametro;
  cboSectorObject: MaestraParametro;
  cboTipoEntPubObject: MaestraParametro;

  constructor(
    private activatedRoute: ActivatedRoute,
    private fb: FormBuilder,
    private solicitudExRepository: SolicitudExternaRepository,
    private toastService: ToastService,
    public dialog: MatDialog,
    private router: Router,
    private parameterRepository: ParameterRepository,
    private maestraParametroRepository: MaestraParametroRepository
  ) {
    this.frm = this.fb.group({
      ruc: [''],
      razonSocial: [''],
      abreviatura: [''],
      nivelGobierno: [''],
      sector: [''],
      tipoEntidad: [''],
      nombres: [''],
      apePaterno: [''],
      apeMaterno: [''],
      documento: [''],
      correo: [''],
      telefono: [''],
      anexo: [''],
      celular: [''],
      estado: [''],
      uuid: [''],
    });
  }
  get f() {
    return this.frm.controls;
  }

  ngOnInit(): void {
    this.activatedRoute.params.subscribe((params) => {
      if (params.solicitudExtId) {
        this.solicitudExtId = params.solicitudExtId;
      }
      this.loadCombox();
    });
  }

  loadCombox() {
    const getSolicitud = this.solicitudExRepository.getSolicitudExtId(
      this.solicitudExtId
    );
    forkJoin([getSolicitud]).subscribe(
      (results) => {
        this.solicitud = results[0];
        console.info(this.solicitud);
        this.initForm();
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  llenarCombox2() {
    const getNivel = this.parameterRepository.getNiveles();
    const getSector = this.parameterRepository.getSectores();
    const getTipoFilter = this.parameterRepository.getTipoEntidad();
    forkJoin([getNivel, getSector, getTipoFilter]).subscribe(
      (results) => {
        this.cboNivel = results[0];
        this.cboSector = results[1];
        this.cboTipoEntPub = results[2];
        this.llenarInformacion();
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm.controls.ruc.setValue(this.solicitud.rucEntidad);
    this.frm.controls.razonSocial.setValue(this.solicitud.razonSocial);
    this.frm.controls.abreviatura.setValue(this.solicitud.abreviatura);

    this.frm.controls.documento.setValue(this.solicitud.numeroDocumento);
    this.frm.controls.apePaterno.setValue(this.solicitud.apellidoPaterno);
    this.frm.controls.apeMaterno.setValue(this.solicitud.apellidoMaterno);
    this.frm.controls.nombres.setValue(this.solicitud.nombres);
    this.frm.controls.correo.setValue(this.solicitud.correoElectronico);
    this.frm.controls.telefono.setValue(this.solicitud.telefonoFijo);
    this.frm.controls.anexo.setValue(this.solicitud.anexo);
    this.frm.controls.celular.setValue(this.solicitud.celular);
    this.frm.controls.uuid.setValue(this.solicitud.uuidDocumento);
    this.frm.controls.estado.setValue(this.solicitud.estado);
    this.estadoColor = this.solicitud.estado.toLowerCase();

    this.frm.controls['ruc'].disable();
    this.frm.controls['razonSocial'].disable();
    this.frm.controls['abreviatura'].disable();
    this.frm.controls['nivelGobierno'].disable();
    this.frm.controls['sector'].disable();
    this.frm.controls['tipoEntidad'].disable();
    this.frm.controls['documento'].disable();
    this.frm.controls['apePaterno'].disable();
    this.frm.controls['apeMaterno'].disable();
    this.frm.controls['nombres'].disable();
    this.frm.controls['correo'].disable();
    this.frm.controls['telefono'].disable();
    this.frm.controls['celular'].disable();
    this.frm.controls['anexo'].disable();
    this.frm.controls['uuid'].disable();
    this.frm.controls['estado'].disable();

    // this.frm.controls.estado.setValue(this.solicitud.colorEstado);
    this.habilitarOpcion = this.solicitud.estadoId === 1 ? false : true;

    this.llenarCombox2();
  }

  llenarInformacion() {
    this.cboNivelObject = this.cboNivel.find(
      (cboNivel) => cboNivel.parametroId === this.solicitud.nivelGobiernoId
    );
    this.cboSectorObject = this.cboSector.find(
      (cboSector) => cboSector.parametroId === this.solicitud.sectorId
    );
    this.cboTipoEntPubObject = this.cboTipoEntPub.find(
      (cboTipoEntPub) =>
        cboTipoEntPub.parametroId === this.solicitud.tipoEntidadId
    );

    if (this.cboNivelObject !== undefined) {
      this.frm.controls.nivelGobierno.setValue(this.cboNivelObject.descripcion);
    }

    if (this.cboSectorObject !== undefined) {
      this.frm.controls.sector.setValue(this.cboSectorObject.descripcion);
    }

    if (this.cboTipoEntPubObject !== undefined) {
      this.frm.controls.tipoEntidad.setValue(
        this.cboTipoEntPubObject.descripcion
      );
    }
  }

  validaEntidad(tipo: number) {
    let mensaje = '';
    if (tipo === 1) {
      mensaje =
        'Se validará la siguiente solicitud <br> ¿Está realmente seguro de realizar la siguiente acción ?';
    } else if (tipo === 2) {
      mensaje =
        'Se observará la siguiente solicitud <br> ¿Está realmente seguro de realizar la siguiente acción ?';
    } else if (tipo === 3) {
      mensaje =
        'Se rechazará la siguiente solicitud <br> ¿Está realmente seguro de realizar la siguiente acción ?';
    }

    this.abreModal(mensaje, tipo);
  }

  abreModal(mensajeTotal: string, tipo: number) {
    const add = this.dialog.open(ModalEliminarQuestionComponent, {
      data: {
        title: ' ',
        bodyText: mensajeTotal,
        rutaImagen: './assets/images/question.png',
        textCancel: 'NO',
        textOk: 'SI',
      },
      disableClose: true,
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        console.info(any, this.solicitudExtId);
        if (tipo === 1) {
          this.solicitudExRepository
            .validaSolicitudExt(this.solicitudExtId)
            .subscribe((item: ResponseRequest) => {
              if (item.status.success) {
                this.toastService.showToast(
                  'Se actualizó el estado de la solicitud en el sistema',
                  'success',
                  'Cambio de estado exitoso'
                );
                this.redirect();
              } else {
                this.toastService.showToast(
                  item.status.error.messages[0],
                  'danger',
                  'Error'
                );
              }
            });
        } else if (tipo === 2) {
          const addObs = this.dialog.open(ModalObservarSolicitudComponent, {
            data: {
              title: ' ',
              bodyText: mensajeTotal,
              rutaImagen: './assets/images/question.png',
              textCancel: 'Cancelar',
              textOk: 'Observar',
            },
            disableClose: true,
          });
          addObs.afterClosed().subscribe((any) => {
            if (any.valida) {
              if (any.mensaje.length > 0) {
                this.solicitudExRepository
                  .observaSolicitudExt(this.solicitudExtId, any.mensaje)
                  .subscribe((item: ResponseRequest) => {
                    if (item.status.success) {
                      this.toastService.showToast(
                        'Se actualizó el estado de la solicitud en el sistema',
                        'success',
                        'Cambio de estado exitoso'
                      );
                      this.redirect();
                    } else {
                      this.toastService.showToast(
                        'Ocurrio un error al actualizar la solicitud',
                        'danger',
                        'Error'
                      );
                    }
                  });
              } else {
                this.toastService.showToast(
                  'Debe de ingresar el motivo de observación',
                  'warning',
                  'Observación no ingresada'
                );
              }
            }
          });
        } else if (tipo === 3) {
          let mensajeNotificacion =
            'Se notificará el rechazo de la solicitud por correo electrónico <br> ¿Está realmente seguro de realizar la siguiente acción? ';

          const addObs = this.dialog.open(ModalEliminarQuestionComponent, {
            data: {
              title: ' ',
              bodyText: mensajeNotificacion,
              rutaImagen: './assets/images/question.png',
              textCancel: 'NO',
              textOk: 'SI',
            },
            disableClose: true,
          });
          addObs.afterClosed().subscribe((any) => {
            if (any) {
              this.solicitudExRepository
                .rechazaSolicitudExt(this.solicitudExtId)
                .subscribe((item: ResponseRequest) => {
                  if (item.status.success) {
                    this.toastService.showToast(
                      'Se actualizó el estado de la solicitud en el sistema',
                      'success',
                      'Cambio de estado exitoso'
                    );
                    this.redirect();
                  } else {
                    this.toastService.showToast(
                      'Ocurrio un error al actualizar la solicitud',
                      'danger',
                      'Error'
                    );
                  }
                });
            }
          });
        }
      }
    });
  }

  redirect() {
    this.router.navigate([`pages/solicitudesExt`]);
  }
}
