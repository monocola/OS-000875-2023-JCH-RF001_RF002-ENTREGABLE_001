import { Component, Inject, Input, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { ToastService } from '../../../@common-components/toast';
import { forkJoin } from 'rxjs';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'gme-web-notificar-solicitud-ext',
  templateUrl: './notificar-solicitud-ext.component.html',
})
export class NotificarSolicitudExtComponent implements OnInit {

  @Input() rutaImagen: string = './assets/images/icons/mail.png';
  [x: string]: any;

  frm: FormGroup = null;
  notificacion: MaestraParametro[] = [];
  solicitud = null;
  plantillaCorreo = null;
  subjectCorreo = null;
  usuarioId = JSON.parse(sessionStorage.getItem('userId'));
  userGestor = null;
  CORREO = null;
  LINK_GDR = null;
  correo1 = null;
  correo2 = null;
  identidad = null;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<NotificarSolicitudExtComponent>,
    private MaestraParametroRepository: MaestraParametroRepository,
    private authenticationRepository: AuthenticationRepository,
    private toastService: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: NotificarSolicitudExtComponent,
    ) {
    this.solicitud = data.solicitud;
  }

  ngOnInit(): void {
    this.initForm();
    this.loadCombox();
    this.getEncryptID();
  }

  initForm() {
    this.frm = this.fb.group({
    });
  }

  get f() {
    return this.frm.controls;
  }

  dismiss(success: boolean) {
    this.matDialog.close(success);
  }

  loadCombox() {
    const getNotificacion = this.MaestraParametroRepository.getMaestraParametro('NOTIFICACION_GDR');
    const getUsuario = this.authenticationRepository.getUsuarioId(this.usuarioId);
    forkJoin( [getNotificacion, getUsuario]).subscribe(
      (results) => {
        this.notificacion = results[0];
        this.userGestor = results[1].usuario;
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  enviarCorreo() {
    let correoParameter = this.notificacion;
    let linkParameter = this.notificacion;
    this.CORREO = correoParameter.find(item => item.codigoNumero === 1).valorTexto;
    this.LINK_GDR = linkParameter.find(item => item.codigoNumero === 2).valorTexto;
    this.correo1 = this.solicitud.correoElectronico;
    this.correo2 = this.solicitud.correoGestorGdr;

    if (this.solicitud.estadoId === 2) {
      this.plantillaCorreo = "OBS_SOL_ENT_EXT";
      this.subjectCorreo = 'Observación de Solicitud Externa';
      this.LINK_GDR = this.identidad
    } else if (this.solicitud.estadoId === 3) {
      this.plantillaCorreo = "CAN_SOL_ENT_EXT";
      this.subjectCorreo = 'Cancelación de Solicitud Externa';
    } else if (this.solicitud.estadoId === 4) {
      this.plantillaCorreo = 'APR_SOL_ENT_EXT';
      this.subjectCorreo = 'Aprobación de Solicitud Externa';
    }
    let body = [
      { body: this.correoElectronico(this.correo1, this.LINK_GDR) },
      { body: this.correoGestorGDR(this.correo2, this.LINK_GDR) }
    ];

    this.matDialog.close(body);
  }

  correoElectronico(correo: string, link: any) {
    return {
      trace: {
        traceId: "string"
      },
      payload: {
        data: {
          templateCode: this.plantillaCorreo,
          to: correo,
          subject: this.subjectCorreo,
          bodyValues: {
            NOMBRE_USUARIO: this.solicitud.nombres + ' ' + this.solicitud.apellidoPaterno + ' ' + this.solicitud.apellidoMaterno,
            LINK_SGM: this.LINK_GDR,
            CORREO_SERVIR: this.CORREO,
            USUARIO: this.userGestor,
            NOMBRE_ENTIDAD: this.solicitud.razonSocial,
            OBSERVACION: this.solicitud.solicitudObs
          }
        },
        includeAttachments: false
      }
    };
  }

  correoGestorGDR(correoGDR: string, link: any) {
    return {
      trace: {
        traceId: "string"
      },
      payload: {
        data: {
          templateCode: this.plantillaCorreo,
          to: correoGDR,
          subject: this.subjectCorreo,
          bodyValues: {
            NOMBRE_USUARIO: this.solicitud.nombres + ' ' + this.solicitud.apellidoPaterno,
            LINK_SGM: this.LINK_GDR,
            CORREO_SERVIR: this.CORREO,
            USUARIO: this.userGestor,
            NOMBRE_ENTIDAD: this.solicitud.razonSocial,
            OBSERVACION: this.solicitud.solicitudObs
          }
        },
        includeAttachments: false
      }
    };
  }

  getEncryptID() {
    const getSolicitud = this.authenticationRepository.getEncryptSolicitudExtId(
      this.solicitud.solicitudEntidadExtId
    );
    forkJoin([getSolicitud]).subscribe(
      (results) => {
        this.identidad = results[0].encrypt
       
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

}
