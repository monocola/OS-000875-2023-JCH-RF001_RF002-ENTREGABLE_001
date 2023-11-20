import { Component, Inject, Input, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ConfiguracionRepository } from 'src/app/@domain/repository/configuracion.repository';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-notificar-gdr',
  templateUrl: './notificar-gdr.component.html',
  styleUrls: ['./notificar-gdr.component.scss']
})
export class NotificarGdrComponent implements OnInit {
  @Input() rutaImagen: string = './assets/images/icons/mail.png';
  [x: string]: any;

  searchMode = false;
  userGdr = null;
  userGestor = null;
  nombre1 = null;
  apellidoP = null;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<NotificarGdrComponent>,
    private MaestraParametroRepository: MaestraParametroRepository,
    private toastService: ToastService,
    private authenticationRepository: AuthenticationRepository,
    private configuracionRepository: ConfiguracionRepository,
    @Inject(MAT_DIALOG_DATA) public data: NotificarGdrComponent,
  ) {
    this.userGdr = data;
  }

  frm: FormGroup = null;
  notificacion: MaestraParametro[] = [];

  ngOnInit(): void {
    this.initForm();
    this.loadCombox();
  }

  get f() {
    return this.frm.controls;
  }

  initForm() {
    this.frm = this.fb.group({
    });
  }

  dismiss(success: boolean) {
    this.matDialog.close(success);
  }

  loadCombox() {
    const getNotificacion = this.MaestraParametroRepository.getMaestraParametro('NOTIFICACION_GDR');
    const getUsuario = this.authenticationRepository.getUsuarioId(this.userGdr.gestor.usuarioId);
    forkJoin( [getNotificacion, getUsuario]).subscribe(
      (results) => {
        this.notificacion = results[0];
        this.userGestor = results[1].usuario;
        console.info(this.notificacion);
        console.info(this.userGestor);
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  enviarCorreo() {
    let correo = this.notificacion;
    let link = this.notificacion;
    this.nombre1 = this.userGdr.gestor.nombres.split(' ')[0];
    this.apellidoP = this.userGdr.gestor.apeMaterno;
    this.configuracionRepository.getCorreo(this.userGdr.gestor.personaId)
      .subscribe(response => {
        if ( response ) {
          let correoUsuario = response.find(item => item.tipoCorreo === "PRINC").correo;
          let CORREO = correo.find(item => item.codigoNumero === 1).valorTexto;
          let LINK_GDR = link.find(item => item.codigoNumero === 2).valorTexto;
          let body = {
            trace: {
              traceId: "string"
            },
            payload: {
              data: {
                templateCode: "EXI_GESTOR_GDR",
                to: correoUsuario,
                // subject: "Credenciales",
                bodyValues: {
                  NOMBRE_USUARIO: this.nombre1 + ' ' + this.apellidoP,
                  LINK_GDR: LINK_GDR,
                  CORREO_SERVIR: CORREO,
                  USUARIO: this.userGestor
                }
              },
              includeAttachments: false
            }
          };
          console.info(body);
          this.matDialog.close(body);
        }
      });
  }

}
