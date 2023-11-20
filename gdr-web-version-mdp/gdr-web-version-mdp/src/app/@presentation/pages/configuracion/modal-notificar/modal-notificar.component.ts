import { Component, OnInit, Input, Inject } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatDialogRef } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { ToastService } from '../../../@common-components/toast';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { ConfiguracionRepository } from '../../../../@domain/repository/configuracion.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';

@Component({
  selector: 'serv-talento-modal-notificar',
  templateUrl: './modal-notificar.component.html',
  styleUrls: ['./modal-notificar.component.scss']
})
export class ModalNotificarComponent implements OnInit {
  @Input() rutaImagen: string = './assets/images/icons/mail.png';
  [x: string]: any;

  searchMode = false;
  userRector = null;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalNotificarComponent>,
    private MaestraParametroRepository: MaestraParametroRepository,
    private toastService: ToastService,
    protected ref: MatDialogRef<ModalNotificarComponent>,
    private configuracionRepository: ConfiguracionRepository,
    @Inject(MAT_DIALOG_DATA) public data: ModalNotificarComponent,
  ) {
    this.userRector = data;
  }

  frm: FormGroup = null;
  cicloDefaultDesc;
  notificacion: MaestraParametro[] = [];

  get f() {
    return this.frm.controls;
  }

  initForm() {
    this.frm = this.fb.group({
    });
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  ngOnInit(): void {
    this.initForm();
    this.loadCombox();
  }

  loadCombox() {
    const getNotificacion = this.MaestraParametroRepository.getMaestraParametro('NOTIFICACION_GDR');
    forkJoin( [getNotificacion]).subscribe(
      (results) => {
        this.notificacion = results[0];

      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  enviarCorreo() {
    let correo = this.notificacion;
    let link = this.notificacion;
    let rector = this.userRector.rector;
    this.configuracionRepository.getCorreo(rector.personaId)
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
                templateCode: "REGISTRO_RECTOR",
                to: correoUsuario,
                // subject: "Credenciales",
                bodyValues: {
                  NOMBRE_USUARIO: rector.nombres.split(' ')[0] + ' ' + rector.apellidoPaterno,
                  LINK_GDR: LINK_GDR,
                  CORREO_SERVIR: CORREO,
                }
              },
              includeAttachments: false
            }
          };
          this.matDialog.close(body);
        } else {
          this.matDialog.close(false);
        }
      });
  }
}
