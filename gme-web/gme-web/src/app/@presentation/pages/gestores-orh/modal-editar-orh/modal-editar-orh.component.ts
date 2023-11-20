import { Component, Inject, NgModuleRef, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import {
  MAT_DIALOG_DATA,
  MatDialog,
  MatDialogRef,
} from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { ModalEliminarQuestionComponent } from '../modal-eliminar-question/modal-eliminar-question.component';
import { ToastService } from '../../../@common-components/toast';
import { GestoresOrhRepository } from '../../../../@domain/repository/gestores-orh.repository';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'gme-web-modal-editar-orh',
  templateUrl: './modal-editar-orh.component.html',
  styleUrls: ['./modal-editar-orh.component.scss'],
})
export class ModalEditarOrhComponent implements OnInit {
  frm: FormGroup;
  tipoDocumento: MaestraParametro[];
  fechaActiva: any;
  fechaNaci: any;
  toggleActivo: Boolean;
  alertaTexto: string = '';
  numeroDocumentoMaxlength: number = 8;
  numeroDocumentoType: string = 'integer';
  tipoDoc: boolean = false;
  validaOk: any;
  profile = JSON.parse(sessionStorage.getItem('entidad'));

  constructor(
    public ref: NgModuleRef<ModalEditarOrhComponent>,
    private matDialog: MatDialogRef<ModalEditarOrhComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalEditarOrhComponentModel,
    private fb: FormBuilder,
    public dialog: MatDialog,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,
    private gestoresOrhRepository: GestoresOrhRepository
  ) {
    if (!this.data.isNew) {
      this.fechaActiva = this.data.gestorORH.tipoDocumento === 1;
      if (this.data.gestorORH.fechaNacimiento != null) {
        this.fechaNaci = new Date(this.data.gestorORH.fechaNacimiento);
      }
      this.toggleActivo = this.data.gestorORH.estadoRegistro === '1';
      this.frm = this.fb.group({
        tipoDocumento: new FormControl(
          { value: data.gestorORH.tipoDocumento, disabled: true },
          Validators.required
        ),
        numeroDocumento: new FormControl(
          { value: data.gestorORH.numeroDocumento, disabled: true },
          Validators.required,
        ),
        apepaterno: new FormControl(
          { value: data.gestorORH.apellidoPaterno, disabled: true },
          Validators.required
        ),
        apematerno: new FormControl(
          { value: data.gestorORH.apellidoMaterno, disabled: true },
          Validators.required
        ),
        nombres: new FormControl(
          { value: data.gestorORH.nombres, disabled: true },
          Validators.required
        ),
        fechaNac: new FormControl(
          { value: this.fechaNaci, disabled: this.fechaActiva },
          Validators.required
        ),
        correo: new FormControl(this.data.gestorORH.correo, [
          Validators.required,
          Validators.email,
        ]),
        telefono: new FormControl(this.data.gestorORH.numeroTelefono),
        anexo: new FormControl(this.data.gestorORH.anexoTelefono),
        celular: new FormControl(this.data.gestorORH.numeroCelular),
        estado: new FormControl(this.toggleActivo, Validators.required),
      });
    } else {
      this.frm = this.fb.group({
        tipoDocumento: new FormControl('', Validators.required),
        numeroDocumento: new FormControl(null, [
          Validators.required,
          Validators.maxLength(15),
          Validators.minLength(8),
        ]),
        apepaterno: new FormControl('', Validators.required),
        apematerno: new FormControl('', Validators.required),
        nombres: new FormControl('', Validators.required),
        fechaNac: new FormControl('', Validators.required),
        correo: new FormControl('', [Validators.required, Validators.email]),
        telefono: new FormControl(null, [
          Validators.required,
          Validators.pattern(/[0-9]$/),
          Validators.minLength(7),
          Validators.maxLength(9),
        ]),
      });
    }
  }

  get f() {
    return this.frm.controls;
  }

  ngOnInit(): void {
    this.loadCombox();
  }

  loadCombox() {
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    forkJoin([getTipoDocumento]).subscribe((results) => {
      this.tipoDocumento = results[0];
    });
  }

  changeType(event){
    this.frm.patchValue({
      numeroDocumento: null,
    });
    if (event == 4) {
      this.numeroDocumentoMaxlength = 12;
      this.tipoDoc = true;
      this.frm.get('numeroDocumento').setValidators([Validators.required, Validators.minLength(9)]);
      this.frm.get('numeroDocumento').updateValueAndValidity();
    } else {
      this.numeroDocumentoMaxlength = 8;
      this.tipoDoc = false;
      this.frm.get('numeroDocumento').setValidators([Validators.required, Validators.minLength(8)]);
      this.frm.get('numeroDocumento').updateValueAndValidity();
    }
  }

  validaCampos(frm: any) {

    let EMAIL_REGEX = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;

    if (frm.telefono?.length < 1 || frm.celular?.length < 1) {
      this.validaOk = false;
      this.toastService.showToast(
        'Seleccione un teléfono o un Celular',
        'warning'
      );
    } else if (!frm.correo.match(EMAIL_REGEX)) {
      this.validaOk = false;
      this.toastService.showToast(
        'El correo no tiene formato xxx@abc.abc.',
        'warning'
      );
    } else {
      this.validaOk = true;
      if (this.data.isNew) {
        this.alertaTexto =
          'Se realizará el registro de un nuevo gestor ORH a su entidad <br> ¿Está realmente seguro de realizar la siguiente acción';
      } else {
        if (frm.estado) {
          this.alertaTexto =
            'Se activará el siguiente gestor registrado <br> ¿Está realmente seguro de realizar la siguiente acción ?';
        } else {
          this.alertaTexto =
            'Se dará de baja el siguiente gestor registrado <br> ¿Está realmente seguro de realizar la siguiente acción ?';
        }
      }
    }
  }

  save() {
    this.validaCampos(this.frm.value);
    if (this.validaOk) {
      this.dialog
        .open(ModalEliminarQuestionComponent, {
          data: {
            title: 'Alerta',
            bodyText: this.alertaTexto,
            rutaImagen: './assets/images/question.png',
            textCancel: 'NO',
            textOk: 'SI',
          },
          disableClose: true,
        })
        .afterClosed()
        .subscribe((any) => {
          if (any) {
            if (this.data.isNew) {
              this.setRegister();
            } else {
              let body = {
                gestorId: this.data.gestorORH.gestorId,
                personaId: this.data.gestorORH.personaId,
                usuarioId: this.data.gestorORH.usuarioId,
                entidadId: this.data.gestorORH.entidadId,
                rolId: this.data.gestorORH.rolId,
                numeroDocumento: this.data.gestorORH.numeroDocumento,
                tipoDocumento: this.data.gestorORH.tipoDocumento,
                correoId: this.data.gestorORH.correoId,
                correo: this.frm.value.correo,
                flagUPDT: this.frm.value.estado ? 1 : 0,
                telefonoId: this.data.gestorORH.telefonoId,
                telefono: this.frm.value.telefono,
                anexo: this.frm.value.anexo,
                tipoTelefono: this.data.gestorORH.tipoTelefono,
                celularId: this.data.gestorORH.celularId,
                celular: this.frm.value.celular,
                tipoCelular: this.data.gestorORH.tipoCelular,
                fechaNacimiento: this.frm.value.fechaNac,
              };

              this.updateGestorORH(body);
            }
          }
        });
    }
  }

  setRegister = () => {
    let fechaNac = null;
    if (this.frm.value.fechaNac) {
      fechaNac = Utils.formatFechaDate(this.frm.value.fechaNac, 'DD/MM/YYYY');
    }
    let body = {
      entidadId: this.profile.entidadId,
      tipoDocumento: this.frm.value.tipoDocumento,
      numeroDocumento: this.frm.value.numeroDocumento,
      nombres: this.frm.value.nombres,
      apellidoPaterno: this.frm.value.apepaterno,
      apellidoMaterno: this.frm.value.apematerno,
      fechaNacimiento: fechaNac,
      correo: this.frm.value.correo,
      telefono: this.frm.value.telefono,
      paisId: 12,
    };

    this.gestoresOrhRepository.setRegistrarGestor(body).subscribe(
      (response) => {
        if (response) {
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
          this.ngOnInit();
        } else {
          this.toastService.showToast(
            'Hubo un error al registrar, verifique haber llenado los campos',
            'danger'
          );
        }
        this.dismiss(true);
      },
      (err) => this.toastService.showToast(err.message, 'danger')
    );
  }

  updateGestorORH(body: any) {
    this.gestoresOrhRepository.saveGestorORH(body).subscribe(
      (item) => {
        if (item) {
          this.toastService.showToast(
            'Se guardaron los cambios de manera exitosa',
            'success',
            'Edición exitosa'
          );
        } else {
          this.toastService.showToast(
            'Ocurrio un error al guardar los cambios del usuario',
            'danger',
            'Error'
          );
          console.log('item');
        }
        this.dismiss(true);
      },
      (error) => {
        console.log(error);
      }
    );
  }

  dismiss(success: boolean) {
    this.matDialog.close(success);
  }
}
export interface ModalEditarOrhComponentModel {
  gestorORH: any;
  isNew: boolean;
}
