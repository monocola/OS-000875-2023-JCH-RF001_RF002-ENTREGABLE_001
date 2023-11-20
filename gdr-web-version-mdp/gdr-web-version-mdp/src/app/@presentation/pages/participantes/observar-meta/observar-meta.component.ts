import { Component, Inject, Input, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalMsjEnvioDeCorreoComponent } from '../modal-msj-envio-de-correo/modal-msj-envio-de-correo.component';



@Component({
  selector: 'serv-talento-observar-meta',
  templateUrl: './observar-meta.component.html',
  styleUrls: ['./observar-meta.component.scss']
})
export class ObservarMetaComponent implements OnInit {
  registerForm: FormGroup;

  @Input() title = 'Observar meta';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';

  constructor(
    private fb: FormBuilder,
    private matDialogRef: MatDialogRef<ObservarMetaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,
    public dialog: MatDialog,


  ) { }

  ngOnInit(): void {
    this.initializeForm();

  }

  get f() { return this.registerForm.controls; }


  initializeForm() {
    this.registerForm = this.fb.group({
      descripcionObservacion: ['', [Validators.required]],

    });
  }


  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }


  /*Dedo abajo */
  observarMetas() {
  let obs = this.registerForm.value.descripcionObservacion;
  this.data.body.payload.descripcionObservacion = obs;
    let body = this.data.body;
    console.info(body);
    if (this.registerForm.valid) {
         this.servidoresRepository.observarMeta(body).subscribe(
          (res) => {
           console.log('%cvalidar-meta.component.ts line:46 res', 'color: red;', res);
           if (res.status) {
            this.dialog.open(ModalMsjEnvioDeCorreoComponent, {
              data: {  }
            });
             this.onNoClick(true);
           } /* else {
             this.toastService.showToast(
               'error',
               'danger'
             );
           } */
         },
         (err) => this.toastService.showToast(err.message, 'danger')
       ); 
      }
    }
}


export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}


