
import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from 'src/app/@data/services/const';


@Component({
  selector: 'serv-talento-modal-data-entidad',
  templateUrl: './modal-data-entidad.component.html',
  styleUrls: ['./modal-data-entidad.component.scss']
})
export class ModalDataEntidadComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  TipoDoc = [
    { value: 1, documento: 'DNI' },
    { value: 4, documento: 'C.E' },

  ];
  puestoResponsableOrh: any = '';
  responsableOrh: any = '';
  nroDocResponsable: any = '';

  const = Const;


  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalDataEntidadComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService

  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('42%', '80%');
    this.title = 'Editar datos de Entidad';
    this.text = this.data.text;
    this.puestoResponsableOrh = this.data.puestoResponsableOrh;
    this.responsableOrh = this.data.responsableOrh;
    this.nroDocResponsable = this.data.nroDocResponsable;

    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      puestoResponsableOrh: [this.puestoResponsableOrh, Validators.required],
      responsableOrh: [this.responsableOrh, Validators.required],
      TipoDoc: this.TipoDoc[0].value,
      nroDocResponsable: [this.nroDocResponsable, Validators.required],


    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      const resolucion = {
        puestoResponsableOrh: this.f.puestoResponsableOrh.value,
        responsableOrh: this.f.responsableOrh.value,
        TipoDoc: this.f.TipoDoc.value,
        nroDocResponsable: this.f.nroDocResponsable.value,

      };

      this.onNoClick(resolucion);
    }
  }

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }
}











