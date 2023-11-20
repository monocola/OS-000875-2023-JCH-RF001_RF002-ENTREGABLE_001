

import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from 'src/app/@data/services/const';



@Component({
  selector: 'serv-talento-modal-convenio',
  templateUrl: './modal-convenio.component.html',
  styleUrls: ['./modal-convenio.component.scss']
})
export class ModalConvenioComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  EstadoConv = [];
  direccionLabores: any = '';
  periodoConvenio: any = '';
  const = Const;


  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalConvenioComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService

  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('35%', '90%');
    this.title = 'Editar Datos de Convenio';
    this.text = this.data.text;
    this.direccionLabores = this.data.direccionLabores;
    this.periodoConvenio = this.data.periodoConvenio;
    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      direccionLabores: [this.direccionLabores, Validators.required],
      horaIniPractica: ['', Validators.required],
      horaFinPractica: ['', Validators.required],
      fechaIniPractica: ['', Validators.required],
      fechaFinPractica: ['', Validators.required],
      periodoConvenio: [this.periodoConvenio, Validators.required],
    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      const resolucion = {
        direccionLabores: this.f.direccionLabores.value,
        fechaIniPractica: this.f.fechaIniPractica.value,
        fechaFinPractica: this.f.fechaFinPractica.value,
        horaIniPractica: this.f.horaIniPractica.value,
        horaFinPractica: this.f.horaFinPractica.value,
        periodoConvenio: this.f.periodoConvenio.value,

      };
      this.onNoClick(resolucion);
    }
  }

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }
}
