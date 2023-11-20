
import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-postulante',
  templateUrl: './modal-postulante.component.html',
  styleUrls: ['./modal-postulante.component.scss']
})
export class ModalPostulanteComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  EstadoConv = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalPostulanteComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('33%', '74%');
    this.title = 'Editar Datos de Entidad';
    this.text = this.data.text;
    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      EstadoConv: null,
    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      this.onNoClick(this.f.observadorText.value);
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
