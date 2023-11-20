
import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-entidad',
  templateUrl: './modal-entidad.component.html',
  styleUrls: ['./modal-entidad.component.scss']
})
export class ModalEntidadComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  resolResponOrh: string = '';
  nroNorma: string = '';
  EstadoConv = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalEntidadComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('33%', '74%');
    this.title = 'Editar Datos de Entidad';
    this.text = this.data.text;
    this.resolResponOrh = this.data.resolResponOrh;
    this.nroNorma = this.data.nroNorma;
    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  /*
          articulo: this.contratoList.articulo,
        nroNorma: this.contratoList.nroNorma
  */
  initializeForm() {
    this.Form = this.fb.group({
      resolResponOrh: [this.resolResponOrh, Validators.required],
      nroNorma: [this.nroNorma, Validators.required],
    });
  }

  GuardarComunicado() {
    const resolucion = {
      resolResponOrh: this.f.resolResponOrh.value,
      nroNorma: this.f.nroNorma.value,
    };

    this.onNoClick(resolucion);
  }

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }
}
