import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-calificar-seccion',
  templateUrl: './modal-calificar-seccion.component.html'
})
export class ModalCalificarSeccionComponent implements OnInit {

  form: FormGroup;
  title: string = '';
  postulantes: any;

  nombreApellido: string;

  constructor(
    private ref: MatDialogRef<ModalCalificarSeccionComponent>,
    private fb: FormBuilder,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {
    this.form = this.fb.group({
      comentario: new FormControl(null, Validators.required),
    });
  }

  ngOnInit(): void {
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  get f() {
    return this.form.controls;
  }

  guardar() {

  }

}
