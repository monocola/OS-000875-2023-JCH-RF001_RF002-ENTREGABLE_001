import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-comunicado',
  templateUrl: './modal-comunicado.component.html',
  styleUrls: ['./modal-comunicado.component.scss'],
})
export class ModalComunicadoComponent implements OnInit {
  Form: FormGroup;
  title: string = '';

  perfiles = [
    { id: 1, descripcion: 'Medico General' },
    { id: 2, descripcion: 'Medico Internista' },
    { id: 3, descripcion: 'Medico Cardiologo' },
  ];

  comunicados = [
    { id: 1, descripcion: 'Examen de conocimiento 01' },
    { id: 2, descripcion: 'Examen de conocimiento 02' },
  ];
  fileName: string = '';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalComunicadoComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('50%', '60%');
    this.title = this.data.titulo;
    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      perfil: '',
      comunicado: ['', Validators.required],
    });
  }

  onFileSelected(event: any) {
    const file: File = event.target.files[0];
    this.fileName = file.name;
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      this.onNoClick(true);
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
