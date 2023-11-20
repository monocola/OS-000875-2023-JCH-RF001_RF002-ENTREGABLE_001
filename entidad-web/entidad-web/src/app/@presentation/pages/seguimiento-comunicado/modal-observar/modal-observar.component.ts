import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-observar',
  templateUrl: './modal-observar.component.html',
  styleUrls: ['./modal-observar.component.scss'],
})
export class ModalObservarComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalObservarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('25%', '55%');
    this.title = 'Registrar observaciones';
    this.text = this.data.text;
    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      observadorText: [this.text, Validators.required],
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
