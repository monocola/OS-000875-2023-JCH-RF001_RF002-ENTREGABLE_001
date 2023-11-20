import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-aprobar',
  templateUrl: './modal-aprobar.component.html',
  styleUrls: ['./modal-aprobar.component.scss'],
})
export class ModalAprobarComponent implements OnInit {
  registerForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalAprobarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.initializeForm();
  }

  get f() {
    return this.registerForm.controls;
  }
  initializeForm() {
    this.registerForm = this.fb.group({
      descripcion: ['', Validators.required],
    });
  }
  onClickGuardar() {
    if (this.registerForm.valid) {
      this.onNoClick(true);
    }
  }
  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
