import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

export interface DialogData {
  descripcion: string;
  entidadId: number;
}

@Component({
  selector: 'serv-talento-entidad',
  templateUrl: './entidad.component.html',
  styleUrls: ['./entidad.component.scss']
})
export class EntidadComponent implements OnInit {

  frm: FormGroup;

  lstEntidad = null;


  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<EntidadComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData
  ) {
    this.frm = this.fb.group({
      entidadId: new FormControl(null, Validators.required),
      name: new FormControl(null, Validators.required),
    });
    this.lstEntidad = this.data;
  }

  ngOnInit(): void {
  }

  get f() {
    return this.frm.controls;
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  getPerfil() {
    let entidad = this.lstEntidad.find(item => item.entidadId === this.frm.value.entidadId);
    this.dismiss(entidad);
  }


}
