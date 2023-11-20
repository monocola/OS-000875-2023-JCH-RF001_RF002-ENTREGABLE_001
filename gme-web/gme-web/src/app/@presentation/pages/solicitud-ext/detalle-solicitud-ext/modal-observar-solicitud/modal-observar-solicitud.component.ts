import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { FormBuilder, FormGroup } from '@angular/forms';

@Component({
  selector: 'gme-web-modal-observar-solicitud',
  templateUrl: './modal-observar-solicitud.component.html',
  styleUrls: ['./modal-observar-solicitud.component.scss']
})
export class ModalObservarSolicitudComponent implements OnInit {
  frm: FormGroup = null;
  obj = {
    valida: false,
    mensaje: ''
  };
  constructor(
    private matDialog: MatDialogRef<ModalObservarSolicitudComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalObservarSolicitudModel,
    private fb: FormBuilder
  ) { }

  ngOnInit(): void {
    this.initForm();
  }

  cerrar(flag: boolean = false) {
    if (flag) {
      this.obj.valida = flag;
      if (this.frm.value.obs.length > 0) {
        this.obj.mensaje = this.frm.value.obs;
      } 
    }
    this.matDialog.close(this.obj);
  }

  initForm() {
    this.frm = this.fb.group({
      obs: ['']
    });
  }

}
export interface ModalObservarSolicitudModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}
