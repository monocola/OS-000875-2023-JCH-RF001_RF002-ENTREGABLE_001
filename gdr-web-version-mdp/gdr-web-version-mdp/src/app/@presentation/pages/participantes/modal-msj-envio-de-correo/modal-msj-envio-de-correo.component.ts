import { Component, Inject, OnInit } from '@angular/core';
import {
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-msj-envio-de-correo',
  templateUrl: './modal-msj-envio-de-correo.component.html',
  styleUrls: ['./modal-msj-envio-de-correo.component.scss']
})
export class ModalMsjEnvioDeCorreoComponent implements OnInit {

  constructor(
    private matDialogRef: MatDialogRef<ModalMsjEnvioDeCorreoComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  ngOnInit(): void {
  }

  onNoClick(ans: boolean = false) {
    this.matDialogRef.close(ans);
  }

}
