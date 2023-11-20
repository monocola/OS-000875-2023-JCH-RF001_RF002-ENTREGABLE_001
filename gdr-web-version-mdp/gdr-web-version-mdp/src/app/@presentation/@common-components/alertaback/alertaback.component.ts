import { Component, Inject, Input, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ModalConfirmationModel } from '../../pages/organigrama/modal-remove/modal-remove.component';

@Component({
  selector: 'serv-talento-alertaback',
  templateUrl: './alertaback.component.html',
  styleUrls: ['./alertaback.component.scss']
})
export class AlertabackComponent implements OnInit {

  info: any = null;
  @Input() rutaImagen: string = 'assets/images/icons/pregunta.svg';

  constructor(
    private matDialogRef: MatDialogRef<AlertabackComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) {
    this.info = data;
    this.info.bodyText = this.info.bodyText.replace("//", "/");
    console.log("this.info");
    console.log(this.info);
  }

  ngOnInit(): void {
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }
}
