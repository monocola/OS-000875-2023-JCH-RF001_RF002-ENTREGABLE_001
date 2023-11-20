import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-eliminar-meta',
  templateUrl: './modal-eliminar-meta.component.html',
  styleUrls: ['./modal-eliminar-meta.component.scss']
})
export class ModalEliminarMetaComponent implements OnInit {
  @Input() rutaImagen: string = 'assets/images/icons/icontrash.png';
  posItem = null;

  constructor(
    private matDialogRef: MatDialogRef<ModalEliminarMetaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) { }

  ngOnInit(): void {
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close({
      result: type,
      posItem: this.posItem,
    });
  }

  eliminar() {
    this.matDialogRef.close({
      result: true,
      posItem: this.posItem,
    });
  }
}


export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}


