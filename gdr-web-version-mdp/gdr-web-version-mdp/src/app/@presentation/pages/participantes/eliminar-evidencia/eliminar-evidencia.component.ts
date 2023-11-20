import { Component, Input, OnInit, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-eliminar-evidencia',
  templateUrl: './eliminar-evidencia.component.html',
  styleUrls: ['./eliminar-evidencia.component.scss']
})
export class EliminarEvidenciaComponent implements OnInit {
  @Input() rutaImagen: string = 'assets/images/icons/icontrash.png';
  posItem = null;

  constructor(
    private matDialogRef: MatDialogRef<EliminarEvidenciaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) {
    this.posItem = data;
  }

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

