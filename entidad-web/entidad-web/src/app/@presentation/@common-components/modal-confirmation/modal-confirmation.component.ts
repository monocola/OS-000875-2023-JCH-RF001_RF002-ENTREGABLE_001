import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-confirmation',
  templateUrl: './modal-confirmation.component.html',
  styleUrls: ['./modal-confirmation.component.scss'],
})
export class ModalConfirmationComponent implements OnInit {
  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() bodyHtmlText: string;
  @Input() rutaImagen: string = 'assets/images/icons/eliminar.png';
  @Input() visibleCancel: boolean = true;

  constructor(
    private matDialogRef: MatDialogRef<ModalConfirmationComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) { }

  ngOnInit(): void {
    if (
      this.data.visibleCancel === undefined ||
      this.data.visibleCancel === null
    ) {
      this.data.visibleCancel = true;
    }
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }
}

export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  bodyHtmlText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
  visibleCancel: boolean;
}
