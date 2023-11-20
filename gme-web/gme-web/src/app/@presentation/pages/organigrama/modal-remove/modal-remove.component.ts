import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';


@Component({
  selector: 'serv-talento-modal-remove',
  templateUrl: './modal-remove.component.html',
  styleUrls: ['./modal-remove.component.scss']
})
export class ModalRemoveComponent implements OnInit {
  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/icons/icontrash.png';

  constructor(
    private matDialogRef: MatDialogRef<ModalRemoveComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) { }

  ngOnInit(): void {
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }

}

export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}
