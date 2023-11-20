import { Component, OnInit, Inject, Input } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-remove-cronograma',
  templateUrl: './modal-remove-cronograma.component.html',
  styleUrls: ['./modal-remove-cronograma.component.scss']
})
export class ModalRemoveCronogramaComponent implements OnInit {

  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/icons/icontrash.png';

  constructor(
    private matDialogRef: MatDialogRef<ModalRemoveCronogramaComponent>,
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
