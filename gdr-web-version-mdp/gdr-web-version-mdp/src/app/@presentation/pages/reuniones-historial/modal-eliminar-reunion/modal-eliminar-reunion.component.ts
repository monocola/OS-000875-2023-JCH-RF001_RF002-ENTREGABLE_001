import { Component, OnInit, Input, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-eliminar-reunion',
  templateUrl: './modal-eliminar-reunion.component.html',
  styleUrls: ['./modal-eliminar-reunion.component.scss']
})
export class ModalEliminarReunionComponent implements OnInit {

  @Input() title = 'Eliminar reunión';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';

  constructor(
    private matDialogRef: MatDialogRef<ModalEliminarReunionComponent>,
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
