import { Component, Inject, Input, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'gme-web-modal-eliminar-question',
  templateUrl: './modal-eliminar-question.component.html',
  styleUrls: ['./modal-eliminar-question.component.scss']
})
export class ModalEliminarQuestionComponent implements OnInit {
  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';

  constructor(
    private matDialog: MatDialogRef<ModalEliminarQuestionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalQuestionConfirmationModel
  ) { }

  ngOnInit(): void {
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }

}
export interface ModalQuestionConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}
