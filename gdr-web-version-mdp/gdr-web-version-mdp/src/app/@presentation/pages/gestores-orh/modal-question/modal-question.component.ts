import { Component, Inject, Input, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'gme-web-modal-question',
  templateUrl: './modal-question.component.html',
  styleUrls: ['./modal-question.component.scss'],
})
export class ModalQuestionComponent implements OnInit {
  @Input() title = 'Alerta';
  @Input() bodyText = 'Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';
  question: boolean = true;
  info = true;

  constructor(
    private matDialog: MatDialogRef<ModalQuestionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalQuestionConfirmationModel
  ) {
    if (data.info !== undefined) {
      this.info = data.info;
    }
  }

  ngOnInit(): void {}

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
  info: boolean;
}
