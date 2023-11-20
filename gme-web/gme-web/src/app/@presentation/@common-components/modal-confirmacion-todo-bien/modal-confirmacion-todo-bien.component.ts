import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'gme-web-modal-confirmacion-todo-bien',
  templateUrl: './modal-confirmacion-todo-bien.component.html',
  styleUrls: ['./modal-confirmacion-todo-bien.component.scss']
})
export class ModalConfirmacionTodoBienComponent {

  constructor(
    protected ref: MatDialogRef<ModalConfirmacionTodoBienComponent>,
  ) { }


  onClickAceptar(): void {
    this.ref.close();
  }

}
