import { Component, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';


@Component({
  selector: 'serv-talento-modal-confirmacion-todo-bien',
  templateUrl: './modal-confirmacion-todo-bien.component.html',
  styleUrls: ['./modal-confirmacion-todo-bien.component.scss']
})
export class ModalConfirmacionTodoBienComponent implements OnInit {

  constructor(

   protected ref: MatDialogRef<ModalConfirmacionTodoBienComponent>,

  ) { }

  ngOnInit(): void {
  }


  onClickAceptar(): void {
    this.ref.close();
  }

}
