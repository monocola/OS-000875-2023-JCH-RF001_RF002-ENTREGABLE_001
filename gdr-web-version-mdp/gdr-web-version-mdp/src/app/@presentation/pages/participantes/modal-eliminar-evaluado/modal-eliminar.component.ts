import { Component, Inject, Input, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-eliminar',
  templateUrl: './modal-eliminar.component.html',
  styleUrls: ['./modal-eliminar.component.scss'],
})
export class ModalEliminarEvaluadoComponent implements OnInit {
  constructor(
    private matDialog: MatDialogRef<ModalEliminarEvaluadoComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  get titulo() {
    return this.data?.titulo ?? 'evaluado';
  }

  ngOnInit(): void {}

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
}
