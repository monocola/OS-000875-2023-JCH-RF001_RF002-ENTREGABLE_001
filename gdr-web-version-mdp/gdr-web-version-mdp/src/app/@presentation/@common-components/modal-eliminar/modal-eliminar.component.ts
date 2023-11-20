import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

export interface ModalEliminarData {
  titulo?: string;
  mensaje?: string;
  textoConfirmar?: string;
  textoCancelar?: string;
}

@Component({
  selector: 'serv-talento-modal-eliminar',
  templateUrl: './modal-eliminar.component.html',
  styleUrls: ['./modal-eliminar.component.scss'],
})
export class ModalEliminarComponent implements OnInit {
  constructor(
    private matDialog: MatDialogRef<ModalEliminarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalEliminarData
  ) {}

  ngOnInit(): void {}

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
}
