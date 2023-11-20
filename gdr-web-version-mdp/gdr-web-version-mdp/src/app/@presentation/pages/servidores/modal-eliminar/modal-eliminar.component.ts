import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-eliminar',
  templateUrl: './modal-eliminar.component.html',
  styleUrls: ['./modal-eliminar.component.scss']
})
export class ModalEliminarComponent implements OnInit {

  constructor(
    private matDialog: MatDialogRef<ModalEliminarComponent>
    ) { }

  ngOnInit(): void {
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
}
