import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-notificaciones',
  templateUrl: './modal-notificaciones.component.html',
  styleUrls: ['./modal-notificaciones.component.scss']
})
export class ModalNotificacionesComponent implements OnInit {

  constructor(
    private matDialog: MatDialogRef<ModalNotificacionesComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataNotificaciones,
  ) { }

  ngOnInit(): void {
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }


}
interface DataNotificaciones {
  tipo?: string;
}

