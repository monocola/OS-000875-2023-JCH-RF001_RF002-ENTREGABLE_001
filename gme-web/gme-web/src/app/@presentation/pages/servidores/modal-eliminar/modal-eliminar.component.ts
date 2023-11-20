import { Component, OnInit, Input, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-eliminar',
  templateUrl: './modal-eliminar.component.html',
  styleUrls: ['./modal-eliminar.component.scss']
})
export class ModalEliminarComponent implements OnInit {

  @Input() rutaImagen: string = 'assets/images/icons/icontrash.png';


  constructor(
    private matDialogRef: MatDialogRef<ModalEliminarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel

  ) { }

  ngOnInit(): void {
  }

/*   cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
 */

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
