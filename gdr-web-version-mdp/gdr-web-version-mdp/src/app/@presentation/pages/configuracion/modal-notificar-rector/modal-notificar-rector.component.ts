import { Component, OnInit, Input, Inject } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatDialogRef } from '@angular/material/dialog';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { ModalNotificarComponent } from '../modal-notificar/modal-notificar.component';

@Component({
  selector: 'serv-talento-modal-notificar-rector',
  templateUrl: './modal-notificar-rector.component.html',
  styleUrls: ['./modal-notificar-rector.component.scss']
})
export class ModalNotificarComponentRector implements OnInit {
  @Input() rutaImagen: string = './assets/images/icons/mail.png';
  [x: string]: any;

  searchMode = false;
  userRector = null;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalNotificarComponent>,
    protected ref: MatDialogRef<ModalNotificarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalNotificarComponent,
  ) {
    this.userRector = data;
  }

  frm: FormGroup = null;
  cicloDefaultDesc;
  notificacion: MaestraParametro[] = [];

  get f() {
    return this.frm.controls;
  }

  initForm() {
    this.frm = this.fb.group({
    });
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  ngOnInit(): void {
    this.initForm();
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
}
