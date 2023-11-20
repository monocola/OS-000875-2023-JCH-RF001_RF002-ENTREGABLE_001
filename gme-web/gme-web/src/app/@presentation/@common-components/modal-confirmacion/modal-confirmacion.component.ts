import { Component, EventEmitter, Inject, OnInit, Output } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import FileSaver from 'file-saver';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import { ToastService } from '../toast';

@Component({
  selector: 'gme-web-modal-confirmacion',
  templateUrl: './modal-confirmacion.component.html',
  styleUrls: ['./modal-confirmacion.component.scss']
})
export class ModalConfirmacionComponent  {

  @Output() closeOrgano = new EventEmitter();
  @Output() updateOrganos = new EventEmitter();

  fileArchivo = null;
  fileOrganoBase64 = null;
  file: any[];

  constructor(
    private toastService: ToastService,
    protected ref: MatDialogRef<ModalConfirmacionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any[]

  ) {
    this.file = data;
  }

  onClickNo(): void {
    this.ref.close();
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  uploadFile() {
    const rutaFileError = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${this.file}`;
    base64ToFilePromise(
      rutaFileError,
      'Archivo con errores.xlsm',
      ''
    ).then((file) => FileSaver.saveAs(file));
  }
}
