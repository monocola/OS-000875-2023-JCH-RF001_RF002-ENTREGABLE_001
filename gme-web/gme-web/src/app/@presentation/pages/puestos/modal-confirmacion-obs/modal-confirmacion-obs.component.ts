import { Component, EventEmitter, Inject, OnInit, Output } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { base64ToFilePromise } from '../../../../utils/converterFile';
import FileSaver from 'file-saver';

@Component({
  selector: 'gme-web-modal-confirmacion-obs',
  templateUrl: './modal-confirmacion-obs.component.html',
  styleUrls: ['./modal-confirmacion-obs.component.scss']
})
export class ModalConfirmacionObsComponent implements OnInit {

  // @Input() downloadFile: boolean = true;
  @Output() closeOrgano = new EventEmitter();
  @Output() updateOrganos = new EventEmitter();

  fileArchivo = null;
  fileOrganoBase64 = null;
  file: any[];
  extension: string;

  constructor(
    protected ref: MatDialogRef<ModalConfirmacionObsComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmacionObsPuesto
  ) {
    this.file = data.archivo;
    this.extension = data.extension;
  }

  ngOnInit(): void {
  }

  onClickNo(): void {
    this.ref.close(true);
  }

  uploadFile() {
    const rutaFileError = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${this.file}`;
    base64ToFilePromise(
      rutaFileError,
      'Archivo con errores.' + this.extension,
      ''
    ).then((file) => FileSaver.saveAs(file));
  }
}

export interface ModalConfirmacionObsPuesto {
  archivo: any[];
  extension: string;
}
