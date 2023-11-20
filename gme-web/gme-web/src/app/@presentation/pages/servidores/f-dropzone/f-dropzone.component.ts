import { Component, EventEmitter, Input, Output } from '@angular/core';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmacionComponent } from 'src/app/@presentation/pages/servidores/modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from 'src/app/@presentation/pages/servidores/modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';

@Component({
  selector: 'serv-talento-f-dropzone',
  templateUrl: './f-dropzone.component.html',
  styleUrls: ['./f-dropzone.component.scss']
})
export class FDropzoneComponent {

 @Input() model;
 @Output() modelChange: EventEmitter<any> = new EventEmitter();
 @Output() afterRemoveFile: EventEmitter<any> = new EventEmitter();
 @Input() allowedFiles: string[];
 @Input() maxSize = 0;
/*  @Input() uploadFile; */
 
  errorsFromFile: any[] = [];

  fileOrgano: File = null;
  fileOrganoBase64 = '';

  public fileDataModel = null;
  public previewUrl = null;
  public errorMsg = '';

  constructor(
    private toastService: ToastService,
    private servidoresRepository: ServidoresRepository,
    public dialog: MatDialog,
  ) {}

  get fileName() {
    return this.model ? this.model.name : null; 
  }

  fileChangeEvent(fileInput) {
  this.errorsFromFile = [];

  if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
    this.toastService.showToast(
      'El archivo excede el tamaño de 500Kb',
      'danger'
    );
  } else {
    const extension = fileInput.target.files[0].name.split('.')[
      fileInput.target.files[0].name.split('.').length - 1
    ];
    const extensionesPermitidas = ['xlsx', 'XLSX'];
    if (extensionesPermitidas.includes(extension)) {
      this.fileOrgano = fileInput.target.files[0] as File;
      getBase64(this.fileOrgano).then((data: string) => {
        this.fileOrganoBase64 = data;
      });
      if (!this.fileOrgano ) {
        this.removeFile();
    }
      
    } else {
      this.removeFile();
      this.fileOrgano = null;
      this.fileOrganoBase64 = '';
      this.toastService.showToast(
        'Solo están permitidos los archivos xls y xlsx.',
        'danger'
      );
    }
  }
}

removeFile() {
  this.fileDataModel = null;
  this.fileOrgano = null;
}

  uploadFile() {
    this.servidoresRepository
      .uploadFileMasivo(this.fileOrganoBase64)
      .subscribe(
        (res) => {
          if (res === true) {
            this.openDialogOk();
 
          } else {
            this.openDialog(res.archivo);
          }
        },
        (err) => {
          this.toastService.showToast(
            'Ocurrió un error en el servidor',
            'danger'
          );
        }
      );
  }

  openDialog(archivo: any[]) {
      this.dialog.open(ModalConfirmacionComponent, {
        disableClose: true,
        data: archivo
      }) ;
  }
  
  openDialogOk() {
      this.dialog.open(ModalConfirmacionTodoBienComponent, {
        disableClose: true,
        data: this.fileOrganoBase64
      }) ;
  }

}
