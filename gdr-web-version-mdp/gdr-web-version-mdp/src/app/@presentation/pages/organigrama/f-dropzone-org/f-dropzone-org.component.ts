import { Component, EventEmitter, Input, Output } from '@angular/core';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise, getBase64 } from 'src/app/utils/converterFile';
import { OrganigramaRepository } from '../../../../@domain/repository/organigrama.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmacionComponent } from 'src/app/@presentation/pages/organigrama/modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from 'src/app/@presentation/pages/organigrama/modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';

@Component({
  selector: 'serv-talento-f-dropzone-org',
  templateUrl: './f-dropzone-org.component.html',
  styleUrls: ['./f-dropzone-org.component.scss']
})
export class FDropzoneOrgComponent {
 @Input() model;
 @Output() modelChange: EventEmitter<any> = new EventEmitter();
 @Output() afterRemoveFile: EventEmitter<any> = new EventEmitter();
 @Input() allowedFiles: string[];
 @Input() maxSize = 0;
 
  errorsFromFile: any[] = [];
  fileOrgano: File = null;
  fileOrganoBase64 = '';

  public fileDataModel = null;
  public previewUrl = null;
  public errorMsg = '';

  constructor(
    private toastService: ToastService,
    private organigramaRepository: OrganigramaRepository,
    public dialog: MatDialog,
  ) { }

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
    console.log(extensionesPermitidas, 'extenciones permitidas');
    if (extensionesPermitidas.includes(extension)) {
      this.fileOrgano = fileInput.target.files[0] as File;
      getBase64(this.fileOrgano).then((data: string) => {
        this.fileOrganoBase64 = data;
      });
      
    } else {
      this.fileOrgano = null;
      this.fileOrganoBase64 = '';
      this.toastService.showToast(
        'Solo están permitidos los archivos xlsx',
        'danger'
      );
    }
  }
}
  uploadFile() {
    this.organigramaRepository
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

  openDialog( archivo: any[]) {
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

