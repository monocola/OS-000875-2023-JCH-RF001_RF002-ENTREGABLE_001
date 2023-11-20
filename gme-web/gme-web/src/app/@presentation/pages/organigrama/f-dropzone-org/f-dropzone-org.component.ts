import { Component, EventEmitter, Input, Output, OnInit, ChangeDetectionStrategy } from '@angular/core';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { OrganigramaRepository } from '../../../../@domain/repository/organigrama.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmacionComponent } from 'src/app/@presentation/pages/organigrama/modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from 'src/app/@presentation/pages/organigrama/modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { NbIconConfig } from '@nebular/theme';

@Component({
  selector: 'serv-talento-f-dropzone-org',
  templateUrl: './f-dropzone-org.component.html',
  styleUrls: ['./f-dropzone-org.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FDropzoneOrgComponent  implements OnInit  {
  disabledIconConfig: NbIconConfig = { icon: 'settings-2-outline', pack: 'eva' };
 @Input() model;
 @Output() modelChange: EventEmitter<any> = new EventEmitter();
 @Output() afterRemoveFile: EventEmitter<any> = new EventEmitter();
 @Input() allowedFiles: string[];
 @Input() maxSize = 0;

 // file: File;
  errorsFromFile: any[] = [];
  fileOrgano: File = null;
  fileOrganoBase64 = '';

  public fileDataModel = null;
  public previewUrl = null;
  public errorMsg = '';

  get fileName() {
    return this.model ? this.model.name : null;
  }

  constructor(
    private toastService: ToastService,
    private organigramaRepository: OrganigramaRepository,
    public dialog: MatDialog,
  ) { }

  ngOnInit() {
   /*  this.preview(this.fileOrgano);
      this.modelChange.next(this.fileOrgano); */
  }


  fileChangeEvent(fileInput: any) {

    this.errorsFromFile = [];

    if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
      if ( fileInput.target.files[0].type === 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' ) {
        this.fileOrgano = fileInput.target.files[0];
        console.log(this.fileOrgano);
        console.log(fileInput.target.files[0], 'fielInput.targettttttttt');
        this.toastService.showToast(
          'El archivo excede el tamaño de 500Kb',
          'danger'
        );
      }  else {
        this.toastService.showToast('Subir un archivo con extensión xlsx', 'danger');

      }
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
          'Solo están permitidos los archivos xlsx',
          'danger'
        );

      }
  /*     this.preview(this.fileOrgano);
      this.modelChange.next(this.fileOrgano); */
    }

}

  removeFile() {
    this.fileDataModel = null;
    this.fileOrgano = null;
  }


/*   private preview(f: File) {
     console.log(f);

     const mimeType = f.type;
     console.log(mimeType);
     if (mimeType) {
       console.log(mimeType);
       this.previewUrl;
       return;
     }
     const reader = new FileReader();
     console.log(reader);
     reader.readAsDataURL(f);
     console.log(reader.readAsDataURL(f));
     reader.onload = (_event) => {
       console.log(reader.onload );
       this.previewUrl = reader.result;
       console.log(this.previewUrl);
     };
  }
 */

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

