import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise, getBase64 } from 'src/app/utils/converterFile';
import * as FileSaver from 'file-saver';

@Component({
  selector: 'serv-talento-registro-masivo-unidad-organica',
  templateUrl: './registro-masivo-unidad-organica.component.html',
  styleUrls: ['./registro-masivo-unidad-organica.component.scss'],
})
export class RegistroMasivoUnidadOrganicaComponent implements OnInit {
  @Output() closeOrgano = new EventEmitter();
  @Output() updateUnidadesOrganicas = new EventEmitter();

  fileOrgano: File = null;
  fileOrganoBase64 = '';

  errorsFromFile: any[] = [];

  constructor(
    private toastService: ToastService,
    private unidadOrganicaRepository: UnidadOrganicaRepository
  ) {}

  ngOnInit(): void {}

  remove() {
    this.errorsFromFile = [];
    this.fileOrgano = null;
    this.fileOrganoBase64 = null;
    this.toastService.showToast('Archivo removido', 'primary');
  }

  downloadFile() {
    this.unidadOrganicaRepository.downloadExcel().subscribe(
      (res) => {
        const nameFile = `Plantilla de carga masiva para unidades orgánicas.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  uploadFile() {
    this.unidadOrganicaRepository
      .uploadFileMasivo(this.fileOrganoBase64)
      .subscribe(
        (res) => {
          if (res === true) {
            this.toastService.showToast(
              'Las unidades orgánicas han sido registradas exitosamente',
              'success'
            );
            this.updateUnidadesOrganicas.emit();
            this.closeOrgano.emit(0);
          } else {
            this.updateUnidadesOrganicas.emit();
            this.toastService.showToast(
              'Algunos datos ingresados en el excel son incorrectos, los correctos han sido registrados en el sistema',
              'danger'
            );
            this.errorsFromFile = res.unidadOrganica.slice(0);
            const rutaFileError = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res.archivo}`;
            base64ToFilePromise(
              rutaFileError,
              'Archivo con errores.xlsx',
              ''
            ).then((file) => FileSaver.saveAs(file));
          }
        },
        (err) => {
          this.toastService.showToast(
            err || 'Ocurrió un error en el servidor',
            'danger'
          );
        }
      );
  }

  fileChangeEvent(fileInput) {
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
      } else {
        this.fileOrgano = null;
        this.fileOrganoBase64 = '';
        this.toastService.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }
}
