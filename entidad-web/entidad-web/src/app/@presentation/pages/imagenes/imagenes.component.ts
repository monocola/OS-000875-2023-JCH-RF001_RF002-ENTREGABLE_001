import {
  CdkDragDrop,
  moveItemInArray,
  transferArrayItem,
} from '@angular/cdk/drag-drop';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { DomSanitizer } from '@angular/platform-browser';
import { BannerRepository } from 'src/app/@domain/repository/banner.repository';
import { ToastService } from '../../@common-components/toast';
import { ModalLinkComponent } from './modal-link/modal-link.component';

@Component({
  selector: 'serv-talento-imagenes',
  templateUrl: './imagenes.component.html',
  styleUrls: ['./imagenes.component.scss'],
})
export class ImagenesComponent implements OnInit {
  selectedFiles?: FileList;
  currentFile?: File;
  fileList: File[] = [];

  listaImagenes;
  url;
  conteoupload: number = 0;

  deleteImg: Array<number> = [];
  updateUrl: Array<any> = [];

  contador: number = 0;

  imagens2;
  flagOrden: boolean = false;

  listbd: number = 0;
  listfinal: number = 0;
  listadd: number = 0;

  flagGuardar: boolean = false;

  constructor(
    private sanitizer: DomSanitizer,
    private bannerService: BannerRepository,
    private dialog: MatDialog,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.obtenerImages();
  }

  seleccionarImagenes(event: any) {
    for (let i = 0; i <= event.target.files.length - 1; i++) {
      const extension = event.target.files[i].name.split('.')[
        event.target.files[i].name.split('.').length - 1
      ];
      const extensionesPermitidas = [
        'jpg',
        'JPG',
        'png',
        'PNG',
        'JPEG',
        'jpeg',
      ];

      if (extensionesPermitidas.includes(extension)) {
        const reqFileName = event.target.files[i].name.split('.')[0];
        const pattern = new RegExp(/^[A-Za-z0-9]+$/g);
        if (!pattern.test(reqFileName)) {
          this.toastService.showToast(
            ' El nombre de la imagen solo debe contener números y letras.',
            'danger',
            'Atención'
          );
        } else {
          let selectedFilex = event.target.files[i];

          let reader = new FileReader();
          reader.onload = (e: any) => {
            if (this.listfinal < 10) {
              this.fileList.push(selectedFilex);
              this.listaImagenes.push({
                bannerId: 0,
                orden: 0,
                descripcion: event.target.files[i].name,
                urlImg: e.target.result,
                urlWeb: '',
              });
              this.contador = this.listaImagenes.length;
              this.imagens2 = this.listaImagenes.map((x) => ({ ...x }));
              this.listadd++;
              this.listfinal = this.listbd + this.listadd;
              this.flagGuardar = true;
            }
          };

          reader.readAsDataURL(event.target.files[i]);
        }
      } else {
        this.toastService.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }

  remvoverImagenesSeleccionadas(index) {
    if (this.listaImagenes[index].bannerId !== 0) {
      this.deleteImg.push(this.listaImagenes[index].bannerId);
    }
    this.fileList.forEach((e, i) => {
      if (e.name === this.listaImagenes[index].descripcion)
        this.fileList.splice(i, 1);
    });
    this.listaImagenes.splice(index, 1);
    this.contador = this.listaImagenes.length;
    this.fileList.splice(index, 1);

    this.imagens2 = this.listaImagenes.map((x) => ({ ...x }));
    this.listadd--;
    this.listfinal = this.listbd + this.listadd;
    this.flagGuardar = true;
  }

  openModalAgregarLink(data?: any) {
    // Validando que la categoría se haya creado

    const modalNivelEducativo = this.dialog.open(ModalLinkComponent, {
      width: '40rem',
      data: {
        urlImg: data.urlImg,
        urlWeb: data.urlWeb,
      },
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.listaImagenes.map(function (dato) {
          if (dato.urlImg === data.urlImg) {
            dato.urlWeb = res;
          }
        });
        this.imagens2.map(function (dato) {
          if (dato.urlImg === data.urlImg) {
            dato.urlWeb = res;
          }
        });
        this.flagGuardar = true;
        if (data.bannerId !== 0) {
          this.updateUrl.push({
            bannerId: data.bannerId,
            url: res,
          });
        }
      }
    });
  }

  obtenerImages() {
    this.bannerService.getImagenes().subscribe((res: any) => {
      this.listaImagenes = res;
      this.contador = res.length;

      this.imagens2 = this.listaImagenes.map((x) => ({ ...x }));

      this.listbd = this.listaImagenes.length;
      this.listfinal = this.listbd + this.listadd;
    });
  }

  guardarImagenesNuevas() {
    let countNuevo = this.fileList.length;

    let conteo2 = 1;
    if (countNuevo !== 0) {
      for (let i = 0; i < this.fileList.length; i++) {
        for (let index = 0; index < this.listaImagenes.length; index++) {
          if (this.fileList[i].name === this.listaImagenes[index].descripcion) {
            this.bannerService
              .saveImagenes(this.fileList[i], this.listaImagenes[index].urlWeb)
              .subscribe((event: any) => {
                if (event.type === HttpEventType.UploadProgress) {
                  this.conteoupload = Math.round(
                    (100 * event.loaded) / event.total
                  );
                } else if (event instanceof HttpResponse) {
                  let base64 = this.listaImagenes[i].urlImg;

                  this.imagens2.map(function (dato) {
                    if (dato.urlImg === base64) {
                      dato.bannerId = event.body.payload.bannerId;
                    }
                  });

                  if (countNuevo === conteo2) {
                    this.fileList = [];
                    this.conteoupload = 0;
                    this.actualizarLink();
                  }
                  conteo2++;
                }
              });
          }
        }
      }
    } else {
      this.actualizarLink();
    }
  }

  actualizarLink() {
    // Actualizar Links
    let update = this.updateUrl.length;
    let conteo = 1;
    if (update !== 0) {
      for (let index = 0; index < this.updateUrl.length; index++) {
        this.bannerService
          .updateUrlWeb(
            this.updateUrl[index].bannerId,
            this.updateUrl[index].url
          )
          .subscribe((res: any) => {
            if (update === conteo) {
              this.updateUrl = [];
              this.actualizarOrden();
            }
            conteo++;
          });
      }
    } else {
      this.actualizarOrden();
    }
  }

  actualizarOrden() {
    let conteo = 1;
    if (this.flagOrden) {
      let count = this.imagens2.length;
      for (let index = 0; index < this.imagens2.length; index++) {
        this.bannerService
          .updateOrden(this.imagens2[index].bannerId, index + 1)
          .subscribe((res: any) => {
            if (count === conteo) {
              this.flagOrden = false;
              this.obtenerImages();
              this.toastService.showToast(
                'Se guardaron todos los cambios realizados.',
                'success'
              );
              this.flagGuardar = false;
            }
            conteo++;
          });
      }
    } else {
      this.flagGuardar = false;
      this.obtenerImages();
      this.toastService.showToast(
        'Se guardaron todos los cambios realizados.',
        'success'
      );
    }
  }

  guardarImagenes() {
    let countDelete = this.deleteImg.length;
    let conteo = 1;
    if (countDelete !== 0) {
      for (let index = 0; index < this.deleteImg.length; index++) {
        this.bannerService
          .eliminarImage(this.deleteImg[index])
          .subscribe((res: any) => {
            if (countDelete === conteo) {
              this.deleteImg = [];
              this.guardarImagenesNuevas();
            }
            conteo++;
          });
      }
    } else {
      this.guardarImagenesNuevas();
    }
  }

  dropOld(event: CdkDragDrop<string[]>) {
    if (event.previousContainer === event.container) {
      moveItemInArray(
        event.container.data,
        event.previousIndex,
        event.currentIndex
      );
    } else {
      transferArrayItem(
        event.previousContainer.data,
        event.container.data,
        event.previousIndex,
        event.currentIndex
      );
    }
  }

  drop(event: CdkDragDrop<string[]>) {
    if (event.previousContainer === event.container) {
      moveItemInArray(
        event.container.data,
        event.previousIndex,
        event.currentIndex
      );
    } else {
      const imagen = { ...(event.container.data[event.currentIndex] as any) };
      const previousImagen = {
        ...(event.previousContainer.data[event.previousIndex] as any),
      };
      event.container.data.splice(event.currentIndex, 1, previousImagen);
      event.previousContainer.data.splice(event.previousIndex, 1, imagen);
    }
    this.flagOrden = true;
    this.flagGuardar = true;
  }
}
