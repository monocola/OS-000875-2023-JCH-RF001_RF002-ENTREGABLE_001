import { Component, OnInit } from '@angular/core';
import { EntidadRepository } from '../../../@domain/repository/entidad.repository';
import { FormBuilder, Validators } from '@angular/forms';
import { Entidad } from 'src/app/@data/model/entidad';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-entidad-perfil',
  templateUrl: './entidad-perfil.component.html',
  styleUrls: ['./entidad-perfil.component.scss'],
})
export class EntidadPerfilComponent implements OnInit {
  ubigeoToUpdate: number[] = [];

  imgData64 = '';
  imgFile: File = null;

  entidad: Entidad;

  flagUpdatePhoto = 0;

  imgData64P = '';
  imgFileP: File = null;
  flagUpdatePhotoP = 0;
  urlRegex = /(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})/;

  entidadForm = this.fb.group({
    descripcionEntidad: ['', Validators.required],
    sigla: ['', Validators.required],
    direccionFiscal: [{ value: '', disabled: true }, Validators.required],
    direccion: [{ value: '', disabled: false }],
    departamentoId: [{ value: '', disabled: true }, Validators.required],
    provinciaId: [{ value: '', disabled: true }, Validators.required],
    distritoId: [{ value: '', disabled: true }, Validators.required],
    telefono: [
      '',
      [Validators.required, Validators.minLength(6), Validators.maxLength(9)],
    ],
    anexo: ['', [Validators.minLength(1), Validators.maxLength(6)]],
    correo: ['', [Validators.required, Validators.pattern("^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$")]],
    urlWeb: [
      '',
      [
        Validators.minLength(7),
        Validators.maxLength(60),
        Validators.pattern(this.urlRegex),
      ],
    ],
  });

  constructor(
    private entidadServicio: EntidadRepository,
    private authRepository: AuthenticationRepository,
    private toastService: ToastService,
    private fb: FormBuilder
  ) {}

  ngOnInit(): void {
    setTimeout(() => {
      this.obtenerRegistro();
    }, 0);
  }

  get f() {
    return this.entidadForm.controls;
  }

  private obtenerRegistro() {
    const user = this.authRepository.getCurrentUserValue;
    this.entidadServicio.getRegistro(user.entidadId).subscribe((entidad) => {
      console.log (entidad);
      this.entidad = Object.assign({}, entidad);
      this.entidadForm.patchValue({
        descripcionEntidad: entidad.descripcionEntidad,
        sigla: entidad.sigla,
        direccionFiscal: entidad.direccionFiscal,
        anexo: entidad.anexo,
        telefono: entidad.telefono,
        correo: entidad.correo,
        urlWeb: entidad.urlWeb,
        direccion: entidad.direccion,
      });

      this.ubigeoToUpdate = [
        entidad.departamentoId,
        entidad.provinciaId,
        entidad.distritoId,
      ];
    });
  }

  fileLogoEvent(fileInput) {
    if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension = fileInput.target.files[0].name.split('.')[
        fileInput.target.files[0].name.split('.').length - 1
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
        this.imgFile = fileInput.target.files[0] as File;
        getBase64(this.imgFile).then((data: string) => {
          this.flagUpdatePhoto = 0;
          this.entidad.logo = null;
          this.imgData64 = data;
          this.entidadForm.patchValue({ imgExist: true });
        });
      } else {
        this.imgFile = null;
        this.toastService.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }

  filePortadaEvent(fileInputP) {
    if (fileInputP.target.files[0].size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension = fileInputP.target.files[0].name.split('.')[
        fileInputP.target.files[0].name.split('.').length - 1
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
        this.imgFileP = fileInputP.target.files[0] as File;
        getBase64(this.imgFileP).then((data: string) => {
          this.flagUpdatePhotoP = 0;
          this.entidad.urlPortada = null;
          this.imgData64P = data;
          this.entidadForm.patchValue({ imgExist: true });
        });
      } else {
        this.imgFileP = null;
        this.toastService.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }

  clearImage() {
    this.entidad.logo = null;
    this.imgData64 = '';
    this.imgFile = null;
    this.flagUpdatePhoto = 1;
  }
  clearImageP() {
    this.entidad.urlPortada = null;
    this.imgData64P = '';
    this.imgFileP = null;
    this.flagUpdatePhotoP = 1;
  }

  saveEntidad() {
    if (this.entidadForm.valid) {
      this.entidadServicio
        .saveRegistro(
          this.entidadForm.getRawValue(),
          this.imgData64,
          this.imgFile,
          this.entidad,
          this.flagUpdatePhoto,
          this.imgData64P,
          this.imgFileP,
          this.flagUpdatePhotoP
        )
        .subscribe(
          (res) => {
            this.imgData64 = '';
            this.imgFile = null;
            this.entidad = null;
            this.flagUpdatePhoto = 0;
            this.imgData64P = '';
            this.imgFileP = null;
            this.flagUpdatePhotoP = 0;
            this.toastService.showToast(
              'Los datos se han actualizado correctamente',
              'success'
            );
            this.obtenerRegistro();
          },
          (err) => {
            this.imgData64 = '';
            this.imgFile = null;
            this.entidad = null;
            this.flagUpdatePhoto = 0;
            this.imgData64P = '';
            this.imgFileP = null;
            this.flagUpdatePhotoP = 0;
            this.toastService.showToast(
              'Ocurrió un error al actualizar la entidad',
              'danger'
            );
          }
        );
    } else {
      this.toastService.showToast(
        'Complete los campos marcados con rojo',
        'danger'
      );
    }
  }
}
