import { Component, Inject, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import {
  MatDialog,
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';
import { Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { Entity } from 'src/app/@data/model/entity';
import { PersonaJuridica } from 'src/app/@data/model/persona-juridica';
import { Const } from 'src/app/@data/services/const';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { SunatRepository } from 'src/app/@domain/repository/sunat.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { ModalQuestionComponent } from '../../gestores-orh/modal-question/modal-question.component';

@Component({
  selector: 'serv-talento-modal-registrar',
  templateUrl: './modal-editar.component.html',
  styleUrls: ['./modal-editar.component.scss'],
})
export class ModalEditarComponent implements OnInit {
  editionMode: boolean = false;
  frm: FormGroup;
  mensaje: any;
  sigla: any;
  codigo: any;
  conteoupload: number = 0;
  token: any = null;
  personaJuridica: PersonaJuridica = null;
  entity: Entity;

  searchMode = false;
  cboNivel: MaestraParametro[] = [];
  cboSector: MaestraParametro[] = [];
  cboTipoEntPub: MaestraParametro[] = [];
  actualizaRazon: number = 0;
  entidadId: number;
  extension: string;
  razonSocial: string;

  constructor(
    private matDialog: MatDialogRef<ModalEditarComponent>,
    private fb: FormBuilder,
    private route: Router,
    private authenticationRepository: AuthenticationRepository,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private sunatRepository: SunatRepository,
    private toastService: ToastService,
    private parameterRepository: ParameterRepository,
    private dialog: MatDialog,
    private entidadService: EntidadService,
    private authenticationService: AuthenticationRepository,
    private entidad : EntidadService
  ) {}

  logoProfile;
  nomArchivo = '';
  imgData64 = '';
  imgFile: File = null;
  imgString = '';
  imgInitialString = '';

  flagUpdatePhoto = 0;
  imgEdit = true;
  tieneImagen: boolean = false;

  ngOnInit(): void {
    this.loadCombox();
    this.initializeForm();
  }

  get f() {
    return this.frm.controls;
  }
  initializeForm() {
    const entidad = this.data.dataToEdit;

    if (entidad !== undefined) {
      this.entity = entidad;
      this.entidadId = this.entity.entidadId;
      this.imgString =  this.entity.logo;

      if(this.imgString !== null) {
        this.tieneImagen = true
      }
    }

    this.razonSocial = this.entity.razonSocial;

    if(this.tieneImagen) {
      if (this.imgEdit && this.imgString.length > 0) {
        this.imgString = Const.API_FILE_SERVER + this.imgString;
          this.cargarDataImagenes(this.imgString);
        }
    }


    this.frm = this.fb.group({
      numeroruc: [
        this.entity.numeroDocumento,
        [Validators.required, Validators.maxLength(11)],
      ],
      razonSocial: [this.entity.razonSocial, [Validators.required]],
      nombreabreviado: [
        this.entity.sigla,
        [Validators.required, Validators.maxLength(11)],
      ],
      nroSindicatos: [this.entity.nroSindicatos, null],
      nivelGobiernoId: [
        this.entity.nivelGobiernoId, null,
      ],
      tipoEntidadId: [
        this.entity.tipoEntidadPubId, null,
      ],
      sectorId: [
        this.entity.sectorId, null,
      ],
      base64Image: new FormControl(null),
      archivo: new FormControl(null),
    });
  }

  cargarDataImagenes(imagen) {

    let arrayImg = imagen.split('/');
    let tmnArray = arrayImg.length;

    this.nomArchivo = arrayImg[tmnArray - 1];
    if(this.tieneImagen) {
      this.imgInitialString = this.nomArchivo;
    }

  }


  blobUrlToFile = (blobUrl: string): Promise<File> => new Promise((resolve) => {
    fetch(blobUrl, {
      mode: 'no-cors',
      headers: {
        'Access-Control-Allow-Origin': '*'
      }}).then((res) => {
      res.blob().then((blob) => {
        // please change the file.extension with something more meaningful
        // or create a utility function to parse from URL
        const file = new File([blob], 'file.jpg', {type: blob.type});

        resolve(file);
      });
    });
  })
   getBase64FromUrl = async (url) => {
    const data = await fetch(url, {
      mode: 'no-cors',
      headers: {
        'Access-Control-Allow-Origin': '*'
      }
    });
    const blob = await data.blob();
    return new Promise((resolve) => {
      const reader = new FileReader();
      reader.readAsDataURL(blob);
      reader.onloadend = () => {
        const base64data = reader.result;
        resolve(base64data);
      };
    });
  }

  loadCombox() {
    const getNivel = this.parameterRepository.getNiveles();
    const getSector = this.parameterRepository.getSectores();
    const getTipoFilter = this.parameterRepository.getTipoEntidad();

    forkJoin([getNivel, getSector, getTipoFilter]).subscribe(
      (results) => {
        this.cboNivel = results[0];
        this.cboSector = results[1];
        this.cboTipoEntPub = results[2];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  fileLogoEvent(fileInput) {
    if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension =
        fileInput.target.files[0].name.split('.')[
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
        this.nomArchivo = this.imgFile.name;
        this.extension = this.imgFile.type;
        getBase64(this.imgFile).then((data: string) => {
          this.flagUpdatePhoto = 0;
          this.logoProfile = null;
          this.imgData64 = data;
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

  clearImage() {
    this.logoProfile = null;
    this.imgData64 = '';
    this.imgFile = null;
    if(this.tieneImagen) {
      this.nomArchivo = this.imgInitialString
    }
  }

  validarCamposObligatoriosGeneral() {
    this.frm.controls.nombreabreviado.markAsDirty();
  }

  enviar() {

    if(this.imgFile===null && this.tieneImagen === false) {

        this.toastService.showToast(
          'Pendiente subir imágen',
          'warning',
          'Actualizar entidad'
        );
      
    }

    if(this.tieneImagen && this.imgData64 == '') {

      this.imgData64 = null
    }

    this.validarCamposObligatoriosGeneral();

      if (this.frm.valid) {
        const add = this.dialog.open(ModalQuestionComponent, {
          data: {
            title: ' ',
            bodyText:
              'Se actualizará datos de la entidad <br> ¿Está realmente seguro de realizar la siguiente acción?',
            rutaImagen: './assets/images/question.png',
            textCancel: 'NO',
            textOk: 'SI',
          },
        });
        add.afterClosed().subscribe((res) => {
          if (res) {
            this.actualizarEntidad();
          } else {
            this.toastService.showToast(
              'Cancelo el registro de entidad',
              'danger',
              'Error'
            );
          }
        });
      }  
  }

  validarRUC() {
    this.sunatRepository
      .getSunatInfo(this.frm.value.numeroruc)
      .subscribe((item) => {
        this.personaJuridica = item.personaJuridica;
        const razonSocial: string = this.razonSocial;

        if (
          razonSocial !== this.personaJuridica.razonSocial.trim().toLowerCase()
        ) {
          this.frm.controls['razonSocial'].setValue(
            this.personaJuridica.razonSocial
          );
          this.frm.controls['razonSocial'].patchValue(
            this.personaJuridica.razonSocial
          );
          this.actualizaRazon = 1;
        }
      });
  }

  getToken() {
    this.authenticationRepository.generatePublicToken().subscribe((item) => {
      this.token = item;
    });
  }

  actualizarEntidad() {
   
    const bodyParam = {
      rucEntidad: this.frm.value.numeroruc,
      razonSocial: this.frm.value.razonSocial,
      sigla: this.frm.value.nombreabreviado,
      nivelGobiernoId: this.frm.value.nivelGobiernoId,
      sectorId: this.frm.value.sectorId,
      tipoEntidadId: this.frm.value.tipoEntidadId,
      base64Image: this.imgData64,
      actualizaRazon: this.actualizaRazon,
      entidadId: this.entidadId,
      nrosSindicatos: this.frm.value.nroSindicatos,
      fileName: this.nomArchivo,
      extension: 'png',
    };

    this.entidadService
      .actualizarEntidad(bodyParam)
      .subscribe((response: any) => {
        if (response.status.success) {
          this.toastService.showToast(
            'Se guardaron los cambios en forma exitosa',
            'success',
            'Atención'
          );

          this.authenticationService
            .verifyEntityUpdatedV2(this.entidadId)
            .subscribe(
              (res) => {
                if (res === '1') {
                  this.onNoClick(true);
                }
              },
              (err) => {
                this.authenticationService.clearUser();
              }
            );

        } else {
          let respTexto = response.status.error.messages[0];
          let cad = respTexto.substring(
            respTexto.indexOf('[') + 2,
            respTexto.indexOf(']') - 1
          );
          this.toastService.showToast(cad, 'danger');
        }
      });
  }
}

export interface DataModel {
  editMode: boolean;
  dataToEdit: any;
  estados: any[];
}
