import { HttpEventType, HttpResponse } from '@angular/common/http';
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
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { PuestoRepository } from 'src/app/@domain/repository/puesto.repository';
import { SunatRepository } from 'src/app/@domain/repository/sunat.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
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

  unidadOrganicaCbo: UnidadOrganicaCombo[] = [];
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
    private authenticationService: AuthenticationRepository
  ) {}

  logoProfile;
  nomArchivo = '';
  imgData64 = '';
  imgFile: File = null;
  flagUpdatePhoto = 0;

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
    }

    this.razonSocial = this.entity.razonSocial;

    this.frm = this.fb.group({
      numeroruc: [
        this.entity.numeroDocumento,
        [Validators.required, Validators.maxLength(11)],
      ],
      razonSocial: [this.entity.razonSocial, [Validators.required]],
      nombreabreviado: [
        this.entity.sigla,
        [Validators.required, Validators.maxLength(10)],
      ],
      nroSindicatos: [this.entity.nroSindicatos, [Validators.required]],
      nivelGobiernoId: [
        this.entity.nivelGobiernoId,
        [Validators.required, Validators.maxLength(11)],
      ],
      tipoEntidadId: [
        this.entity.tipoEntidadPubId,
        [Validators.required, Validators.maxLength(11)],
      ],
      sectorId: [
        this.entity.sectorId,
        [Validators.required, Validators.maxLength(11)],
      ],
      base64Image: new FormControl(null),
      archivo: new FormControl(null),
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

  fileLogoEvent(events) {

    // this.filesSize.push(...events.addedFiles);
    console.log(events)
    if (events.addedFiles[0].size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension =
      events.addedFiles[0].name.split('.')[
          events.addedFiles[0].name.split('.').length - 1
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
        this.imgFile = events.addedFiles[0] as File;
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
  }

  validarCamposObligatoriosGeneral() {
    this.frm.controls.nombreabreviado.markAsDirty();
    this.frm.controls.nroSindicatos.markAsDirty();
    this.frm.controls.nivelGobiernoId.markAsDirty();
    this.frm.controls.tipoEntidadId.markAsDirty();
    this.frm.controls.sectorId.markAsDirty();
  }

  enviar() {
    this.validarCamposObligatoriosGeneral();

    if (this.frm.valid && this.imgFile) {
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
            'Cancelo el registro de solicitud',
            'danger',
            'Error'
          );
        }
      });
    } else {
      if (!this.imgFile) {
        this.toastService.showToast(
          'Pendiente subir logo',
          'warning',
          'Actualizar entidad'
        );
      }
    }
  }

  validarRUC() {
    this.sunatRepository
      .getSunatInfo(this.frm.value.numeroruc)
      .subscribe((item) => {
        this.personaJuridica = item.personaJuridica;
        let razonSocial: string = this.razonSocial;

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

          this.toastService.showToast(
            'Nº de RUC',
            'success',
            'Razón Social ha sido actualizada'
          );
        }
      });
  }

  getToken() {
    this.authenticationRepository.generatePublicToken().subscribe((item) => {
      this.token = item;
    });
  }

  actualizarEntidad() {
    const niv = this.cboNivel.find(
      (niv) => niv.parametroId === this.frm.value.nivelGobiernoId
    );
    const sec = this.cboSector.find(
      (sec) => sec.parametroId === this.frm.value.sectorId
    );
    const tipoEntidad = this.cboTipoEntPub.find(
      (tipEnt) => tipEnt.parametroId === this.frm.value.tipoEntidadId
    );

    const bodyParam = {
      rucEntidad: this.frm.value.numeroruc,
      razonSocial: this.frm.value.razonSocial,
      sigla: this.frm.value.nombreabreviado,
      nivelGobiernoId: this.frm.value.nivelGobiernoId,
      sectorId: this.frm.value.sectorId,
      tipoEntidadId: this.frm.value.tipoEntidadId,
      base64Image: this.imgData64,
      actualizaRazon: 1,
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
                console.log(res);
                if (res === '1') {
                  this.onNoClick(true);
                }
              },
              (err) => {
                this.authenticationService.clearUser();
              }
            );

          this.route.navigateByUrl('/pages/entidad');
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
