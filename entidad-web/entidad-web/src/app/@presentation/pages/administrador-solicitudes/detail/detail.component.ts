import { ChangeDetectorRef, Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityRequest } from 'src/app/@data/model/entityRequest';
import { Const } from 'src/app/@data/services/const';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReniecRepository } from 'src/app/@domain/repository/reniec.repository';
import { SunatRepository } from 'src/app/@domain/repository/sunat.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalVerificationComponent } from './modal-verification/modal-verification.component';

@Component({
  selector: 'serv-talento-detail',
  templateUrl: './detail.component.html',
  styleUrls: ['./detail.component.scss'],
})
export class DetailComponent implements OnInit {
  entity: EntityRequest;
  idEntity = '';

  correctNamePerson = '';
  correctNameEntity = '';
  correctGenreDescription = '';
  correctGenre = '';

  personVerified = false;
  entityVerified = false;

  errorReniec = false;
  errorSunat = false;
  errorGenre = false;

  editMode = false;
  genres: [];

  updateNameForm = this.fb.group({
    name: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    fatherName: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    motherName: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    genre: ['', [Validators.required]],
  });

  constructor(
    public route: ActivatedRoute,
    public router: Router,
    private fb: FormBuilder,
    private toastService: ToastService,
    private administratorRepository: AdministratorRepository,
    private parameterRepository: ParameterRepository,
    private reniecRepository: ReniecRepository,
    private sunatRepository: SunatRepository,
    private cdRef: ChangeDetectorRef,
    private dialog: MatDialog
  ) {
    this.changeRouteListener();
  }

  ngOnInit(): void {
    this.getCombox();
  }

  get f() {
    return this.updateNameForm.controls;
  }

  getCombox() {
    this.parameterRepository.getGenres().subscribe(
      (res: any) => (this.genres = res),
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  getEntityDetail(id) {
    this.administratorRepository.getSolicitudById(id).subscribe(
      (res) => {
        this.entity = res;
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
        this.router.navigateByUrl('/gestionsolicitud');
      }
    );
  }

  verifyReniec() {
    const doc = this.entity.personaNatural.numeroDocumento;
    this.reniecRepository.getPersonInfo(doc).subscribe(
      (res) => {
        this.verifySunat();
        this.personVerified = true;
        if (res !== true) {
          if (
            (res.sexo && this.entity.personaNatural.sexo !== res.sexo) ||
            this.entity.personaNatural.getFullNameAdmin().trim() !==
              res.nombreCompleto
          ) {
            this.toastService.showToast(
              'Los datos ingresados no coinciden con RENIEC/SUNAT',
              'danger'
            );
            this.correctGenre = res.sexo;
            if (res.sexo && this.entity.personaNatural.sexo !== res.sexo) {
              this.errorGenre = true;
              this.correctGenreDescription =
                res.sexo === '1' ? 'MASCULINO' : 'FEMENINO';
            }
            if (
              this.entity.personaNatural.getFullNameAdmin().trim() !==
              res.nombreCompleto
            ) {
              this.correctNamePerson = res.nombreCompleto;
              this.errorReniec = true;
            }
          }
        }
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  verifySunat() {
    this.errorSunat = false;
    this.entityVerified = false;
    const doc = this.entity.personaJuridica.numeroDocumento;
    this.sunatRepository.getSunatInfo(doc).subscribe(
      (res) => {
        this.entityVerified = true;
        if (res?.code === 1 || res?.code === 2) {
          this.errorSunat = true;
          this.toastService.showToast(res.message, 'danger');
        } else {
          if (
            this.entity.personaJuridica.razonSocial !==
            res.personaJuridica.razonSocial
          ) {
            this.toastService.showToast(
              'Los datos ingresados no coinciden con RENIEC/SUNAT',
              'danger'
            );
            this.correctNameEntity = res.personaJuridica.razonSocial;
            this.errorSunat = true;
          } else {
            this.entity.personaJuridica.ubigeoId =
              res.direcciones[0].ubigeoFull?.distritoUbigeoId;
            this.entity.personaJuridica.direccionCompleta =
              res.direcciones[0].direccionCompleta;
            this.entity.personaJuridica.referenciaDireccion =
              res.direcciones[0].referencia;
          }
        }
      },
      (err) => {
        this.errorSunat = true;
        this.entityVerified = false;
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  editAdministrator() {
    this.editMode = true;
    this.cdRef.detectChanges();
    setTimeout(() => {
      this.updateNameForm.patchValue({
        name: this.entity.personaNatural.nombres,
        fatherName: this.entity.personaNatural.apellidoPaterno,
        motherName: this.entity.personaNatural.apellidoMaterno,
        genre: this.entity.personaNatural.sexo,
      });
    }, 0);
  }

  saveData() {
    const id = this.entity.solicitudEntidad.solicitudEntidadId;
    this.entity.personaNatural.apellidoPaterno = this.updateNameForm
      .get('fatherName')
      .value.toUpperCase();
    this.entity.personaNatural.apellidoMaterno = this.updateNameForm
      .get('motherName')
      .value?.toUpperCase();
    this.entity.personaNatural.nombres = this.updateNameForm
      .get('name')
      .value.toUpperCase();
    this.entity.personaNatural.sexo = this.updateNameForm.get('genre').value;
    this.administratorRepository
      .updateSolicitudPerson(id, this.entity)
      .subscribe(
        (res) => {
          this.getEntityDetail(id);
          this.toastService.showToast(
            'Los datos se actualizaron correctamente',
            'success'
          );
        },
        (err) => {
          this.toastService.showToast(err || 'Ocurrió un error', 'danger');
        }
      );
    this.editMode = false;
    this.errorReniec = false;
    this.personVerified = false;
    this.errorGenre = false;
  }

  changeRouteListener() {
    this.route.params.subscribe((queryParams) => {
      this.correctNamePerson = '';
      this.correctNameEntity = '';

      this.personVerified = false;
      this.entityVerified = false;

      this.errorReniec = false;
      this.errorSunat = false;

      this.editMode = false;
      this.idEntity = queryParams['id'];
      this.getEntityDetail(this.idEntity);
    });
  }

  viewFile() {
    this.dialog.open(FileVisualizerComponent, {
      data: {
        base64String: this.entity.archivoSolicitud.archivo,
        filename: this.entity.archivoSolicitud.nombreRealArchivo,
        extension: this.entity.archivoSolicitud.getExtension(),
      },
    });
  }

  openVerificationModal() {
    this.dialog.open(ModalVerificationComponent, {
      data: {
        entity: this.entity,
        errorReniec: this.errorReniec,
        errorSunat: this.errorSunat,
        errorGenre: this.errorGenre,
      },
      width: '37.5rem',
    });
  }

  verifyBtnUpdateData() {
    return (
      this.editMode ||
      (this.personVerified && this.entityVerified) ||
      this.entity?.solicitudEntidad?.estadoSolicitud !==
        Const.ENT_ESTADO_PENDIENTE
    );
  }

  verifyBtnVerificationModal() {
    return (
      !this.personVerified ||
      this.entity?.solicitudEntidad?.estadoSolicitud !==
        Const.ENT_ESTADO_PENDIENTE
    );
  }
}
