import {
  AfterViewInit,
  Component,
  OnDestroy,
  OnInit,
  ViewChild,
} from '@angular/core';
import { AbstractControl, FormBuilder, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { Country } from 'src/app/@data/model/country';
import { Genre } from 'src/app/@data/model/genre';
import { Occupation } from 'src/app/@data/model/occupation';
import { TypeDocument } from 'src/app/@data/model/typeDocument';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { SunatRepository } from 'src/app/@domain/repository/sunat.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { forkJoin, Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { ModalEmailConfirmComponent } from './modal-email-confirm/modal-email-confirm.component';
import { ParameterItem } from 'src/app/@data/model/parameterItem';
import { base64toFile, getBase64 } from 'src/app/utils/converterFile';
import { EntityRequest } from 'src/app/@data/model/entityRequest';
import moment from 'moment';
import { getHash } from 'src/app/utils/general';

declare var $: any;

@Component({
  selector: 'serv-talento-administrator',
  templateUrl: './administrator.component.html',
  styleUrls: ['./administrator.component.scss'],
})
export class AdministratorComponent
  implements OnInit, OnDestroy, AfterViewInit {
  options: Country[];
  filteredOptions$: Observable<Country[]>;

  optionsOccupation: Occupation[];
  filteredOptionsOccupation$: Observable<Occupation[]>;

  typeDocuments: TypeDocument[];
  typeGenres: Genre[];
  governmentLevels: ParameterItem[];
  governmentSector: ParameterItem[];
  countries: Country[];
  occupations: Occupation[];

  document: File = null;
  documentBase64: string = '';
  emailValid: boolean = false;

  dataReceived: EntityRequest = null;
  updateMode = false;
  flagFileChanged = false;

  maxDate = moment().subtract(18, 'years').toDate();
  minDate = moment().subtract(100, 'years').toDate();

  messageTooltipDoc =
    'Tiene que subir un documento que sustente la pertenencia a la entidad';

  adminForm = this.fb.group(
    {
      typeDocument: ['', [Validators.required]],
      numberDocument: [
        '',
        [Validators.required, Validators.pattern(/^[0-9]{8}$/)],
      ],
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
          Validators.email,
          Validators.minLength(2),
          Validators.maxLength(30),
        ],
      ],
      email: [
        '',
        [
          Validators.required,
          Validators.email,
          Validators.minLength(10),
          Validators.maxLength(50),
        ],
      ],
      alternativeEmail: [
        '',
        [Validators.email, Validators.minLength(8), Validators.maxLength(50)],
      ],
      phone: [
        '',
        [
          Validators.required,
          Validators.pattern(/[0-9]$/),
          Validators.minLength(7),
          Validators.maxLength(9),
        ],
      ],
      alternatePhone: [
        '',
        [
          Validators.pattern(/[0-9]$/),
          Validators.minLength(7),
          Validators.maxLength(9),
        ],
      ],
      birthday: ['', [Validators.required]],
      genre: ['', [Validators.required]],
      occupation: ['', [Validators.required]],
      occupationObject: ['', [Validators.required]],
      ruc: [
        '',
        [Validators.required, Validators.pattern(/^20[a-zA-Z0-9]{9}$/)],
      ],
      businessName: [
        { value: '', disabled: true },
        [
          Validators.required,
          Validators.minLength(5),
          Validators.maxLength(200),
        ],
      ],
      governmentSector: ['', [Validators.required]],
      governmentLevel: ['', [Validators.required]],
      country: [''],
      countryObject: [''],
      rucValidated: 0,
      inputCaptcha: ['', [Validators.required, Validators.minLength(5)]],
      ubigeoId: '',
      direction: '',
    },
    {
      validators: [this.duplicateEmailPrevent, this.duplicatePhonePrevent],
    }
  );

  @ViewChild('inputCountry') input;
  @ViewChild('inputOccupation') inputOccupation;

  constructor(
    private sunatRepository: SunatRepository,
    private parameterRepository: ParameterRepository,
    private authenticationService: AuthenticationRepository,
    private administratorRepository: AdministratorRepository,
    private activateRoute: ActivatedRoute,
    public fb: FormBuilder,
    private toast: ToastService,
    public dialog: MatDialog,
    private router: Router,
    private toastService: ToastService
  ) {}

  ngAfterViewInit(): void {
    this.inputOccupation.nativeElement.blur();
    this.adminForm.get('occupation').markAsUntouched();
    setTimeout(() => {
      this.inicializarCaptcha();
    }, 1000);
  }

  ngOnDestroy(): void {
    this.authenticationService.clearUser();
  }

  ngOnInit(): void {
    sessionStorage.clear();
    sessionStorage.clear();
    this.authenticationService.clearUser();
    this.authenticationService.generatePublicToken().subscribe((res) => {
      this.loadCombox();
    });
  }

  verifyCode(code, id) {
    this.administratorRepository.verifyCodeObservation(code, id).subscribe(
      (res) => {
        if (res === true) {
          this.updateMode = true;
          this.updateForms(id);
        }
      },
      (err) => {
        this.router.navigateByUrl('/auth');
        this.toast.showToast(err, 'danger');
      }
    );
  }

  updateForms(id) {
    this.administratorRepository.getSolicitudById(id).subscribe(
      (res: EntityRequest) => {
        this.dataReceived = res;
        this.adminForm.patchValue({
          typeDocument: res.personaNatural.tipoDocumento,
          numberDocument: res.personaNatural.numeroDocumento,
          name: res.personaNatural.nombres,
          fatherName: res.personaNatural.apellidoPaterno,
          motherName: res.personaNatural.apellidoMaterno,
          email: res.personaNatural.correoPrincipal,
          alternativeEmail: res.personaNatural.correoSecundario || '',
          phone: res.personaNatural.celularPrincipal,
          alternatePhone: res.personaNatural.celularSecundario,
          birthday: moment(
            `${res.personaNatural.fechaNacimiento}`,
            'DD/MM/YYYY'
          ).toDate(),
          genre: res.personaNatural.sexo,
          occupation: res.personaNatural.descripcionCargo,
          occupationObject: this.occupations.filter(
            (e) => e.cargoId === res.personaNatural.cargoId
          )[0],
          ruc: res.personaJuridica.numeroDocumento,
          businessName: res.personaJuridica.razonSocial,
          governmentSector: res.solicitudEntidad.sector,
          governmentLevel: res.solicitudEntidad.nivelGobierno,
          country: res.personaNatural.paisId
            ? this.countries.filter(
                (e) => e.paisId === res.personaNatural.paisId
              )[0].nombrePais
            : null,
          countryObject: res.personaNatural.paisId
            ? this.countries.filter(
                (e) => e.paisId === res.personaNatural.paisId
              )[0]
            : null,
          rucValidated: res.personaJuridica.validar,
          inputCaptcha: '',
          ubigeoId: res.personaJuridica.ubigeoId,
          direction: res.personaJuridica.direccionCompleta,
        });
        this.documentBase64 = res.archivoSolicitud.archivo;
        this.document = base64toFile(
          this.documentBase64,
          res.archivoSolicitud.nombreRealArchivo,
          res.archivoSolicitud.getExtension()
        );
        this.inputOccupation.nativeElement.blur();
        this.adminForm.get('occupation').markAsUntouched();
        this.changeTypeDocument();
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  duplicatePhonePrevent(
    control: AbstractControl
  ): { duplicatedPhone: boolean } {
    if (
      control.get(['phone'])?.value === control.get(['alternatePhone'])?.value
    ) {
      if (control.get(['alternatePhone'])?.value !== '') {
        return { duplicatedPhone: true };
      }
    }
  }

  duplicateEmailPrevent(
    control: AbstractControl
  ): { duplicatedEmail: boolean } {
    if (
      control.get(['email'])?.value?.toLowerCase() ===
      control.get(['alternativeEmail'])?.value?.toLowerCase()
    ) {
      if (control.get(['alternativeEmail'])?.value?.toLowerCase() !== '') {
        return { duplicatedEmail: true };
      }
    }
  }

  get f() {
    return this.adminForm.controls;
  }

  loadCombox() {
    const typeDocuments = this.parameterRepository.getTypeDocuments();
    const typeGenres = this.parameterRepository.getGenres();
    const govermentLevels = this.parameterRepository.getGovermentLevel();
    const governmentSector = this.parameterRepository.getGovernmentSector();
    const countries = this.administratorRepository.getCountries();
    const occupations = this.administratorRepository.getOccupations();

    forkJoin([
      typeDocuments,
      typeGenres,
      govermentLevels,
      governmentSector,
      countries,
      occupations,
    ]).subscribe(
      (results) => {
        this.typeDocuments = results[0];
        this.typeGenres = results[1];
        this.governmentLevels = results[2];
        this.governmentSector = results[3];
        this.countries = results[4];
        this.occupations = results[5];

        this.options = this.countries;
        this.filteredOptions$ = of(this.options);

        this.optionsOccupation = this.occupations;
        this.filteredOptionsOccupation$ = of(this.occupations);

        this.activateRoute.params.subscribe((e) => {
          if (e.codeSecurity) {
            this.verifyCode(e.codeSecurity, e.idRequest);
          }
        });
      },
      (err) => {}
    );
  }

  searchSunat() {
    if (this.f.ruc.valid) {
      this.sunatRepository.getSunatInfo(this.f.ruc.value).subscribe(
        (res) => {
          this.f.businessName.disable();
          this.f.businessName.setValue(res.personaJuridica.razonSocial);
          this.f.ubigeoId.setValue(res.direcciones[0].ubigeoId || null);
          this.f.direction.setValue(
            res.direcciones[0].direccionCompleta || null
          );
          this.f.rucValidated.setValue(1);
        },
        (err) => {
          this.toast.showToast(
            'Servicio de Sunat no disponible. Ingrese los datos de la entidad manualmente',
            'danger'
          );
          this.f.businessName.enable();
          this.f.businessName.setValue('');
          this.f.ubigeoId.setValue('');
          this.f.direction.setValue('');
          this.f.rucValidated.setValue(0);
        }
      );
    } else {
      this.toast.showToast(
        'El numero SUNAT debe comenzar con 20 y tener 11 dígitos',
        'danger'
      );
    }
  }

  clearSunat() {
    this.adminForm.get('businessName').setValue('');
    this.adminForm.get('rucValidated').setValue(0);
  }

  registerOrUpdateAdmin() {
    if (this.disabledCreateAdmin()) {
      if (this.document && this.documentBase64) {
        const valorCaptcha = $('#captchaRegisterAdmin').realperson('getHash');
        if (
          valorCaptcha === getHash(this.adminForm.get('inputCaptcha').value)
        ) {
          if (this.updateMode) {
            this.administratorRepository
              .registerOrUpdateAdminRequest(
                this.adminForm.getRawValue(),
                this.document,
                this.documentBase64,
                1,
                this.flagFileChanged,
                this.dataReceived
              )
              .subscribe(
                (res) => this.openDialog(),
                (err) => this.toast.showToast(err.message, 'danger')
              );
          } else {
            this.validAddRequest();
          }
        } else {
          this.adminForm.get('inputCaptcha').setValue('');
          this.toast.showToast('El valor del captcha es incorrecto', 'danger');
        }
      } else {
        this.toast.showToast(
          'Debe incluir el documento sustentatorio',
          'danger'
        );
      }
    } else {
      if (!this.document) {
        this.toast.showToast(
          'Debe incluir el documento sustentatorio',
          'danger'
        );
      } else {
        this.toast.showToast(
          'Debe completar los campos que son obligatorios',
          'danger'
        );
      }
    }
  }

  async validAddRequest() {
    if (this.validEmail) {
      this.validDuplicateRequest(
        this.adminForm.getRawValue().ruc,
        this.adminForm.getRawValue().numberDocument
      )
        .then((duplicate: boolean) => {
          if (!duplicate) {
            this.validUserData().then((acepto: boolean) => {
              if (acepto) {
                this.administratorRepository
                  .registerOrUpdateAdminRequest(
                    this.adminForm.getRawValue(),
                    this.document,
                    this.documentBase64,
                    0,
                    true
                  )
                  .subscribe(
                    (res: any) => {
                      this.openDialog();
                    },
                    (err) => this.toast.showToast(err.message, 'danger')
                  );
              }
            });
          } else {
            this.openDuplicateDialog(this.adminForm.getRawValue());
          }
        })
        .catch((error: any) => {
          this.toast.showToast(JSON.stringify(error), 'danger');
        });
    } else {
      this.toast.showToast('Debe validar el correo electrónico', 'danger');
    }
  }

  async validUserData(): Promise<boolean> {
    return await new Promise((resolve, reject) => {
      const dialogRef = this.dialog.open(ModalConfirmationComponent, {
        data: {
          title: 'Veracidad de datos',
          bodyText: `Los datos consignados en la plataforma de Talento Perú son correctos, veraces y tienen carácter de declaración jurada.`,
          rutaImagen: 'assets/images/icons/declaración.svg',
          textCancel: 'Rechazar',
        },
      });

      dialogRef.afterClosed().subscribe((result: boolean) => {
        resolve(result === true);
      });
    });
  }

  async validDuplicateRequest(ruc: number, doc: number): Promise<boolean> {
    return await new Promise((resolve, reject) => {
      this.administratorRepository
        .validDuplicateRequest(ruc, doc)
        .toPromise()
        .then((res: any) => {
          resolve(res);
        })
        .catch((error: any) => {
          reject(error);
        });
    });
  }

  openDuplicateDialog(form: any) {
    this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Solicitud ya registrada',
        bodyText: `El N° de RUC: ${form.ruc} ya tiene una solicitud pendiente de aprobación.`,
        visibleCancel: false,
        rutaImagen: 'assets/images/icons/alert.png',
      },
    });
  }

  emailChanged(event: any) {
    this.emailValid = false;
  }

  validEmailDialog() {
    if (this.adminForm.controls['email'].invalid) {
      this.toast.showToast(
        'Debe completar el campo de correo electrónico',
        'danger'
      );
      return;
    }

    if (this.emailValid === false) {
      this.validEmail()
        .then((res: boolean) => {
          this.emailValid = res;
        })
        .catch((error: any) => {
          this.emailValid = true;
          this.toast.showToast(
            'Servicio de validacion de correo no disponible.',
            'danger'
          );
        });
    }
  }

  async validEmail(): Promise<boolean> {
    return await new Promise((resolve, reject) => {
      this.administratorRepository
        .verifyEmail(this.adminForm.get('email').value)
        .toPromise()
        .then((code: any) => {
          const dialogRef = this.dialog.open(ModalEmailConfirmComponent, {
            data: {
              correo: this.adminForm.get('email').value,
              code: code,
            },
          });

          dialogRef.afterClosed().subscribe((result: any) => {
            if (result && result === true) {
              resolve(result);
            } else {
              resolve(false);
            }
          });
        })
        .catch((error: any) => {
          reject(error);
        });
    });
  }

  openDialog() {
    const dialogRef = this.dialog.open(ModalConfirmacionComponent, {
      width: '34.375rem',
      data: {
        correo: this.adminForm.get('email').value,
      },
    });

    dialogRef.afterClosed().subscribe((result) => {
      this.router.navigateByUrl('/auth/');
    });
  }

  changeTypeDocument() {
    switch (this.f.typeDocument.value) {
      case 1:
        this.adminForm.get('country').clearValidators();
        this.adminForm.get('motherName').setValidators(Validators.required);
        this.adminForm.get('country').updateValueAndValidity();
        this.adminForm.get('motherName').updateValueAndValidity();
        this.adminForm.get('numberDocument').clearValidators();
        this.adminForm
          .get('numberDocument')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[0-9]{8}$/),
          ]);
        this.adminForm.get('numberDocument').updateValueAndValidity();
        break;
      case 4:
        this.adminForm.get('country').setValidators(Validators.required);
        this.adminForm.get('motherName').clearValidators();
        this.adminForm.get('country').updateValueAndValidity();
        this.adminForm.get('motherName').updateValueAndValidity();
        this.adminForm.get('numberDocument').clearValidators();
        this.adminForm
          .get('numberDocument')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[a-zA-Z0-9]{9,12}$/),
          ]);
        this.adminForm.get('numberDocument').updateValueAndValidity();
        break;
      default:
        break;
    }
  }

  fileChange(event) {
    this.document = null;
    this.documentBase64 = null;
    if (event.target.files[0].size / (1024 * 1024) > 1) {
      this.toast.showToast('El archivo excede el tamaño de 1MB', 'danger');
    } else {
      const extension = event.target.files[0].name.split('.')[
        event.target.files[0].name.split('.').length - 1
      ];
      const extensionesPermitidas = ['pdf', 'jpg', 'png', 'jpeg'];
      if (extensionesPermitidas.includes(extension.toLowerCase())) {
        this.document = event.target.files[0] as File;
        getBase64(this.document).then((data: string) => {
          this.documentBase64 = data;
          this.flagFileChanged = true;
        });
      } else {
        this.document = null;
        this.documentBase64 = null;
        this.flagFileChanged = true;
        this.toast.showToast(
          'Solo se permiten archivos PDF, JPG, JPEG y PNG',
          'danger'
        );
      }
    }
  }

  disabledCreateAdmin() {
    return (
      this.adminForm.valid && this.adminForm.get('businessName').value !== ''
    );
  }

  // ---------------------------------------------- //
  // --- Captcha ---- //
  // ----------------------------------------------- //

  inicializarCaptcha(): void {
    $('#captchaRegisterAdmin').realperson({ length: 5 });

    $('#captchaRegisterAdmin').click(() => {
      const disable = $(this).text() === 'Disable';
      $(this).text(disable ? 'Enable' : 'Disable');
      $('#captchaRegisterAdmin').realperson(disable ? 'disable' : 'enable');
    });

    $('#captchaRegisterAdmin').click(() => {
      const destroy = $(this).text() === 'Remove';
      $(this).text(destroy ? 'Re-attach' : 'Remove');
      $('#captchaRegisterAdmin').realperson(destroy ? 'destroy' : {});
    });
  }

  // ---------------------------------------------- //
  // --- Autocomplete for countries ---- //
  // ----------------------------------------------- //

  private filter(value: string): Country[] {
    const filterValue = value?.toLowerCase();
    return this.options?.filter((optionValue) =>
      optionValue.nombrePais.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptions(value: string): Observable<Country[]> {
    return of(value).pipe(map((filterString) => this.filter(filterString)));
  }

  onChange() {
    this.verifyCountry();
    this.filteredOptions$ = this.getFilteredOptions(
      this.input.nativeElement.value
    );
  }

  onSelectionChange($event) {
    this.setCountry($event);
    this.filteredOptions$ = this.getFilteredOptions($event);
  }

  setCountry(item: string) {
    this.adminForm
      .get('countryObject')
      .setValue(this.countries?.filter((o) => o.nombrePais === item)[0]);
  }

  verifyCountry() {
    const actualValue = this.adminForm.get('country').value;
    if (!this.countries?.filter((o) => o.nombrePais === actualValue)[0]) {
      this.adminForm.get('country').setErrors({ notfound: true });
      this.adminForm.get('countryObject').setValue('');
    } else {
      this.setCountry(actualValue);
    }
  }

  // ---------------------------------------------- //
  // --- Autocomplete for Occupations ---- //
  // ----------------------------------------------- //

  private filterOccupation(value: string): Occupation[] {
    const filterValue = value?.toLowerCase();
    return this.optionsOccupation?.filter((optionValue) =>
      optionValue.descripcion.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsOccupation(value: string): Observable<Occupation[]> {
    return of(value).pipe(
      map((filterString) => this.filterOccupation(filterString))
    );
  }

  onChangeOccupation() {
    this.verifyOccupation();
    this.filteredOptionsOccupation$ = this.getFilteredOptionsOccupation(
      this.inputOccupation.nativeElement.value
    );
  }

  onSelectionChangeOccupation($event) {
    this.setOccupation($event);
    this.filteredOptionsOccupation$ = this.getFilteredOptionsOccupation($event);
  }

  setOccupation(item: string) {
    this.adminForm
      .get('occupationObject')
      .setValue(this.occupations?.filter((o) => o.descripcion === item)[0]);
  }

  verifyOccupation() {
    const actualValue = this.adminForm.get('occupation').value;
    if (!this.occupations?.filter((o) => o.descripcion === actualValue)[0]) {
      this.adminForm.get('occupation').setErrors({ notfound: true });
      this.adminForm.get('occupationObject').setValue('');
    } else {
      this.setOccupation(actualValue);
    }
  }
}
