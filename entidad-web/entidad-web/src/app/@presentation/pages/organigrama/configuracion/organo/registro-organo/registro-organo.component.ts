import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  OnInit,
  Output,
  SimpleChanges,
  ViewChild,
} from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { Country } from 'src/app/@data/model/country';
import { Organo } from 'src/app/@data/model/organo';
import { ParameterItem } from 'src/app/@data/model/parameterItem';
import { ReniecResponse } from 'src/app/@data/model/reniecResponse';
import { TypeDocument } from 'src/app/@data/model/typeDocument';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReniecRepository } from 'src/app/@domain/repository/reniec.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin, Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-registro-organo',
  templateUrl: './registro-organo.component.html',
  styleUrls: ['./registro-organo.component.scss'],
})
export class RegistroOrganoComponent implements OnInit, OnChanges {
  @Output() closeOrgano = new EventEmitter();
  @Output() updateOrganos = new EventEmitter();

  @Input() editMode = false;
  @Input() dataToEdit?: Organo = null;

  dataForEdit: Organo = null;

  @ViewChild('inputCountry') input;

  options: Country[];
  filteredOptions$: Observable<Country[]>;
  countries: Country[] = [];

  states: ParameterItem[];
  orgNaturalezaItems: ParameterItem[];
  typeDocuments: TypeDocument[];
  niveles: ParameterItem[];
  organos: Organo[] = [];
  organosFiltered: Organo[] = [];

  personFinded = false;

  organoForm = this.fb.group({
    estado: ['', [Validators.required]],
    nivel: ['', [Validators.required]],
    nombreOrgano: [
      '',
      [Validators.required, Validators.minLength(5), Validators.maxLength(200)],
    ],
    sigla: ['', [Validators.required]],
    naturaleza: ['', [Validators.required]],
    organoQueDepende: [''],
    tipoDocumento: ['', [Validators.required]],
    numeroDocumento: [
      '',
      [Validators.required, Validators.pattern(/^[0-9]{8}$/)],
    ],
    nombres: [
      { value: '', disabled: true },
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    apellidoPaterno: [
      { value: '', disabled: true },
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    apellidoMaterno: [
      { value: '', disabled: true },
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    puesto: ['', [Validators.required]],
    celular: [
      '',
      [
        Validators.required,
        Validators.pattern(/[0-9]$/),
        Validators.minLength(7),
        Validators.maxLength(9),
      ],
    ],
    correoLaboral: [
      '',
      [
        Validators.email,
        Validators.minLength(8),
        Validators.maxLength(50),
        Validators.required,
      ],
    ],
    pais: [{ value: '', disabled: false }],
    paisObject: [''],
  });

  constructor(
    public parameterRepository: ParameterRepository,
    public administratorRepository: AdministratorRepository,
    public organoRepository: OrganoRepository,
    public organigramaRepository: OrganigramaRepository,
    public reniecRepository: ReniecRepository,
    private toastService: ToastService,
    public fb: FormBuilder
  ) {}

  ngOnInit(): void {
    this.loadCombox();
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.dataToEdit?.currentValue && !changes.dataToEdit?.firstChange) {
      if (this.editMode) {
        const aux = this.organos.slice(0);
        this.organosFiltered = aux.filter(
          (o) => o.organigramaId !== this.dataToEdit.organigramaId
        );
        this.dataForEdit = changes.dataToEdit.currentValue;
        const organoToEdit = changes.dataToEdit.currentValue;
        this.setData(organoToEdit);
      }
    }
  }

  get f() {
    return this.organoForm.controls;
  }

  loadCombox() {
    const states = this.parameterRepository.getRegistryStates();
    const organoNaturaleza = this.parameterRepository.getOrganoNaturaleza();
    const typeDocuments = this.parameterRepository.getTypeDocuments();
    const niveles = this.parameterRepository.getNivelesOrgano();
    const countries = this.administratorRepository.getCountries();
    const organos = this.organoRepository.getOrganos(false);

    forkJoin([
      states,
      organoNaturaleza,
      typeDocuments,
      niveles,
      countries,
      organos,
    ]).subscribe(
      (results) => {
        this.states = results[0];
        this.orgNaturalezaItems = results[1];
        this.typeDocuments = results[2];
        this.niveles = results[3];
        this.niveles = this.niveles.filter((nivel) => nivel.parametroId !== 80);
        this.countries = results[4];
        this.organos = results[5];
        this.options = this.countries;
        this.filteredOptions$ = of(this.options);

        if (this.editMode === true) {
          const aux = this.organos.slice(0);
          this.organosFiltered = aux.filter(
            (o) => o.organigramaId !== this.dataToEdit.organigramaId
          );
          this.setData(this.dataToEdit);
          this.f.estado.enable();
        } else {
          this.organosFiltered = this.organos.slice(0);
          this.f.estado.setValue('1');
          this.f.estado.disable();
        }

        if (this.organigramaRepository.getOrganoStored()) {
          this.editMode = true;
          this.dataToEdit = this.organigramaRepository.getOrganoStored();
          this.setData(this.organigramaRepository.getOrganoStored());
        }
      },
      (err) => {}
    );
  }

  changeTypeDocument() {
    switch (this.f.tipoDocumento.value) {
      case 1:
        this.organoForm.get('pais').clearValidators();
        this.organoForm.get('pais').updateValueAndValidity();
        this.organoForm
          .get('apellidoMaterno')
          .setValidators(Validators.required);
        this.organoForm.get('apellidoMaterno').updateValueAndValidity();
        this.organoForm.get('numeroDocumento').clearValidators();
        this.organoForm
          .get('numeroDocumento')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[0-9]{8}$/),
          ]);
        this.organoForm.get('numeroDocumento').updateValueAndValidity();
        break;
      case 4:
        this.organoForm.get('pais').setValidators(Validators.required);
        this.organoForm.get('pais').enable();
        this.organoForm.get('pais').updateValueAndValidity();
        this.organoForm.get('apellidoMaterno').clearValidators();
        this.organoForm.get('apellidoMaterno').updateValueAndValidity();
        this.organoForm.get('numeroDocumento').clearValidators();
        this.organoForm
          .get('numeroDocumento')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[a-zA-Z0-9]{9,12}$/),
          ]);
        this.organoForm.get('numeroDocumento').updateValueAndValidity();
        setTimeout(() => {
          if (<HTMLInputElement>document.getElementById('pais'))
            (<HTMLInputElement>(
              document.getElementById('pais')
            )).disabled = false;
        }, 0);
        break;
      default:
        break;
    }
  }

  searchReniec() {
    if (this.organoForm.get('numeroDocumento').valid) {
      const doc = this.organoForm.get('numeroDocumento').value;
      this.reniecRepository.getPersonInfo(doc).subscribe(
        (res: any) => {
          this.personFinded = true;
          if (res === true) {
            this.toastService.showToast(
              'La persona no se encuentra registrada, ingrese los campos manualmente',
              'warning'
            );
            this.organoForm.get('apellidoPaterno').enable();
            this.organoForm.get('apellidoMaterno').enable();
            this.organoForm.get('nombres').enable();
            this.organoForm.get('tipoDocumento').disable();
          } else {
            const persona: ReniecResponse = res;
            this.organoForm.get('nombres').setValue(persona.nombres);
            this.organoForm
              .get('apellidoPaterno')
              .setValue(persona.apellidoPaterno);
            this.organoForm
              .get('apellidoMaterno')
              .setValue(persona.apellidoMaterno);
            this.organoForm.get('tipoDocumento').disable();
            if (persona.paisId) {
              this.organoForm
                .get('paisObject')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                );
              this.organoForm
                .get('pais')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                    .nombrePais
                );
              this.organoForm
                .get('pais')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                    .nombrePais
                );
              this.organoForm.get('pais').disable();
              if (<HTMLInputElement>document.getElementById('pais'))
                (<HTMLInputElement>(
                  document.getElementById('pais')
                )).disabled = true;
              if (this.input) {
                this.input.nativeElement.blur();
              }
              this.organoForm.get('pais').markAsUntouched();
            }
          }
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    } else {
      this.toastService.showToast(
        'Ingrese correctamente el número de documento',
        'danger'
      );
      this.organoForm.get('numeroDocumento').markAsTouched();
    }
  }

  clearFields(flagLimpiarNroDoc?: boolean) {
    if (flagLimpiarNroDoc) {
      this.organoForm.get('numeroDocumento').setValue('');
    }
    this.personFinded = false;
    if (<HTMLInputElement>document.getElementById('pais'))
      (<HTMLInputElement>document.getElementById('pais')).disabled = false;
    this.f.tipoDocumento.enable();
    this.organoForm.get('nombres').setValue('');
    this.organoForm.get('apellidoPaterno').setValue('');
    this.organoForm.get('apellidoMaterno').setValue('');
    this.organoForm.get('pais').setValue('');
    this.organoForm.get('puesto').setValue('');
    this.organoForm.get('celular').setValue('');
    this.organoForm.get('correoLaboral').setValue('');
    this.organoForm.get('pais').enable();
    this.organoForm.get('nombres').disable();
    this.organoForm.get('apellidoPaterno').disable();
    this.organoForm.get('apellidoMaterno').disable();
    if (this.input) {
      this.input.nativeElement.blur();
    }
    this.organoForm.get('pais').markAsUntouched();
    this.dataToEdit.correoId = null;
    this.dataToEdit.telefonoId = null;
  }

  setData(organoToEdit: Organo) {
    this.personFinded = true;
    setTimeout(() => {
      this.organoForm.patchValue({
        estado: organoToEdit?.estadoRegistro,
        nivel: organoToEdit.nivel,
        nombreOrgano: organoToEdit.descripcion,
        sigla: organoToEdit.sigla,
        naturaleza: organoToEdit.naturalezaOrgano,
        organoQueDepende: organoToEdit.padreOrganigramaId,
        tipoDocumento: organoToEdit.tipoDocumento,
        numeroDocumento: organoToEdit.nroDocumento,
        nombres: organoToEdit.nombres,
        apellidoPaterno: organoToEdit.apellidoPaterno,
        apellidoMaterno: organoToEdit.apellidoMaterno,
        puesto: organoToEdit.puesto,
        celular: organoToEdit.telefono,
        correoLaboral: organoToEdit.correo,
        pais: organoToEdit.nombrePais,
        paisObject:
          this.countries?.filter(
            (c) => c.nombrePais === organoToEdit.nombrePais
          )[0]?.paisId || null,
      });
      this.f.estado.enable();
      this.f.pais.disable();
      this.f.tipoDocumento.disable();
      this.changeTypeDocument();
      if (this.input) {
        this.input.nativeElement.blur();
      }
      setTimeout(() => {
        if (<HTMLInputElement>document.getElementById('pais'))
          (<HTMLInputElement>document.getElementById('pais')).disabled = true;
      }, 0);
    }, 0);
  }

  saveOrgano() {
    this.organoRepository
      .createOrUpdateOrgano(
        this.organoForm.getRawValue(),
        this.editMode,
        this.dataToEdit?.organigramaId,
        this.dataToEdit || null
      )
      .subscribe(
        (res) => {
          this.editMode === false
            ? this.toastService.showToast(
                'El órgano ha sido creado correctamente',
                'success'
              )
            : this.toastService.showToast(
                'Se editaron correctamente los datos',
                'success'
              );
          this.organoForm.reset();
          this.editMode = false;
          this.dataToEdit = null;
          this.personFinded = false;
          this.organoForm.get('tipoDocumento').enable();
          this.closeOrgano.emit(0);
          this.updateOrganos.emit();
          this.f.estado.setValue('1');
          this.f.estado.disable();
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
  }

  cancelItem() {
    this.organigramaRepository.setOrganoOrUnidadFromChart(null);
    this.closeOrgano.emit(1);
    this.dataToEdit = null;
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
    this.organoForm
      .get('paisObject')
      .setValue(this.countries?.filter((o) => o.nombrePais === item)[0]);
  }

  verifyCountry() {
    const actualValue = this.organoForm.get('pais').value;
    if (!this.countries?.filter((o) => o.nombrePais === actualValue)[0]) {
      this.organoForm.get('pais').setErrors({ notfound: true });
      this.organoForm.get('paisObject').setValue('');
    } else {
      this.setCountry(actualValue);
    }
  }
}
