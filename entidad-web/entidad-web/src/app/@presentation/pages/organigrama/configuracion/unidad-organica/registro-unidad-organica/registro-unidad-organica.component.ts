import { FormBuilder, Validators } from '@angular/forms';
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
import { Country } from 'src/app/@data/model/country';
import { ParameterItem } from 'src/app/@data/model/parameterItem';
import { Organo } from 'src/app/@data/model/organo';
import { ReniecResponse } from 'src/app/@data/model/reniecResponse';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { TypeDocument } from 'src/app/@data/model/typeDocument';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReniecRepository } from 'src/app/@domain/repository/reniec.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin, Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-registro-unidad-organica',
  templateUrl: './registro-unidad-organica.component.html',
  styleUrls: ['./registro-unidad-organica.component.scss'],
})
export class RegistroUnidadOrganicaComponent implements OnInit, OnChanges {
  @Output() closeOrgano = new EventEmitter();
  @Output() updateUnidadesOrganicas = new EventEmitter();

  @Input() editMode = false;
  @Input() dataToEdit?: Organo = null;
  @Input() unidadesOrganicas: any[] = [];

  @ViewChild('inputCountry') input;

  options: Country[];
  filteredOptions$: Observable<Country[]>;
  countries: Country[];

  states: ParameterItem[];
  orgNaturalezaItems: ParameterItem[];
  typeDocuments: TypeDocument[];
  niveles: ParameterItem[];
  organos: Organo[] = [];
  unidadesOrganicasFiltered: any[] = [];

  personFinded = false;

  uniOrganForm = this.fb.group({
    estado: ['', [Validators.required]],
    nivel: ['', [Validators.required]],
    sigla: ['', [Validators.required]],
    nombreUnidadOrgano: [
      '',
      [Validators.required, Validators.minLength(5), Validators.maxLength(200)],
    ],
    undadOrganicaQueDepende: [''],
    tipoDocumento: ['', [Validators.required]],
    organoQueDepende: ['', Validators.required],
    numeroDocumento: [
      '',
      [Validators.required, Validators.pattern(/^[0-9]{8}$/)],
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
    nombres: [
      { value: '', disabled: true },
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    puesto: ['', [Validators.required]],
    correoLaboral: [
      '',
      [
        Validators.email,
        Validators.minLength(8),
        Validators.maxLength(50),
        Validators.required,
      ],
    ],
    celular: [
      '',
      [
        Validators.required,
        Validators.pattern(/[0-9]$/),
        Validators.minLength(7),
        Validators.maxLength(9),
      ],
    ],
    pais: [{ value: '', disabled: false }],
    paisObject: [''],
  });

  constructor(
    public parameterRepository: ParameterRepository,
    public administratorRepository: AdministratorRepository,
    public unidadOrganicaService: UnidadOrganicaRepository,
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
        const aux = this.unidadesOrganicas.slice(0);
        this.unidadesOrganicasFiltered = aux.filter(
          (uo) => uo.organigramaId !== this.dataToEdit.organigramaId
        );
      }
      const organoToEdit = changes.dataToEdit.currentValue;
      this.setData(organoToEdit);
    } else {
      this.unidadesOrganicasFiltered = changes.unidadesOrganicas?.currentValue;
    }
  }

  get f() {
    return this.uniOrganForm.controls;
  }

  getOrganosDependientes() {
    this.organoRepository.getOrganos(false).subscribe(
      (res) => {
        this.organos = res;
      },
      (err) => {
        this.toastService.showToast(
          'Ocurrió un error al traer los órganos',
          'danger'
        );
      }
    );
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
        this.countries = results[4];
        this.organos = results[5];

        this.options = this.countries;
        this.filteredOptions$ = of(this.options);

        this.niveles = this.niveles.filter((nivel) => nivel.parametroId === 80);

        this.f.nivel.setValue(80);
        this.f.nivel.disable();

        if (this.editMode === true) {
          const aux = this.unidadesOrganicas.slice(0);
          this.unidadesOrganicasFiltered = aux.filter(
            (uo) => uo.organigramaId !== this.dataToEdit.organigramaId
          );
          this.setData(this.dataToEdit);
          this.f.estado.enable();
        } else {
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

  changedHierarchy(type: number) {
    if (type === 0) {
      this.uniOrganForm
        .get('organoQueDepende')
        .setValidators(Validators.required);
      this.uniOrganForm.get('organoQueDepende').updateValueAndValidity();
      this.uniOrganForm.get('undadOrganicaQueDepende').patchValue('');
      this.uniOrganForm.get('undadOrganicaQueDepende').clearValidators();
      this.uniOrganForm.get('undadOrganicaQueDepende').updateValueAndValidity();
    } else {
      this.uniOrganForm
        .get('undadOrganicaQueDepende')
        .setValidators(Validators.required);
      this.uniOrganForm.get('undadOrganicaQueDepende').updateValueAndValidity();
      this.uniOrganForm.get('organoQueDepende').patchValue('');
      this.uniOrganForm.get('organoQueDepende').clearValidators();
      this.uniOrganForm.get('organoQueDepende').updateValueAndValidity();
    }
  }

  changeTypeDocument() {
    switch (this.f.tipoDocumento.value) {
      case 1:
        this.uniOrganForm.get('pais').clearValidators();
        this.uniOrganForm.get('pais').updateValueAndValidity();
        this.uniOrganForm
          .get('apellidoMaterno')
          .setValidators(Validators.required);
        this.uniOrganForm.get('apellidoMaterno').updateValueAndValidity();
        this.uniOrganForm.get('numeroDocumento').clearValidators();
        this.uniOrganForm
          .get('numeroDocumento')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[0-9]{8}$/),
          ]);
        this.uniOrganForm.get('numeroDocumento').updateValueAndValidity();
        break;
      case 4:
        this.uniOrganForm.get('pais').setValidators(Validators.required);
        this.uniOrganForm.get('pais').enable();
        this.uniOrganForm.get('pais').updateValueAndValidity();
        this.uniOrganForm.get('apellidoMaterno').clearValidators();
        this.uniOrganForm.get('apellidoMaterno').updateValueAndValidity();
        this.uniOrganForm.get('numeroDocumento').clearValidators();
        this.uniOrganForm
          .get('numeroDocumento')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[a-zA-Z0-9]{9,12}$/),
          ]);
        this.uniOrganForm.get('numeroDocumento').updateValueAndValidity();
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
    if (this.uniOrganForm.get('numeroDocumento').valid) {
      const doc = this.uniOrganForm.get('numeroDocumento').value;
      this.reniecRepository.getPersonInfo(doc).subscribe(
        (res: any) => {
          this.personFinded = true;
          if (res === true) {
            this.toastService.showToast(
              'La persona no se encuentra registrada, ingrese los campos manualmente',
              'warning'
            );
            this.uniOrganForm.get('apellidoPaterno').enable();
            this.uniOrganForm.get('apellidoMaterno').enable();
            this.uniOrganForm.get('nombres').enable();
            this.uniOrganForm.get('tipoDocumento').disable();
          } else {
            const persona: ReniecResponse = res;
            this.uniOrganForm.get('nombres').setValue(persona.nombres);
            this.uniOrganForm
              .get('apellidoPaterno')
              .setValue(persona.apellidoPaterno);
            this.uniOrganForm
              .get('apellidoMaterno')
              .setValue(persona.apellidoMaterno);
            this.uniOrganForm.get('tipoDocumento').disable();
            if (persona.paisId) {
              this.uniOrganForm
                .get('paisObject')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                );
              this.uniOrganForm
                .get('pais')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                    .nombrePais
                );
              this.uniOrganForm
                .get('pais')
                .setValue(
                  this.countries.filter((c) => c.paisId === persona.paisId)[0]
                    .nombrePais
                );
              this.uniOrganForm.get('pais').disable();
              if (<HTMLInputElement>document.getElementById('pais'))
                (<HTMLInputElement>(
                  document.getElementById('pais')
                )).disabled = true;

              if (this.input) {
                this.input.nativeElement.blur();
              }
              this.uniOrganForm.get('pais').markAsUntouched();
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
      this.uniOrganForm.get('numeroDocumento').markAsTouched();
    }
  }

  clearFields(flagLimpiarNroDoc?: boolean) {
    if (flagLimpiarNroDoc) {
      this.uniOrganForm.get('numeroDocumento').setValue('');
    }
    this.personFinded = false;
    if (<HTMLInputElement>document.getElementById('pais'))
      (<HTMLInputElement>document.getElementById('pais')).disabled = false;
    this.f.tipoDocumento.enable();
    this.uniOrganForm.get('nombres').setValue('');
    this.uniOrganForm.get('apellidoPaterno').setValue('');
    this.uniOrganForm.get('apellidoMaterno').setValue('');
    this.uniOrganForm.get('pais').setValue('');
    this.uniOrganForm.get('puesto').setValue('');
    this.uniOrganForm.get('celular').setValue('');
    this.uniOrganForm.get('correoLaboral').setValue('');
    this.uniOrganForm.get('pais').enable();
    this.uniOrganForm.get('nombres').disable();
    this.uniOrganForm.get('apellidoPaterno').disable();
    this.uniOrganForm.get('apellidoMaterno').disable();
    // NOSONAR
    if (this.input) {
      this.input.nativeElement.blur();
    }
    this.uniOrganForm.get('pais').markAsUntouched();
    this.dataToEdit.correoId = null;
    this.dataToEdit.telefonoId = null;
  }

  setData(organoToEdit: any) {
    this.personFinded = true;
    this.uniOrganForm.patchValue({
      estado: organoToEdit?.estadoRegistro,
      nivel: organoToEdit.nivel,
      nombreUnidadOrgano: organoToEdit.unidadOrganica,
      sigla: organoToEdit.sigla,
      naturaleza: organoToEdit.naturalezaOrgano,
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
    // NOSONAR
    if (this.input) {
      this.input.nativeElement.blur();
    }
    setTimeout(() => {
      const paisDOM = <HTMLInputElement>document.getElementById('pais');
      if (paisDOM) {
        paisDOM.disabled = true;
      }
      const id = organoToEdit.padreOrganigramaId;
      const unidadOrganica = this.unidadesOrganicas.filter(
        (uo) => uo.organigramaId === id
      )[0];
      if (unidadOrganica) {
        this.changedHierarchy(1);
        this.uniOrganForm.patchValue({
          undadOrganicaQueDepende: id,
        });
      } else {
        this.changedHierarchy(0);
        this.uniOrganForm.patchValue({
          organoQueDepende: id,
        });
      }
    }, 0);
  }

  saveOrgano() {
    this.unidadOrganicaService
      .createOrUpdateUnidad(
        this.uniOrganForm.getRawValue(),
        this.editMode,
        this.dataToEdit?.organigramaId,
        this.dataToEdit || null
      )
      .subscribe(
        (res) => {
          this.editMode === false
            ? this.toastService.showToast(
                'La unidad orgánica ha sido creado correctamente',
                'success'
              )
            : this.toastService.showToast(
                'Se editaron correctamente los datos',
                'success'
              );
          this.uniOrganForm.reset();
          this.editMode = false;
          this.dataToEdit = null;
          this.personFinded = false;
          this.uniOrganForm.get('tipoDocumento').enable();
          this.updateUnidadesOrganicas.emit();
          this.closeOrgano.emit(0);
          this.f.estado.setValue('1');
          this.f.estado.disable();
          this.getOrganosDependientes();
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
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
    this.uniOrganForm
      .get('paisObject')
      .setValue(this.countries?.filter((o) => o.nombrePais === item)[0]);
  }

  verifyCountry() {
    const actualValue = this.uniOrganForm.get('pais').value;
    if (!this.countries?.filter((o) => o.nombrePais === actualValue)[0]) {
      this.uniOrganForm.get('pais').setErrors({ notfound: true });
      this.uniOrganForm.get('paisObject').setValue('');
    } else {
      this.setCountry(actualValue);
    }
  }
}
