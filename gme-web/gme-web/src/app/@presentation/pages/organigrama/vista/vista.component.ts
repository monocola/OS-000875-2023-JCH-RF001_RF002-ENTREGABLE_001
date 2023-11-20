import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Organo } from 'src/app/@data/model/organo';
import { Persona } from 'src/app/@data/services/organigrama.service';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';
import { debounceTime, map, startWith, tap } from 'rxjs/operators';

@Component({
  selector: 'gme-web-vista',
  templateUrl: './vista.component.html',
  styleUrls: ['./vista.component.scss']
})
export class VistaComponent implements OnInit {

  @ViewChild('refreshPage') refreshPage : ElementRef;

  showFilters = false;
  filterForm: FormGroup;
  index = 0;
  showLoading = true;
  personas: Persona[] = [];
  organos: Organo[] = [];
  unidadesOrganicas: any[] = [];
  organosFinded: any[] = [];
  organosFindedByPuesto: any[] = [];

  constructor(
    private fb: FormBuilder,
    private organoRepository: OrganoRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private organigramaRepository: OrganigramaRepository,
    private toastService: ToastService
  ) {}

  ngOnInit() {
    // alert("hhhhh")
    this.initForm();
    console.log("pruebaaaa")
    
    this.loadCombox();

  }

  get f() {
    return this.filterForm.controls;
  }

  initForm() {
    this.filterForm = this.fb.group({
      responsable: '',
      puesto: '',
    });
    this.initSubscritionPersonaField();
  }

  loadCombox() {
    const organos = this.organoRepository.getOrganos(true);
    const unidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      true
      );
    forkJoin([organos, unidadesOrganicas]).subscribe(
      (results) => {
        
        this.organos = results[0];
        this.unidadesOrganicas = results[1];

      },
      (err) => {}
    );
  }

 refresh() {
  window.location.reload();
 }

  cleanFilters() {
    this.initForm();
    this.organosFinded = [];
    this.organosFindedByPuesto = [];
  }

  searchOrganigrama() {
    if (this.filterForm.value.puesto) {
      this.findItemsByPuesto();
    } else {
      if (this.filterForm.value.responsable) {
        const responsable = this.filterForm.value.responsable;
        if (typeof responsable === 'object') {
          this.organosFindedByPuesto = [];
          this.findItemByPerson();
        } else {
          this.toastService.showToast(
            'Seleccione una persona válida',
            'danger'
          );
        }
      } else {
        this.toastService.showToast(
          'Debe ingresar algo en los filtros antes de realizar la búsqueda',
          'primary'
        );
      }
    }
  }

  clickFiltro() {
    this.showFilters = !this.showFilters;
  }

  findItemsByPuesto() {
    const coincidencias = [];
    const query = this.filterForm.getRawValue().puesto.toUpperCase();
    this.organos.forEach((organo) => {
      if (organo.puesto.toUpperCase().includes(query)) {
        coincidencias.push(organo);
      }
    });
    this.unidadesOrganicas.forEach((uo) => {
      if (uo.puesto.toUpperCase().includes(query)) {
        coincidencias.push(uo);
      }
    });
    this.organosFindedByPuesto = coincidencias.slice(0);
    if (this.organosFindedByPuesto.length === 0) {
      this.toastService.showToast('No se encontraron resultados', 'warning');
    }
  }

  findItemByPerson() {
    const arrayToRender = [];
    const idResponsable = this.filterForm.value.responsable
      .personaResponsableId;
    const unidadesAndOrganos = this.organos.concat(this.unidadesOrganicas);
    const organoToRender = unidadesAndOrganos.filter(
      (f) => f.personaResponsableId === idResponsable
    )[0];
    arrayToRender.push(organoToRender);
    unidadesAndOrganos.forEach((item) => {
      if (
        item.organigramaId === organoToRender.padreOrganigramaId ||
        item.padreOrganigramaId === organoToRender.organigramaId
      ) {
        if (item.organigramaId === organoToRender.padreOrganigramaId) {
          item.padreOrganigramaId = null;
        }
        arrayToRender.push(item);
      }
    });
    this.organosFinded = arrayToRender.slice(0);
  }

  keyupPuesto() {
    this.filterForm.patchValue({
      responsable: '',
    });
  }

  keyupResponsable() {
    this.filterForm.patchValue({
      puesto: '',
    });
  }

  // -------------------------------------------------------------------- //
  // ---- AutoComplete W/Server ---------------------------------- //
  // -------------------------------------------------------------------- //

  initSubscritionPersonaField() {
    this.filterForm
      .get('responsable')
      .valueChanges.pipe(
        startWith(''),
        debounceTime(500),
        map((value) => this._filterPersonas(value)),
        tap(() => this._validValue())
      )
      .subscribe();
  }

  _filterPersonas(value) {
    if (typeof value === 'string' && value.length > 0) {
      this.organigramaRepository
        .getPeopleAdmin(value)
        .subscribe((res) => (this.personas = res));
    } else {
      this.personas = [];
    }
  }

  private _validValue() {
    const typeValue = typeof this.filterForm.get('responsable').value;
    if (this.personas.length === 0 && typeValue === 'string') {
      this.filterForm.get('responsable').setErrors({ notfound: true });
    }
    if (this.filterForm.get('responsable').value === '') {
      this.filterForm.get('responsable').reset();
    }
  }

  _blurEvent() {
    const typeValue = typeof this.filterForm.get('responsable').value;
    if (typeValue === 'string') {
      this.filterForm.get('responsable').setValue('');
    }
  }

  showField(option) {
    return option?.nombreCompleto || '';
  }
}
