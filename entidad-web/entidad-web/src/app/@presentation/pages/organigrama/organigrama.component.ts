import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { PageEvent } from '@angular/material/paginator';
import { Router } from '@angular/router';
import {
  NbSortDirection,
  NbSortRequest,
  NbTreeGridDataSource,
  NbTreeGridDataSourceBuilder,
} from '@nebular/theme';
import { Organo } from 'src/app/@data/model/organo';
import { Persona } from 'src/app/@data/services/organigrama.service';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';
import { forkJoin } from 'rxjs';
import { debounceTime, map, startWith, tap } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-organigrama',
  templateUrl: './organigrama.component.html',
  styleUrls: ['./organigrama.component.scss'],
})
export class OrganigramaComponent implements OnInit {
  length = 100;
  pageSize = 10;
  pageSizeOptions: number[] = [5, 10, 25];
  organos: Organo[];
  unidadesOrganicas: any[];
  estados: any[];
  dataExport: ExportExcelModel;

  customColumn = 'id';
  defaultColumns = ['descripcion', 'nivel', 'sigla', 'naturaleza'];
  defaultColumnsNames = ['DESCRIPCIÓN', 'NIVEL', 'SIGLA', 'NATURALEZA'];
  customColumn2 = 'ACCIONES';
  customColumn3 = 'ESTADO';

  allColumns = [
    this.customColumn,
    ...this.defaultColumns,
    this.customColumn3,
    this.customColumn2,
  ];
  dataSource: NbTreeGridDataSource<any>;

  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

  filterForm: FormGroup = null;

  personas: Persona[] = [];
  organosPadre: any[] = [];
  data2: any[] = [];

  constructor(
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>,
    private organoRepository: OrganoRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private organigramaRepository: OrganigramaRepository,
    private parameterRepository: ParameterRepository,
    private exportExcelService: ExportExcelService,
    private fb: FormBuilder,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadCombox();
    this.initForm();
  }

  get f() {
    return this.filterForm.controls;
  }

  initForm() {
    this.filterForm = this.fb.group({
      personaId: '',
      organigramaId: '',
      unidadId: '',
      puesto: '',
      estado: '',
    });
    this.initSubscritionPersonaField();
    this.searchOrganigrama();
  }

  initSubscritionPersonaField() {
    this.filterForm
      .get('personaId')
      .valueChanges.pipe(
        startWith(''),
        debounceTime(500),
        map((value) => this._filterPersonas(value)),
        tap(() => this._validValue())
      )
      .subscribe();
  }

  showField(option) {
    return option?.nombreCompleto || '';
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
    const typeValue = typeof this.filterForm.get('personaId').value;
    if (this.personas.length === 0 && typeValue === 'string') {
      this.filterForm.get('personaId').setErrors({ notfound: true });
    }
    if (this.filterForm.get('personaId').value === '') {
      this.filterForm.get('personaId').reset();
    }
  }

  _blurEvent() {
    const typeValue = typeof this.filterForm.get('personaId').value;
    if (typeValue === 'string') {
      this.filterForm.get('personaId').setValue('');
    }
  }

  loadCombox() {
    const organos = this.organoRepository.getOrganos(false);
    const unidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      false
    );
    const getEstados = this.parameterRepository.getEstadoRegistro();
    forkJoin([organos, unidadesOrganicas, getEstados]).subscribe(
      (results) => {
        this.organos = results[0];
        this.unidadesOrganicas = results[1];
        this.estados = results[2];
      },
      (err) => {}
    );
  }

  searchOrganigrama() {
    const body = this.filterForm.getRawValue();
    body.personaId = body.personaId?.personaResponsableId || '';
    this.organigramaRepository.searchOrganigramas(body).subscribe(
      (res: any[]) => {
        this.organosPadre = res;
        this.length = this.organosPadre.length;
        const aux = this.organosPadre.slice(0);
        this.data2 = aux.splice(0, this.pageSize);
        const arrayToBuild = this.data2.map((r) => this.buildTree(r));
        this.dataSource = this.dataSourceBuilder.create(arrayToBuild);
      },
      (err) => {}
    );
  }

  cleanFilters() {
    this.initForm();
  }

  buildTree(data) {
    const formatted = {
      data: {
        id: data.idOrganigrama,
        descripcion: data.descripcion,
        nivel: data.desNivel,
        sigla: data.sigla,
        naturaleza: data.desNaturaleza,
        estado: data.estado,
        restOfData: data,
      },
      children:
        data.listaOrganigramaHijo?.length > 0
          ? data.listaOrganigramaHijo.map((hijo) => this.buildTree(hijo))
          : [],
    };
    return formatted;
  }

  updateSort(sortRequest: NbSortRequest): void {
    this.sortColumn = sortRequest.column;
    this.sortDirection = sortRequest.direction;
  }

  getSortDirection(column: string): NbSortDirection {
    if (this.sortColumn === column) {
      return this.sortDirection;
    }
    return NbSortDirection.NONE;
  }

  getShowOn(index: number) {
    const minWithForMultipleColumns = 400;
    const nextColumnStep = 100;
    return minWithForMultipleColumns + nextColumnStep * index;
  }

  getDataEvent(e: PageEvent) {
    this.pageSize = e.pageSize;
    const aux = this.organosPadre.slice(0);
    this.data2 = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    const arrayToBuild = this.data2.map((r) => this.buildTree(r));
    this.dataSource = this.dataSourceBuilder.create(arrayToBuild);
  }

  editAction(action) {
    this.organigramaRepository.setOrganoOrUnidadFromChart(
      action.data.restOfData
    );
    this.router.navigateByUrl('pages/organigrama/configuracion');
  }

  changeFormValue(type: number) {
    type === 1
      ? this.filterForm.get('organigramaId').patchValue('')
      : this.filterForm.get('unidadId').patchValue('');
  }

  getAllJson(data, array) {
    data.nombreCompleto = `${data.nombres} ${data.apellidoPaterno} ${
      data.apellidoMaterno || ''
    }`.trim();
    array.push(data);
    if (data.listaOrganigramaHijo && data.listaOrganigramaHijo.length > 0) {
      data.listaOrganigramaHijo.map((d) => this.getAllJson(d, array));
    }
    return array;
  }

  exportData() {
    const aux = this.organosPadre.slice(0);
    let arrayToBuild = [];
    aux.forEach((r) => this.getAllJson(r, arrayToBuild));
    this.dataExport.data = arrayToBuild;
    this.exportExcelService.exportExcel(this.dataExport);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Organos y unidades orgánicas';
    model.headers = [
      '#ID',
      'DESCRIPCIÓN',
      'NIVEL',
      'SIGLA',
      'NATURALEZA',
      'TIPO',
      'RESPONSABLE',
      'TIPO DOCUMENTO',
      'NUMERO DOCUMENTO',
      'CORREO RESPONSABLE',
      'TELEFONO RESPONSABLE',
      'PAIS',
      'ESTADO',
    ];
    model.keys = [
      'idOrganigrama',
      'descripcion',
      'desNivel',
      'sigla',
      'desNaturaleza',
      'desTipoOrgano',
      'nombreCompleto',
      'tipoDocumento',
      'numeroDocumento',
      'correo',
      'telefono',
      'nombrePais',
      'estado',
    ];
    return model;
  }
}
