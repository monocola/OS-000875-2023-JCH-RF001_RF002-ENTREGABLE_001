
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { Convocatoria, ListaConocimiento, Regimen } from '../../../@data/model/lista-evaluaciones/entity';
import { Component, OnInit, ElementRef, ViewChild } from '@angular/core';
import { FormGroup, FormBuilder, FormControl } from '@angular/forms';
import { Sort } from '@angular/material/sort';
import { map } from 'rxjs/operators';


import { Observable, of, forkJoin } from 'rxjs';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Router } from '@angular/router';
import { armarPayload, Utils } from 'src/app/utils/utils';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';

@Component({
  selector: 'serv-talento-lista-evaluaciones',
  templateUrl: './lista-evaluaciones.component.html',
  styleUrls: ['./lista-evaluaciones.component.scss'],
})
export class ListaEvaluacionesComponent implements OnInit {
  filterForm: FormGroup;

  conocimientoTableColumns: TableColumn[];
  inputFormControl: FormControl;

  filteredOptionsConvocatoria$: Observable<Convocatoria[]>;
  optionsConvocatoria: Convocatoria[] = [];
  lstConvocatoria: Convocatoria[];

  filteredOptionsRegimen$: Observable<Regimen[]>;
  optionsRegimen: Regimen[] = [];
  lstRegimen: Regimen[];

  lstEvaluaciones: any[] = [];
  lstEstado: any[] = [];

  @ViewChild('inputConvocatoria') inputConvocatoria: ElementRef;
  @ViewChild('inputRegimen') inputRegimen: ElementRef;
  @ViewChild('rangepicker') rangepicker: ElementRef;

  constructor(
    private fb: FormBuilder,
    private srvSeleccionRepository: SeleccionServirRepository,
    private authenticationRepository: AuthenticationRepository,
    private maestraService: MaestraService,
    private router: Router
  ) {
    this.filterForm = this.fb.group({
      convocatoria: new FormControl(''),
      convocatoriaId: new FormControl(''),
      regimen: new FormControl(''),
      regimenId: new FormControl(''),
      rangoFechas: new FormControl(''),
      estado: new FormControl(''),
    });
  }

  ngOnInit(): void {
    this.initializeColumns();
    this.listarConvocatorias();
    this.listarRegimen();
    this.listarEstados();
    this.listarEvaluaciones();
    this.formatColor();
  }

  listarEvaluaciones() {
    let request: any;

    request = {
      trace: {
        traceId: 'string',
      },
      payload: {
        convocatoriaId: null,
        idRegimen: null,
        fecha: null,
        estadoExamen: null,
        entidadId: this.authenticationRepository.getCurrentUserValue.entidadId,
      },
    };

    this.srvSeleccionRepository
      .getListaConocimientos(request)
      .subscribe((mc) => {
        this.lstEvaluaciones = mc;

        this.lstEvaluaciones.forEach((element, index) => {
          element.correlativo = index + 1;
        });

      });
  }

  listarEstados(): void {
    const getEstadosDif = this.maestraService.getMaestraDetalleByCod(
      'TIP_EST_CONV'
    );
    forkJoin([getEstadosDif]).subscribe((results) => {
      this.lstEstado = results[0];
    });
  }

  // -----------------CONVOCATORIAS---------------------//
  listarConvocatorias(): void {
    this.srvSeleccionRepository.getConvocatoriasEntidad(this.authenticationRepository.getCurrentUserValue.entidadId).subscribe((mc) => {
      this.filteredOptionsConvocatoria$ = of(mc);
      this.optionsConvocatoria = mc;
      this.lstConvocatoria = mc;
    });
  }

  verifyConvocatoria(): void {
    const actualValue = this.filterForm.get('convocatoriaId').value;
    if (
      !this.lstConvocatoria?.filter((o) => o.idConvocatoria === actualValue)[0]
    ) {
      this.filterForm.get('convocatoriaId').setErrors({ notfound: true });
      this.filterForm.get('convocatoriaId').setValue('');
    } else {
      this.setConvocatoriaById(actualValue);
    }
  }

  onChangeConvocatoria() {
    this.filteredOptionsConvocatoria$ = this.getFilteredOptionsConvocatoria(
      this.inputConvocatoria.nativeElement.value
    );
    this.onSelectionChangeConvocatoria(this.inputConvocatoria.nativeElement.value);
  }

  private filterConvocatoria(value: string): Convocatoria[] {
    const filterValue = value?.toLowerCase();
    return this.optionsConvocatoria?.filter((optionValue) =>
      optionValue.codigoConvocatoria == null
        ? ''
        : optionValue.codigoConvocatoria.toLowerCase().includes(filterValue)
    );
  }

  onSelectionChangeConvocatoria($event) {
    this.setConvocatoria($event);
    this.filteredOptionsConvocatoria$ = this.getFilteredOptionsConvocatoria(
      $event
    );
  }

  setConvocatoria(item: string) {
    let lstConvocatoria = this.lstConvocatoria?.filter(
      (o) => o.codigoConvocatoria === item)[0]?.idConvocatoria;
    this.filterForm.get('convocatoriaId').setValue(lstConvocatoria);
  }

  getFilteredOptionsConvocatoria(value: string): Observable<Convocatoria[]> {
    return of(value).pipe(
      map((filterString) => this.filterConvocatoria(filterString))
    );
  }

  setConvocatoriaById(item: number) {
    let convocatoria = this.lstConvocatoria?.filter(
      (o) => o.idConvocatoria === item
    )[0]?.idConvocatoria;
    this.filterForm.get('convocatoriaId').setValue(convocatoria);
  }

  // -----------------REGIMEN---------------------//
  listarRegimen(): void {
    this.srvSeleccionRepository.getRegimenEntidad(this.authenticationRepository.getCurrentUserValue.entidadId).subscribe((mc) => {
      this.filteredOptionsRegimen$ = of(mc);
      this.optionsRegimen = mc;
      this.lstRegimen = mc;
    });
  }

  verifyRegimen(): void {
    const actualValue = this.filterForm.get('regimenId').value;
    if (!this.lstRegimen?.filter((o) => o.idRegimen === actualValue)[0]) {
      this.filterForm.get('regimenId').setErrors({ notfound: true });
      this.filterForm.get('regimenId').setValue('');
    } else {
      this.setRegimenById(actualValue);
    }
  }

  onChangeRegimen(): void {
    this.filteredOptionsRegimen$ = this.getFilteredOptionsRegimen(
      this.inputRegimen.nativeElement.value
    );

    this.onSelectionChangeRegimen(this.inputRegimen.nativeElement.value);
  }

  getFilteredOptionsRegimen(value: string): Observable<Regimen[]> {
    return of(value).pipe(
      map((filterString) => this.filterRegimen(filterString))
    );
  }

  private filterRegimen(value: string): Regimen[] {
    const filterValue = value?.toLowerCase();
    return this.optionsRegimen?.filter((optionValue) =>
      optionValue.desRegimen == null
        ? ''
        : optionValue.desRegimen.toLowerCase().includes(filterValue)
    );
  }

  onSelectionChangeRegimen($event) {
    this.setRegimen($event);
    this.filteredOptionsRegimen$ = this.getFilteredOptionsRegimen($event);
  }

  setRegimen(item: string) {
    let lstRegimen = this.lstRegimen?.filter((o) => o.desRegimen === item)[0]
      ?.idRegimen;
    this.filterForm.get('regimenId').setValue(lstRegimen);
  }

  setRegimenById(item: number) {
    let regimen = this.lstRegimen?.filter((o) => o.idRegimen === item)[0]
      ?.idRegimen;
    this.filterForm.get('regimenId').setValue(regimen);
  }

  buscar(): void {
    let fecha: string = "";
    if (this.filterForm.get('rangoFechas').value != null && this.filterForm.get('rangoFechas').value.end != null) {
      fecha = Utils.formatFechaString(this.filterForm.get('rangoFechas').value.start, "DD/MM/YYYY") + " - "
        + Utils.formatFechaString(this.filterForm.get('rangoFechas').value.end, "DD/MM/YYYY");
    } else if (this.filterForm.get('rangoFechas').value != null) {
      fecha = Utils.formatFechaString(this.filterForm.get('rangoFechas').value.start, "DD/MM/YYYY");
    }

    let item: any;
    item = {
      convocatoriaId: this.filterForm.get('convocatoriaId').value,
      idRegimen: this.filterForm.get('regimenId').value,
      fecha: fecha === 'Fecha inválida' ? null : fecha,
      estadoExamen: this.filterForm.get('estado').value,
      entidadId: this.authenticationRepository.getCurrentUserValue.entidadId,
    };

    let request = armarPayload<any>(item);
    this.srvSeleccionRepository
      .getListaConocimientos(request)
      .subscribe((mc) => {
        this.lstEvaluaciones = mc;
      });
  }

  limpiar(): void {
    this.filterForm.reset();
    this.filterForm.get('convocatoria').setValue('');
    this.filterForm.get('convocatoriaId').setValue('');
    this.filterForm.get('regimen').setValue('');
    this.filterForm.get('regimenId').setValue('');
    this.filterForm.get('rangoFechas').setValue('');
    this.filterForm.get('estado').setValue('');

  }

  initializeColumns(): void {
    this.conocimientoTableColumns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CÓDIGO DE CONVOCATORIA',
        dataKey: 'codigoConvocatoria',
        position: 'left',
        isSortable: true,
        width: '35%',
      },
      {
        name: 'RÉGIMEN',
        dataKey: 'regimen',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'CANT PERFILES',
        dataKey: 'cantidadPerfil',
        position: 'center',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CANT GRUPOS',
        dataKey: 'cantidadGrupos',
        position: 'center',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'FECHA DE EVALUACIÓN',
        dataKey: 'fechaInicioExamen',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estado',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstConvocatoria);
  }

  formatColor() {
    this.lstEvaluaciones.forEach((el) => {
      el.settings = {};

      el.settings.estado = {};
      el.settings.estado.color = '#6FCF97';
      el.settings.estado.background = '#6FCF97';

      el.settings.convocatoria = {};
      el.settings.convocatoria.color = '#0d88bc';
    });
  }

  viewDetailEvaluacion(event) {
    sessionStorage.setItem('convocatoriaId', event.convocatoriaId);
    this.router.navigateByUrl('pages/lista-evaluaciones/lista-gestion-notas');
  }

  getDataExportEvaluaciones(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista completa de evaluaciones';
    model.headers = [
      '#',
      'CÓDIGO DE CONVOCATORIA',
      'RÉGIMEN',
      'CANT PERFILES',
      'CANT GRUPOS',
      'FECHA DE EVALUACIÓN',
      'EVALUACIONES',
      'ESTADO',
    ];
    model.keys = [
      'correlativo',
      'convocatoria',
      'regimen',
      'cantidadPerfil',
      'cantidadGrupos',
      'fechaInicioExamen',
      'evaluaciones',
      'estado',
    ];
    return model;
  }
}
