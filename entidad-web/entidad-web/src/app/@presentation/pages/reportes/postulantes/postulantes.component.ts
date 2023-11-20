import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReportesRepository } from 'src/app/@domain/repository/reportes.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';
import { ReportePostulanteRepository } from 'src/app/@domain/repository/reporte-postulante-repository';
import { Utils } from 'src/app/utils/utils';
import { filter, map } from 'rxjs/operators';
import { Observable, of } from 'rxjs';
import { Const } from '../../../../@data/services/const';


@Component({
  selector: 'serv-talento-postulantes',
  templateUrl: './postulantes.component.html',
  styleUrls: ['./postulantes.component.scss'],
})
export class PostulantesComponent implements OnInit {
  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';

  roles: any;
  user: any;
  entidadId: number;
  flagTipo: boolean = true;

  lstTipoDocumento: any[];
  lstNombre: any[];
  lstDepartamentos: any[];
  lstCodigoConvocatoria: any[];
  lstPerfiles: any[];
  lstEtapas: any[];
  lstDuracionContrato: any[];
  lstEstadoConvocatoria: any[];
  lstPostulantes: any[];

  columns: TableColumn[];
  data: any[] = [];
  dataHeader: any;
  page: number = 0;
  size: number = 20;

  filteredOptionsCodigo$: Observable<any[]>;
  @ViewChild('inputCodigo') inputCodigo: ElementRef;
  optionsCodigo: any[] = [];

  filteredOptionsPost$: Observable<any[]>;
  @ViewChild('inputPost') inputPost: ElementRef;
  optionsPost: any[] = [];

  filteredOptionsPerfil$: Observable<any[]>;
  @ViewChild('inputPerfil') inputPerfil: ElementRef;
  optionsPerfil: any[] = [];

  myRangoDuracionContra: any[] = [
    { id: 1, dura: '1-6' },
    { id: 2, dura: '7-11' },
  ];

  myRangoSalarial: any[] = [
    { id: 1, rango: '800-1500' },
    { id: 2, rango: '1501-3000' },
    { id: 3, rango: '3001-5000' },
  ];

  constructor(
    private authenticationRepository: AuthenticationRepository,
    private srvReportesRepository: ReportesRepository,
    private srvParameterRepository: ParameterRepository,
    private srvMaestraRepository: MaestraRepository,
    public router: Router,
    private fb: FormBuilder,
    private reportePostulanteRepository: ReportePostulanteRepository,
    private constant: Const
  ) {
    this.filterForm = this.fb.group({
      nombre: new FormControl(null),
      postulanteId: new FormControl(null),

      tipoDocumento: new FormControl(null),
      nroDocumento: new FormControl(null),
      periodo: new FormControl(null, [Validators.required]),

      departamento: new FormControl(null),

      codConvDesc: new FormControl(null),
      convocatoriaId: new FormControl(null),

      perfilDesc: new FormControl(null),
      perfilId: new FormControl(null),

      etapa: new FormControl(null),
      rango: new FormControl(null),
      duracion: new FormControl(null),
      estado: new FormControl(null),
    });
  }

  ngOnInit(): void {
    this.user = this.authenticationRepository.getCurrentUserValue;
    if (this.user.rolId !== Const.R_ADMIN_SERVIR) {
      this.entidadId = this.user.entidadId;
    }
    this.getCodigoConvocatoria();
    this.getPostulantes();
    this.getTipoDocumento();
    this.getDepartamentos();
    this.getPerfilesEntidad();
    this.getEtapaProceso();
    this.getEstadoConvocatoria();

    this.initializeColumns();
  }

  initializeColumns() {
    this.columns = [
      {
        name: '#',
        dataKey: 'id',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
    ];
  }

  limpiar(): void {
    this.filterForm.reset();
    this.filterForm.controls['nombre'].setValue("");
    this.filterForm.controls['postulanteId'].setValue(null);
    this.filterForm.controls['codConvDesc'].setValue("");
    this.filterForm.controls['convocatoriaId'].setValue(null);
    this.filterForm.controls['perfilDesc'].setValue("");
    this.filterForm.controls['perfilId'].setValue(null);

    this.data = [];
    this.getData();
  }

  get f() {
    return this.filterForm.controls;
  }

  buscar(): void {
    this.filterForm.markAllAsTouched();
    if (this.filterForm.invalid) {
      return;
    }

    let fecha: string = '';
    if (
      this.filterForm.get('periodo').value != null &&
      this.filterForm.get('periodo').value.end != null
    ) {
      fecha = Utils.formatFechaString(
        this.filterForm.get('periodo').value.start, 'DD/MM/YYYY'
      ) +
        '-' +
        Utils.formatFechaString(this.filterForm.get('periodo').value.end, 'DD/MM/YYYY'
        );
    } else if (this.filterForm.get('periodo').value != null) {
      fecha = Utils.formatFechaString(
        this.filterForm.get('periodo').value.start,
        'DD/MM/YYYY'
      );
    }

    let payload = {
      postulanteId: this.filterForm.controls['postulanteId'].value,
      tipoDocumento: this.filterForm.controls['tipoDocumento'].value,
      nroDocumento: this.filterForm.controls['nroDocumento'].value,
      periodo: fecha,
      departamentoId: this.filterForm.controls['departamento'].value,
      departamento: this.getUbigeoDesc(this.filterForm.controls['departamento'].value),
      // nombre: this.filterForm.controls['postulanteId'].value,
      // codigo: this.filterForm.controls['codigo'].value,
      convocatoriaId: this.filterForm.controls['convocatoriaId'].value,
      perfilId: this.filterForm.controls['perfilId'].value,
      etapaId: this.filterForm.controls['etapa'].value,
      rango: this.buscarRango(this.filterForm.controls['rango'].value),
      duracion: this.buscarDuracionCon(this.filterForm.controls['duracion'].value),
      // estado: this.filterForm.controls['estado'].value,
      entidadId: this.entidadId,
      page: this.page,
      size: this.size
    };

    this.reportePostulanteRepository.buscarReportePostulantes(payload).subscribe((mc) => {
      this.dataHeader = mc;
      this.data = mc.listaDetalleBandeja;
      this.data.forEach((part, index, theArray) => {
        theArray[index].id = index + 1;
      });
    });

  }


  buscarDuracionCon(indice: string): string {
    let newMap = null;
    if (indice !== null) {
      if (indice !== "*" && indice !== "+") {
        newMap = this.myRangoDuracionContra.filter((fil) => fil.id === +indice)[0];
        return newMap.dura;
      } else {
        newMap = indice;
      }
    }
    return newMap;
  }

  buscarRango(indice: string): string {
    let newMap = null;
    if (indice !== null) {
      if (indice === "*") {
        return "*";
      } else {
        newMap = this.myRangoSalarial.filter((fil) => fil.id === +indice)[0];
        return newMap.rango;
      }

    }
    return newMap;
  }

  getEstadoConvocatoria() {
    this.srvReportesRepository.getEstadoConvocatoria().subscribe((mc) => {
      this.lstEstadoConvocatoria = mc;
    });
  }

  getPostulantes() {
    this.srvReportesRepository
      .getPostulantes(this.entidadId)
      .subscribe((mc) => {
        this.lstPostulantes = mc;
        this.filteredOptionsPost$ = of(mc);
        this.optionsPost = this.lstPostulantes;
      });
  }

  getUbigeoDesc(ubigeoId: number) {
    let nameDep = null;
    if (ubigeoId > 0) {
      nameDep = this.lstDepartamentos?.filter((o) => o.ubigeoId === ubigeoId)[0]
        ?.nombre;
    }
    return nameDep;
  }


  getDepartamentos() {
    this.srvParameterRepository.getDepartamento().subscribe((mc) => {
      this.lstDepartamentos = mc;
    });
  }

  getPerfilesEntidad() {
    this.srvReportesRepository
      .getPerfilesEntidad(this.entidadId)
      .subscribe((mc) => {
        this.lstPerfiles = mc;
        this.filteredOptionsPerfil$ = of(mc);
        this.optionsPerfil = this.lstPerfiles;
      });
  }

  getEtapaProceso() {
    this.srvMaestraRepository
      .getMaestraDetalleByCod('TIP_ETA_PRO')
      .subscribe((mc) => {
        this.lstEtapas = mc;
      });
  }

  getCodigoConvocatoria() {
    this.srvReportesRepository
      .getCodigoConvocatoria(this.entidadId)
      .subscribe((mc) => {
        this.lstCodigoConvocatoria = mc;
        this.filteredOptionsCodigo$ = of(mc);
        this.optionsCodigo = this.lstCodigoConvocatoria;
      });
  }

  getTipoDocumento() {
    this.srvParameterRepository.getTypeDocuments().subscribe((mc) => {
      this.lstTipoDocumento = mc;
    });
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['periodo'].value;

    if (this.filterForm.controls['periodo'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }




  private filterCodigo(value: string): any[] {
    const filterValue = value?.toLowerCase();
    return this.optionsCodigo?.filter((optionValue) =>
      optionValue.codigoConvocatoria == null
        ? ''
        : optionValue.codigoConvocatoria.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsCodigo(value: string): Observable<any[]> {
    return of(value).pipe(
      map((filterString) => this.filterCodigo(filterString))
    );
  }

  onChangeCodigo() {
    this.filteredOptionsCodigo$ = this.getFilteredOptionsCodigo(
      this.inputCodigo.nativeElement.value
    );

    this.onSelectionChangeCodigo(this.inputCodigo.nativeElement.value);
  }

  onSelectionChangeCodigo($event) {
    this.setCodigo($event);
    this.filteredOptionsCodigo$ = this.getFilteredOptionsCodigo($event);
  }
  setCodigo(item: string) {
    let lstCodigo = this.lstCodigoConvocatoria?.filter((o) => o.codigoConvocatoria === item)[0]
      ?.idConvocatoria;
    this.filterForm.get('convocatoriaId').setValue(lstCodigo);
  }

  private filterPost(value: string): any[] {
    const filterValue = value?.toLowerCase();
    return this.optionsPost?.filter((optionValue) =>
      optionValue.nombreCompleto == null
        ? ''
        : optionValue.nombreCompleto.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsPost(value: string): Observable<any[]> {
    return of(value).pipe(
      map((filterString) => this.filterPost(filterString))
    );
  }

  onChangePost() {
    this.filteredOptionsPost$ = this.getFilteredOptionsPost(
      this.inputPost.nativeElement.value
    );

    this.onSelectionChangePost(this.inputPost.nativeElement.value);
  }

  onSelectionChangePost($event) {
    this.setCodigoPost($event);
    this.filteredOptionsPost$ = this.getFilteredOptionsPost($event);
  }
  setCodigoPost(item: string) {
    let lstCodigo = this.lstPostulantes?.filter((o) => o.nombreCompleto === item)[0]
      ?.idPostulante;
    this.filterForm.get('postulanteId').setValue(lstCodigo);
  }

  private filterPerfil(value: string): any[] {
    const filterValue = value?.toLowerCase();
    return this.optionsPerfil?.filter((optionValue) =>
      optionValue.nombrePuesto == null
        ? ''
        : optionValue.nombrePuesto.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsPerfil(value: string): Observable<any[]> {
    return of(value).pipe(
      map((filterString) => this.filterPerfil(filterString))
    );
  }

  onChangePerfil() {
    this.filteredOptionsPerfil$ = this.getFilteredOptionsPerfil(
      this.inputPerfil.nativeElement.value
    );

    this.onSelectionChangePerfil(this.inputPerfil.nativeElement.value);
  }

  onSelectionChangePerfil($event) {
    this.setCodigoPerfil($event);
    this.filteredOptionsPerfil$ = this.getFilteredOptionsPerfil($event);
  }
  setCodigoPerfil(item: string) {
    let lstCodigo = this.lstPerfiles?.filter((o) => o.nombrePuesto === item)[0]
      ?.perfilId;
    this.filterForm.get('perfilId').setValue(lstCodigo);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de comunicados';
    model.headers = [
      '#',
      'NOMBRE/S Y APELLIDOS',
      'F.POSTULACIÓN',
      'CÓDIGO',
      'RÉGIMEN',
      'PERFIL',
      'DURACION CONTRATO',
      'ETAPA',
      'SALARIO',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'nombreCompleto',
      'fechaPostulacion',
      'codigo',
      'regimen',
      'perfil',
      'duracion',
      'etapa',
      'salario',
      'estado',
    ];
    return model;
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  private getData_() {
    this.data = [
      {
        id: '1',
        nombre: 'Juan Andrés Gonzales DNI 44730485',
        fecha: '10/09/2020',
        codigo: 'CAS N° 037-2020',
        regimen: 'Ley 30057',
        perfil: 'Medico cirujano',
        duracion: '2 meses',
        etapa: 'Evaluación',
        rango: 'S/. 3,000.00',
        estado: 'En proceso',
      },
    ];
  }

  private getData() {

    this.dataHeader = {
      periodoInicio: "",
      setPeriodoFin: "",
      departamento: "",
      cantConvocatoria: 0,
      cantPostulantes: 0,
      cantPerfiles: 0,
      califican: 0,
      noCalifican: 0,
    };
  }

  verMensaje(e: any) {
    console.log(e);
  }
}
