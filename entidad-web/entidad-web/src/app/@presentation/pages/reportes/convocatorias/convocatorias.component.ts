import { ConvocatoriaDataService } from './convocatoria-data.service';
import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { Ubigeo } from 'src/app/@data/model/ubigeo';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ReportesRepository } from 'src/app/@domain/repository/reportes.repository';
import { Const } from 'src/app/@data/services/const';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { armarPayload, Utils } from 'src/app/utils/utils';
import { filter, map } from 'rxjs/operators';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { IConvocatoria } from 'src/app/@data/model/reporte';
import { Observable, of } from 'rxjs';

@Component({
  selector: 'serv-talento-convocatorias',
  templateUrl: './convocatorias.component.html',
  styleUrls: ['./convocatorias.component.scss'],
})
export class ConvocatoriasComponent implements OnInit {
  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';
  lstCodigoConvocatoria: any[];
  lstRegimen: any[];
  lstDepartamentos: Ubigeo[];
  lstEstadoConvocatoria: any[];
  lstModalidadAcceso: any[];
  lstModalidadIngreso: any[];
  lstResponsables: any[];
  lstEtapas: any[];
  lstRoles: any[];
  lstPerfiles: any[];
  entidadId: number;
  flagTipo: boolean = true;

  columns: TableColumn[];
  data: IConvocatoria[] = [];
  resumen: any = {};
  page: number = 0;
  size: number = 20;

  filteredOptionsCodigo$: Observable<any[]>;
  @ViewChild('inputCodigo') inputCodigo: ElementRef;
  optionsCodigo: any[] = [];
  grupo: any[];

  myMap: any[] = [
    { id: 1, rango: '800-1500' },
    { id: 2, rango: '1501-3000' },
    { id: 3, rango: '3001-5000' },
  ];

  constructor(
    private authenticationRepository: AuthenticationRepository,
    private srvReportesRepository: ReportesRepository,
    private srvParameterRepository: ParameterRepository,
    private srvMaestraRepository: MaestraRepository,
    private toast: ToastService,
    public router: Router,
    private fb: FormBuilder,
    private dataService: ConvocatoriaDataService
  ) {
    this.filterForm = this.fb.group({
      codigo: new FormControl(null),
      desCodigo: new FormControl(null),
      regimen: new FormControl(null, [Validators.required]),
      periodo: new FormControl(null, [Validators.required]),
      departamento: new FormControl(null),
      estado: new FormControl(null),
      modalidad: new FormControl(null),
      tipo: new FormControl(null),
      perfil: new FormControl(null),
      etapa: new FormControl(null),
      rango: new FormControl(null),
      rol: new FormControl(null),
      responsable: new FormControl(null),
    });
  }

  ngOnInit(): void {
    this.entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;

    this.getCodigoConvocatoria();
    this.getRegimen();
    this.getDepartamentos();
    this.getEstadoConvocatoria();
    this.getModalidadAcceso();
    this.getEtapas();
    this.getRoles();
    this.getPerfilesEntidad();

    this.initializeColumns();
  }

  initializeColumns() {
    this.columns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
    ];
  }

  getPerfilesEntidad() {
    this.srvReportesRepository
      .getPerfilesEntidad(this.entidadId)
      .subscribe((mc) => {
        this.lstPerfiles = mc;
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

    console.log(this.lstCodigoConvocatoria);
  }

  getRegimen() {
    this.srvReportesRepository.getRegimen(this.entidadId).subscribe((mc) => {
      this.lstRegimen = mc;
    });
  }

  getRoles() {
    let coordinarId = Const.R_COORDINADOR;
    let gestorId = Const.R_GESTOR_ORH;

    this.srvReportesRepository.getRoles().subscribe((mc: any []) => {
      this.lstRoles = mc.filter(
        (item: any) => item.rolId === coordinarId || item.rolId === gestorId
      );
      this.lstRoles.forEach ((m: any) => {
        m.nombreRol = m.nombreRol.toUpperCase();
      });
    });
  }

  getDepartamentos() {
    this.srvParameterRepository.getDepartamento().subscribe((mc) => {
      this.lstDepartamentos = mc;
    });
  }

  getEstadoConvocatoria() {
    this.srvReportesRepository.getEstadoConvocatoria().subscribe((mc) => {
      this.lstEstadoConvocatoria = mc;
    });
  }

  getModalidadAcceso() {
    this.srvMaestraRepository
      .getMaestraDetalleByCod('TBL_MODALIDAD')
      .subscribe((mc) => {
        this.lstModalidadAcceso = mc;
      });
  }

  getEtapas() {
    this.srvMaestraRepository
      .getMaestraDetalleByCod('TIP_ETA_PRO')
      .subscribe((mc) => {
        this.lstEtapas = mc;
      });
  }

  onChangeCondicion($event: any) {}

  onChangeRegimen($event: number) {
    this.flagTipo = false;
    this.srvReportesRepository
      .getModalidadIngreso(this.entidadId, $event)
      .subscribe((mc) => {
        this.lstModalidadIngreso = mc;
      });
  }

  onChangeRol($event: number) {
    this.srvReportesRepository
      .getResponsables(this.entidadId, $event)
      .subscribe((mc) => {
        this.lstResponsables = mc;
      });
  }

  limpiar(): void {
    this.filterForm.reset();
    this.inputCodigo.nativeElement.value = null;
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
      fecha =
        Utils.formatFechaString(
          this.filterForm.get('periodo').value.start,
          'DD/MM/YYYY'
        ) +
        '-' +
        Utils.formatFechaString(
          this.filterForm.get('periodo').value.end,
          'DD/MM/YYYY'
        );
    } else if (this.filterForm.get('periodo').value != null) {
      fecha = Utils.formatFechaString(
        this.filterForm.get('periodo').value.start,
        'DD/MM/YYYY'
      );
    }

    let item: any;
    item = {
      codigoConvocatoria: this.f.codigo.value,
      regimenId: this.f.regimen.value,
      periodo: fecha,
      departamentoId: this.f.departamento.value,
      estadoConvocatoriaId: this.f.estado.value,
      modalidadId: this.f.modalidad.value,
      tipoId: this.f.tipo.value,
      reportePerfilId: this.f.perfil.value,
      etapaConvocatoriaId: this.f.etapa.value,
      rangoSalarial: this.buscarRango(this.f.rango.value),
      entidadId: this.entidadId,
      page: 0,
      size: 10,
    };

    let request = armarPayload<any>(item);

    this.srvReportesRepository.getListaConvocatoria(request).subscribe(
      (mc) => {
        this.data = mc;
        this.data.forEach((part, index, theArray) => {
          theArray[index].correlativo = index + 1;
        });
        this.getResumenConvocatoria(request);
      },
      (error) => {
        console.log (error);
        this.toast.showToast(error.message, 'warning');
      }
    );
  }

  getResumenConvocatoria(request: any) {
    this.srvReportesRepository
      .getResumenConvocatoria(request)
      .subscribe((mc) => {
        this.resumen = mc;
      });
  }

  buscarRango(indice: string): string {
    let newMap = null;
    if (indice !== null) {
      newMap = this.myMap.filter((fil) => fil.id === +indice)[0];
      return newMap.rango;
    }
    return newMap;
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['periodo'].value;

    if (this.filterForm.controls['periodo'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de comunicados';
    model.headers = [
      '#',
      'CODIGO',
      'INICIO ETAPA',
      'FIN ETAPA',
      'REGIMEN',
      'VACANTE',
      'GESTOR ORH',
      'COORDINADOR',
      'POSTULANTES',
      'CALIFICAN',
      'ETAPA',
      'PERFILES',
      'RANGO SALARIAL',
      'ESTADO',
    ];
    model.keys = [
      'correlativo',
      'codigoConvocatoria',
      'periodoIniEtapa',
      'periodoFinEtapa',
      'regimen',
      'nroVacantes',
      'nombreGestor',
      'nombreCoordinador',
      'nroPostPorConvo',
      'nroPostContratados',
      'etapaConvocatoria',
      'perfiles',
      'remuneracion',
      'estadoConvocatoria',
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

  openDetalleConvocatoria(row: any) {
    console.info(row);

    this.dataService.convocatoriaId = row.baseId;
    this.dataService.objConv = row;
    console.info(this.dataService.objConv);
    this.router.navigateByUrl('pages/reportes/detalleconvocatoria');
  }

  verMensaje(e: any) {
    console.log(e);
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
      ?.codigoConvocatoria;
    this.filterForm.get('codigo').setValue(lstCodigo);
  }

  setCodigoById(item: number) {
    let codigo = this.lstCodigoConvocatoria?.filter((o) => o.idConvocatoria === item)[0]
      ?.idConvocatoria;
    this.filterForm.get('codigo').setValue(codigo);
  }

}
