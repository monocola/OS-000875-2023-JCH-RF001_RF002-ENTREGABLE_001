import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { forkJoin } from 'rxjs';
import { ComboitemModel } from '../../../@data/model/generic/comboitem.model';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { SeguimientoRepository } from '../../../@domain/repository/seguimiento.repository';
import moment from 'moment';
import { Router } from '@angular/router';
import { SeguimientoComunicadoService } from '../seguimiento-comunicado/seguimiento-comunicado.service';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-seguimiento-proceso',
  templateUrl: './seguimiento-proceso.component.html',
  styleUrls: ['./seguimiento-proceso.component.scss'],
})
export class SeguimientoProcesoComponent implements OnInit {
  regimenes: ComboitemModel[] = [];
  etapas: ComboitemModel[] = [];
  estados: ComboitemModel[] = [];
  filterForm: FormGroup = null;
  columns: TableColumn[];
  data: any[] = [];

  page: number = 0;
  size: number = 10;
  total: number = 0;
  filtros: any;
  rangePickerStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private seguimientoRepository: SeguimientoRepository,
    private router: Router,
    public helperService: SeguimientoComunicadoService
  ) {}

  ngOnInit(): void {
    this.helperService.initializeValues();
    this.initializeForm();
    this.initializeColumns();
    this.loadCombox();
    this.buscar();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      regimen: null,
      etapa: null,
      periodo: null,
      estado: null,
    });
  }

  get g() {
    return this.helperService.formConvocatoria.controls;
  }

  loadCombox() {
    const getRegimenes = this.seguimientoRepository.getFiltroMaestra(
      'TBL_REGIMEN'
    );
    const getEtapasRegistro = this.seguimientoRepository.getFiltroMaestra(
      'TIP_ETA_PRO'
    );
    const getEstados = this.seguimientoRepository.getFiltroMaestra(
      'TIP_EST_CONV'
    );

    forkJoin([getRegimenes, getEtapasRegistro, getEstados]).subscribe(
      (results) => {
        this.regimenes = results[0];
        this.etapas = results[1];
        this.estados = results[2];
      }
    );
  }

  limpiar() {
    this.initializeForm();
    this.buscar();
  }

  buscar() {
    let filtros = this.filterForm.value;

    filtros.fechaIni =
      filtros.periodo != null
        ? moment(filtros.periodo.start).format('DD/MM/yyyy')
        : null;
    filtros.fechaFin =
      filtros.periodo != null
        ? moment(filtros.periodo.end).format('DD/MM/yyyy')
        : null;
    filtros = this.cleanFiltros(filtros);
    this.filtros = filtros;
    this.page = 0;

    this.getData();
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  initializeColumns() {
    this.columns = [
      {
        name: 'CÓD. DE CONVOCATORIA y RÉGIMEN',
        dataKey: 'codConvAndCodRegimen',
        position: 'left',
        isSortable: true,
        width: '26%',
      },
      {
        name: 'FECHA DE PUB.',
        dataKey: 'fechaPublicacion',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'VAC.',
        dataKey: 'vacantes',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'ETAPA.',
        dataKey: 'etapa',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      {
        name: 'POSTULANTES CALIFICAN.',
        dataKey: 'postulantesAndCalifican',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      {
        name: 'GESTOR',
        dataKey: 'gestor',
        position: 'center',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'Coordinador',
        dataKey: 'coordinador',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Listado de seguimientos';
    model.headers = [
      'CÓD. DE CONVOCATORIA Y RÉGIMEN',
      'FECHA DE PUB.',
      'VAC.',
      'ETAPA.',
      'POSTULANTES CALIFICAN.',
      'GESTOR',
      'COORDINADOR',
      'ESTADO',
    ];
    model.keys = [
      'codConvAndCodRegimen',
      'fechaPublicacion',
      'vacantes',
      'etapa',
      'postulantesAndCalifican',
      'gestor',
      'coordinador',
      'estadoConvocatoria',
    ];
    return model;
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getData();
  }

  private getData() {
    this.seguimientoRepository
      .getData(this.filtros, this.page, this.size)
      .subscribe((items) => {
        this.data = items.items;
        this.total = items.total;
      });
  }

  private cleanFiltros (filtros: any) {
    if (filtros != null) {
      for (let key of Object.keys(filtros)) {
        if (filtros[key] == null || filtros[key] === 0 || filtros[key] === '') {
          delete filtros[key];
        }
      }
      delete filtros['periodo'];
    }
    return filtros;
  }

  procesoAction(e) {
    let valor: number = +e.codProgEtapa;
    this.helperService.enviarBase(e);
    switch (valor) {
      case 1:
        this.router.navigateByUrl('pages/etapas');
        break;
      case 2:
        this.router.navigateByUrl('pages/etapas');
        break;
      case 3:
        this.router.navigateByUrl('pages/seguimiento-evaluacion');
        break;
      case 4:
        this.router.navigateByUrl('pages/seguimiento-eleccion');
        break;
    }
  }

  verComunicados(e) {
    this.helperService.enviarBase(e);
    this.router.navigateByUrl('pages/seguimientoconvocatoria/comunicado');
  }

  verCronograma(e) {
    this.helperService.enviarBase(e);
    this.router.navigateByUrl('pages/seguimientoconvocatoria/cronograma');
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const periodo = this.filterForm.controls['periodo'].value;

    if (this.filterForm.controls['periodo'].errors && periodo === null) {
      this.rangePickerStatus = 'danger';
    }
  }
}
