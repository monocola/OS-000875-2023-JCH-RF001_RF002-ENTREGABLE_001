import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Sort } from '@angular/material/sort';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { ModalContratoComponent } from './modal-contrato/modal-contrato.component';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Router } from '@angular/router';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { ContratosService } from './contratos.service';
import { EvaluacionesServirService } from 'src/app/@data/services/evaluaciones-servir.service';

import moment from 'moment';
import { forkJoin } from 'rxjs';

import { ModalDescargaComponent } from './modal-descarga/modal-descarga.component';
import FileSaver from 'file-saver';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-contratos',
  templateUrl: './contratos.component.html',
  styleUrls: ['./contratos.component.scss'],
})
export class ContratosComponent implements OnInit {
  filterForm: FormGroup;
  TableColumns: TableColumn[];
  fileName: string = 'Subir documento';
  filepdf: string = '';
  pathpdf: string = '';
  urlpdf: string = null;
  maeCabeceraId: any = [];
  detalle: any = '';
  searchMode = false;
  regimenes = [];
  estados = [];
  perfiles = [];
  textObservacion: string = null;
  title: string = 'Crear';

  contratos = [];

  page: number = 0;
  size: number = 20;
  total: number = 0;
  filtros: any = {
    tipo: 0,
    regimen: 0,
    perfil: 0,
    estado: 0,
    fecIni: '',
    fecFin: '',
  };
  rangePickerStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private evaluacionServirService: EvaluacionesServirService,
    private maestraService: MaestraRepository,
    private perfilService: PerfilesRepository,
    private dialog: MatDialog,
    private toast: ToastService,
    private seguimientoService: SeguimientoRepository,
    private router: Router,
    private helperService: ContratosService
  ) {}

  ngOnInit(): void {
    this.helperService.initializeValues();
    this.initializeForm();
    this.loadCombox();
    this.initializeColumns();
  }

  loadCombox() {
    const getRegimenes = this.evaluacionServirService.getRegimenesCabecera(
      'TBL_REGIMEN'
    );

    forkJoin([getRegimenes]).subscribe((results) => {
      this.maeCabeceraId = results;
      this.detalle = this.maeCabeceraId[0].maeCabeceraId;
      this.getCombox();
    });
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      regimen: 0,
      perfil: 0,
      estado: 0,
      fecha: null
    });
  }

  getCombox() {
    this.maestraService.getComboSection(this.detalle).subscribe((res) => {
    let array = res.filter ((del) => {
      return del.estadoRegistro === '1';
    });
    this.regimenes = array;
    });

    this.maestraService
      .getMaestraDetalleByCod('TBL_EST_CONT_CONV')
      .subscribe((res: any[]) => {
        this.estados = res;
        if (this.estados.length !== 0) {
          this.estados.sort((a: any, b: any) => {
            return a.maeDetalleId - b.maeDetalleId;
          });
        }
      });

    this.perfilService.getPerfiles({estadoRevision: Const.EST_PERFILES_REVISADO}).subscribe((res) => {
      this.perfiles = res;
      if (this.perfiles.length !== 0) {
        this.perfiles.sort((a, b) => {
          if (a.nombrePuesto < b.nombrePuesto) {
            return -1;
          }
          if (a.nombrePuesto > b.nombrePuesto) {
            return 1;
          }
          return 0;
        });
      }
    });

    this.maestraService
      .getMaestraDetalleByCodandCodProg('TIP_TRABAJO', 1)
      .subscribe((res) => {
        if (res.length !== 0) {
          this.filtros.tipo = res[0].maeDetalleId;
          this.getBandeja();
        }
      });
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getBandeja();
  }

  getBuscar() {
    this.page = 0;
    this.filtros.regimen = this.f.regimen.value;
    this.filtros.perfil = this.f.perfil.value;
    this.filtros.estado = this.f.estado.value;
    if (this.f.fecha.value !== null) {
      this.filtros.fecIni =
        this.f.fecha.value.start === undefined
          ? ''
          : moment(this.f.fecha.value.start).format('DD/MM/yyyy');
      this.filtros.fecFin =
        this.f.fecha.value.end === undefined
          ? ''
          : moment(this.f.fecha.value.end).format('DD/MM/yyyy');
    } else {
      this.filtros.fecIni = '';
      this.filtros.fecFin = '';
    }
    this.getBandeja();
  }

  getBandeja() {
    this.seguimientoService
      .getContratosOrConvenios(
        this.filtros.tipo,
        this.filtros.regimen,
        0,
        this.filtros.perfil,
        this.filtros.fecIni,
        this.filtros.fecFin,
        this.filtros.estado,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.total = res.total;
        this.contratos = res.items;
        let count: number = 1;
        if (this.contratos.length !== 0) {
          this.searchMode = true;
        }
        this.contratos.forEach((item) => {
          item.id = count++ + this.page * this.size;
          item.nombres = item.nombres !== null ? item.nombres : '';
          item.apellidos = item.apellidos !== null ? item.apellidos : '';
          item.codigoConvocatoria =
            item.codigoConvocatoria + ' | ' + item.regimen;
          item.nroDocumento =
            item.nroDocumento !== null ? item.nroDocumento : '';
          item.postulante =
            ((item.nombres + ' ' + item.apellidos).length > 18
              ? (item.nombres + ' ' + item.apellidos).substring(0, 18) + '... '
              : item.nombres + ' ' + item.apellidos + ' ,') +
            (item.tipoDoc === 1 ? 'DNI ' : ' CE ') +
            item.nroDocumento;
          item.fechaContrato =
            item.fechaContrato !== null
              ? moment(item.fechaContrato).format('DD/MM/YYYY')
              : item.fechaContrato;
        });
      });
  }

  clear() {
    this.initializeForm();
    this.getBuscar();
  }

  ver(e: any) {
    this.helperService.enviarContrato(
      e.postulanteSelId,
      e.estado,
      e.idContrato,
      e.codProEstado
    );
    this.router.navigateByUrl('pages/generacioncontrato/elaborar-contrato');
  }

  editar(e: any) {
    this.helperService.enviarContrato(
      e.postulanteSelId,
      e.estado,
      e.idContrato,
      e.codProEstado
    );
    this.router.navigateByUrl('pages/generacioncontrato/elaborar-contrato');
  }

  descargar(e: any) {
    const modal = this.dialog.open(ModalDescargaComponent, {
      data: {
        validador: 1,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        let fileName = e.archivoSuscrito.split('/').pop();

        this.maestraService
          .downloadBase64(e.archivoSuscrito)
          .subscribe((resp) => {
            let base64String = 'data:application/pdf;base64,' + resp;
            FileSaver.saveAs(base64String, fileName.split('.')[0]);
          });
      }
    });
  }

  subir(e) {
    const modal = this.dialog.open(ModalContratoComponent, {
      width: '400px',
      data: {
        titulo: 'Registro de comunicado',
        contratoId: e.idContrato,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.getBandeja();
        this.toast.showToast(
          'El contrato fue suscrito con éxito',
          'success',
          ''
        );
      }
    });
  }

  initializeColumns() {
    this.TableColumns = [
      {
        name: '#',
        dataKey: 'id',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CONVOCATORIA | REGIMEN',
        dataKey: 'codigoConvocatoria',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'DATOS PERSONALES DEL POSTULANTE',
        dataKey: 'postulante',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'PERFIL',
        dataKey: 'perfil',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'FECHA DE CONTRATO',
        dataKey: 'fechaContrato',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.contratos);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de contratos';
    model.headers = [
      '#',
      'CONVOCATORIA | REGIMEN',
      'DATOS PERSONALES DEL POSTULANTE',
      'PERFIL',
      'FECHA DE CONTRATO',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'codigoConvocatoria',
      'postulante',
      'perfil',
      'fechaContrato',
      'estado',
    ];
    return model;
  }

  validRangeDateFormat() {
    this.rangePickerStatus = 'basic';
    const periodo = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && periodo === null) {
      this.rangePickerStatus = 'danger';
    }
  }
}
