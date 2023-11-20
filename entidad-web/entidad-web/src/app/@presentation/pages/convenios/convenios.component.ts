import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Sort } from '@angular/material/sort';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { ModalContratoComponent } from '../contratos/modal-contrato/modal-contrato.component';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Router } from '@angular/router';
import moment from 'moment';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { ConveniosService } from './convenios.service';
import { ModalDescargaComponent } from '.././contratos/modal-descarga/modal-descarga.component';
import FileSaver from 'file-saver';

@Component({
  selector: 'serv-talento-convenios',
  templateUrl: './convenios.component.html',
  styleUrls: ['./convenios.component.scss'],
})
export class ConveniosComponent implements OnInit {
  filterForm: FormGroup;
  TableColumns: TableColumn[];

  searchMode = false;

  practicas = [];
  estados = [];

  convenios = [];

  page: number = 0;
  size: number = 50;
  total: number = 0;
  filtros: any = {
    tipo: 0,
    tipoPractica: 0,
    estado: 0,
    fecIni: '',
    fecFin: '',
  };
  rangePickerStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    private perfilService: PerfilesRepository,
    private dialog: MatDialog,
    private toast: ToastService,
    public router: Router,
    private seguimientoService: SeguimientoRepository,
    private helpService: ConveniosService
  ) {}

  ngOnInit(): void {
    this.helpService.initializeValues();
    this.initializeForm();
    this.initializeColumns();
    this.getCombox();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      practica: 0,
      estado: 0,
      fecha: null,
    });
  }

  getCombox() {
    this.maestraService
      .getMaestraDetalleByCod('TBL_PER_TIP_PRA')
      .subscribe((res) => {
        this.practicas = res;
      });

    this.maestraService
      .getMaestraDetalleByCod('TBL_EST_CONT_CONV')
      .subscribe((res) => {
        this.estados = res;
      });

    this.maestraService
      .getMaestraDetalleByCodandCodProg('TIP_TRABAJO', 2)
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
    this.filtros.tipoPractica = this.f.practica.value;
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
    }

    this.getBandeja();
  }

  getBandeja() {
    this.seguimientoService
      .getContratosOrConvenios(
        this.filtros.tipo,
        0,
        this.filtros.tipoPractica,
        0,
        this.filtros.fecIni,
        this.filtros.fecFin,
        this.filtros.estado,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.total = res.total;
        this.convenios = res.items;
        let count: number = 1;
        if (this.convenios.length !== 0) {
          this.searchMode = true;
        }
        this.convenios.forEach((item) => {
          item.id = count++ + this.page * this.size;
          item.nombres = item.nombres !== null ? item.nombres : '';
          item.apellidos = item.apellidos !== null ? item.apellidos : '';
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
    this.helpService.enviarContrato(
      e.postulanteSelId,
      e.estado,
      e.idContrato,
      e.codProEstado
    );
    this.router.navigateByUrl('pages/generacionconvenio/elaborar-convenio');
  }

  editar(e: any) {
    this.helpService.enviarContrato(
      e.postulanteSelId,
      e.estado,
      e.idContrato,
      e.codProEstado
    );
    this.router.navigateByUrl('pages/generacionconvenio/elaborar-convenio');
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
        titulo: 'Registro de convenio',
        contratoId: e.idContrato,
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res !== false) {
        this.getBandeja();
        this.toast.showToast(
          'El convenio fue suscrito con éxito',
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
        name: 'FECHA DE CONVENIO',
        dataKey: 'fechaContrato',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.convenios);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de contratos';
    model.headers = [
      '#',
      'CONVOCATORIA | REGIMEN',
      'DATOS PERSONALES DEL POSTULANTE',
      'PERFIL',
      'FECHA',
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

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }
}
