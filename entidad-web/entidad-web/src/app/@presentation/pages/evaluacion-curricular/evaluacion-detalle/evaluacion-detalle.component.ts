import { Component, OnInit } from '@angular/core';

import { FormGroup, FormControl } from '@angular/forms';
import { TableColumn } from '../../../../@presentation/@common-components/material-table/table-column';
import { Router } from '@angular/router';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from 'src/app/utils/general';
import { MatDialog } from '@angular/material/dialog';
import { FileVisualizerComponent } from '../../../@common-components/file-visualizer/file-visualizer.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalRedereciComponent } from './modal-redereci/modal-redereci.component';

@Component({
  selector: 'serv-talento-evaluacion-detalle',
  templateUrl: './evaluacion-detalle.component.html',
  styleUrls: ['./evaluacion-detalle.component.scss'],
})
export class EvaluacionDetalleComponent implements OnInit {
  conocimientoTableColumns: TableColumn[];
  data: any[] = [];
  comboEstados: any[] = [];
  comboPerfilPuesto: any[] = [];
  itemEvuluacionesCurri: any[] = [];
  idBase: any;
  convocatoriaSelId: any;
  estadoEvaluacion: any;
  nombres: any;
  nroDocumento: any;
  filterForm: FormGroup;
  selectedEvaluacion: any;
  califica: number = 0;
  noCalifica: number = 0;
  total: number = 0;
  mostrarFiltroCompletos: boolean = false;
  page: number = 0;
  size: number = 10;

  constructor(
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    private router: Router,
    private toast: ToastService,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.selectedEvaluacion = JSON.parse(
      localStorage.getItem('selectedEvaluacion')
    );
    this.idBase = this.selectedEvaluacion.idBase;
    this.initializeFilterForm();
    this.listarEstadosCombocatorias();
    this.listarPerfilPuestobyConvocatoria();
    this.initializeColumns();
  }

  initializeFilterForm() {
    this.filterForm = new FormGroup({
      nombres: new FormControl(''),
      nroDocumento: new FormControl(''),
      perfilPuesto: new FormControl(''),
      estadoCurricular: new FormControl(''),
    });
  }

  initializeColumns(): void {
    this.conocimientoTableColumns = [
      {
        name: '#',
        dataKey: 'index',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'NOMBRES Y APELLIDOS',
        dataKey: 'nombres',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'PERFIL',
        dataKey: 'nombrePuesto',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'PUNTOS',
        dataKey: 'puntaje',
        position: 'center',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estadoEvaluacion',
        position: 'center',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  get f() {
    return this.filterForm.value;
  }

  limpiar(): void {
    this.mostrarFiltroCompletos = false;
    this.filterForm.reset();
    this.data = [];
  }

  buscar(): void {
    this.listarConvocatoriasPostulante();
  }

  viewDetailEvaluacion(event: any): void {
    localStorage.setItem(
      'convocatoriaPostulanteSelected',
      JSON.stringify({
        nombres: event.nombreApellido,
        nroDocumento: event.nroDocumento,
        postulanteId: event.postulanteId,
        nombrePuesto: event.nombrePuesto,
        convocatoriaPostulanteId: event.convocatoriaPostulanteId,
        idPerfil: event.idPerfil,
        convocatoria: this.selectedEvaluacion.convocatoria,
        regimen: this.selectedEvaluacion.regimen,
        idBase: this.selectedEvaluacion.idBase,
      })
    );

    this.router.navigateByUrl(
      'pages/evaluacion-curricular/evaluacion-perfil-detalle'
    );
  }

  viewOtrosRoles() {
    this.router.navigateByUrl('pages/evaluacion-curricular/otros-roles');
  }

  listarEstadosCombocatorias(): void {
    let flag = 'ESTADO_CURRICULAR';
    this.evaluacionCurricularService
      .listarEstadoEvaluacionCurricular(flag)
      .subscribe((res: any) => {
        this.comboEstados = res;
      });
  }

  listarConvocatoriasPostulante(): void {
    this.nombres = this.filterForm.controls['nombres'].value;
    this.nroDocumento = this.filterForm.controls['nroDocumento'].value;
    this.convocatoriaSelId = this.filterForm.controls['perfilPuesto'].value;
    this.estadoEvaluacion = this.filterForm.controls['estadoCurricular'].value;
    if (this.convocatoriaSelId !== "" || (this.estadoEvaluacion !== ""
          || this.nroDocumento !== "" || this.estadoEvaluacion !== "" )) {
      const request = {
        trace: {
          traceId: 'string',
        },
        payload: {
          convocatoriaSelId: this.convocatoriaSelId,
          estadoEvaluacion: this.estadoEvaluacion,
          nombres: this.nombres,
          nroDocumento: this.nroDocumento,
        },
      };

      this.evaluacionCurricularService
        .listarConvocatoriasPostulante(request)
        .subscribe((res: any) => {
          this.califica = res.califica;
          this.noCalifica = res.noCalifica;
          // this.total = res.total;
          this.total = res.count;
          this.data = res.listaDetalleBandeja;
          this.data.forEach((item: any, index: number) => {
            this.mostrarFiltroCompletos = true;
            item.index = index + 1;
            item.redereci = item.redereci === '1';
            item.nombreApellido = item.nombres;
            item.nombres =
              item.nombres +
              ' ' +
              (item.tipoDocumento === 1 ? 'DNI' : 'C.E.') +
              ' ' +
              item.nroDocumento;
            item.estadoEvaluacion = item.descripcionEstadoEvaluacion;
            item.tipoDocumento = item.tipoDocumento === 1 ? 'DNI' : 'C.E.';
          });
        });
    } else {
      this.toast.showToast('Seleccione una opción de busqueda', 'warning', 'Atención');
    }
  }

  getCalificaTextt(estado: any) {
    if (estado === null || estado === undefined) {
      return '';
    }

    if (estado === 0 || estado === '0') {
      return 'NO CALIFICA';
    } else if (estado === 1 || estado === '1') {
      return 'CALIFICA';
    } else if (estado === 2 || estado === '2') {
      return 'DESCALIFICADO';
    } else if (estado === 3 || estado === '3') {
      return 'NO ASISTIO';
    } else {
      return '';
    }
  }

  listarPerfilPuestobyConvocatoria(): void {
    this.evaluacionCurricularService
      .listarPerfilPuestobyConvocatoria(this.idBase)
      .subscribe((res: any) => {
        this.comboPerfilPuesto = res;
      });
  }

  back() {
    this.router.navigateByUrl('pages/evaluacion-curricular');
  }

  getDataExportEvaluaciones(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista completa de postulantes';
    model.headers = [
      '#',
      'NOMBRES Y APELLIDOS',
      'PERFIL',
      'PUNTOS',
      'REDERECI',
      'ESTADO',
    ];
    model.keys = [
      'index',
      'nombres',
      'nombrePuesto',
      'puntaje',
      'redereci',
      'estadoEvaluacion',
    ];

    return model;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  viewPdf(event: any) {
    this.gestionPDF(event.convocatoriaPostulanteId);
  }

  gestionPDF(convocatoriaPostulanteId: any) {
    this.evaluacionCurricularService
      .getPDFEvaluacionCurricular(convocatoriaPostulanteId)
      .subscribe((res) => {
        if ( res ) {
          this.dialog.open(FileVisualizerComponent, {
            data: {
              base64String: res,
              filename: 'EC_' + convocatoriaPostulanteId + this.nombres,
              extension: 'pdf',
            },
          });
        } else {
          this.toast.showToast("El postulante no ha sido calificado correctamente", 'danger', 'Atención');
        }
      });
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
  }

  changeRedereci(e: any) {
    const modalRedereci = this.dialog.open(ModalRedereciComponent, {
      width: '40rem',
      data: {
        nombreApellido: e.nombreApellido,
        redereci: e.redereci,
        convocatoriaPostulante: e.convocatoriaPostulanteId,
      },
    });
    modalRedereci.afterClosed().subscribe((res) => {
      if (res) {
        this.toast.showToast('Registro exitoso', 'success', 'Atención');
      } else {
        let redereci = !e.redereci;
        e.redereci = redereci;
      }
    });
  }
}
