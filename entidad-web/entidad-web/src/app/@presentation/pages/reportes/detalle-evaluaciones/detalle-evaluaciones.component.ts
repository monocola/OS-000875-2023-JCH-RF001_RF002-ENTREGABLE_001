import { FileVisualizerComponent } from './../../../@common-components/file-visualizer/file-visualizer.component';
import { filter } from 'rxjs/operators';
import { ReporteEvaluacionesRepository } from 'src/app/@domain/repository/reporte-evaluaciones.repository';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd } from '@angular/router';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { ResultadosPostulanteRepository } from 'src/app/@domain/repository/resultados-postulante.repository';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-detalle-evaluaciones',
  templateUrl: './detalle-evaluaciones.component.html',
  styleUrls: ['./detalle-evaluaciones.component.scss'],
})
export class DetalleEvaluacionesComponent implements OnInit {
  columns: TableColumn[];
  data: any[] = [];

  convocatoriaPostulanteId: number = 0;
  baseId: number;

  lstIndicadoresPosulante: any[] = [];
  datosPostulante: any;

  rowEvaluacion: any;
  infoPostulante: any;

  constructor(
    private router: Router,
    private dialog: MatDialog,
    private reporteEvaluacionesRepository: ReporteEvaluacionesRepository,
    private resultadosPostulanteRepository: ResultadosPostulanteRepository
  ) {
    this.router.events
      .pipe(filter((rs): rs is NavigationEnd => rs instanceof NavigationEnd))
      .subscribe((event) => {
        if (event.id === 1 && event.url === event.urlAfterRedirects) {
          this.back();
        }
      });
  }

  ngOnInit(): void {
    this.rowEvaluacion = JSON.parse(sessionStorage.getItem('rowEvaluacion'));
    if (this.rowEvaluacion) {
      this.convocatoriaPostulanteId = Number(this.rowEvaluacion.postulanteId);
    }
    this.initializeColumns();
    this.initializeData();
  }

  back() {
    this.router.navigateByUrl('pages/reportes');
  }

  initializeColumns() {
    this.columns = [];
  }

  initializeData() {
    this.reporteEvaluacionesRepository
      .getIndicadoresPostulante(this.convocatoriaPostulanteId)
      .subscribe((mc) => {
        if (mc) {
          let objIndicadorRNSSC = {
            descripcion: mc.nameRnssc,
            validacion: mc.flagRnssc,
            fecha: mc.fechaRnssc,
          };

          let objIndicadorREDERECI = {
            descripcion: mc.nameRederesi,
            validacion: mc.flagRederesi,
            fecha: mc.fechaRederesi,
          };

          let objIndicadorREDAM = {
            descripcion: mc.nameRedam,
            validacion: mc.flagReam,
            fecha: mc.fechaRedam,
          };

          this.lstIndicadoresPosulante.push(objIndicadorRNSSC);
          this.lstIndicadoresPosulante.push(objIndicadorREDERECI);
          this.lstIndicadoresPosulante.push(objIndicadorREDAM);
        }
      });

    this.resultadosPostulanteRepository
      .obtenerDatosPostulanteSeleccion(this.convocatoriaPostulanteId)
      .subscribe((mc) => {
        this.datosPostulante = mc;
      });
console.info(this.rowEvaluacion);
    this.reporteEvaluacionesRepository
      .getinfoevaluacionpostulante(
        this.rowEvaluacion.baseId,
        this.rowEvaluacion.perfilId,
        this.convocatoriaPostulanteId
      )
      .subscribe((mc) => {
        // this.reporteEvaluacionesRepository.getinfoevaluacionpostulante(56,10,62).subscribe((mc) => {
        this.infoPostulante = mc;
      });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  descargarPDF() {
    this.reporteEvaluacionesRepository
      .getDescargaPDF(
        this.convocatoriaPostulanteId,
        this.rowEvaluacion.perfilId,
        this.rowEvaluacion.postulanteId
      )
      .subscribe((res) => {
        // this.reporteEvaluacionesRepository.getDescargaPDF(56,10,62).subscribe((res) => {
        this.dialog.open(FileVisualizerComponent, {
          data: {
            base64String: res,
            filename: 'convocatoria',
            extension: 'pdf',
          },
        });
      });
  }
}
