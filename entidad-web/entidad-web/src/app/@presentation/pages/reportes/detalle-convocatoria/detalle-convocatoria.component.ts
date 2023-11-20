import { filter } from 'rxjs/operators';
import { ConvocatoriaDataService } from '../convocatorias/convocatoria-data.service';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Component, OnInit } from '@angular/core';
import { Router, NavigationEnd } from '@angular/router';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { ReportesRepository } from 'src/app/@domain/repository/reportes.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { MatDialog } from '@angular/material/dialog';
import { ReporteDetalleConvocatoriaRepository } from 'src/app/@domain/repository/reporte-detalle-convocatoria.repository';

@Component({
  selector: 'serv-talento-detalle-convocatoria',
  templateUrl: './detalle-convocatoria.component.html',
  styleUrls: ['./detalle-convocatoria.component.scss'],
})
export class DetalleConvocatoriaComponent implements OnInit {
  columns: TableColumn[];
  data: any[] = [];
  detalleConvocatoria: any;
  detalleCronograma: any;
  vacantes: any;
  lstDetallePerfiles: any;
  baseId: number;
  constructor(
    private router: Router,
    private dialog: MatDialog,
    private reporteDetConvocatoriaService: ReporteDetalleConvocatoriaRepository,
    private convocatoriaDataService: ConvocatoriaDataService,
    private dataService: ConvocatoriaDataService,
    private srvReportesRepository: ReportesRepository
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
    this.initializeData();
    this.detalleConvocatoria = this.dataService.objConv;
  }

  back() {
    this.router.navigateByUrl('pages/reportes');
  }

  initializeData() {
    this.reporteDetConvocatoriaService
      .getListaDetalleConvocatoria(this.convocatoriaDataService.convocatoriaId)
      .subscribe((mc) => {
        this.detalleCronograma = mc;
      });

    this.reporteDetConvocatoriaService
      .getVacantesEnEtapaActual(this.convocatoriaDataService.convocatoriaId)
      .subscribe((mc) => {
        this.vacantes = mc;
      });

    this.reporteDetConvocatoriaService
      .getPerfilPuestoDetallePorConvocatoria(this.convocatoriaDataService.convocatoriaId)
      .subscribe((mc) => {
        this.lstDetallePerfiles = mc;
        let totalVac: number = 0;
        let totalPost: number = 0;
        let totalCalif: number = 0;

        this.lstDetallePerfiles.forEach((element) => {
          if (element.postulantes === null) {
            element.postulantes = 0;
          }
          totalVac += element.vacantes == null ? 0 : element.vacantes;
          totalPost += element.postulantes == null ? 0 : element.postulantes;
          totalCalif += element.califican == null ? 0 : element.califican;
        });

        let objDetalle = {
          perfilId: 0,
          nombrePuesto: 'TOTAL',
          departamento: '',
          remuneracion: '',
          vacantes: totalVac,
          postulantes: totalPost,
          califican: totalCalif,
        };

        this.lstDetallePerfiles.push(objDetalle);
      });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  descargarPDF() {
    let request = {
      codigo: this.detalleCronograma?.codigoConvocatoria,
      regimen: this.detalleConvocatoria?.regimen,
      modalidad: this.detalleConvocatoria?.modalidad,
      tipo: this.detalleConvocatoria?.tipo,
      condicion: 'MINIMA',
      gestor: this.detalleConvocatoria?.nombreGestor,
      coordinador: this.detalleConvocatoria?.nombreCoordinador,
    };
    this.baseId = this.dataService.convocatoriaId;

    this.srvReportesRepository.getDescarga(request, this.baseId).subscribe((res) => {
        this.dialog.open(FileVisualizerComponent, {
          data: {
            base64String: res,
            filename: 'convocatoria',
            extension: 'pdf',
          },
        });
      });
  }

  exportData() {}
}
