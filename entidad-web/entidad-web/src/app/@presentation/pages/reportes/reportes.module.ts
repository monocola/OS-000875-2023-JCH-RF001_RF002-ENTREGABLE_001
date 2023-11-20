import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReportesRoutingModule } from './reportes-routing.module';
import { ReportesComponent } from './reportes.component';
import { MatDividerModule } from '@angular/material/divider';
import { MatTabsModule } from '@angular/material/tabs';
import { ConvocatoriasComponent } from './convocatorias/convocatorias.component';
import { PostulantesComponent } from './postulantes/postulantes.component';
import { EvaluacionesComponent } from './evaluaciones/evaluaciones.component';
import {
  NbAutocompleteModule,
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { DetalleConvocatoriaComponent } from './detalle-convocatoria/detalle-convocatoria.component';
import { TableDetalleConvocatoriaComponent } from './detalle-convocatoria/table-detalle-convocatoria/table-detalle-convocatoria.component';
import { DetalleEvaluacionesComponent } from './detalle-evaluaciones/detalle-evaluaciones.component';
import { TableReporteConvocatoriaComponent } from './convocatorias/table-reporte-convocatoria/table-reporte-convocatoria.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { TableReporteEvaluacionesComponent } from './evaluaciones/table-reporte-evaluaciones/table-reporte-evaluaciones.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { TableReportePostulanteComponent } from './postulantes/table-reporte-postulante/table-reporte-postulante.component';

@NgModule({
  declarations: [ReportesComponent, ConvocatoriasComponent, PostulantesComponent, EvaluacionesComponent, DetalleConvocatoriaComponent, TableDetalleConvocatoriaComponent, TableReporteConvocatoriaComponent, TableReportePostulanteComponent, TableReporteEvaluacionesComponent, DetalleEvaluacionesComponent],
  imports: [
    CommonModule,
    ReportesRoutingModule,
    MatDividerModule,
    MatTabsModule,
    NbFormFieldModule,
    NbSelectModule,
    NbInputModule,
    FormsModule,
    ReactiveFormsModule,
    NbButtonModule,
    NbDatepickerModule,
    NbIconModule,
    MatIconModule,
    MatTableModule,
    CommonComponentsModule,
    NbAutocompleteModule,
    MatPaginatorModule,
  ]
})
export class ReportesModule { }
