import { DetalleConvocatoriaComponent } from './detalle-convocatoria/detalle-convocatoria.component';
import { DetalleEvaluacionesComponent } from './detalle-evaluaciones/detalle-evaluaciones.component';
import { ReportesComponent } from '../reportes/reportes.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  { path: '', component: ReportesComponent },
  { path: 'detalleconvocatoria', component: DetalleConvocatoriaComponent},
  { path: 'detalle-evaluaciones', component: DetalleEvaluacionesComponent}
 ];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ReportesRoutingModule {}
