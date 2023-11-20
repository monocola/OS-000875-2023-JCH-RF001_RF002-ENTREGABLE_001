import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AdministrarEvaluacionesComponent } from './administrar-evaluaciones/administrar-evaluaciones.component';
import { EvaluacionEntidadComponent } from './evaluacion-entidad.component';

const routes: Routes = [
  { path: '', component: EvaluacionEntidadComponent },
  { path: 'edicion', component: AdministrarEvaluacionesComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EvaluacionEntidadRoutingModule {}
