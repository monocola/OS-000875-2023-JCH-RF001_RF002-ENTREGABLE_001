import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ConfiguracionEvaluacionComponent } from './configuracion-evaluacion/configuracion-evaluacion.component';
import { EvaluacionServirComponent } from './evaluacion-servir.component';

const routes: Routes = [
  { path: '', component: EvaluacionServirComponent },
  { path: 'configuracion', component: ConfiguracionEvaluacionComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EvaluacionServirRoutingModule {}
