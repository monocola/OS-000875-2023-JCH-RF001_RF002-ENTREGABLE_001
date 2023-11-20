import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { EvaluacionCurricularComponent } from './evaluacion-curricular.component';
import { EvaluacionDetalleComponent } from './evaluacion-detalle/evaluacion-detalle.component';
import { EvaluacionPerfilDetalleComponent } from './evaluacion-perfil-detalle/evaluacion-perfil-detalle.component';

const routes: Routes = [
  { path: '', component: EvaluacionCurricularComponent },
  { path: 'evaluacion-detalle', component: EvaluacionDetalleComponent },
  { path: 'evaluacion-perfil-detalle', component: EvaluacionPerfilDetalleComponent}
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EvaluacionCurricularRoutingModule {}
