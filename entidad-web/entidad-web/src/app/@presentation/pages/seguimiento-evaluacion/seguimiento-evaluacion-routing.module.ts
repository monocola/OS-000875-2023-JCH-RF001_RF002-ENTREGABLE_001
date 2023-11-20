import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SeguimientoEvaluacionComponent } from './seguimiento-evaluacion.component';

const routes: Routes = [
  { path: '', component: SeguimientoEvaluacionComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SeguimientoEvaluacionRoutingModule {}
