import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { FactoresEvaluacionComponent } from './factores-evaluacion.component';

const routes: Routes = [
  { path: '', component: FactoresEvaluacionComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})

export class FactoresEvaluacionRoutingModules {}
