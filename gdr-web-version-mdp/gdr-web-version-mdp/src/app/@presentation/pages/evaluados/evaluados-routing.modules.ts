import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EvaluadosComponent } from './evaluados.component';

const routes: Routes = [
  { path: '', component: EvaluadosComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})

export class EvaluadosRoutingModules {}
