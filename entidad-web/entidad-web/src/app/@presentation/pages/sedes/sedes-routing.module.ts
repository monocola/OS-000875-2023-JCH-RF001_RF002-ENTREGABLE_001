import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { SedesComponent } from './sedes.component';

const routes: Routes = [
  { path: '', component: SedesComponent }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class SedesRoutingModule { }
