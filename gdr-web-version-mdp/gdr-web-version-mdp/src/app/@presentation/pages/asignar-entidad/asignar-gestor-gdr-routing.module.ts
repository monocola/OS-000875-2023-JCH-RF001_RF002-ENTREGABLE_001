import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AsignarGestorGdrComponent } from './asignar-gestor-gdr.component';

const routes: Routes = [
  { path: '', component: AsignarGestorGdrComponent },
];
 
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AsignarGestorGdrRoutingModule { }
