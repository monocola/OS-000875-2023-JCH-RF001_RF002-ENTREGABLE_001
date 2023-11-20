import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AdministradorSolicitudesComponent } from './administrador-solicitudes.component';
import { DetailComponent } from './detail/detail.component';

const routes: Routes = [
  { path: '', component: AdministradorSolicitudesComponent },
  { path: ':id', component: DetailComponent }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AdministradorSolicitudesRoutingModule { }
