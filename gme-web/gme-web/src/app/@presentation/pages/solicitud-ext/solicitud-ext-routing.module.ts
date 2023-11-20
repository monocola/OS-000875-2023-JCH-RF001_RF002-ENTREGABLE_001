import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SolicitudExtComponent } from './solicitud-ext.component';
import { DetalleSolicitudExtComponent } from './detalle-solicitud-ext/detalle-solicitud-ext.component';

const routes: Routes = [
  { path: '', component: SolicitudExtComponent },
  { path: 'detalleSolicitudesExt/:solicitudExtId', component: DetalleSolicitudExtComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class SolicitudExtRoutingModule { }
