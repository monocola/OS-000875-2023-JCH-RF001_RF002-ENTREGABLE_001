import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BonificacionesComponent } from './bonificaciones.component';
import { CreacionBonificacionComponent } from './creacion-bonificacion/creacion-bonificacion.component';
import { GestionBonificacionComponent } from './gestion-bonificacion/gestion-bonificacion.component';

const routes: Routes = [
  { path: '', component: BonificacionesComponent },
  { path: 'creacion', component: CreacionBonificacionComponent },
  { path: 'gestion', component: GestionBonificacionComponent },
  { path: 'gestion/:bonificacionId', component: GestionBonificacionComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class BonificacionesRoutingModule {}
