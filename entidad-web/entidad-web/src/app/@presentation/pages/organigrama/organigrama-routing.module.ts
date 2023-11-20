import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ConfiguracionComponent } from './configuracion/configuracion.component';
import { OrganigramaComponent } from './organigrama.component';
import { VistaComponent } from './vista/vista.component';

const routes: Routes = [
  { path: '', component: OrganigramaComponent },
  { path: 'configuracion', component: ConfiguracionComponent },
  { path: 'vista', component: VistaComponent },
  { path: 'vista/:id', component: VistaComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class OrganigramaRoutingModule {}
