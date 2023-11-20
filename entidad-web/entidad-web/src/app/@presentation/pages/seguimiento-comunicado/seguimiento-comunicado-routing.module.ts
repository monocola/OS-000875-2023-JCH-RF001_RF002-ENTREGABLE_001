import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { RegistroComunicadoComponent } from './registro-comunicado/registro-comunicado.component';
import { SeguimientoComunicadoComponent } from './seguimiento-comunicado.component';

const routes: Routes = [
  { path: '', component: SeguimientoComunicadoComponent },
  { path: 'comunicado/registro', component: RegistroComunicadoComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SeguimientoComunicadoRoutingModule {}
