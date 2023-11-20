import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { SeguimientoComunicadoComponent } from '../seguimiento-comunicado/seguimiento-comunicado.component';
import { CronogramaComponent } from '../seguimiento/cronograma/cronograma.component';
import { SeguimientoProcesoComponent } from './seguimiento-proceso.component';

const routes: Routes = [
  { path: '', component: SeguimientoProcesoComponent },
  {
    path: 'comunicado',
    component: SeguimientoComunicadoComponent,
  },
  {
    path: 'cronograma',
    component: CronogramaComponent,
  },
  // { path: ':idExperiencia',  component: DetalleExperienciaComponent }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SeguimientoProcesoRoutingModule {}
