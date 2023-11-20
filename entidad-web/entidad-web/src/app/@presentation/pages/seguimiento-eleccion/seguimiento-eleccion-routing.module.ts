import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SeguimientoEleccionComponent } from './seguimiento-eleccion.component';

const routes: Routes = [
  { path: '', component: SeguimientoEleccionComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SeguimientoEleccionRoutingModule {}
