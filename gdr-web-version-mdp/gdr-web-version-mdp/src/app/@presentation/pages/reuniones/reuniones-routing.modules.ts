import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HistorialReunionesComponent } from './historial-reuniones/historial-reuniones.component';
import { ReunionesComponent } from './reuniones.component';

const routes: Routes = [
  { path: '', component: ReunionesComponent },
  { path: ':evaluadoDetalleUoId/:detalleUoId', component: HistorialReunionesComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ReunionesRoutingModules {}
