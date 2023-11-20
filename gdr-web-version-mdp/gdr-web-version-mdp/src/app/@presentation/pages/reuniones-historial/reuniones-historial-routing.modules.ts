import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HistorialReunionesEvaluadoComponent } from './historial-reuniones-evaluado/historial-reuniones-evaluado.component';
import { HistorialReunionesComponent } from './historial-reuniones/historial-reuniones.component';
import { ReunionesHistorialComponent } from './reuniones-historial.component';

const routes: Routes = [
 { path: '', component: HistorialReunionesEvaluadoComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ReunionesHistorialRoutingModules {}
