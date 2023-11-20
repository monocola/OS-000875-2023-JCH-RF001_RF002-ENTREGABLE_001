import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EtapasComponent } from './etapas.component';
import { ResultadosPostulanteComponent } from './resultados-postulante/resultados-postulante.component';

const routes: Routes = [
  { path: '', component: EtapasComponent },
  { path: 'resultados-postulante', component: ResultadosPostulanteComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EtapasRoutingModule {}
