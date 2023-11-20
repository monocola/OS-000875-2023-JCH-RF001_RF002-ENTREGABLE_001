import { ListaEvaluacionesComponent } from '../lista-evaluaciones/lista-evaluaciones.component';
import { ListaGestionNotasComponent } from '../lista-evaluaciones/lista-gestion-notas/lista-gestion-notas.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ResumenEvaluacionesComponent } from '../lista-evaluaciones/resumen-evaluaciones/resumen-evaluaciones.component';

const routes: Routes = [
  { path: '', component: ListaEvaluacionesComponent },
  {
    path: 'lista-evaluaciones',
    component: ListaEvaluacionesComponent,
  },
  {
    path: 'lista-gestion-notas',
    component: ListaGestionNotasComponent,
  },
  {
    path: 'resumen-evaluacion-notas',
    component: ResumenEvaluacionesComponent,
  },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ListaEvaluacionesRoutingModule {}
