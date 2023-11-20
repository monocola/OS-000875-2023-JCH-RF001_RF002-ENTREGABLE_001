import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EvaluadosComponent } from './evaluados/evaluados.component';
import { MetasComponent } from './metas/metas.component';
import { ParticipantesComponent } from './participantes.component';
import { AgregarMetaComponent } from './agregar-meta/agregar-meta.component';
import { EditarMetaComponent } from './editar-meta/editar-meta.component';


const routes: Routes = [
  { path: '', component: ParticipantesComponent },
  { path: 'evaluados', component: EvaluadosComponent },
  { path: 'evaluados/metas', component: MetasComponent},
  { path: 'evaluados/metas/agregar-meta', component: AgregarMetaComponent},
  { path: 'evaluados/metas/:metaId', component: EditarMetaComponent}, // idMeta

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})

export class ParticipantesRoutingModules {}
