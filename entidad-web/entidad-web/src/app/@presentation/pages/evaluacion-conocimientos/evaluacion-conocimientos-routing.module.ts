import { VisualizarExamenComponent } from './examenes/visualizar-examen/visualizar-examen.component';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { EvaluacionConocimientosComponent } from './evaluacion-conocimientos.component';
import { ExamenesComponent } from './examenes/examenes.component';
import { CategoriaComponent } from './categoria/categoria.component';
import { CategoriaAddComponent } from './categoria/add/addcategoria.component';
import { RegistroComponent } from './examenes/registro/registro.component';
import { ProgramarComponent } from './programar/programar.component';
import { NuevoGrupoComponent } from './programar/nuevo-grupo/nuevogrupo-component';
import { ProgEvaluacionComponent } from './programar/crear/progeva-component';
import { ExamenVirtualComponent } from './programar/crear/examen-virtual/examen-virtual/examen-virtual.component';
import { ExamenVirtualCabeceraComponent } from './programar/crear/examen-virtual/examen-virtual-cabecera/examen-virtual-cabecera.component';
import { ExamenVirtualResultadosComponent } from './programar/crear/examen-virtual/examen-virtual-resultados/examen-virtual-resultados.component';


const routes: Routes = [
  { path: '', component: EvaluacionConocimientosComponent },
  { path: 'examenes', component: ExamenesComponent },
  { path: 'examenes/crear-registro', component: RegistroComponent },
  { path: 'examenes/editar-registro/:idEdicion', component: RegistroComponent },
  { path: 'categorias', component: CategoriaComponent },
  { path: 'categorias/addcategorias', component: CategoriaAddComponent },
  { path: 'programar', component: ProgramarComponent },
  { path: 'programar/nuevo-grupos', component: NuevoGrupoComponent },
  {
    path: 'programar/grupos',
    component: ProgEvaluacionComponent,
  },
  {
    path: 'programar/grupos/examen-virtual',
    component: ExamenVirtualCabeceraComponent,
  },
  {
    path: 'programar/grupos/examen-virtual/preguntas',
    component: ExamenVirtualComponent,
  },
  {
    path: 'programar/grupos/examen-virtual/resultados',
    component: ExamenVirtualResultadosComponent,
  },
  {
    path: 'examenes/visualizar-examen', component: VisualizarExamenComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EvaluacionConocimientosRoutingModule {}
