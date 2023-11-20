import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AdminComponent } from './admin/admin.component';
import { CoordinadorComponent } from './coordinador/coordinador.component';
import { CuadroMandoComponent } from './cuadro-mando.component';
import { GestorComponent } from './gestor/gestor.component';
import { ServirComponent } from './servir/servir.component';

const routes: Routes = [
  { path: '', component: CuadroMandoComponent },
  { path: 'gestor', component: GestorComponent },
  { path: 'coordinador', component: CoordinadorComponent },
  { path: 'admin', component: AdminComponent },
  { path: 'servir', component: ServirComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CuadroMandoRoutingModule {}
