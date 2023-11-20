import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { EntidadPerfilComponent } from './entidad-perfil.component';

const routes: Routes = [{ path: '', component: EntidadPerfilComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EntidadPerfilRoutingModule {}
