import { NgModule } from '@angular/core';
import { ServidoresComponent } from './servidores.component';
import { RouterModule, Routes } from '@angular/router';
import { EditarServidorComponent } from './editar-servidor/editar-servidor.component';

const routes: Routes = [
  { path: '', component: ServidoresComponent },
  { path: 'editar', component: EditarServidorComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServidoresRoutingModules {}
