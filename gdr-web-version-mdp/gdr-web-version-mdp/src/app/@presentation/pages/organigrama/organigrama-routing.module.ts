import { NgModule } from '@angular/core';
import { OrganigramaComponent } from './organigrama.component';
import { RouterModule, Routes } from '@angular/router';
import { VistaComponent } from './vista/vista.component';


const routes: Routes = [
  { path: '', component: OrganigramaComponent },
  { path: 'vista', component: VistaComponent },
  { path: 'vista/:id', component: VistaComponent },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class OrganigramaRoutingModule {}
