import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ConveniosComponent } from './convenios.component';
import { ElaborarConvenioComponent } from './elaborar-convenio/elaborar-convenio.component';

const routes: Routes = [
  { path: '', component: ConveniosComponent },
  { path: 'elaborar-convenio', component: ElaborarConvenioComponent },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ConveniosRoutingModule { }
