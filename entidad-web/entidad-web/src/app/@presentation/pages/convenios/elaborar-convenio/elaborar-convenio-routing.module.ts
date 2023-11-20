import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ElaborarConvenioComponent } from './elaborar-convenio.component';

const routes: Routes = [{ path: '', component: ElaborarConvenioComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ElaborarConvenioRoutingModule { }
