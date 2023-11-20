import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ElaborarContratoComponent } from './elaborar-contrato.component';

const routes: Routes = [{ path: '', component: ElaborarContratoComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ElaborarContratoRoutingModule { }
