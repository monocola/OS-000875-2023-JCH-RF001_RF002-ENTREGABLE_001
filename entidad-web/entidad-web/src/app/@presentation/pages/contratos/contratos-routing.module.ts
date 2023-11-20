import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ContratosComponent } from './contratos.component';
import { ElaborarContratoComponent } from './elaborar-contrato/elaborar-contrato.component';

const routes: Routes = [
  { path: '', component: ContratosComponent },
  { path: 'elaborar-contrato', component: ElaborarContratoComponent },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ContratosRoutingModule { }
