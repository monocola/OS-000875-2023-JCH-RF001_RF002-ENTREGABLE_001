import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { AdministratorComponent } from './administrator.component';

const routes: Routes = [
  { path: '', component: AdministratorComponent },
  { path: ':codeSecurity/:idRequest', component: AdministratorComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class AdministratorRoutingModule { }
