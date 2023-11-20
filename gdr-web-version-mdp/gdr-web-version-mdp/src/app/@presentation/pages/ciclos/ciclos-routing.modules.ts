import { NgModule } from '@angular/core';
import { CiclosComponent } from './ciclos.component';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  { path: '', component: CiclosComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CiclosRoutingModules {}
