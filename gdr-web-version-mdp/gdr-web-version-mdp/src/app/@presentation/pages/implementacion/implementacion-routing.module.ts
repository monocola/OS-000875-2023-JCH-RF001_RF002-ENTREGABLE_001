import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ImplementacionComponent } from './implementacion.component';

const routes: Routes = [
  { path: '', component: ImplementacionComponent },
];
  
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ImplementacionRoutingModule { }
