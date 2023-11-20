import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { GestoresOrhComponent } from './gestores-orh.component';

const routes: Routes = [
  { path: '', component: GestoresOrhComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class GestoresOrhRoutingModule { }
